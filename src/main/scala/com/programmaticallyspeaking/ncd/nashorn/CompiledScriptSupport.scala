package com.programmaticallyspeaking.ncd.nashorn

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent, ValueNode}
import com.programmaticallyspeaking.ncd.infra.{CompiledScript, Hasher}
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try

trait CompiledScriptSupport { self: NashornDebuggerHost =>

  private val runnerByScriptId = new TrieMap[String, CompiledScriptRunner]() // must be thread safe
  private val scriptPromiseByHash = mutable.Map[String, Promise[Option[Script]]]() // doesn't need to be thread safe

  //TODO: Let surviveResume control our maps + scripts in Scripts.
  override def compileScript(script: String, url: String, persist: Boolean): Future[Option[Script]] = {
    pausedData match {
      case Some(pd) =>
        implicit val marshaller = pd.marshaller

        // Create a hash that includes both the contents and the URL.
        val hash = Hasher.md5(s"$script:$url".getBytes(StandardCharsets.UTF_8))

        // Did we compile the script already?
        scriptPromiseByHash.get(hash) match {
          case Some(promise) =>
            log.info(s"Reusing compiled script with hash $hash.")
            promise.future

          case None =>
            val correlationId = UUID.randomUUID().toString.replace("-", "")
            log.info(s"Compiling script with requested URL '$url' and hash $hash and unique ID $correlationId.")

            // If not persisting, embed a marker that makes ScriptPublisher suppress ScriptAdded, now and
            // on subsequent sessions
            val marker = if (persist) "" else s";${ScriptPublisher.PreventPublishingMarker}"

            // Embed a unique ID in the script so that we can match the ScriptAdded event.
            val wrapper =
              s"""$script
                 |/*$correlationId$marker*/
               """.stripMargin

            var runner: CompiledScriptRunner = null
            val promise = Promise[Option[Script]]

            // Enter the promise into the script-reuse cache
            scriptPromiseByHash += hash -> promise

            // Listen to InternalScriptAdded since we may have muted ScriptAdded
            var subscription: Subscription = null
            subscription = events.subscribe(Observer.from[ScriptEvent] {
              case s: NashornDebuggerHost.InternalScriptAdded if s.script.contents.contains(correlationId) =>
                subscription.unsubscribe()

                if (persist) {
                  runnerByScriptId += s.script.id -> runner
                  promise.success(Some(s.script))
                } else {
                  promise.success(None)
                }
            })

            val actualUrl = CompiledScript(correlationId, url).toCodeUrl

            val lifecycle = if (persist) Lifecycle.Session else Lifecycle.None
            runner = _scanner.withClassTracking(codeEval.compileScript(wrapper, actualUrl, lifecycle))
            promise.future
        }
      case None =>
        throw new IllegalStateException("Script compilation can only be done in a paused state.")
    }
  }

  override def runCompiledScript(scriptId: String): Try[ValueNode] = Try {
    pausedData match {
      case Some(pd) =>
        runnerByScriptId.get(scriptId) match {
          case Some(runner) =>
            log.info(s"Running script with ID $scriptId")
            virtualMachine.withDisabledBreakpoints {
              runner.run()(pd.marshaller)
            }
          case None =>
            val known = runnerByScriptId.keys.mkString(", ")
            log.warn(s"Cannot find compiled script with ID $scriptId. Known compiled script IDs: $known")
            throw new IllegalArgumentException("Unknown script ID: " + scriptId)
        }
      case None =>
        throw new IllegalStateException("Running a compiled script can only be done in a paused state.")
    }
  }
}