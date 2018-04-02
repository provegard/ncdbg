package com.programmaticallyspeaking.ncd.nashorn

import java.nio.charset.StandardCharsets
import java.util.UUID

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{CompiledScript, Hasher}
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try

case class RunnerForStackFrame(runner: CompiledScriptRunner, stackFrameId: String, scriptId: String)

trait CompiledScriptSupport { self: NashornDebuggerHost =>

  // Accessed from multiple threads.
  private val runnerByScriptId = new TrieMap[String, CompiledScriptRunner]()

  private val nonPersistRunnerByScriptHash = new TrieMap[String, RunnerForStackFrame]()

  // Doesn't need to be thread safe, always accessed from the main host thread.
  private val scriptPromiseByHash = mutable.Map[String, Promise[Script]]()

  protected def clearNonPersistedScripts(): Unit = {
    nonPersistRunnerByScriptHash.clear()
  }

  protected def runCompiledScriptWithHash(scriptHash: String, stackFrameId: String): Option[(ValueNode, String)] = {
    pausedData match {
      case Some(pd) if pd.stackFrames.head.id == stackFrameId =>

        nonPersistRunnerByScriptHash.get(scriptHash).map { rr =>
          val vn = virtualMachine.withDisabledBreakpoints {
            rr.runner.run()(pd.marshaller)
          }
          (vn, rr.scriptId)
        }

      case _ => None
    }
  }

  override def compileScript(script: String, url: String, persist: Boolean): Future[Option[Script]] = {
    pausedData match {
      case Some(pd) =>
        implicit val marshaller = pd.marshaller
        val compileResultTransformer = toCompileResult(persist)(_)

        // Create a hash that includes both the contents and the URL.
        val scriptHash = Hasher.md5(script.getBytes(StandardCharsets.UTF_8))
        val compileHash = Hasher.md5(s"$scriptHash:$url".getBytes(StandardCharsets.UTF_8))

        findScript(ScriptIdentity.fromHash(scriptHash)) match {
          case Some(ss) if !persist =>
            // Syntax check of a script that we already have. Since it's not possible for us to have a script
            // with a syntax error (Nashorn doesn't allow it as far as I can tell), we simply return None as if
            // we did the syntax check and it turned out good.
            log.debug(s"Ignoring syntax check of code identical to script ${ss.id}, assuming it's good.")
            return Future.successful(None)
          case None =>
        }

        // Did we compile the script already?
        scriptPromiseByHash.get(compileHash) match {
          case Some(promise) =>
            promise.future.map { s =>
              log.debug(s"No need to compile, reusing compiled script ${s.id} based on hash $compileHash.")
              compileResultTransformer(s)
            }

          case None =>
            val correlationId = UUID.randomUUID().toString.replace("-", "")
            log.debug(s"Compiling script with requested URL '$url' and hash $compileHash and unique ID $correlationId.")

            // If not persisting, embed a marker that makes ScriptPublisher suppress ScriptAdded, now and
            // on subsequent sessions
            val marker = if (persist) "" else s";${ScriptPublisher.PreventPublishingMarker}"

            // Embed a unique ID in the script so that we can match the ScriptAdded event.
            val wrapper =
              s"""$script
                 |/*$correlationId$marker*/
               """.stripMargin

            val scriptPromise = Promise[Script]

            // Enter the promise into the script-reuse cache
            scriptPromiseByHash += compileHash -> scriptPromise

            // Listen to InternalScriptAdded since ScriptAdded is suppressed when persist==false
            var subscription: Subscription = null
            subscription = events.subscribe(Observer.from[ScriptEvent] {
              case s: NashornDebuggerHost.InternalScriptAdded if s.script.contents.contains(correlationId) =>
                subscription.unsubscribe()
                log.debug(s"Compilation of script with hash $compileHash resulted in script with ID ${s.script.id} (persist=$persist)")
                scriptPromise.success(s.script)
            })

            val actualUrl = CompiledScript(correlationId, url).toCodeUrl

            // For a non-persisted script, find the scope object so that we can reuse the script for evaluation.
            val stackFrame = pd.stackFrames.head
            val scopeObjectRef = if (persist) None else {
              stackFrame.scopeChain.headOption.map(_.value).flatMap {
                case cn: ComplexNode =>
                  mappingRegistry.byId(cn.objectId).flatMap(_.native)
                case _ => None
              }
            }

            val lifecycle = if (persist) Lifecycle.Session else Lifecycle.Paused
            val tRunner = Try(_scanner.withClassTracking(codeEval.compileScript(wrapper, actualUrl, scopeObjectRef.orNull, lifecycle)))

            // We may observe InternalScriptAdded before the _scanner.withClassTracking call returns, so
            // connect runner with script here rather than in the InternalScriptAdded observer.
            Future.fromTry(tRunner).flatMap { theRunner =>
              scriptPromise.future.map { script =>
                // Save the runner for reuse during evaluation (we test evaluation by hash)
                nonPersistRunnerByScriptHash += scriptHash -> RunnerForStackFrame(theRunner, stackFrame.id, script.id)

                // Save the runner for runScript (we run by ID)
                runnerByScriptId += script.id -> theRunner

                compileResultTransformer(script)
              }
            }
        }
      case None =>
        throw new IllegalStateException("Script compilation can only be done in a paused state.")
    }
  }

  private def toCompileResult(persist: Boolean)(script: Script) = if (persist) Some(script) else None

  override def runCompiledScript(scriptId: String): Try[ValueNode] = Try {
    pausedData match {
      case Some(pd) =>
        runnerByScriptId.get(scriptId) match {
          case Some(runner) =>
            log.debug(s"Running script with ID $scriptId")
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