package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Profiler.{Profile, ProfileNode}
import com.programmaticallyspeaking.ncd.host._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

object ProfileBuilder {
  class Tree {
    private var id = 0

    private def nextId: Int = {
      id += 1
      id
    }

    //    {
    //      "id" : 1,
    //      "callFrame" : {
    //        "functionName" : "(root)",
    //        "scriptId" : "0",
    //        "url" : "",
    //        "lineNumber" : -1,
    //        "columnNumber" : -1
    //      },
    //      "hitCount" : 0,
    //      "children" : [2, 3, 4, 6]
    //    }
    private val tree = new Node(nextId, Runtime.CallFrame("(root)", "0", "", -1, Some(-1)), false)

    def profileNodes(): Seq[ProfileNode] = {
      val flatList = ListBuffer[ProfileNode]()
      val q = mutable.Queue[Node](tree)
      while (q.nonEmpty) {
        val next = q.dequeue()
        val children = next.children
        val profileNode = ProfileNode(next.id, next.callFrame, next.topCount, children.map(_.id))
        flatList += profileNode
        q.enqueue(children: _*)
      }

      flatList
    }

    private def stackFrameToCallFrame(sf: StackFrame): Runtime.CallFrame = {
      val headLocation = sf.location
      Runtime.CallFrame(sf.functionDetails.name, sf.scriptId,
        sf.scriptURL.toString,
        headLocation.lineNumber1Based - 1,
        headLocation.columnNumber1Based.map(_ - 1))
    }

    def addSample(sample: Sample): Int = {
      val callFrames = sample.sampleType match {
        case SampleType.Script =>
          // Reverse the order so that we can process the bottom stack frame first. Protocol/Chrome terminology is not
          // super clear (in DevTools, the default view is "top down", but the root isn't a top as in "stack top" frame.
          sample.stackFrames.reverse.map(stackFrameToCallFrame)
        case SampleType.Java =>
          Seq(Runtime.CallFrame("(program)", "0", "", -1, Some(-1)))
        case SampleType.Idle =>
          Seq(Runtime.CallFrame("(idle)", "0", "", -1, Some(-1)))
        case other =>
          throw new RuntimeException("Unknown sample type: " + other)
      }

      var currentNode = tree
      var topId: Int = -1
      callFrames.zipWithIndex.foreach {
        case (callFrame, idx) =>
          val isTopFrame = idx == callFrames.size - 1

          currentNode = currentNode.findImmediateChild(callFrame) match {
            case Some(n) =>
              // Existing node
              if (isTopFrame) n.markTop()
              n
            case None =>
              // Add new node
              val newNode = new Node(nextId, callFrame, isTopFrame)
              currentNode.addChild(newNode)
              newNode
          }

          if (topId < 0 && isTopFrame) {
            topId = currentNode.id
          }
      }

      topId
    }
  }

  class Node(val id: Int, val callFrame: Runtime.CallFrame, isTop: Boolean) {
    private val _children = ListBuffer[Node]()
    private var _topCount: Int = if (isTop) 1 else 0

    def topCount: Int = _topCount
    def findImmediateChild(cf: Runtime.CallFrame): Option[Node] = _children.find(_.callFrame == cf)
    def addChild(n: Node): Unit = _children += n
    def markTop(): Unit = _topCount += 1
    def children: Seq[Node] = _children
  }
}

trait ProfileBuilder {
  import ProfileBuilder._

  def fromProfileData(data: ProfilingData): Profile = {
    val topNodeIds = ListBuffer[Int]()
    val timeDeltas = ListBuffer[Int]()
    val tree = new Tree

    var lastTime = data.startNanos
    data.samples.foreach { sample =>
      val delta = (sample.nanoTime - lastTime).nanos
      val topId = tree.addSample(sample)
      if (topId < 0) throw new RuntimeException("Didn't find a root node for " + sample)
      topNodeIds += topId
      timeDeltas += delta.toMicros.toInt
      lastTime = sample.nanoTime
    }

    // nodes - The list of profile nodes. First item is the root node.
    Profile(tree.profileNodes(), 0, (data.stopNanos - data.startNanos).nanos.toMicros, topNodeIds, timeDeltas)
  }
}
