package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Profiler.{Profile, ProfileNode}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.testing.UnitTest

trait ProfilerTesting {
  def emptyRootNode = ProfileNode(1, Runtime.CallFrame("(root)", "0", "", -1, None), 0, Seq.empty)
  def createStackFrame(scriptId: String, url: String, functionName: String, lineNo1: Int, colNo1: Int) = new StackFrame {
    override val functionDetails: FunctionDetails = FunctionDetails(functionName)
    override val scopeChain: Seq[Scope] = Seq.empty
    override val breakpoint: Breakpoint = Breakpoint("xx", scriptId, Some(ScriptURL.create(url)), Seq(ScriptLocation(lineNo1, Some(colNo1))))
    override val id: String = scriptId
    override val thisObj: ValueNode = null
  }
}

class ProfileBuilderTest extends UnitTest with ProfilerTesting {

  "ProfileBuilder" - {
    "fromProfilingData" - {

      "handles 0 samples" in {
        val input = ProfilingData(Seq.empty, 0, 0)
        val result = Profile.fromProfileData(input)
        result should be (Profile(Seq(emptyRootNode), 0, 0, Seq.empty, Seq.empty))
      }

      "normalizes start time to 0 and converts end time to microseconds" in {
        val input = ProfilingData(Seq.empty, 1000L, 5000L)
        val result = Profile.fromProfileData(input)
        result should be (Profile(Seq(emptyRootNode), 0, 4, Seq.empty, Seq.empty))
      }

      "with a single script sample with a single stack frame" - {
        val stackFrame = createStackFrame("a", "file:///some/script.js", "myFun", 1, 1)
        val sample = Sample(2000L, Seq(stackFrame), SampleType.Script)
        val input = ProfilingData(Seq(sample), 1000, 5000)
        val result = Profile.fromProfileData(input)

        def findProfileNode = result.nodes.find(_.children.isEmpty).getOrElse(throw new RuntimeException("Failed to find the profile node"))

        "calculates the time delta in microseconds" in {
          result.timeDeltas should be (Seq(1))
        }

        "records top hit count" in {
          findProfileNode.hitCount should be (1)
        }

        "assigns no children to the resulting node" in {
          findProfileNode.children should be ('empty)
        }

        "makes the resulting node a child of the root node" in {
          val nodeId = findProfileNode.id
          result.nodes.head.children should be (Seq(nodeId))
        }

        "associates the resulting node with the correct call frame" in {
          findProfileNode.callFrame should be (Runtime.CallFrame("myFun", "a", "file:///some/script.js", 0, Some(0)))
        }
      }

      "with a single script sample with a two stack frames" - {
        val stackFrame1 = createStackFrame("a", "file:///some/script.js", "myFun", 1, 1)
        val stackFrame2 = createStackFrame("a", "file:///some/script.js", "myFun", 3, 1)
        val sample = Sample(2000L, Seq(stackFrame1, stackFrame2), SampleType.Script)
        val input = ProfilingData(Seq(sample), 1000, 5000)
        val result = Profile.fromProfileData(input)

        "lists top node IDs, where top is 'top of stack'" in {
          // root has 1, child 2, grandchild 3
          result.samples should be (Seq(3))
        }
      }

      "with two samples with stack frame" - {
        val stackFrame1a = createStackFrame("a", "file:///some/script.js", "myFun", 1, 1)
        val stackFrame1b = createStackFrame("a", "file:///some/script.js", "child", 2, 1)
        val stackFrame2 = createStackFrame("a", "file:///some/script.js", "myFun", 3, 1)
        val sample1 = Sample(2000L, Seq(stackFrame1a, stackFrame1b), SampleType.Script)
        val sample2 = Sample(3000L, Seq(stackFrame2), SampleType.Script)
        val input = ProfilingData(Seq(sample1, sample2), 1000, 5000)
        val result = Profile.fromProfileData(input)

        "lists top node IDs" in {
          // root - sf1a - sf2b - sf2
          // 1      2      3      4
          result.samples should be (Seq(3, 4))
        }
      }

      "with two samples featuring the same stack frame" - {
        val stackFrame = createStackFrame("a", "file:///some/script.js", "myFun", 1, 1)
        val sample1 = Sample(2000L, Seq(stackFrame), SampleType.Script)
        val sample2 = Sample(4000L, Seq(stackFrame), SampleType.Script)
        val input = ProfilingData(Seq(sample1, sample2), 1000, 5000)
        val result = Profile.fromProfileData(input)

        def leafNodes: Seq[ProfileNode] = result.nodes.filter(_.children.isEmpty)

        "calculates the time deltas in microseconds" in {
          result.timeDeltas should be (Seq(1, 2))
        }

        "merges the two samples into one profile node" in {
          leafNodes should have size (1)
        }

        "records top hit count" in {
          leafNodes.head.hitCount should be (2)
        }
      }

      "with a single program sample" - {
        val sample = Sample(2000L, Seq.empty, SampleType.Java)
        val input = ProfilingData(Seq(sample), 1000, 5000)
        val result = Profile.fromProfileData(input)

        def findProfileNode = result.nodes.find(_.children.isEmpty).getOrElse(throw new RuntimeException("Failed to find the profile node"))

        "associates the resulting node with the correct call frame" in {
          findProfileNode.callFrame should be (Runtime.CallFrame("(program)", "0", "", -1, None))
        }
      }

      "with a single idle sample" - {
        val sample = Sample(2000L, Seq.empty, SampleType.Idle)
        val input = ProfilingData(Seq(sample), 1000, 5000)
        val result = Profile.fromProfileData(input)

        def findProfileNode = result.nodes.find(_.children.isEmpty).getOrElse(throw new RuntimeException("Failed to find the profile node"))

        "associates the resulting node with the correct call frame" in {
          findProfileNode.callFrame should be (Runtime.CallFrame("(idle)", "0", "", -1, None))
        }
      }
    }
  }

}
