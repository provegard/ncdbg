package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.ObjectId
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ScriptEvaluateSupportTest extends UnitTest {

  "wrapInFunction" - {
    "doesn't wrap if there are no call arguments" in {
      val functionDecl = "function () {}"
      val args = Seq.empty[Runtime.CallArgument]
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      wrapper should be ((functionDecl, Seq.empty))
    }

    "doesn't wrap if there are only object arguments" in {
      val functionDecl = "function (arg) {}"
      val args = Seq(Runtime.CallArgument(None, None, Some(ObjectId("x").toString)))
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      wrapper should be ((functionDecl, Seq(ObjectId("x"))))
    }

    "wraps if there are only value arguments" in {
      val functionDecl = "function (arg) {}"
      val args = Seq(Runtime.CallArgument(Some("test"), None, None))
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      val wf =
        """function() {
          |  var argsInOrder=["test"];
          |  var f=(function (arg) {});
          |  return f.apply(this,argsInOrder);
          |}
        """.stripMargin.trim
      wrapper should be ((wf, Seq.empty))
    }

    "wraps if there are only unserializable arguments" in {
      val functionDecl = "function (arg) {}"
      val args = Seq(Runtime.CallArgument(None, Some("NaN"), None))
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      val wf =
        """function() {
          |  var argsInOrder=[NaN];
          |  var f=(function (arg) {});
          |  return f.apply(this,argsInOrder);
          |}
        """.stripMargin.trim
      wrapper should be ((wf, Seq.empty))
    }

    "supports a mix of object + value" in {
      val functionDecl = "function (a1, a2) {}"
      val args = Seq(Runtime.CallArgument(None, None, Some(ObjectId("x").toString)), Runtime.CallArgument(Some("test"), None, None))
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      val wf =
        """function(__o_0) {
          |  var argsInOrder=[__o_0,"test"];
          |  var f=(function (a1, a2) {});
          |  return f.apply(this,argsInOrder);
          |}
        """.stripMargin.trim
      wrapper should be ((wf, Seq(ObjectId("x"))))
    }

    "supports undefined as call argument" in {
      val functionDecl = "function (arg) {}"
      val args = Seq(Runtime.CallArgument(None, None, None))
      val wrapper = ScriptEvaluateSupport.wrapInFunction(functionDecl, args)

      val wf =
        """function() {
          |  var argsInOrder=[void 0];
          |  var f=(function (arg) {});
          |  return f.apply(this,argsInOrder);
          |}
        """.stripMargin.trim
      wrapper should be ((wf, Seq.empty))
    }
  }
}
