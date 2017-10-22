package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.ActorRef
import com.programmaticallyspeaking.ncd.testing.UnitTest

class NetworkTest extends UnitTest with DomainActorTesting {

  "Network" - {
    "is enabled via a custom 'enable' method" in {
      val actor = newActorInstance[Network]
      val response = requestAndReceiveResponse(actor, "1", Network.enable())

      response should be (Accepted)
    }

    "accepts setCacheDisabled so that VSCode doesn't print an error in the debug console" in {
      val actor = enabledActor
      val response = requestAndReceiveResponse(actor, "2", Network.setCacheDisabled(true))

      response should be (Accepted)
    }
  }

  private def enabledActor: ActorRef = {
    val actor = newActorInstance[Network]
    val response = requestAndReceiveResponse(actor, "1", Network.enable())
    actor
  }
}
