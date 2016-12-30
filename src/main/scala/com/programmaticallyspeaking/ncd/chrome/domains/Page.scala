package com.programmaticallyspeaking.ncd.chrome.domains

object Page {
  case object getResourceTree

  case class setOverlayMessage(message: String)

  case class getResourceContent(frameId: String, url: String)

  case class GetResourceTreeResult(frameTree: FrameResourceTree)

  case class FrameResourceTree(frame: Frame, childFrames: Seq[FrameResourceTree], resources: Seq[FrameResource])

  case class Frame(id: String, parentId: String, loaderId: String, name: String, url: String, securityOrigin: String, mimeType: String)

  // type - Document, Stylesheet, Image, Media, Font, Script, TextTrack, XHR, Fetch, EventSource, WebSocket, Manifest, Other.
  case class FrameResource(url: String, `type`: String, mimeType: String, failed: Boolean, canceled: Boolean)

  case class GetResourceContentResult(content: String, base64Encoded: Boolean)

  private[Page] val FrameUrl = "/"
  private[Page] val ScriptType = "Document"
  private[Page] val FrameMimeType = "text/html"
  private[Page] val ScriptMimeType = "text/javascript" // or application/javascript ??
  private[Page] val FrameContent = "<html></html>"
}

class Page extends DomainActor {
  import Page._

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Page.getResourceTree =>
      val frame = Frame("1", null, "LOADER", "", FrameUrl, FrameUrl, FrameMimeType)
      val tree = FrameResourceTree(frame, null, Seq.empty)
      GetResourceTreeResult(tree)

    case Page.getResourceContent(frameId, url) =>
      if (FrameUrl.equals(url))
        GetResourceContentResult(FrameContent, false)
      else
        throw new IllegalArgumentException("Unknown content URL: " + url)

    case Page.setOverlayMessage(msg) =>
      log.info("Chrome Developer Tools says: " + msg)
  }
}
