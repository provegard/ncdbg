# Changelog

## 0.3.0 (2017-03-27)

* Don't leak the code evaluation marker (issue #19)
* Remember variables defined in the console (issue #20)
* Handle _Unexpected JDWP Error: 35_ when getting variable values (issue #21)
* Fixed `Debugger.setBreakpointByUrl` throwing for non-existent location (issue #22)
* Marshal typed arrays properly (issue #24)
* Support `Debugger.setVariableValue` and `Runtime.evaluate` (issue #25)
* Remove dependency on Akka HTTP, use [tinyws](https://github.com/provegard/tinyws) instead.
* Clear object properties cache on code evaluation (issue #26)
* Create proper (DevTools-recognizable) file URLs for scripts.
* Use correct class names for objects (e.g. "ArrayBuffer" instead of just "Object").

## 0.2.0 (2017-02-26)

* First public release

