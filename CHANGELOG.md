# Changelog

# Unreleased

* Support setting local variables of primitive type (issue #46)
* Support Debugger.continueToLocation (issue #47)
* Support Debugger.pause (issue #49)
* Pause in certain situations even if breakpoints are disabled (issue #50)

# 0.5.0 (2017-05-28)

* Show a java.util.Hashtable as a JS object with properties, not as a native Java
  object (issue #36).
* Faster extraction of properties from a plain Java object.
* More robust getObjectProperties handling for ScriptObject (issue #38)
* Always return only own properties for scope objects (issue #39)
* Include the __proto__ property for JS objects (issue #40)
* Faster object properties by extracting properties in a script in the remote VM
* Faster stepping by stepping natively instead of activating one-time breakpoints
* Limited support for column numbers; on lines that contain one-liner functions, it's
  possible to set individual breakpoints (issue #35)
* When marshalling a newed JS object, use the constructor name as class name
* Handle that both scope and `this` are `null` (issue #43)

## 0.4.0 (2017-04-24)

* Don't include tools.jar in the distibuted zip; grab it from the runtime JDK instead.
* Source map support, both file based (requires the file to be readable/accessible by
  NCDbg) and inline (issue #23).
* Fix marshalling of concatenated JS strings (issue #29)
* Allow setting local variables using Debugger.setVariableValue (issue #30)
* Limited Java 9 support (can debug a program running on Java 9)
* Support conditional breakpoints
* List inherited properties from an arbitrary Java object
* JavaBeans support (issue #33)
* Support `Debugger.restartFrame` (issue #31)
* Support the Profiling domain and CPU profiling (issue #32)

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

