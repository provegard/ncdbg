# Changelog

## Unreleased

* Don't rely on Nashorn syntax extensions in the property extractor code, since the target may
  have disabled them (issue #68)
* Pause on reference errors if pausing on exceptions.
* Detect a script added during code evaluation, e.g. when the `load` extension is used (issue #66)
* Don't capture print statements while paused, in order to avoid deadlock (issue #66)
* Try to make general pause more stable, only set breakpoints in script frames (issue #71)
* Make script contents hashing thread safe - avoids duplicate scripts with the same contents.
* Support setting a breakpoint in an unknown script (via URL or URL regexp). Emit BreakpointResolved
  when a script that matches the requested breakpoint is seen. This makes it possible to use the
  DevTools Filesystem/workspace feature.
* Ensure that changing a local variable via console code evaluation works after step/resume, i.e.
  that the new value actually remains (issue #72)
* Handle JAR URLs better. The `jar:` prefix is dropped so that embedded resources appear as
  "navigable" files in the DevTools source tree.
* Fix preview generation for typed arrays (issue #73)
* Fix console expansion of big arrays (ES6 needs to be transpiled to ES5) (issue #76)
    * Add dependency on [Google Closure Compiler](https://github.com/google/closure-compiler)
      for transpilation of ES6 code.
* Fix object property extraction when Java is disabled (Nashorn script engine created with `--no-java`)
  (issue #80)
* Fix VS Code connection problem, serve web sockets via /json/list also (issue #79)
* Fix VS Code locals property extraction error, handle arguments to `Runtime.callFunctionOn`
  being `null` (issue #79)
* Be friendly towards VS Code when it asks about user agent (for internal Debugger for Chrome logging)
  (issue #79)
* Serialize domain actor requests to avoid race conditions that prevent object properties from
  being fetched because the VM has resumed (issue #77)
* Fix stepping over a line where there is a breakpoint (issue #82)
* Make sure script-based property extraction happens on the correct thread (issue #78)
* Cache certain functions in Runtime.callFunctionOn. DevTools in Chrome 65.0.3325.146 introduces some
  caching but prior to that autocompletion is slow.
* Handle event request that is `null` when invoking a request-specific event handler (issue #88)
* Support Runtime.compileScript (and runScript) (issue #84)
    * This adds support for script snippets in DevTools.
    * It also enables code continuation in the console, since DevTools uses
      compileScript to detect unterminated statements.
    * Please see the documentation for limitations with compiled scripts.
* Ignore breakpoints (including `debugger` statements) when evaluating code, to prevent deadlock.
* Faster property extraction for scope objects (issues #89)
* List known bugs as console warnings when DevTools connects.
* Fix evaluation of erroneous code in `Runtime.compileScript` - always include an exception value
  (issue #90)
* Show entries for `Map` and `Set` as an internal [[Entries]] property (issue #75)
* Show proper preview for `Map` and `Set` (issue #74)
* Ignore the silent flag passed to many methods, since we ignore exceptions anyway while in
  paused mode. Always return exception details back to the caller (issue #87)
* Less verbose error message after evaluation, don't show ECMAException (issue #92)
* Don't leak hidden entries array for Map/Set to console autocompletion (issue #94)
* Fixed broken class scanning, now scanning isn't complete until all classes have been scanned,
  and yielding/resuming scan works.
* Improve class scanning speed by giving NCDbg-evaluated scripts a special source name that can
  be detected before getting the script source.
    * **NOTE**! If NCDbg is attached to a process that has been debugged with an older version,
      code eval scripts may appear in DevTools. Restarting the target process should fix that. 
* Make sure undefined variable access in artificial local scope results in ReferenceError.
* Avoid NPE in UserAccessorProperty (Nashorn) when evaluating code on global scope after
  evaluating same code on local artificial scope.
* Faster code evaluation (~50%) for Runtime.callFunctionOn, and faster construction of artificial local
  scope.
* Reuse syntax check via compileScript for subsequent code evaluation (EXPERIMENTAL).
* Ensure DevTools sees Debugger.scriptParsed for a script before compileScript returns.
* Fix bug with evaluating code inside a `with` block (issue #93)

## 0.6.0 (2017-10-22)

* Lift requirement that a script path must be absolute - support relative paths as well
  (though relative source maps won't work).
* Intercept calls to Nashorn's `print` extension and send the printed message to DevTools
  as a console log message.
* Handle GC better during code evaluation (issue #62)
* Initial support for building with Java 9 (issue #67)
* Support debugging in Visual Studio Code.

## 0.5.6 (2017-09-15)

* Return internal properties (`[[Prop]]`) (issue #58):
    * `[[TargetFunction]]` for a bound function
    * `[[Scopes]]` for any function
    * `[[JavaStack]]` for a Java exception (was previously a regular property)
    * `[[Message]]` for a Java exception (was previously a regular property)
* Don't return any Java array properties if only accessors are requested.
* Merge scripts based on contents hash regardless of recompilation/original order (issue #64)
* Support script reload with changed contents and same path (issue #65)
* Scan classes in chunks to prevent locking up and preventing other operations (issue #63)

## 0.5.5 (2017-08-24)

* Remove column guessing&mdash;it's too fragile.
* Make it possible to pause in a script/function that hasn't been compiled yet (issue #61)
* Fix step-into in Java 9 (issue #60)
* When stepping out of a function, try to stay in the callee to avoid unwinding
  a long call chain.

## 0.5.4 (2017-08-14)

* Trying to make tests more stable wrt breakpoint columns.
* Return only distinct locations when setting a breakpoing.
* Parse ES6 arrow functions when guessing columns.

## 0.5.3 (2017-08-13)

* Properly pause on uncaught exceptions (issue #52)
* Fix variable setting when stack contains recusion (issue #53)
* Emit Runtime.exceptionThrown for uncaught errors.
* Support marshalling of ES6 Symbol (Java 9) (issue #54)
* Support marshalling of ES6 Map, Set, WeakMap and WeakSet (Java 9) (issue #56)
* Stability fixes
* Marshal __proto__ correctly

## 0.5.2 (2017-07-31)

* More robust pause support (issue #51)

## 0.5.1 (2017-07-18)

* Support setting local variables of primitive type (issue #46)
* Support Debugger.continueToLocation (issue #47)
* Support Debugger.pause (issue #49)
* Pause in certain situations even if breakpoints are disabled (issue #50)
* Implement Debugger.setSkipAllPauses (issue #48)

## 0.5.0 (2017-05-28)

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

