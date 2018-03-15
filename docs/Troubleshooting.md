# Troubleshooting

## NCDbg doesn't work

Please open an issue and _include the versions of Chrome and Java_.

I've created NCDbg based on the stable
[Chrome Debugging Protocol](https://chromedevtools.github.io/debugger-protocol-viewer/1-2/), but since
NCDbg doesn't advertise that (becuase I don't know how), Chrome Developer Tools happily sends tip-of-tree
commands and gets cranky when it doesn't get proper responses back.

Furthermore, Oracle sometimes breaks Nashorn backwards compatibility between minor versions of Java, and
to make things worse NCDbg uses some classes that are internal to Nashorn. In other words, a new minor version
of Java may not play well with NCDbg.

## Console evaluation doesn't work

I've observed this behavior in Chrome 55 but not in Chrome 56, so if you're running Chrome 55 you
may want to upgrade. The problem is that a debugger breakpoint may be hit before the Developer Tools console 
"subsystem" has initialized, and in this situation console input is basically ignored. A reload usually
helps also.

## When I restart a frame, the target application crashes

There are two Java bugs in play here:

* Due to [JDK-8179072](http://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8179072),
`AbstractMethodError` may be thrown after the VM is resumed after stack frames have been popped. The bug entry
comments suggest that 8u132 and later may be free from the bug.
* Due to [JDK-8187143](http://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8187143), frame restart
  is likely to crash when the target runs Java 9. I have observed the same behavior in Java 1.8 from
  build 151 (but not 144).

## A variable is undefined right after an assignment

Let's say you have a piece of Nashorn code like this:

    var now = new Date();
    debugger;

When the debugger pauses on the `debugger` statement, the value of `now` may
indeed be `undefined`. This happens if the variable isn't used, in which case
it is optimized away (by the Java runtime). To see its value, make sure it's used somewhere, e.g.:

    var now = new Date();
    debugger;
    now.toString(); // dummy usage

## Evaluating a local variable shows the contents of another variable

(TODO: Find a good example)

Sometimes you may observe that when you hover the mouse over a variable name, or evaluate the
same variable in the console, the contents of a different variable is shown. I think this
happens when the Java runtime decides to reuse a local variable slot since the variable
previously living in that slot is no longer used.