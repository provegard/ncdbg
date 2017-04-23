# Documentation

## Requirements

* Java version 1.8.0_66 or newer, though NCDbg currently won't enforce it.

### Java 9

I've tested running a _debug target_ with Java 9 (JDK build 9-ea+164) and NCDbg running with Java 8 could debug it just fine. I haven't tested building and running NCDbg on Java 9 simply because I couldn't get Gradle to work with Java 9.

## The debug target

The Nashorn application to debug is called the _debug target_. Start the debug target with the following VM arguments:

    -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=7777

This makes the target listen for debugger connections on port 7777, which is the port NCDbg by default will connecto to.
If you choose a different port number, NCDbg has to be configured accordingly.

If you prefer, use `suspend=y` instead to have the debug target wait until the debugger connects.

## Running NCDbg

To run the bleeding edge version of NCDbg, you'll want to clone the repository and use Gradle. Otherwise, you can
download a distribution, unpack it somewhere, and run using one of the distribution scripts.

Running with Gradle is straightforward but the syntax to pass arguments is extremely awkward. If you happen to use a
Unix-like shell such as Git Bash, run:

    ./gradlew run -Pargs="--help"
    
In a Windows command prompt, omit the leading dot-slash:

    gradlew run -Pargs="--help"
    
Running from the distribution works in a similar way, except passing arguments is sane:

    bin/ncdbg --help
    
Or on Windows:

    bin\ncdbg --help

When NCDbg starts, the console only shows information messages, warnings and errors. For debug messages, see the 
file _ncdbg.log_ in the _logs_ directory.

In the console output, you'll find the following line:

> Open this URL in Chrome: chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=localhost:7778/dbg

(The exact address towards the end depends on runtime configuration options, see below.)

Copy that URL and open it in Chrome. Chrome Developer Tools should now open. Go to the Sources tab and
take a look at the scripts, set breakpoints etc.!

Note that the Developer Tools console can only be used when the debugger is paused in a breakpoint.

## Configuring Options

Running with `--help` shows all available options.

### Setting the address of the debug target
    
The default port NCDbg will connect to is 7777. To connect do a different port, use `--connect` or `-c`.
For example, to connect to port 9999, run:

    bin/ncdbg --connect 9999

The argument value can be a port number of an address on the form _&lt;host>:&lt;port>_.
    
### Setting the listening address

By default, NCDbg listens on port 7778 for connections from Chrome Developer Tools. To listen on a different port, use
`--listen` or `-l`. For example, to listen on port 10000, run:

    bin/ncdbg --listen 10000

The argument value can be a port number of an address on the form _&lt;host>:&lt;port>_.

## Nashorn behavior

This section lists some Nashorn behavior that may be surprising at first.

### A variable is undefined right after an assignment

Let's say you have a piece of Nashorn code like this:

    var now = new Date();
    debugger;

When the debugger pauses on the `debugger` statement, the value of `now` may
indeed be `undefined`. This may happen if the variable isn't used, in which case
it is optimized away. To see its value, make sure it's used somewhere, e.g.:

    var now = new Date();
    debugger;
    now.toString(); // dummy usage

### Setting a breakpoint in a function doesn't work

Consider this code:

    function seldomCalled() {
      return "testing";
    }
    if (unlikelyConditionIsMet) seldomCalled();

You may not be able to set a breakpoint on the line inside the `seldomCalled`
function. Functions are by default (__TODO__: figure out since which version) lazily 
compiled by Nashorn, and if a function hasn't been called yet, its line locations
don't exist.

To turn off lazy compilation (not recommended), run the debug target with
`-Dnashorn.args=--lazy-compilation=false`.

## Troubleshooting

### NCDbg doesn't work

Please open an issue and _include the versions of Chrome and Java_.

I've created NCDbg based on the stable
[Chrome Debugging Protocol](https://chromedevtools.github.io/debugger-protocol-viewer/1-2/), but since
NCDbg doesn't advertise that (becuase I don't know how), Chrome Developer Tools happily sends tip-of-tree
commands and gets cranky when it doesn't get proper responses back.

Furthermore, Oracle sometimes breaks Nashorn backwards compatibility between minor versions of Java, and
to make things worse NCDbg uses some classes that are internal to Nashorn. In other words, a new minor version
of Java may not play well with NCDbg.

### Console evaluation doesn't work

I've observed this behavior in Chrome 55 but not in Chrome 56, so if you're running Chrome 55 you
may want to upgrade. The problem is that a debugger breakpoint may be hit before the Developer Tools console 
"subsystem" has initialized, and in this situation console input is basically ignored. A reload usually
helps also.

### When I restart a frame, the target application crashes

Due to Java bug [JDK-8161579](http://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8161579),
`AbstractMethodError` may be thrown after the VM is resumed after stack frames have been popped. The bug entry
comments suggest that 8u132 and later may be free from the bug.