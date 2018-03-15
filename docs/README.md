# Documentation

## Requirements

* Java version 1.8.0_66 or newer, though NCDbg currently won't enforce it.

### Java 9

I've tested running a _debug target_ with Java 9 (JDK build 9-ea+164) and NCDbg running with Java 8 could debug it just fine.

Building with Java 9.0.1 works. One test fails due to [JDK-8187143](http://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8187143).

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

### Visual Studio Code

VSCode has a [separate documentation](VSCode.md).

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

## Troubleshooting

Please see [the separate Troubleshooting document](Troubleshooting.md).