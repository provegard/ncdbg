# NCDbg

NCDbg is a debugger for Nashorn (the JavaScript engine in Java 8 and onward) that
uses Google Chrome DevTools as frontend.

NCDbg is an abbreviation of Nashorn Chrome Debugger. I chose that name because of
my profound lack of imagination when it comes to naming things.

## Building & running tests

Make sure [Gradle](https://gradle.org/) 2.13 is installed. If you have a newer version, the version of the 
`com.github.maiflai.scalatest` plugin needs to be adjusted accordingly. See *build.gradle*
for details.

To compile in a Unix-like shell:

    ./gradlew compileScala
    
If you prefer a Windows command prompt, use `gradlew.bat` instead of `./gradlew`.

To run tests:

    ./gradlew test
    
Note that at least one test is unstable. If the test suite fails, re-run it. If the failure
persists, please open an issue.

## Usage

Start the debug target with the following VM arguments:

    -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=7777

Port 7777 is just an example, but is the port that NCDbg by default will connect to. If you use a different
port, make sure you configure NCDbg appropriately, see below.

If you prefer, use `suspend=y` instead to have the debug target wait until the debugger
attaches.

It's possible to run NCDbg using Gradle, which you'll want to do to run the bleeding edge version.
Another option is to download and run a distribution.

### The Gradle way

Once the debug target is up and running, connect the debugger:

    ./gradlew run
    
The console only shows information messages, warnings and errors. For debug messages, see the file _logs/ncdbg.log_.

In the console output, you'll find the following line:

> Open this URL in Chrome: chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=localhost:7778/dbg

(The exact address towards the end depends on runtime configuration options, see below.)

Copy that URL and open it in Chrome. Chrome Dev Tools should now open. Go to the Sources tab and
take a look at the scripts, set breakpoints etc.!

Note that the Dev Tools console can only be used when the debugger is paused in a breakpoint.

### The distribution way

TBD

## Configuring options

NCDbg supports configuration via the command line. When running using Gradle, the syntax is a bit awkward. To see the possible configuration options, run:

    ./gradlew run -Pargs='--help'
    
For example, to connect to a debug target listening at port 9999 and listen on port 10000, run:

    ./gradlew run -Pargs='-c 9999 -l 10000'

When running a distribution, just pass arguments the normal way.

## FAQ

The FAQ is [here](FAQ.md).

## Contributing

Pull requests are welcome! However, before creating a pull request, please open an issue to describe
the missing/limited/faulty behavior that you intend to fix, along with a solution proposal.

In the absence of a proper style guide, please follow the style of existing code.

## License

The code in this repository is licensed under the MIT license. See the *LICENCE* file
for details.

## Sponsoring

This project is partly sponsored by my employer, [factor10](http://factor10.com/).

## Contact

Twitter: [@provegard](https://twitter.com/provegard)

