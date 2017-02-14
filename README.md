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
    
**Note**: Right now, ncdbg hard-codes localhost:7777 (see [this issue](https://github.com/provegard/ncdbg/issues/11)).

If you prefer, use `suspend=y` instead to have the debug target wait until the debugger
attaches.

Once the debug target is up and running, connect the debugger. Right now, there is no release
package to run, so run using Gradle:

    ./gradlew run
    
The log output may be a bit intimidating, but look for a line that looks something like this:

> chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=localhost:7778/dbg

(Yes, localhost:7778 is currently also hard-coded...)

Copy that URL and open it in Chrome. Chrome Dev Tools should now open. Go to the Sources tab and
take a look at the scripts!

Note that the Dev Tools console can only be used when the debugger is paused in a breakpoint.

## Contributing

Pull requests are welcome! However, before creating a pull request, please open an issue to describe
the missing/limited/faulty behavior that you intend to fix, along with a solution proposal.

In the absence of a proper style guide, please follow the style of existing code.

## License

The code in this repository is licensed under the MIT license. See the *LICENCE* file
for details.

## Contact

Twitter: [@provegard](https://twitter.com/provegard)

