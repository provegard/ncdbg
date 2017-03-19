# NCDbg

[![Build Status](https://travis-ci.org/provegard/ncdbg.svg?branch=master)](https://travis-ci.org/provegard/ncdbg)

NCDbg is a debugger for Nashorn (the JavaScript engine in Java 8 and onward) that
uses Google Chrome DevTools as frontend.

NCDbg is an abbreviation of Nashorn Chrome Debugger. I chose that name because of
my profound lack of imagination when it comes to naming things.

## Building & running tests

To compile in a Unix-like shell:

    ./gradlew compileScala
    
If you prefer a Windows command prompt, just use `gradlew` instead of `./gradlew`.

To run tests:

    ./gradlew test
    
Note that at least one test is unstable. If the test suite fails, re-run it. If the failure
persists, please open an issue.

If you use a globally installed Gradle, then you must use version 2.13 because of the ScalaTest plugin. If you use
another version, the version of the `com.github.maiflai.scalatest` plugin needs to be adjusted accordingly.
See *build.gradle* for details.

## Documentation

The documentation is [here](DOCUMENTATION.md).

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

