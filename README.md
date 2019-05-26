# NCDbg

[![Build Status](https://travis-ci.org/provegard/ncdbg.svg?branch=master)](https://travis-ci.org/provegard/ncdbg)

NCDbg is a debugger for Nashorn (the JavaScript engine in Java 8 and onward) that
uses Google Chrome DevTools or [Visual Studio Code](https://code.visualstudio.com/) (VSCode) as frontend.

NCDbg is an abbreviation of Nashorn Chrome Debugger. I chose that name because of
my profound lack of imagination when it comes to naming things. Since VSCode can act as a 
debugger frontend as well (since 0.6.0), there is a great need for a better name. Suggestions
are welcome!

Both Java 8 and Java 9 are supported. Please see [the documentation](docs/README.md) for information
about Java 9.

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

Documentation can be found in the _docs_ folder.

### Visual Studio Code

VSCode has a [separate documentation](docs/VSCode.md).

## Contributing

Pull requests are welcome! However, before creating a pull request, please open an issue to describe
the missing/limited/faulty behavior that you intend to fix, along with a solution proposal.

In the absence of a proper style guide, please follow the style of existing code.

## Changelog

Please see CHANGELOG.md.

## License

The code in this repository is licensed under the 3-Clause BSD license. See the *LICENCE* file
for details.

## Sponsoring

This project is partly sponsored by my employer, [factor10](http://factor10.com/).

## Contact

Twitter: [@provegard](https://twitter.com/provegard)

