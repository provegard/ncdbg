# Naming scripts

Code that is evaluated using `ScriptEngine.eval` will show in Chrome
with an anonymous name, like _Script1/eval_.

To assign a name to a script, the following options are available.

## Load the script

Use Nashorn's `load` extension to load the script:

    scriptEngine.eval("load('path/to/script.js');");

Note that `load` can load a resource also, see [Nashorn extensions](https://wiki.openjdk.java.net/display/Nashorn/Nashorn+extensions#Nashornextensions-load).

## Load anonymous code

The `load` extension can be used to evaluate code with a script name, e.g.:

    scriptEngine.eval("load({ script: 'function foo() {}', name: 'myscript.js' });");

If you have the code to evaluate stored in a Java variable:

    String code = ...;
    scriptEngine.put("theCode", code);
    try {
        scriptEngine.eval("load({ script: theCode, name: 'myscript.js' });");
    } finally {
        scriptEngine.put("theCode", null);
    }

## Set ScriptEngine.FILENAME

Set the `ScriptEngine.FILENAME` value prior to evaluating code:

    scriptEngine.put(ScriptEngine.FILENAME, "thescript.js");
    try {
        scriptEngine.eval("function foo() {}");
    } finally {
        scriptEngine.put(ScriptEngine.FILENAME, null);
    }

## Use sourceURL

Insert a _sourceURL_ comment to have Chrome show the evaluated code under that name:

    scriptEngine.eval("function foo() {}\n//# sourceURL=thatscript.js");
