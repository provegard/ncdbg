# Printing messages

The `print` function is a [Nashorn extension](https://wiki.openjdk.java.net/display/Nashorn/Nashorn+extensions). By default,
it prints a message on standard out. Example:

    print('Hello world');

Or, with multiple arguments:

    print('Hello', 'world');
    
Multiple arguments will be separated by a single space.

NCDbg intercepts calls to the `print` function and sends the string output to DevTools as a console message.

## Controlling output

The Java scripting API provides a way to redirect output from scripts through the 
[`ScriptContext.setWriter`](https://docs.oracle.com/javase/7/docs/api/javax/script/ScriptContext.html#setWriter(java.io.Writer)) 
method.
For example, to redirect the output of `print` to a file, use:

    ScriptEngine engine = new NashornScriptEngineFactory().getScriptEngine();
    ScriptContext context = engine.getContext();
    context.setWriter(new PrintWriter("output.txt"));

Note that NCDbg intercepts calls to `print` regardless of the final output destination.