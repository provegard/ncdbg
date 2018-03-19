# Compiled scripts

A compiled script is a script that is prepared but not evaluated yet. DevTools uses compiled
scripts in three places:

1. For script snippets.
2. For detecting partial input in the console, to enter continuation mode.
3. For finding syntax errors in a script.

There are two main limitations with compiled scripts as snippets compared to
Chrome:

1. It's only possible to run a snippet when the debugger is paused, and
2. as a result, it's not possible to debug a snippet. Breakpoints will be ignored.
