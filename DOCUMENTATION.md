# FAQ

Frequently asked questions with answers are listed below. Except that's not true! Nobody has
asked them yet, so they are more accurately described as questions _anticipated_ to be frequently asked...

## Debugging

### Why is a variable undefined right after an assignment?

Let's say you have a piece of Nashorn code like this:

    var now = new Date();
    debugger;

When the debugger pauses on the `debugger` statement, the value of `now` may
indeed be `undefined`. This may happen if the variable isn't used, in which case
it is optimized away. To see its value, make sure it's used somewhere, e.g.:

    var now = new Date();
    debugger;
    now.toString(); // dummy usage

### Why can't I set a breakpoint in a function?

Consider this code:


    function seldomCalled() {
      return "testing";
    }
    if (unlikelyConditionIsMet) seldomCalled();

You may not be able to set a breakpoint on the line inside the `seldomCalled`
function. Functions are by default (TODO: figure out since which version) lazily 
compiled by Nashorn, and if a function hasn't been called yet, its line locations
don't exist.

To turn off lazy compilation (not recommended), run the debug target with
`-Dnashorn.args=--lazy-compilation=false`.
