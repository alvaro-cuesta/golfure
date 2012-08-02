Sure
====
- Better error handling
 - More asserts in builtin macro
 - What happens when stack is completely consumed?
- Stop parsing EVERYTHING as a Clojure symbol and limit to bigintegers/strings (but somehow keep interop)
 - Arbitrary precision integers by default
 - Make a difference between " and '
- `-main` must take up to two args, script and initial stack (both can be either strings or files)
 - If no input is specified in `-main`, read from stdin until all input is consumed
- Block syntax
- All builtins
- Add more tests (all builtins + language)
- README
 - Document differences with pure GolfScript
- Re-read GolfScript site for quirks and review builtins
- Document further
- Clean code
 - Clean imports and such

Maybe
=====
- Make blocks lazy but realizable
- Reverse stack
 - Pros
     - This
 - Cons
     - That
- Dollar
 - Sort by string mapping too
- Use `reduce` instead of `recur` for `golfure.interpreter/execute-block`
- Integrate ":" in RegEx
- Go back to thread-wise `*symbols*`
- Web interface

Check
=====
Quoted from GolfScript site:

- Strings are really just arrays of integers with a different output behavior.

What does this imply?