# GolfureScript
[GolfScript](http://www.golfscript.com) implementation written in Clojure.

## Differences with GolfScript
- No Ruby interop. Clojure interop is available instead.
- Mapping of ints->char when arrays->string might be different than GolfScript's (due to charset differences, I think Ruby uses UTF-8 and Clojure UTF-16.)
- Assignments are local to blocks! (this is probably a bug and will be fixed.)
- Bad escaping of charset characters in a certain operation.
- $ (int) fails if stack is over/underflowed
- $ (block block) behaves differently
- \* (block block) doesn't work
- If a symbol is defined upon block compilation, it will be compiled (and not replaced by the future value upon execution)
- Finishing stack and operating on it will cause and error (e.g. "2+" fails, while in GolfScript it returns "2")

## License

Copyright © 2012 Álvaro Cuesta.

Distributed under the Eclipse Public License, the same as Clojure uses. See the file [COPYING](https://github.com/alvaro-cuesta/clojoban/blob/master/COPYING).