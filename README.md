Anemone: ugly sea creatures
=======

We have a bunch of C functions, as well as Haskell functions for generating C code, that would benefit from being shared across projects, being benchmarked, and being tested with quickcheck.
The plan is for this to be a cabal library that packages up the C functions nicely, allowing you to link them in to an existing Haskell project easier.

## Linking and header files
If we just add the C source files to "c-sources" in the cabal file, I believe they should be compiled and linked in to the library binary.
Then, when using from Icicle or another project, we should be able to just include the local path to the header.

```
#include "lib/anemone/cbits/anemone.h"

int i = anemone_string_to_int(...);
```

For Jetski, we might also want a definition
```
getHeaderFile :: ByteString
```
which we can prepend to the jetski code.
Then, when we load the Jetski generated dylib, the Haskell program will have already loaded the Anemone functions, so any Anemone references in the dylib should work.


## Haskell-side wrappers
We probably want some simple Haskell-side wrappers for all or most of the functions, just so we can call out to them to test.
Or is this easier if we just use Jetski? I suspect it's about the same.


## Quickcheck
We seem to have a pretty good idea of how to test the C functions: for the most part it's not very different.

### Segfaults
I wonder whether there is any way to recover from segfaults in a test.
The ideal, I think, would be to fork off a new process to execute the test.
Perhaps we could even afford to fork a new process per iteration, that way we wouldn't need any communication between the two.
We aren't sure how GHC runtime would deal with us forking, but perhaps jetski would be useful to generate a new function that forks, calls the function and synchronises etc.
That way our forked process would not need to return to GHC.
But this makes returning a value from the forked process hard.

Actually - can we just set up interrupt handlers to handle segfault?

OR I have a totally crazy idea:
```
fork_try(funptr) {
    int pid = fork();
    if (pid == 0) {
        /* child */

        /* try to execute value */
        ret_t ret = funptr(args...);

        /* it succeeded, so kill parent */
        kill(getppid(), 9);

        /* return because we are the new "real deal" */
        return ret;

    } else {
        /* parent */

        /* wait for child to die */
        /* if child dies before parent, child must have segfaulted */
        wait(pid);

        return CHILD_DID_SEGFAULT;
    }
}
```


## Benchmarking
Criterion should be similar to using Quickcheck


## Generating C code
I'm tempted to add a very simple AST for C, with a pretty printer to Doc.
I think the StringWord stuff should go in here.


