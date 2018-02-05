# High-level simulation

**Important Note:** Currently the "probing" mechanism (see `Probe.hs`) is implemented through the use of `traceIO` and the non-safe IO primitives that are intended for debugging purposes only, as they completely break the purely-functional semantics of Haskell's programs.

## Broken tracing implementations

In the `_obsolete/` directory are placed early alternative attempts to implement the "intermediates probing" mechanism. 
They are not functional but some ideas could be useful in the future.

At the end of each file there are relevant (and problematic) use cases.

- **`TraceWriter.hs`**: trying to concatenate logs in the Writer monads by removing *common prefixes*. It works in some cases but at the end it is fundamentally broken. Here the output from the `D.trace` debugging function is the one we are looking for our implementation.

- **`TraceIO.hs`**: using IO monads to print out the traces, it easily explodes. Here even the debugging function is being evaluated too many times.

- **`TraceIO2.hs`**: here is even more clear how this particular use of IO monads has some problems.

- (**`Graph.hs`**: not really interesting.)

- **`Graph2.hs`**: trying to construct a DAG from the interpretation. It kinda works (still incomplete impl.), however it explodes exponentially due to the intrinsic inefficiency in the `GKey` representation.