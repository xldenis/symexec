Subjects to Explore:
- Arena allocating all the AST types
- Memory model
- Incrementality of the SMT solver 
- Arena allocating the SMT solver 
- Reductions
- Hooking up to viper parser?
    - Provides initial benchmarks (use mendel examples?)
- Modularity
- Error reporting

==============

Error Reporting

I tried installing `color_eyre` and `tracing_error` to get nicer errors "out of the box". They both seem quite useful but their combination isn't exactly what is needed to report object-level errors. 
It will definitely be beneficial when reporting 'meta' level errors.
Figuring out how to make the two co-exist is a challenge remaining to be solved. 

I think in the end it might be necessary to manually track object-level errors, but then format them with a library like `miette`. We'll see.... 
This scenario would impl that the overall type of things in the symbolic executor is

```
eyre::Result<Result<T, SymErr>>
```

the outer result captures a meta-level error: the executor crashing. The inner error captures a verification failure.

Maybe `eyre` can let me have a custom-enough presentation of the errors where "Verification errors" are printed the way I expect while bugs are printed with a stack trace.