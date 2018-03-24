# Data Algebrae 🎩

## Intro

This library provides some basic _[1]_ mutation-only _[2]_ reified _[3]_
operations for working with common data types in PureScript, as well as some
convenience functions for generating plans. What does that mean?

_[1]_. **Basic** means that these are literally just data structures. The [docs
on Pursuit](https://pursuit.purescript.org/packages/purescript-data-algebrae/2.0.0)
will tell you everything you need, as each constructor of the data type
represents each possible action.

_[2]_. **Mutation-only** means that none of these allow you to _access_ the
data. This is a deliberate choice: your interpreter implementations can be
written safe in the knowledge that none of the intermediate state is actually
required externally, so it doesn't need to be preserved. If we care about
performance, this means that we are free to write **stateful** interpreters
that modify values in place using [the `ST` operations](https://pursuit.purescript.org/packages/purescript-st)
or even [a `Ref`](https://pursuit.purescript.org/packages/purescript-refs).

_[3]_. **Reified** is a scary word that means "we represent our functions as a
data type". We've _lifted_ the idea of our computation into data, and thus made
it totally pure!

## Contributing

PRs, PRs, PRs! All welcome - this library will be biased by what I need (most
likely for [Panda](https://github.com/i-am-tom/purescript-panda), but I would
be delighted if anyone wanted to add other structures or better tests!
