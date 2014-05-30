# Twenty Questions

Twenty Questions is designed to be an interactive program construction and exploration tool. It takes little or no specification input and works with the user to find the program they have in mind through questions, specification refinements, and access to auxiliary information about program components.

## Approach

Here we provide a short overview of the various functions and capabilities for reference while reading through the example use cases. First lets take a look at how a user with a specification in mind might begin the search for satisfying expressions.

```
$ tq
Welcome to Twenty Questions!
tq > find [Int] -> Int
[1] sum
[2] product
...
[10] head
tq >`
```

The `find` command adds a constraint to the current specification and then rebuilds the, currently empty, list of satisfying expressions. The specification need not be limited to types but can constrain over each of the dimensions measured by Twenty Questions: inputs, outputs, traces, and types. This particular specification provides a fairly large set of candidates so the next thing to do is likely to differentiate those candidates.

```
...
[1] sum
[2] product
...
[10] head
tq > input []
[1] sum     : 0
[2] product : 1
...
[10] head   : error
```

With the user specified input of an empty list Twenty Questions provides the outputs for each of the candidate expressions. The `group` option can be added to organize the results and also weed out candidates by generating constraints to be added to the current set. Additionally, if the user is unsure of which will provide interesting outputs for the candidate functions they can also use the `auto` command and Twenty Questions will select randomized but relevant inputs based on information about the candidate expressions. Consider a simple parser:

```
tq > find String -> AST
[1] foo
[2] bar
...
[10] baz
tq > auto group
input : "foo"
[1] foo : Just FooExpr        [2] bar : Nothing
...                           ...
                              [10] baz : Nothing
```

In many cases random inputs and the corresponding outputs are not sufficient to differentiate between candidates. A random string as input to any one of these parsers would almost always result in a `Nothing` result. Here, Twenty Questions uses trace information from the candidate functions to see that there are an exponentially small number of interesting inputs and chooses one that partitions the results.

## Additional Features

* The built in REPL allows for manual expression construction, evaluation, and addition to a working set of candidate expressions [1].

* The specification set can be altered by addition of specifications through types, constraints on outputs, constraints on trace information, and property tests.


* The traces associated with each candidate can be viewed in isolation and also searched using regular expressions.

* Being a REPL Twenty Questions can also be used at any debugging breakpoint to leverage the debugging context for specifications and inputs.

* Candidate generation includes compound expressions with a few simply combinators depending on the language context.

## Examples

The following examples are included to give a more comprehensive overview of how and when Twenty Questions might be used by a developer:

- [python, oath, codehint](./oauth.md)
- [parser](./parser.md)
<
## Footnotes

1. In fact it's reasonable to consider Twenty Questions as an augmented REPL and this makes intuitive sense given that the goal is exploration.
