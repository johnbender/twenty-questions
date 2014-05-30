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
``

The `find` command adds a constraint to the current specification and then rebuilds the, currently empty, list of satisfying expressions. The specification need not be limited to types but can constrain over each of the dimensions measured by Twenty Questions: inputs, outputs, traces, and types. This particular specification provides a fairly large set of candidates so the next thing to do is likely to differentiate those candidates.

```
...
[1] sum
[2] product
...
[10] head
tq >
```


- start with spec
 - possibly empty
 - types
 - inputs
 - debugging context
 - other context

- generate candidates
 - fit spec
 - heuristic approach
 - composition
 - leverage context

- differentiate candidates
 - traces
 - random inputs
 - nominal

- interact with user to further refine candidates
 - add/remove candidates methods
 - alter specs
  - add/remove input
  - add/remove constraints
  - add/remove properties
 - view traces for candidates
 - grep through any of the above

## Examples

- int functions
- python, oath, codehint
- parser
- Hasam's clock
