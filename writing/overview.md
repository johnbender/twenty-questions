# Twenty Questions and Spec by Example

In API discovery, we want to find something that satisfies our needs, which can be expressed as a kind of specification. Traditional specifications can be used to emulate the object they are describing, as in Hesam's work on declarative mocking.  These specifications are very powerful but they can be unwieldy, require a lot of effort from the programmer and so may not be appropriate for API discovery.

Then what do we need for API discovery?  We need enough information to select a piece of code from a set of possibilities.  The specification serves as communication to the system which choice we want.  If there is only one choice, no specification is needed at all! If there are a finite number of choices, we can adopt some naming scheme, and simply give the name (or a set of names if there are multiple acceptable choices). Once naming is no longer practical, we can specify constraints as input/output pairs or unit tests as in Alex's demo.  Still, we don't need a complete specification -- only need enough to make a choice.

What do we do when there are many possible candidates for a given specification?  It isn't reasonable to require the programmer to give a more precise specification. Instead, the system should be able automatically discover properties that differentiate the available options, and present choices to the user. Our approach repeats this process to reduce the set of candidate programs that satisfy the specification.  We call this approach Twenty Questions, named after the guessing game. Each decision made by the user leads to a more refined
specification, until it is satisfied by only a single candidate. We demonstrate the idea with Specification by Example, which uses a set of required input/output pairs as a specification. This provides a simple basis for finding specifications that differentiate between candidates. The idea of Twenty Questions can be combined with other specification techniques (e.g. logical specifications), though that is outside the scope of this project.

The idea behind twenty questions for specification by example is to use automated testing to find small
input/output pairs that partitions the set of candidates into (ideally) a small number of roughly equal-sized subsets.  Hopefully, this will allow the user to easily recognize and indicate the desired behavior because it should be easier for the user to recognize desired behaviors for particular inputs than to communicate a specification for large sets of inputs.

For the purposes of illustration we initially consider a simple example where the programmer wants to build a program that takes a list of integers and produces a single integer. Generally, she might have a notion of how all these integers are going to be combined but maybe she isn't quite sure what the outcome should be or she simply doesn't know the name of the relevant functions in the programming context.

## Jungloid

With the stage set we turn to Jungloid which attempts to solve this problem for pairs of types (input and output) in the Java programming language. Here and in later examples we have constructed fake command line sessions that illustrate the basic ideas behind each approach the problem of candidate selection and specification refinement.

```
$ jungloid
jungloid > :find [Int] -> Int
[1] sum
[2] product
...
[50] head
jungloid >
```

The type pair being fed to Jungloid is `([Int], Int)` and it produces a list of some fifty expressions that satisfy this type pair ranked by the "size" of the expression. Note, that here the size of the visible expressions is essentially one because they each involve one function but that normally wouldn't be the case. Further, note that the dealing in types here is only for the purpose of establishing an example the reader may have existing intuition for and specifications can range over many different attributes of candidate expressions as we will see.

Clearly, when Jungloid can't provide a small number of candidate expressions the user is left out in the cold.

## CodeHint

The CodeHint project is a continuation of the work started in Jungloid and changes both the means by which the user "defines" a specification and adds some initial tools for refining the candidates.

```
$ codehint app.java
stopped at 1:9 in app.java:
1 => int x = ?([1,2]);
codehint > :find
[1] sum     : 3
[2] product : 2
...
[50] head   : 1
codehint >
```

Instead of entering a specification directly the user provides a breakpoint and an expression at which the debugger stops to find candidates for. The breakpoint provides valuable context since now anything that's in scope (variables, libraries, methods, state, etc) can be used to generate candidates.

```
...
codehint > :find
[1] sum     : 3
[2] product : 2
...
[50] head   : 1
codehint > :step                #1
...
codehint > :spec ?([1,2]) > 1   #2
[1] sum     : 3
[2] product : 2
...
codehint >
```

Additionally, CodeHint provides two ways to further differentiate between candidates (or in other words refine the specification). First it allows the user to continue execution past the breakpoint with each of the candidate expressions to see what happens (#1). Second allows the user to manually filter the expressions based on their final evaluated form (#2).

This gives the user a better chance of synthesizing a useful candidate but it has a few important drawbacks. First, the debugging context can be seen as a very specific and possibly restrictive input for the candidate generation. For example, if the user is aware of some possibly interesting bit of information that is outside the scope of the breakpoint there's no way to make use of it. Second, it's virtually impossible to account for the side-effects of candidate expressions in either the spec or the continued execution. Finally, the specification refinement is manual and often requires intimate knowledge of the information that's available as a result.

## Twenty Questions

Twenty Questions is an attempt to address some of the shortcomings of these approaches to synthesis by generating a specification with help of the user and by providing more information about candidates where possible.

```
$ tq
tq > :find [Int] -> Int
intput  : []
output  : ok        | output  : error
==1=================+==2==================
sum     : 0         | head    : error
product : 1         | head    : error
...                     ...
1 or 2
tq >
```

For this specification Twenty Questions has generated an input that produces "interesting" behavior, which we loosely define as "best differentiating" behavior, and presented the use with a choice between the two groups.

```
...
output  : OK  | output   : error
=1==============2=======================
sum           | head
product       | foo
...
tq > 1
input   : [1,1]
output  : 2   | output   : 1
=1==============2=======================
sum           | product
length        |
...
tq >
```

Selecting one option or the other can be seen as adding to the specification and thereby refining the set of candidates. Here the additional specification in pseudo-code might be `f [] -> Ok` to say that the user expects the function to deal with an empty list gracefully. With that addition in place, a new input is generated that splits the list up once again.

The key takeaway is that the questions can be generated automatically and each answer is an example of the user expects the function to behave that can be added to the specification until a single viable candidate is reached.

The challenge then is to extend this notion of "interesting questions" beyond simple inputs and outputs. As we'll see they are often insufficient to differentiate meaningfully between candidates.

## Further Reading

The following examples are included to give a more comprehensive overview of how and when Twenty Questions might be used by a developer:

* [OAuth and Codehint](./oauth.md) - When inputs and outputs fail.
* [Parsing by Example](./parser.md) - Synthesizing an parser by twenty questions.
