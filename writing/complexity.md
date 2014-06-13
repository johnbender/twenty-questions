# How Powerful is Specification by Example?

During the course of our work we have wondered off and on how we might characterize the difficulty and power of building an expression with the user's help. More precisely, we would like to answer the question: how much computational power would be required to match the combination of a user and reasonably powerful computer in this task?

# An Important Decision

To fit our problem into a  complexity class we first need to find a decision problem that makes for a reasonable approximation and our initial thinking focused on type inhabitation. Type inhabitation is the problem of deciding, given a type environment and a type, if a there exists a term that satisfies the type [1]. If we consider types in this context as a simple initial specification then finding a term that fits that specification is a basic requirement of our tool. Further, since type inhabitation is known to be PSPACE-complete for the simply typed lambda calculus (intuitionistic propositional logic [2]) we at least have a starting point.

# Interactive Proof Protocol



- IP fits the notion of interaction
- type inhabitation
- type/theorem as a basic spec
- finding the right one certainly means it's inhabited
- type inhabitation for stcl is PSPACE
- interaction is attractive
- issues
- it's not known whether terms/proofs are polynomial in the size of the type/theorem
- if they were it would suggest a single round dIP protocol which means it's NP
- proof protocol for first order logic
 - very hard
 - constitutes an alternate proof that IP = PSPACE

# PCP

- inhabitation
- input polytime queryable spec
- proof is function
- exponentially sized set of outputs indexed by inputs
- exponential small inputs can be different
- predicted that random inputs wouldn't be general enough
- fits the notion of using traces as part of the spec

# Other Possibilities

- inhabitation is interesting but basic
- what we really want is to characterize
input spec + polytime computable function, proof is the actual spec or an ideal function
-
- decision problem is difficult

# Footnotes

1. http://en.wikipedia.org/wiki/Type_inhabitation
2. http://deepblue.lib.umich.edu/bitstream/handle/2027.42/23534/0000493.pdf?sequence=1
