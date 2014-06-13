# How Powerful is Specification by Example?

During the course of our work we have wondered off and on how we might characterize the difficulty of building an expression with the user. More precisely, we would like to answer the question: how much computational power would be required to match the combination of a user and reasonably powerful computer in this task?

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
