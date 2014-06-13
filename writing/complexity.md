# How Powerful is Specification by Example?

During the course of our work we have wondered off and on how we might characterize the difficulty and power of building an expression with the user's help. More precisely, we would like to answer the question: how much computational power would be required to match the combination of a user and reasonably powerful computer in this task?

## An Important Decision

To fit our problem into a  complexity class we first need to find a decision problem that makes for a reasonable approximation and our initial thinking focused on type inhabitation. Type inhabitation is the problem of deciding, given a type environment and a type, if a there exists a term that satisfies the type [1]. If we consider types in this context as a simple initial specification then finding a term that fits that specification is a basic requirement of our tool. Further, since type inhabitation is known to be PSPACE-complete for the simply typed lambda calculus (intuitionistic propositional logic [2]) we at least have a starting point.

## Interactive Proof Protocol

A possible advantage of starting with a PSPACE-complete problem is that we know PSPACE and IP [3] (problems that have interactive proofs) are the same class [4] and going through and interactive process to prove inhabitation is an intuitively satisfying fit for the context. Since inhabitation is in PSPACE there exists a reduction from inhabitation to TQBF [5] and then we could appeal to the IP protocol for TQBF but that wouldn't tell us anything new or interesting about the problem and it doesn't fit the setting. So it's useful to consider what an IP protocol for inhabitation would look like.

Our initial idea for an IP protocol for inhabitation was very simple. Send the type across to the prover and ask for a term, when the term comes back check that it satisfies the type. This appears to be a great solution because checking a term as a proof of a type is certainly polynomial for simply typed lambda calculus and it's even deterministic! But therein lies an issue. If inhabitation has a deterministic IP protocol it's also in NP [6]. Which would mean PSPACE = NP since inhabitation is PSPACE-complete. Something is clearly amiss here.

It turns out that we omitted an important assumption above, namely that the response from the prover has to be polynomial in the size of the type otherwise the verifier can't read it in polynomial time. There is a long history of work on bounds for the size of proofs and at present it's unclear whether there is always a polynomial sized proof for a given propositional formula (or a term of a type) [7] so we can't declare victory with a deterministic interactive proof protocol.

As an important aside though, it is strongly suspected that not all types have polynomial sized terms because it has been shown that if they did the polynomial hierarchy [8] would collapse. Here we have informally arrived at the conclusion that, if all first order propositional logic terms have a polynomial sized proof, then PSPACE = NP! Since it's believed that the polynomial hierarchy is properly contained within PSPACE this would constitute further, stronger evidence that some theorems in propositional logic don't have polynomial sized proofs.



- issues
- it's not known whether terms/proofs are polynomial in the size of the type/theorem
- if they were it would suggest a single round dIP protocol which means it's NP
- proof protocol for first order logic
 - very hard
 - constitutes an alternate proof that IP = PSPACE

## PCP

- inhabitation
- input polytime queryable spec
- proof is function
- exponentially sized set of outputs indexed by inputs
- exponential small inputs can be different
- predicted that random inputs wouldn't be general enough
- fits the notion of using traces as part of the spec

## Other Possibilities

- inhabitation is interesting but basic
- what we really want is to characterize
input spec + polytime computable function, proof is the actual spec or an ideal function
-
- decision problem is difficult

### Footnotes

1. http://en.wikipedia.org/wiki/Type_inhabitation
2. http://deepblue.lib.umich.edu/bitstream/handle/2027.42/23534/0000493.pdf?sequence=1
3. http://courses.engr.illinois.edu/cs579/sp2011/slides/CC-S11-Lect16.pdf
4. http://en.wikipedia.org/wiki/IP_(complexity)#Proof_of_IP_.3D_PSPACE
5. http://en.wikipedia.org/wiki/True_quantified_Boolean_formula
6. http://en.wikipedia.org/wiki/Interactive_proof_system#NP
7. http://dl.acm.org/citation.cfm?id=377790
8. http://en.wikipedia.org/wiki/Polynomial_hierarchy
