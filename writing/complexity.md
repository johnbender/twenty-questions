# How Powerful is Specification by Example?

During the course of our work we have wondered off and on how we might characterize the difficulty and power of building an expression with the user's help. More precisely, we would like to answer the question: how much computational power would be required to match the combination of a user and reasonably powerful computer in this task?

## An Important Decision

To fit our problem into a  complexity class we first need to find a decision problem that makes for a reasonable approximation and our initial thinking focused on type inhabitation. Type inhabitation is the problem of deciding, given a type environment and a type, if a there exists a term that satisfies the type [1]. If we consider types in this context as a simple initial specification then finding a term that fits that specification is a basic requirement of our tool. Further, since type inhabitation is known to be PSPACE-complete for the simply typed lambda calculus (intuitionistic propositional logic [2]) we at least have a starting point.

## Interactive Proof Protocol

A possible advantage of starting with a PSPACE-complete problem is that we know PSPACE and IP [3] (problems that have interactive proofs) are the same class [4] and going through and interactive process to prove inhabitation is an intuitively satisfying fit for the context. Since inhabitation is in PSPACE there exists a reduction from inhabitation to TQBF [5] and then we could appeal to the IP protocol for TQBF but that wouldn't tell us anything new or interesting about the problem and it doesn't fit the setting. So it's useful to consider what an IP protocol for inhabitation would look like.

Our initial idea for an IP protocol for inhabitation was very simple. Send the type across to the prover and ask for a term, when the term comes back check that it satisfies the type. This appears to be a great solution because checking a term as a proof of a type is certainly polynomial for simply typed lambda calculus and it's even deterministic! But therein lies an issue. If inhabitation has a deterministic IP protocol it's also in NP [6]. Which would mean PSPACE = NP since inhabitation is PSPACE-complete. Something is clearly amiss here.

It turns out that we omitted an important assumption above, namely that the response from the prover has to be polynomial in the size of the type otherwise the verifier can't read it in polynomial time. There is a long history of work on bounds for the size of proofs and at present it's unclear whether there is always a polynomial sized proof for a given propositional formula (or a term of a type) [7] so we can't declare victory with a deterministic interactive proof protocol.

As an important aside though, it is strongly suspected that not all types have polynomial sized terms because it has been shown that if they did the polynomial hierarchy [8] would collapse. Here by showing a basic deterministic proof protocol for intuitionistic propositional logic we have informally arrived at the conclusion that, if all propositional logic terms have a polynomial sized proof, then PSPACE = NP! Since it's believed that the polynomial hierarchy is properly contained within PSPACE this would constitute further, stronger evidence that some theorems in propositional logic don't have polynomial sized proofs.

Ultimately though, a deterministic proof protocol won't work for us since we can't be sure that the prover can always come back with a term of polynomial size. Instead we would have to design a probabilistic protocol which splits the problem up and uses the power afforded by a small chance of failure to "shrink" the communication size. This would constitute an alternate proof that PSPACE = IP since, again, the inhabitation problem is PSPACE-complete and we leave this possible further work.

## PCP

An alternate way to classify the inhabitation problem is with probabilistically checkable proofs or PCP [9]. That is, we can characterize the function as an exponentially sized proof of satisfying some specification where the outputs are indexed by the inputs and we'd like to verify that function against the spec in polynomial time.

The primary issue here is that the continuity of the function is paramount in getting the probability of soundness we need when verifying the proof. Clearly any function that we might consider with our tool could have exponentially few inputs on which it behaves weirdly. For example a parser `parse "foo"` is a constant function on all inputs that are not `"foo"`, but querying that function as proof in a few places will rarely ever catch that fact.

At first, this fact appears discouraging but it predicts the issues that we eventually ran into initially with random inputs for differentiating candidates. That is, random inputs only really work when all the candidates are continuous but when they change on small sets of inputs random inputs are basically useless. In the same way if we want to show with high confidence that a function satisfies a given spec then we must account for small changes having big effects in the functions behavior.

Our solution in implementation is to include information about the "interesting" inputs/outputs for a given function in the process of generating discriminating inputs for a set of candidates. Similarly we might consider as part of the input (or even the proof!) to our verifier a small set of important inputs or input ranges to query so that we can be sure the proof behaves as we expect. It remains unclear where this normalizing set of input ranges should live and how we should go about using it.

Further, if we're going to try to account for all the weird inputs/output pairs a function might have we can only consider polynomially-wierd functions. That is we can't consider functions that are defined as tables of input and output pairs. This doesn't seem to restrictive given that the primary purpose in defining functions is to avoid defining the pairs directly!

### Footnotes

1. http://en.wikipedia.org/wiki/Type_inhabitation
2. http://deepblue.lib.umich.edu/bitstream/handle/2027.42/23534/0000493.pdf?sequence=1
3. http://courses.engr.illinois.edu/cs579/sp2011/slides/CC-S11-Lect16.pdf
4. http://en.wikipedia.org/wiki/IP_(complexity)#Proof_of_IP_.3D_PSPACE
5. http://en.wikipedia.org/wiki/True_quantified_Boolean_formula
6. http://en.wikipedia.org/wiki/Interactive_proof_system#NP
7. http://dl.acm.org/citation.cfm?id=377790
8. http://en.wikipedia.org/wiki/Polynomial_hierarchy
9. http://en.wikipedia.org/wiki/Probabilistically_checkable_proof
