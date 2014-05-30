In this example, we will use twenty questions to synthesize a parser
for a basic arithmetic calculator. We start by defining our AST
representation, which will be the output type for our parser. For
clarity we will use a haskell syntax:

```haskell
data AST =
    Num Int
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
```

We define an initial specification by input/output pairs:

```haskell

parse      :: String -> AST

-- specification by example:
([1-9][0-9]*)   == Num \1
```

This defines how to parse numbers, using a regular expression.  For
remaining expressions, we add constraining examples in terms of
numbers:

```haskell
"1 + 2"         == Add (Num 1) (Num 2)
"1 - 2"         == Sub (Num 1) (Num 2)
"1 * 2"         == Mul (Num 1) (Num 2)
"1 / 2"         == Div (Num 1) (Num 2)
```

This specification is ambiguous.  Key issues are precedence, and
associativity. Furthermore, the desired parser is most naturally
expressed using left recursion, which can complicate implementation.
There are standard techniques for dealing with each of these issues.
However, with the help of twenty questions, we can abstract away these
techniques.  The system asks questions about the desired behavior, and
implements it using the standard techniques. For example, to establish
the associativity of addition, we can ask the use to choose between
two examples.

```haskell
"1 + 2 + 3" == Add (Add (Num 1) (Num 2)) (Num 3)
"1 + 2 + 3" == Add (Num 1) (Add (Num 2) (Num 3))

      Add
     /   \
    /     \
(Num 1)   Add
         /   \
	    /     \
    (Num 2)  (Num 3)

             Add
		    /   \
		   /     \
	     Add    (Num 3)
	    /   \
	   /     \
   (Num 1)  (Num 2)
```

We can indicate that addition should be left associative by choosing
the second option.  This would result in adding the following example
to be added to the specification:

```haskell
"1 + 2 + 3" == Add (Add (Num 1) (Num 2)) (Num 3)
```

Suppose we now want to specify that all operators should be
left-associative, without having to answer questions for each.  We
could do this via a regex and wildcards:

```haskell
"1 ([+/*-]) 2 ([+/*-]) 3" == <1> (<2> (Num 1) (Num 2)) (Num 3)
```

This constraint says that inputs matching the regular expression
should match the pattern on the right hand side.  Here the symbols
`<1>` and `<2>` represent variables in the constraint language, whose
values are to be determined by the corresponding group in the regex.
This single constraint can be expanded into 16 simpler constraints:

```haskell
"1 + 2 + 3" == Add (Add (Num 1) (Num 2)) (Num 3)
"1 + 2 - 3" == Add (Sub (Num 1) (Num 2)) (Num 3)
"1 + 2 * 3" == Add (Mul (Num 1) (Num 2)) (Num 3)
"1 + 2 / 3" == Add (Div (Num 1) (Num 2)) (Num 3)
...
```

Suppose now that we've established that all operations are
left-associative. Precedence of addition and multiplication.

```haskell
"1 + 2 * 3" == Add (...) (...)
"1 + 2 * 3" == Mul (...) (...)
```

Here the ellipses indicate a folded GUI widget.  One difference
between these options is clear just by considering the constructors
(Add and Mul).  The user could expand the widgets to explore the
choices further before making a choice.  In this example, we can make
a choice immediately: multiplication should take precedence over
addition, so we select the first example (`Add` at the root indicates
it has lower precedence).  This causes the (unfolded) example to be
added to the specification.

```haskell
"1 + 2 * 3" == Add (Num 1) (Mul (Num 2) (Num 3))
```

As we did for associativity, we can manually generalize this rule to
other operators:

```haskell
"1 ([+-]) 2 ([*/]) 3" == <1> (Num 1) (<2> (Num 3) (Num 3))
```

Again, the symbols `<1>` and `<2>` denote variables whose values
should be determined by the first and second groups in the regex,
respectively.  This would expand to:

```haskell
"1 + 2 * 3" == Mul (Add (Num 1) (Num 2)) (Num 3)
"1 + 2 / 3" == Div (Add (Num 1) (Num 2)) (Num 3)
"1 - 2 * 3" == Mul (Sub (Num 1) (Num 2)) (Num 3)
"1 - 2 / 3" == Div (Sub (Num 1) (Num 2)) (Num 3)
```

Now we have have two conflicting constraints:

```haskell
"1 ([+/*-]) 2 ([+/*-]) 3" == <1> (<2> (Num 1) (Num 2)) (Num 3)
"1 ([+-]) 2 ([*/]) 3"     == <1> (Num 1) (<2> (Num 2) (Num 3))
```

The intent is for the second to override the first.  One way to
achieve this is to track the order in which constraints are
listed. The second constraint should take precedence over the first
because it came later in the specification.  Alternatively, we can
view the second as more specific than the first, and say that more
specific constraints take precedence over more general ones.
Formalizing this notion of specificity would require careful thought.

At this point, we should have a fully specified parser.  The complete
specification is:

```haskell
([1-9][0-9]*)   == Num \1
"1 + 2"         == Add (Num 1) (Num 2)
"1 - 2"         == Sub (Num 1) (Num 2)
"1 * 2"         == Mul (Num 1) (Num 2)
"1 / 2"         == Div (Num 1) (Num 2)
"1 ([+/*-]) 2 ([+/*-]) 3" == <1> (<2> (Num 1) (Num 2)) (Num 3)
"1 ([+-]) 2 ([*/]) 3"     == <1> (Num 1) (<2> (Num 2) (Num 3))
```

From this, a sufficiently powerful synthesis engine (with reasonable
heuristics built-in) should be able to synthesize the desired parser.
All without the programmer having to learn tricks for establishing
associativity and precedence, or dealing with left-recursion.
