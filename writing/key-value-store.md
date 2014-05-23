In this example, we consider how to use Twenty Questions to help
discover and communicate the differences between multiple
implementations of a key-value store.  This illustrates the need to go
beyond input/output examples and incorporate varios kinds of side
effects.

We start with specification of something like a map, using an
Alex-style specification (i.e. unit test or contract).

```javascript
test(m) {
  m.put("key","value");
  assert(m.contains("key","value"));
}
```

We can borrow from Alex's idea, allowing the put and contains methods
to be renamed to match the candidate.  More concretely, a candidate
consists of an object and methods put and contains on that object.

We could generalize methods to arbitrary expressions, by rewriting the
test case as:

```javascript
test(m,put,contains) {
  m1 = put(m,"key","value");
  assert(contains(m2,"key","value"));
}
```

This also allows for persistent (side-effect free) maps.  This
translation can be automatic by the system, so we can express the
specification naturally in OO style without overly constraining the
results.

Let's turn now to the kinds of candidates the system might discover,
and how we might differentiate them via these test-based specs.

# Candidates

We can easily imagine multiple candidate implementations of a key
value store.  For example, in JavaScript we might have a simple
mutable (memory-based) dictionary or map, a mutable multi-map
(allowing multiple values for a single key), persistent version of the
previous two, HTML5 local storage, and a server side key value store.

There are important reasons why you might choose each of these
candidates.  Hash map is a normal in memory place for storing
values. Obvious candidate.  Multi map: can have multiple values for a
single key.  Persistent map: can share it among multiple parties,
without undermining the autonomy (encapsulation) of either
party. I.e. updates by one party not visible to the other.  Local
storage: PROS persistence across session, faster (and less expensive
for the server operators) than storing on server. CONS: not shared
among devices.  Server side storage: PROS: persistence, shared among
devices. CONS: slower, expensive for large amounts of data.

Other candidates: cached server side KV store, only writes out, but
caches for reads. Good for certain applications, but if we want to see
writes made by other clients, may not be what is intended.

How do we test for and report these kinds of differences?  We could
attempt to model multiple clients with local storage interacting with
a server, etc.  SUPER crazy complicated.

One might argue that these considerations are misguided; that they
violate the encapsulation of the candidates.  These are implementation
details, right?  Yes, but we are not tightly-coupling with these
details, so we're not writing brittle code.  In fact the problem is
precisely that there are multiple compatible candidates with important
differences. It is important that the programmer understand the
differences before making a decision.

The point is that there are a lot of candidates that satisfy the
initial test case. Now we want to propose new test cases until only a
single candidate satisfies all the tests.  Furthermore, we want to
propose SIMPLE test cases, and FEWEST possible.

# Detecting and Presenting Differences

How can we differentiate mutable hash maps implemented in memory,
HTML5 local storage, or on the server?  It may be difficult or
impossible to detect these differences via input/output behavior
alone.  Still, detecting these differences is only part of the
challenge: we also have to present them to the programmer so they can
make an informed decision.

We'll focus first on presenting the differences.  If we limit
ourselves to test cases, we could do something like:

```javascript
test(m,put,contains) {
  put(m,"key","value");
  assert(contains(m,"key","value"));
  assert(HTML5_LOCAL_STORAGE_MAP_CONTAINS(m,"key","value"));
}
```

Here HTML5_LOCAL_STORAGE_MAP_CONTAINS(m,"key","value") returns true if
m is stored in HTML5 local storage, and m contains the mapping of
"key" to "value".  How could this function be implemented?  How can we
expect the user to understand what it does?

A better approach is to allow a predicate on the effect of running an
expression.

```javascript
test(m,put,contains) {
  put(m,"key","value") @e;
  assert(HTML5LocalStorage.Write in e);
  assert(contains(m,"key","value")) @e;
  assert(HTML5LocalStorage.Read in e);
  assert(HTML5_LOCAL_STORAGE_MAP_CONTAINS(m,"key","value"));
}
```

Here the make-believe syntax @e binds the effects of the preceding
statement.  Then we can make general assertions on the effects.  This
gives us a declarative way to reason about certain kinds of program
behavior at a high level of abstraction.

first diff local, server storage with tests that illustrate looking
them via some lower-level api (html5 local storage api, raw http requests)

this sucks. want higher level of abstraction: effects!

