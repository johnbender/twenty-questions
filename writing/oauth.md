# OAuth, Python, and CodeHint

It's frequently the case that developers are assigned a task or tasks for which there is an existing library that handles most of the details involved ("glue programming"). In these cases the library is a useful abstraction that centralizes the responsibility for correctness and requires only that the developer "learn the library" and not necessarily the details of the task itself. Learning the library though is not always a trivial task.

For example, the various OAuth flows that allow a user to grant authorization to a third party to view and modify their data in some application that supports OAuth. From personal experience the steps are at least mildly confusing and the infrequency of OAuth implementation means that most developers happily forget the details of their favorite library before having to learn them all over again the next time around.

The central difficulty is finding the set of functions and data that are required to get from a request token response (OAuth v1.0) to an access token (both OAuth v1.0 and v2.0). Naturally developers rely on the documentation of the library and possibly of OAuth itself to aid in their implementation. Here we will consider both CodeHint and TwentyQuestions as a replacement for documentation in composing the necessary library functionality to get from a request token to an access token in a command line application that wants authorization from a hypothetical third party.

## On Closer Examination

As stated, the user has a request token and possibly a set of URLs but doesn't know the necessary method calls, server communications, and user interactions required to get a long term access token (long term authorization).

```python
request_token_url = 'http://twitter.com/oauth/request_token'
access_token_url = 'http://twitter.com/oauth/access_token'
authorize_url = 'http://twitter.com/oauth/authorize'

request_token = get_request_token(request_token_url)
```

From the library [README](https://github.com/simplegeo/python-oauth2#twitter-three-legged-oauth-example) the follow up steps in a command line setting are:

1. Provide a link for, or open a browser to, the `authorize_url` with the request token included as a parameter.
2. Wait for the user to provide the verifier PIN from the authorization confirmation page.
3. Request an access token from the service using the PIN and the request token.

The ideal output of a tool meant to stand in for documentation in this example, is set of methods that handle each of these steps. Here we will assume that those methods exist in the OAuth library though in reality the prompting of the user is not included.

## CodeHint

<iframe width="420" height="315" src="https://www.youtube.com/embed/qn5yIEe9kks#t=231" frameborder="0" allowfullscreen></iframe>

In the CodeHint demo above, the approach is centered around types and so the mapping to our example might be that the user has some instance of `RequestToken` and desires to have an instance of `AccessToken`. At a debugger breakpoint CodeHint will search for methods [1] that can be composed with data that is in scope at the breakpoint to satisfy an assignment to a variable of given type. Under the assumption that a set of appropriate methods exist for this purpose, it's not hard to image that it would be able to find them and compose them to match the output type the user needs.

```python
access_token = request_access_token(get_pin(auth_url(request_token))
```
There are several possible issues with this approach:

First, a debugging context is assumed/required. That is the tool requires a large amount of very specific context when building its satisfying candidate expressions. You might also look at this context as a single very specific input to the expression to the exclusion of all others and a limit on the ability of the developer to explore other possibilities. More concretely, if you watch the first demo closely the `jtree` parameter is used in the generated expressions to produce the desired `window` object. Being constrained by the context means that if the developer knows of some other readily accessible object not in scope that might also be of interest the tool can't help.

Second, filtering a large list of candidates requires continued execution (which is not always an option) or some important foreknowledge of the desired output (e.g. "Eve" in the final demo of the video). To continue with the final example of the demo, if the user has some vague notion that they want data a the leaf of a tree but they don't have perfect knowledge of the shape or content of that data, then filtering in the manner prescribed will be difficult.

Finally, the flows for the first and second versions of OAuth are not the same, as the second version can jump strait to forwarding the user to the authorization url without having to gather a request token. Given two nearly identical inputs and outputs (tokens) it's not clear how the developer would decide or even differentiate between the two candidate expressions using the CodeHint approach.

## Twenty Questions

In contrast, Twenty Questions aims to not only generate relevant candidates but also to make differentiation easier by using more information and by working *with* the user to find the ideal solution.

Clearly the REPL described in the overview can be leveraged at a breakpoint so that the same context is available but it's not required. Suppose that, instead, the user decides to simply hop into a TQ REPL to try out the different methods for getting from one token type to another.

```
$ tq
Welcome to Twenty Questions!
tq > import OAuth
tq > rt = RequestToken( ... )
tq > find rt -> AccessToken
...
...
candidates:
[1] request_access_token(get_pin(auth_url(rt)))
[2] request_access_token(get_pin(rt.identifier))
```

There are few things to note here. The `find` primitive begins the search for a new candidate in the current context and takes as an argument some specification. In this case the specification happens to be a type since that accurately captures the goals of the user. Also, in the specification an object in scope is used for it's type and then is also considered as a candidate argument for the method compositions that fit the specification.

Most importantly it's not at all clear which set of method composition is more desirable, and using the CodeHint method there isn't really a way to differentiate the two. The outputs should be basically the same.


- TwentyQuestions approach
 - REPL / debugger/ both
 - suggest paths through method calls
 - differentiate candidates based on interesting inputs (traces)

- TwentyQuestions downsides
 - traces may be hard to get

- another (?) example
 - user has code from oauth two
 - doesn't know that it's different from oauth one request token
 - question: how do i get an access token?
 - library provides both oauth one and two methods
 - differentiation might be impossible with just types / outputs
 - trace can differentiate based on methods called / dependencies

## Footnotes

1. It's not clear exactly what the scope is, we assume it's the classpath.
