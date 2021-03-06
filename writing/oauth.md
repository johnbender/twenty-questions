# OAuth and CodeHint

It's frequently the case that developers are assigned a task or tasks for which there is an existing library that handles most of the details involved ("glue programming"). In these cases the library is a useful abstraction that centralizes the responsibility for correctness and requires only that the developer "learn the library" and not necessarily the details of the task itself. Learning the library though is not always a trivial task.

For example, the various OAuth flows that allow a user to grant authorization to a third party to view and modify their data in some application that supports OAuth. From personal experience the steps are at least mildly confusing and the infrequency of OAuth implementation means that most developers happily forget the details of their favorite library before having to learn them all over again the next time around.

The central difficulty is finding the set of functions and data that are required to get from a request token response (OAuth v1.0) to an access token (both OAuth v1.0 and v2.0). Naturally developers rely on the documentation of the library and possibly of OAuth itself to aid in their implementation. Here we will consider both CodeHint and TwentyQuestions as a replacement for documentation in composing the necessary library functionality to get from a request token to an access token in a command line application that wants authorization from a hypothetical third party.

## On Closer Examination

As stated, the user has a request token and possibly a url but doesn't know the necessary method calls, server communications, and user interactions required to get a long term access token (long term authorization).

```
$ tq
tq > import EGCorpOAuth
tq > let rt  = ReqToken "..."
tq > let url = Url "..."
tq > :find (rt, url) -> AccToken
```

From the library [README](https://github.com/simplegeo/python-oauth2#twitter-three-legged-oauth-example) the follow up steps in a command line setting are:

1. Provide a link for, or open a browser to, the `authorize_url` with the request token included as a parameter.
2. Wait for the user to provide the verifier PIN from the authorization confirmation page.
3. Request an access token from the service using the PIN and the request token.

The ideal output of a tool meant to stand in for documentation in this example, is set of methods that handle each of these steps. Here we will assume that those methods exist in the OAuth library at hand.

## CodeHint

<iframe width="420" height="315" src="https://www.youtube.com/embed/qn5yIEe9kks#t=231" frameborder="0" allowfullscreen></iframe>

In the CodeHint demo above, the approach is centered around runtime types and so the mapping to our example might be that the user has some a URL and request token in scope and desires to have an instance of an access token. At a debugger breakpoint CodeHint will search for methods in scope that can be composed with data that is in scope at the breakpoint to satisfy an assignment to a variable of a given type. Under the assumption that a set of appropriate methods exist for this purpose, it's not hard to image that it would be able to find them and compose them to match the output type the user needs. For example:

```
tq > AccToken token = ?(rt, url)
[1] acc_token . pin . auth_url . fst  : AccToken "..."
[2] acc_token . pin . auth_url . snd  : AccToken "..."
[3] acc_token . pin . malintent . fst : AccToken "..."
tq >
```
There are several possible issues with this approach:

First, a debugging context is assumed/required. That is the tool requires a large amount of very specific context when building its satisfying candidate expressions. You might also look at this context as a single very specific input to the expression to the exclusion of all others and a limit on the ability of the developer to explore other possibilities. More concretely, if you watch the first demo closely the `jtree` parameter is used in the generated expressions to produce the desired `window` object. Being constrained by the context means that if the developer knows of some other readily accessible object not in scope that might also be of interest the tool can't help.

Second, filtering a large list of candidates requires continued execution (which is not always an option) **or** some important foreknowledge of the desired output (e.g. "Eve" in the final demo of the video). To continue with the final example of the demo video, if the user has some vague notion that they want data at the leaf of a tree but they don't have perfect knowledge of the shape or content of that data, then filtering in the manner prescribed will be difficult.

Finally and most importantly, the flows for the versions of OAuth are not the same and there's something fishy about the last entry in spite of it matching requirements. Given three nearly identical inputs and outputs it's not clear how the developer would decide or even differentiate between the two candidate expressions using either of the CodeHint approaches.

## Twenty Questions

In contrast, Twenty Questions aims to not only generate relevant candidates but also to make differentiation easier by using more information and by working *with* the user to find the ideal solution.

```
...
tq > :find (rt, url) -> AccToken
input                                 : (rt, url)
=1==================================================
[1] acc_token . pin . auth_url . fst  : AccToken "..."
[2] acc_token . pin . auth_url . snd  : AccToken "..."
[3] acc_token . pin . malintent . fst : AccToken "..."
tq >
```

Again, the flows are the same and to an uninformed user it's clearly difficult to differentiate between the options. The CodeHint approach to differentiation won't work here since the outputs are basically identical and everything important that marks these expressions as different has to do with effects.

Here, the trace information Twenty Questions uses to produce interesting inputs can also be used to inform the user about her options:

```
...
tq > :trace
input                                 : (rt, url)
=1==================================================
[1] acc_token . pin . auth_url . fst  : AccToken "..."
    1 uri rt url
    2 https_get "example.co/access_token..."
    ...
[2] acc_token . pin . auth_url . snd  : AccToken "..."
    1 https_get "example.co/request_token..."
    2 https_get "example.co/access_token..."
    ...
[3] acc_token . pin . malicious . fst : AccToken "..."
    1 https_get "straight-stealin.shady.tv/..."
    ...
tq >
```

The user views the traces of expression and then, after seeing that the each makes a different sequence of calls sees that he needs to consult his documentation further. The same trace used to provide interesting inputs for other sessions has value as a differentiating feature itself.
