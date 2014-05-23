# OAuth, Python, and CodeHint

It's frequently the case that developers are assigned a task or tasks for which there is an existing library that handles most of the details involved ("glue programming"). In these cases the library is a useful abstraction that centralizes the responsibility for correctness and requires only that the developer "learn the library" and not necessarily the details of the task itself. Learning the library though is not always a trivial task.

For example, the various OAuth flows that allow a user to grant authorization to a third party to view and modify their data in some application that supports OAuth. From personal experience the steps are at least mildly confusing and the infrequency of OAuth implementation means that most developers happily forget the details of their favorite library before having to learn them all over again the next time around.

The central difficulty is finding the set of functions and data that are required to get from a request token response (OAuth v1.0) to an access token (both OAuth v1.0 and v2.0). Naturally developers rely on the documentation of the library and possibly of OAuth itself to aid in their implementation. Here we will consider both CodeHint and TwentyQuestions as a replacement for documentation in composing the necessary library functionality to get from a request token to an access token in a command line application that wants authorization from a hypothetical third party.

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

The ideal output of a tool meant to stand in for documentation in this example, is set of methods that handle each of these steps. Here we will assume that those methods exist in the OAuth library though in reality the prompting of the user is not.


- example
 - python
 - oauth flow using oauth two
 - hypothetical command line set of methods
 - requires token types or object wrappers

- example
 - user has code from oauth two
 - doesn't know that it's different from oauth one request token
 - question: how do i get an access token?
 - library provides both oauth one and two methods
 - differentiation might be impossible with just types / outputs
 - trace can differentiate based on methods called / dependencies

- CodeHint approach
 - stop at breakpoint set by programmer
 - suggests chain of method calls
 - present and ask developer to choose
 - manual differentiation

- TwentyQuestions approach
 - dynamic AND static
 - REPL / debugger/ both
 - suggest paths through method calls
 - differentiate candidates based on interesting inputs (traces)

- CodeHint downsides
 - requires types
 - debugging context required
 - filtering difficult
  - knowledge of desired result required
  - subsequent execution in context required

- TwentyQuestions downsides
 - traces may be hard to get
