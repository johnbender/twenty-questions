# OAuth, Python, and CodeHint

It's frequently the case that developers are assigned a task for which there is an existing library that handles most of the details involved ("glue programming"). In these cases the library is a useful abstraction that centralizes the responsibility for correctness and requires only that the developer "learn the library" and not necessarily the details of the task itself. Learning the library though is not always a trivial task.

For example, the various OAuth flows that allow a user to grant authorization to a third party to view and modify their data in some application that supports OAuth. From personal experience the steps are at least mildly confusing and the infrequency of OAuth implementation means that most developers happily forget the details of their favorite library before having to learn them all over again the next time around.

![oauth yahoo](https://developer.yahoo.com/oauth/guide/images/oauth_graph.gif)

The central difficulty is understanding the set of functions and data that are required to get from a request token (OAuth v1.0) to an access token (both OAuth v1.0 and v2.0). Naturally developers rely on the documentation of the library and possibly of OAuth itself to aid in their implementation.

- use case
 - library doc replacement
 - "I have a request token how do I get an access token?"

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
