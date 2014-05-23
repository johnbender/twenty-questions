# OAuth, Python, and CodeHint

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
