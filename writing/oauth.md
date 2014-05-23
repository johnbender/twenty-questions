# OAuth, Python, and CodeHint

- use case: library doc replacement
- user question: I have a request token how do I get an access token
- oauth flow using oauth2
- hypothetical command line set of methods
 - requires token types or object wrappers

- CodeHint approach
 - stop at breakpoint set by programmer
 - suggest paths through method calls
 - present and ask developer to choose
- TwentyQuestions approach
 - start with debugger breakpt / contextless REPL
 - suggest paths through method calls
 - differentiate candidates based on interesting inputs (traces)
 - dynamic AND static
 - REPL / debugger/ both

- CodeHint downsides
 - requires types
 - debugging context required
 - filtering candidates impossible
- TwentyQuestions downsides
 - traces may be hard to get
