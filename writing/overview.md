# Twenty Questions

Twenty Questions is designed to be an interactive program construction and exploration tool. It takes little or no specification input and works with the user to find the program they have in mind through questions, specification refinements, and access to auxiliary information about program components.

## Approach

Here we provide a short overview of the various functions and capabilities for reference while reading through the example use cases.



- start with spec
 - possibly empty
 - types
 - inputs
 - debugging context
 - other context

- generate candidates
 - fit spec
 - heuristic approach
 - composition
 - leverage context

- differentiate candidates
 - traces
 - random inputs
 - nominal

- interact with user to further refine candidates
 - add/remove candidates methods
 - alter specs
  - add/remove input
  - add/remove constraints
  - add/remove properties
 - view traces for candidates
 - grep through any of the above

## Examples

- int functions
- python, oath, codehint
- parser
- Hasam's clock
