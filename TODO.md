# TODO

## Mandatory changes

* Array initialization for local variables without using `new`

## Important changes

* An array reference can have an expression on the left hand side? `expr [ expr ]`

## Optional changes

* change `Positioned` into `Ranged` so that it provides start and end positions?
* Method overloading?
* Optimize increment and decrement operations
* Fix `newarray` instruction output, optimize one-dimensional array creation
  using `multianewarray`

## Doubtful changes

* support `null`?
