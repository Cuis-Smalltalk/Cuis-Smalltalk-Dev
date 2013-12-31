'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 12 April 2012 at 9:59:52 pm'!

!ExceptionTests methodsFor: 'testing-outer' stamp: 'jmv 4/12/2012 21:58'!
testHandlerFromAction
	"A test ensuring that nested exceptions work as expected."

	| result |
	"This test also fails in Squeak. Check if it is ever fixed."
	self flag: #expectedFailure.
	true ifTrue: [^self].

	result := [
		[
			[self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
		] on: Error do: [ :ex | 3 / 0 ]
	] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer'.
! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SmalltalkCompleter initialize!

