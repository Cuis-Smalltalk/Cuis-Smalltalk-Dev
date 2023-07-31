'From Cuis 6.0 [latest update: #5964] on 31 July 2023 at 12:59:04 pm'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/31/2023 11:01:12'!
assert: actual equals: expected
	"First parameter is actual result from evaluating some expression.
	Second parameter is the value expected by the designer of the test.
	This differs from Squeak, but likely agrees with most other dialects and existing tests.
	See Martin's comments at
	https://lists.cuis.st/mailman/archives/cuis-dev/2023-July/007865.html
	Some tests may assume the previous behavior. Please correct them as you find them.
	"

	self
		assert: expected = actual
		description: [ self comparingStringBetween: expected and: actual ]
! !

!TestCase methodsFor: 'assertions' stamp: 'jmv 7/31/2023 12:53:54'!
should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: anExpectedErrorMessageCreator

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException |
			self assert: anException messageText equals: anExpectedErrorMessageCreator value ]! !

!methodRemoval: TestCase #assert:equalsExpected: stamp: 'jmv 7/31/2023 12:58:07'!
TestCase removeSelector: #assert:equalsExpected:!
