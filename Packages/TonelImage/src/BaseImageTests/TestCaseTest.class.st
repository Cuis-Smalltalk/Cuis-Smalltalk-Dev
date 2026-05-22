Class {
	#name : 'TestCaseTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Testing',
	#package : 'BaseImageTests',
	#tag : 'Tools-Testing'
}

{ #category : 'changes' }
TestCaseTest >> testAssertChangesByPassesWhenActionChangesConditionByTheSpecifiedAmount [

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1; add: 2 ] changes: [ aCollection size ] by: 2
]

{ #category : 'changes' }
TestCaseTest >> testAssertChangesFromToPassesWhenActionChangesConditionFromAndToTheSpecifiedValues [

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection add: 2 ] changes: [ aCollection size ] from: 1 to: 2
]

{ #category : 'changes' }
TestCaseTest >> testAssertChangesPassesWhenConditionIsAlteredByAction [

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1 ] changes: [ aCollection size ]
]

{ #category : 'changes' }
TestCaseTest >> testAssertDoeNotChangePassesWhenConditionIsNotAlteredByAction [

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection sum ] doesNotChange: [ aCollection size ]
]

{ #category : 'includes' }
TestCaseTest >> testAssertIncludesFailsWhenElementIsNotIncludedInCollection [

	| collection anElement |

	anElement := 1.
	collection := #().

	self should: [ self assert: collection includes: anElement ] 
		 raise: TestResult failure
		withExceptionDo: [ :anException | | expectedError |
			expectedError := collection asString, ' does not include ', anElement asString.
			self assert: (anException messageText endsWith: expectedError) ]
]

{ #category : 'includes' }
TestCaseTest >> testAssertIncludesShouldNotFailWhenElementIsInCollection [
	
	| collection anElement |
	
	anElement _ 1.
	collection _ Array with: anElement.
	
	self 
		shouldnt: [ self assert: collection includes: anElement ] 
		raise: TestResult failure
]

{ #category : 'symbols' }
TestCaseTest >> testIsNotTestSelectorWhenHasParameters [

	self deny: (TestCase isTestSelector: #test01:)
]

{ #category : 'symbols' }
TestCaseTest >> testIsNotTestSelectorWhenItDoesNotStartsWithTest [

	self deny: (TestCase isTestSelector: #tes01)
]

{ #category : 'testing' }
TestCaseTest >> testIsTestCase [
	self assert: (TestCase new is: #TestCase).
]

{ #category : 'symbols' }
TestCaseTest >> testIsTestSelectorWhenStartsWithTestAndHasNoParameters [

	self assert: (TestCase isTestSelector: #test01)
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldFailErrorsWhenNoErrorIsSignaled [

	self
		should: [ self shouldFail: [] ]
		raise: TestResult failure
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldFailPassesWhenAnErrorIsSignaled [

	self shouldFail: [ self error: 'an error' ]
]

{ #category : 'should take less than' }
TestCaseTest >> testShouldNotTakeMoreThanFailsWhenClosureTakesMoreThanTheLimit [

	self shouldFail: [ self should: [(Delay forMilliseconds: 50) wait ] notTakeMoreThan: 10 milliSeconds ]
]

{ #category : 'should take less than' }
TestCaseTest >> testShouldNotTakeMoreThanPassesWhenClosureTakesLessThanTheLimit [

	self shouldntFail: [ self should: [] notTakeMoreThan: 1 milliSeconds ]
		
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithExceptionDoCanExpectException [

	"This test is important because TestFailure is a subclass of Exception, therefore if we are expecting Exception to be raised it should not 
	catch the TestFailure - Hernan"
	
	| failureRaised |
	
	failureRaised := false.
	
	[self should: [] raise: Exception ]
		on: TestResult failure
		do: [ :failure | failureRaised := true ].
		
	self assert: failureRaised 
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithExceptionDoFailsWhenNoExceptionIsSignaled [
	
	| failureSignaled |
	
	failureSignaled := false.
	
	[self 
		should: []
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evaluate this block' ]]
		on: TestResult failure
		do: [:failure | failureSignaled := true ].
		
	self assert: failureSignaled 
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithExceptionDoFailsWhenOtherExceptionTypeIsSignaled [

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	[self 
		should: [ exceptionToRaise signal ]
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evalaute this block' ]]
		on: Error
		do: [:anError | self assert: anError equals: anError ].
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithExceptionDoPassSignaledExceptionToAssertionsBlock [

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			self assert: signaledException equals: exceptionToRaise ].
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithExceptionDoValuesAssertionsBlockWhenExceptionIsRaised [

	| exceptionToRaise assertionsBlockEvaluated |
	
	exceptionToRaise := Error new.
	assertionsBlockEvaluated := false.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			assertionsBlockEvaluated := true ].
		
	self assert: assertionsBlockEvaluated 
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithMessageTextDoesNotFailWithRightMessageText [

	| messageText |
	
	messageText := 'some message'.
	
	self 
		shouldnt: [ 
			self 
				should: [ self error: messageText ]
				raise: Error
				withMessageText: messageText ]
		raise: TestResult failure 
		
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShouldRaiseWithMessageTextFailsWithDifferentMessageText [

	self 
		should: [ 
			self 
				should: [ self error: 'some message' ]
				raise: Error
				withMessageText: '' ]
		raise: TestResult failure
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShoulndFailErrorsWhenAnErrorIsSignaled [

	self
		should: [ self shouldntFail: [ self fail ]]
		raise: TestResult failure
]

{ #category : 'exception testing tests' }
TestCaseTest >> testShoulndFailPassesWhenNoErrorIsSignaled [

	self shouldntFail: [ 1+2 ]
]
