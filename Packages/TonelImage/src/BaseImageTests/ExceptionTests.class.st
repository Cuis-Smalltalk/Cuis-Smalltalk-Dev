Class {
	#name : 'ExceptionTests',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Exceptions',
	#package : 'BaseImageTests',
	#tag : 'Exceptions'
}

{ #category : 'private' }
ExceptionTests >> assertSuccess: anExceptionTester [
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testDoubleOuterPass [
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testDoublePassOuter [
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testDoubleResume [
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest ) 
]

{ #category : 'testing-outer' }
ExceptionTests >> testHandlerFromAction [
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [
		[
			[self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
		] on: Error do: [ :ex | 3 / 0 ]
	] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer'
]

{ #category : 'testing' }
ExceptionTests >> testNoTimeout [
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testNonResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler ) 
]

{ #category : 'testing-outer' }
ExceptionTests >> testNonResumableOuter [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error

]

{ #category : 'testing-outer' }
ExceptionTests >> testNonResumablePass [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error

]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testResignalAs [

	| answer |
	answer := [
		[3 zork]
			on: ZeroDivide
			do: [:ex | ex return: 5]
	] on: Error do: [:ex | ex resignalAs: ZeroDivide].
	self assert: answer == 5
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testResignalAsUnwinds [

	| unwound answer |
	unwound := false.
	answer := [
		[3 zork]
			on: ZeroDivide do: [:ex | self assert: unwound.  ex return: 5]
	] on: Error do: [:ex | [ex resignalAs: ZeroDivide] ifCurtailed: [unwound := true]].
	self assert: answer == 5
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler ) 
]

{ #category : 'testing-outer' }
ExceptionTests >> testResumableOuter [

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result = 5

]

{ #category : 'testing-outer' }
ExceptionTests >> testResumablePass [

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
	self assert: result = 4

]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSignalFromHandlerActionTest [
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleEnsure [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleEnsureTestWithError [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleEnsureTestWithNotification [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleEnsureTestWithUparrow [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleIsNested [
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleOuter [
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimplePass [
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleResignalAs [
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleResume [
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleRetry [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleRetryUsing [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest ) 
]

{ #category : 'testing-ExceptionTester' }
ExceptionTests >> testSimpleReturn [
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest ) 
]

{ #category : 'testing' }
ExceptionTests >> testTimeoutWithZeroDuration [
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest ) 
]
