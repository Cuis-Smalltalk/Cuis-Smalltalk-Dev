"
A TestCase is a Command representing the future running of a test case. Create one with the class method #selector: aSymbol, passing the name of the method to be run when the test case runs.

When you discover a new fixture, subclass TestCase, declare instance variables for the objects in the fixture, override #setUp to initialize the variables, and possibly override# tearDown to deallocate any external resources allocated in #setUp.

When you are writing a test case method, send #assert: aBoolean when you want to check for an expected value. For example, you might say "self assert: socket isOpen" to test whether or not a socket is open at a point in a test.
"
Class {
	#name : 'TestCase',
	#superclass : 'Object',
	#instVars : [
		'testSelector',
		'failureString',
		'raisedError',
		'assertCount'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'building suites' }
TestCase class >> addTestsFor: classNameString toSuite: suite [

	| cls  |
	cls := Smalltalk at: classNameString ifAbsent: [ ^suite ].
	^cls isAbstract 
		ifTrue:  [
			cls allSubclasses do: [ :each |
				each isAbstract ifFalse: [
					each addToSuiteFromSelectors: suite ] ].
			suite]
		ifFalse: [ cls addToSuiteFromSelectors: suite ]

]

{ #category : 'building suites' }
TestCase class >> addToSuite: suite fromMethods: testMethods  [
	testMethods do:  [ :selector | 
			suite addTest: (self selector: selector) ].
	^suite
]

{ #category : 'building suites' }
TestCase class >> addToSuiteFromSelectors: suite [
	^self addToSuite: suite fromMethods: (self shouldInheritSelectors
		ifTrue: [ self allTestSelectors ]
		ifFalse: [self testSelectors ])
]

{ #category : 'accessing' }
TestCase class >> allTestSelectors [

	^self sortedTestSelectorsFrom: self allSelectors
]

{ #category : 'building suites' }
TestCase class >> buildSuite [
	| suite |
	suite := TestSuite new.
	^ self isAbstract
		ifTrue: [
			suite name: self name asPlainString.
			self allSubclasses
				do: [:each | each isAbstract
						ifFalse: [each addToSuiteFromSelectors: suite]].
			suite]
		ifFalse: [self addToSuiteFromSelectors: suite]
]

{ #category : 'building suites' }
TestCase class >> buildSuiteFromAllSelectors [

	^self buildSuiteFromMethods: self allTestSelectors
			
]

{ #category : 'building suites' }
TestCase class >> buildSuiteFromLocalSelectors [

	^self buildSuiteFromMethods: self testSelectors
			
]

{ #category : 'building suites' }
TestCase class >> buildSuiteFromMethods: testMethods  [
	| suite |
	suite := TestSuite new
				name: self name asPlainString;
				yourself.
	^self addToSuite: suite fromMethods: testMethods
]

{ #category : 'building suites' }
TestCase class >> buildSuiteFromSelectors [

	^self shouldInheritSelectors
		ifTrue: [self buildSuiteFromAllSelectors]
		ifFalse: [self buildSuiteFromLocalSelectors]
			
]

{ #category : 'instance creation' }
TestCase class >> debug: aSymbol [

	^(self selector: aSymbol) debug
			
]

{ #category : 'instance creation' }
TestCase class >> debugAsFailure: aSymbol  [

	^(self selector: aSymbol) debugAsFailure
]

{ #category : 'testing' }
TestCase class >> is: aSymbol [

	^self ~= TestCase 
		and: [ aSymbol == #TestCaseClass or: [ super is: aSymbol ]]
]

{ #category : 'testing' }
TestCase class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"
	^ thisContext methodClass == self class.
]

{ #category : 'testing' }
TestCase class >> isTestMethod: aCompiledMethod [

	^(self isTestSelector: aCompiledMethod selector)
		and: [aCompiledMethod methodClass inheritsFrom: TestCase]
]

{ #category : 'testing' }
TestCase class >> isTestSelector: aSelector [

	^aSelector beginsWith: self testSelectorPrefix
		:: and: [aSelector numArgs isZero]
]

{ #category : 'accessing' }
TestCase class >> resources [

	^#()
			
]

{ #category : 'instance creation' }
TestCase class >> run: aSymbol [

	^(self selector: aSymbol) run
			
]

{ #category : 'instance creation' }
TestCase class >> selector: aSymbol [

	^self new setTestSelector: aSymbol
			
]

{ #category : 'testing' }
TestCase class >> shouldInheritSelectors [
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."

	^self ~= TestCase 
		and: [ self superclass isAbstract or: [self testSelectors isEmpty]]

"$QA Ignore:Sends system method(superclass)$"
			
]

{ #category : 'accessing' }
TestCase class >> sortedTestSelectorsFrom: aCollection [

	^(aCollection asArray select: [:each | self isTestSelector: each]) sort
]

{ #category : 'building suites' }
TestCase class >> suiteClass [
	^TestSuite
			
]

{ #category : 'accessing' }
TestCase class >> sunitVersion [
	^'3.1'
			
]

{ #category : 'testing' }
TestCase class >> testSelectorPrefix [

	^'test'
]

{ #category : 'accessing' }
TestCase class >> testSelectors [

	^self sortedTestSelectorsFrom: self selectors
]

{ #category : 'assertions' }
TestCase >> assert: aBooleanOrBlock [

	self assert: aBooleanOrBlock description: ''
			
]

{ #category : 'assertions' }
TestCase >> assert: anAction changes: aCondition  [

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self deny: after = before
]

{ #category : 'assertions' }
TestCase >> assert: anAction changes: aCondition by: aDifference  [
	
	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before + aDifference
]

{ #category : 'assertions' }
TestCase >> assert: anAction changes: aCondition from: anInitialObject to: aFinalObject  [

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: before equals: anInitialObject.
	self assert: after equals: aFinalObject
]

{ #category : 'assertions' }
TestCase >> assert: aBooleanOrBlock description: aStringOrBlock [
	self incrementAssertionCount.
	aBooleanOrBlock value ifFalse: [
		failureString := String streamContents: [:stream |
			stream 	nextPutAll: 'Assertion ('.
			assertCount printOn: stream.
			stream nextPutAll: ') failed. '.
			stream nextPutAll: aStringOrBlock value].
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]
]

{ #category : 'assertions' }
TestCase >> assert: aBoolean description: aString resumable: resumableBoolean  [
	| exception |
	aBoolean
		ifFalse: [		
			failureString := aString.
			self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]
			
]

{ #category : 'assertions' }
TestCase >> assert: anAction doesNotChange: aCondition  [

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before
]

{ #category : 'assertions' }
TestCase >> assert: actual equals: expected [
	"First parameter is actual result from evaluating some expression.
	Second parameter is the value expected by the designer of the test.
	This differs from Squeak, but likely agrees with most other dialects and existing tests.
	See Martin's comments at
	https://lists.cuis.st/mailman/archives/cuis-dev/2023-July/007865.html
	Some tests may assume the previous behavior. Please correct them as you find them.
	"
	"
	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: expected to: actual tryWords: true))
			openLabel: 'Expected (red) versus actual (green)'
	"
	self
		assert: expected = actual
		description: [ self comparingStringBetween: expected and: actual ]

]

{ #category : 'assertions' }
TestCase >> assert: actual equals: expected description: aStringOrBlock [
	"First parameter is actual result from evaluating some expression.
	Second parameter is the value expected by the designer of the test.
	This differs from Squeak, but likely agrees with most other dialects and existing tests.
	See Martin's comments at
	https://lists.cuis.st/mailman/archives/cuis-dev/2023-July/007865.html
	Some tests may assume the previous behavior. Please correct them as you find them.
	"
	"
	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: expected to: actual tryWords: true))
			openLabel: 'Expected (red) versus actual (green)'
	"
	self
		assert: expected = actual
		description: aStringOrBlock.
]

{ #category : 'assertions' }
TestCase >> assert: aCollection includes: anObject  [
	
	self assert: (aCollection includes: anObject) description: [ aCollection asString, ' does not include ', anObject asString ]
]

{ #category : 'assertions' }
TestCase >> comparingStringBetween: expected and: actual [
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Expected ';
			nextPutAll: expected shortPrintString;
			nextPutAll: ' but was ';
			nextPutAll: actual shortPrintString;
			nextPutAll: '.'
		]
]

{ #category : 'debugging' }
TestCase >> debug [
	self setUpResources.
	[self debugInstance runCase] ensure: [self tearDownResources]
			
]

{ #category : 'debugging' }
TestCase >> debugAsFailure [
	
	^self debugInstance runCaseAsFailure
]

{ #category : 'debugging' }
TestCase >> debugInstance [
	^ self class selector: testSelector
]

{ #category : 'failing' }
TestCase >> defaultFailDescription [

	^'Test failed'
]

{ #category : 'assertions' }
TestCase >> deny: aBooleanOrBlock [

	self deny: aBooleanOrBlock description: ''
]

{ #category : 'assertions' }
TestCase >> deny: aBooleanOrBlock description: aStringOrBlock [

	self assert: aBooleanOrBlock value not description: aStringOrBlock
			
]

{ #category : 'assertions' }
TestCase >> deny: aBoolean description: aString resumable: resumableBoolean  [
	self
		assert: aBoolean not
		description: aString
		resumable: resumableBoolean
			
]

{ #category : 'accessing' }
TestCase >> errored: anError [
	raisedError := anError
]

{ #category : 'failing' }
TestCase >> expectedFailures [
	"Answer an array o selectors that are expected to fail.
	Usually this is done for tests that document incorrect behavior for which there is not yet a proper fix available."

	^#()
]

{ #category : 'failing' }
TestCase >> fail [

	^self failWith: self defaultFailDescription 
]

{ #category : 'failing' }
TestCase >> failWith: aDescription [

	self signalFailure: aDescription 
]

{ #category : 'log' }
TestCase >> failureLog	 [
	^Transcript	
]

{ #category : 'failing' }
TestCase >> failureString [
	^failureString
]

{ #category : 'running - private' }
TestCase >> incrementAssertionCount [
	assertCount := assertCount + 1
]

{ #category : 'setUp/tearDown' }
TestCase >> initialize [
	assertCount := 0.
]

{ #category : 'testing' }
TestCase >> is: aSymbol [
	^aSymbol == #TestCase or: [ super is: aSymbol ]
]

{ #category : 'testing' }
TestCase >> isLogging [
	"Log to Transcript"
	^true
]

{ #category : 'testing' }
TestCase >> isSameAs: aTestCase [

	^self class = aTestCase class and: [ testSelector = aTestCase selector ]
]

{ #category : 'log' }
TestCase >> logFailure: aString [
	| log |
	self isLogging ifTrue: [
		log := self failureLog.
		log newLine.
		self printOn: log.
		log nextPutAll: ' -- '.
		log nextPutAll: aString ]
]

{ #category : 'accessing' }
TestCase >> methodForTest [

	"Can not call it testMethod because it will be detected as test - Hernan" 

	^self class lookupSelector: self selector
]

{ #category : 'debugging - private' }
TestCase >> openDebuggerOnFailingTestMethod [

	Debugger
		openDebugging: [
			[ self performTest ] ensure: [
			self tearDown.
			self tearDownResources]]
		to: self methodForTest
		label: 'Debug failed test ', self printString.
]

{ #category : 'running - private' }
TestCase >> performTest [

	assertCount := 0.
	self perform: testSelector asSymbol
]

{ #category : 'printing' }
TestCase >> printOn: aStream [

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: (testSelector ifNil: [ 'unknown' ])
			
]

{ #category : 'accessing' }
TestCase >> raisedError [
	^raisedError
]

{ #category : 'dependencies' }
TestCase >> removeDependentFromHierachy: anObject  [
	"an empty method. for Composite compability with TestSuite"


			
]

{ #category : 'accessing' }
TestCase >> resources [
	| allResources resourceQueue |
	allResources := Set new.
	resourceQueue := OrderedCollection new.
	resourceQueue addAll: self class resources.
	[resourceQueue isEmpty] whileFalse: [
		| next |
		next := resourceQueue removeFirst.
		allResources add: next.
		resourceQueue addAll: next resources].
	^allResources
			
]

{ #category : 'running' }
TestCase >> run [
	| result |
	result := TestResult new.
	self run: result skipExpectedFailures: false.
	^result
			
]

{ #category : 'running' }
TestCase >> run: aResult skipExpectedFailures: aBoolean [
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ].
]

{ #category : 'running' }
TestCase >> runCase [

	assertCount := 0.
	[
		self setUp.
		self performTest] ensure: [self tearDown]
			
]

{ #category : 'running' }
TestCase >> runCaseAsFailure [

	assertCount := 0.
	self setUpResources.
	self setUp.
	
	self openDebuggerOnFailingTestMethod
]

{ #category : 'accessing' }
TestCase >> selector [
	^testSelector
			
]

{ #category : 'accessing' }
TestCase >> setTestSelector: aSymbol [
	testSelector := aSymbol
			
]

{ #category : 'setUp/tearDown' }
TestCase >> setUp [
	
]

{ #category : 'setUp/tearDown' }
TestCase >> setUpResources [
			
	self resources do: [:res | res isAvailable ifFalse: [^res signalInitializationError]].

]

{ #category : 'assertions' }
TestCase >> should: aBlock [

	self assert: aBlock value
			
]

{ #category : 'assertions' }
TestCase >> should: aBlock description: aString [

	self assert: aBlock value description: aString
			
]

{ #category : 'assertions' }
TestCase >> should: aClosure notTakeMoreThan: aLimit  [

	| millisecondsLimit |

	millisecondsLimit := aLimit totalMilliseconds.
	self assert: aClosure millisecondsToRun <= millisecondsLimit
		description: [ 'Took more than ', millisecondsLimit printString, ' milliseconds' ].
]

{ #category : 'assertions' }
TestCase >> should: aBlock raise: anExceptionHandlingCondition  [

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: [ :anException | ]
			
]

{ #category : 'assertions' }
TestCase >> should: aBlock raise: anExceptionHandlingCondition description: aFailDescription [

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: [:anException | ] description: aFailDescription
]

{ #category : 'assertions' }
TestCase >> should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock [

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: self defaultFailDescription
]

{ #category : 'assertions' }
TestCase >> should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription [

	| passed |
	passed := [aBlock value.  false]
		on: anExceptionHandlingCondition
		do: [:ex | assertionsBlock value: ex.  ex return: true].
	passed ifFalse: [self failWith: aFailDescription]
]

{ #category : 'assertions' }
TestCase >> should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: expectedErrorMessageOrBlock [

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException |
			self assert: anException messageText equals: expectedErrorMessageOrBlock value ]
]

{ #category : 'assertions' }
TestCase >> shouldFail: aBlock [
	
	self should: aBlock raise: Exception 
]

{ #category : 'testing' }
TestCase >> shouldPass [
	"Unless the selector is in the list we get from #expectedFailures, we expect it to pass"
	^ (self expectedFailures includes: testSelector) not
]

{ #category : 'assertions' }
TestCase >> shouldnt: aBlock [

	self deny: aBlock value
			
]

{ #category : 'assertions' }
TestCase >> shouldnt: aBlock description: aString [

	self deny: aBlock value description: aString
			
]

{ #category : 'assertions' }
TestCase >> shouldnt: aBlock raise: anExceptionHandlingCondition  [

	self shouldnt: aBlock raise: anExceptionHandlingCondition description: anExceptionHandlingCondition printString, ' was not expected to be raised'
]

{ #category : 'assertions' }
TestCase >> shouldnt: aBlock raise: anExceptionHandlingCondition description: aFailDescription [

	aBlock 
		on: anExceptionHandlingCondition 
		do: [ :anException | self failWith: aFailDescription ]

]

{ #category : 'assertions' }
TestCase >> shouldntFail: aBlock [

	self shouldnt: aBlock raise: TestResult exError
]

{ #category : 'failing' }
TestCase >> signalFailure: aString [

	failureString := aString.
	TestResult failure sunitSignalWith: aString
]

{ #category : 'setUp/tearDown' }
TestCase >> tearDown [
			
]

{ #category : 'setUp/tearDown' }
TestCase >> tearDownResources [
	
	self resources do: [:each | each reset]
]
