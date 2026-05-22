"
This is a Collecting Parameter for the running of a bunch of tests. TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. Kent has seen TestResults that recorded coverage information and that sent email when they were done.
"
Class {
	#name : 'TestResult',
	#superclass : 'Object',
	#instVars : [
		'failures',
		'errors',
		'passed'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'exceptions' }
TestResult class >> exError [
	^SUnitNameResolver errorObject
			
]

{ #category : 'exceptions' }
TestResult class >> failure [
	^TestFailure
			
]

{ #category : 'exceptions' }
TestResult class >> resumableFailure [
	^ResumableTestFailure
			
]

{ #category : 'exceptions' }
TestResult class >> signalErrorWith: aString  [
	TestResult exError sunitSignalWith: aString.
]

{ #category : 'exceptions' }
TestResult class >> signalFailureWith: aString  [
	self failure sunitSignalWith: aString
			
]

{ #category : 'accessing' }
TestResult >> addAll: tests asFailureBecause: aReason [

	tests do: [ :aFailedTest |
		self reportFailed: aFailedTest because: aReason.
		self failures add: aFailedTest ]
]

{ #category : 'accessing' }
TestResult >> allFailures [
	^ self unexpectedFailures, self unexpectedPasses 
]

{ #category : 'accessing' }
TestResult >> allPassed [
	^ self expectedPasses, self expectedDefects
]

{ #category : 'accessing' }
TestResult >> correctCount [
	"depreciated - use #passedCount"

	^self passedCount
			
]

{ #category : 'accessing' }
TestResult >> defects [
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself
			
]

{ #category : 'printing' }
TestResult >> description [
	" Return the description of the receiver. "

	| stream |
	stream := '' writeStream.
	stream lf; nextPutAll: 'failures:';lf.
	self failures do: [:testCase |
		stream lf;nextPutAll: '	',testCase selector ,'	' ,testCase failureString
	].
	stream lf; nextPutAll: 'errors:';lf.
	self errors do: [:testCase|
		stream lf;
			nextPutAll: '	',testCase selector ,'	' ,(testCase failureString ifNil: ['']); lf;
			yourself
	].
	stream lf; nextPutAll: 'passed: '; nextPutAll: self correctCount asString; lf.
	stream nextPutAll: 'failures: '; nextPutAll: self failureCount asString; lf.
	stream nextPutAll: 'errors: '; nextPutAll: self errorCount asString; lf.
	^stream contents
]

{ #category : 'accessing' }
TestResult >> errorCount [

	^self errors size
			
]

{ #category : 'accessing' }
TestResult >> errors [

	errors
		ifNil: [errors := OrderedCollection new].
	^errors
			
]

{ #category : 'accessing' }
TestResult >> expectedDefectCount [
	^ self expectedDefects size
]

{ #category : 'accessing' }
TestResult >> expectedDefects [
	^ (errors, failures asOrderedCollection) select: [:each | each shouldPass not] 
]

{ #category : 'accessing' }
TestResult >> expectedPassCount [
	^ self expectedPasses size
]

{ #category : 'accessing' }
TestResult >> expectedPasses [
	^ passed select: [:each | each shouldPass] 
]

{ #category : 'accessing' }
TestResult >> failureCount [

	^self failures size
			
]

{ #category : 'accessing' }
TestResult >> failures [
	failures
		ifNil: [failures := OrderedCollection new].
	^failures
			
]

{ #category : 'printing' }
TestResult >> fullDescription [
	" Return the description of the receiver. "

	| stream |
	stream := '' writeStream.
	stream lf; nextPutAll: 'passed:'; lf.
	self passed do: [:stage|
		stream lf;nextPutAll: '	',stage
	].
	stream lf; nextPutAll: 'failures:';lf.
	self failures do: [:testCase |
		stream lf;nextPutAll: '	',testCase selector ,'	' ,testCase failureString
	].
	stream lf; nextPutAll: 'errors:';lf.
	self errors do: [:testCase|
		stream lf;
			nextPutAll: '	',testCase selector ,'	' ,(testCase failureString ifNil: ['']); lf;
			yourself
	].
	stream lf; nextPutAll: 'passed: '; nextPutAll: self correctCount asString; lf.
	stream nextPutAll: 'failures: '; nextPutAll: self failureCount asString; lf.
	stream nextPutAll: 'errors: '; nextPutAll: self errorCount asString; lf.
	^stream contents
]

{ #category : 'testing' }
TestResult >> hasErrors [

	^self unexpectedErrors notEmpty
			
]

{ #category : 'testing' }
TestResult >> hasFailures [

	^self unexpectedFailures notEmpty
			
]

{ #category : 'testing' }
TestResult >> hasPassed [

	^self hasErrors not and: [self hasFailures not]
			
]

{ #category : 'testing' }
TestResult >> is: aSymbol [
	^aSymbol == #TestResult or: [ super is: aSymbol ]
]

{ #category : 'testing' }
TestResult >> isError: aTestCase [

	^self errors includes: aTestCase
			
]

{ #category : 'testing' }
TestResult >> isFailure: aTestCase [
	^self failures includes: aTestCase
			
]

{ #category : 'testing' }
TestResult >> isPassed: aTestCase [

	^self passed includes: aTestCase
			
]

{ #category : 'accessing' }
TestResult >> passed [

	passed
		ifNil: [passed := OrderedCollection new].

	^passed
			
]

{ #category : 'accessing' }
TestResult >> passedCount [

	^self passed size
			
]

{ #category : 'printing' }
TestResult >> printOn: aStream [
	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self expectedPassCount printString;
		nextPutAll: ' passes, ';
		nextPutAll: self expectedDefectCount printString;
		nextPutAll:' expected failures/errors, ';
		nextPutAll: self unexpectedFailureCount printString;
		nextPutAll: ' failures, ';
		nextPutAll: self unexpectedErrorCount printString;
		nextPutAll:' errors, ';
		nextPutAll: self unexpectedPassCount printString;
		nextPutAll:' unexpected passes'.
]

{ #category : 'accessing' }
TestResult >> removeFromDefectsAndAddToPassed: aPassed  [

	errors 
		detect: [ :anError | anError isSameAs: aPassed ]
		ifFound: [ :anError | errors remove: anError ]
		ifNone: [ 
			failures 
				detect: [ :aFail | aFail isSameAs: aPassed ]
				ifFound: [ :aFail | failures remove: aFail ]
				ifNone: [ self error: aPassed printString, ' is not an error nor a failure' ]].
	passed add: aPassed

]

{ #category : 'logging' }
TestResult >> reportAboutToRun: aTestCase [
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]
]

{ #category : 'logging' }
TestResult >> reportError: aTestCase because: anException [

	Transcript print: anException; newLine.
]

{ #category : 'logging' }
TestResult >> reportFailed: aTestCase because: anException [
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript print: anException; newLine].
]

{ #category : 'logging' }
TestResult >> reportPassed: aTestCase [
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'finished.'; newLine]
]

{ #category : 'running' }
TestResult >> runCase: aTestCase [

	| testCasePassed |

	testCasePassed :=
		[
			[
				"self reportAboutToRun: aTestCase."
				aTestCase runCase.
				"self reportPassed: aTestCase."
				true]
					on: self class failure
					do: [ :signal |
						self reportFailed: aTestCase because: signal.
						(self failures isEmpty or: [ failures last ~~ aTestCase ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: UnhandledError
							do: [ :signal |
								self reportError: aTestCase because: signal.
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]
]

{ #category : 'accessing' }
TestResult >> runCount [

	^self passedCount + self failureCount + self errorCount
			
]

{ #category : 'accessing' }
TestResult >> tests [

	^ OrderedCollection new
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself
			
]

{ #category : 'accessing' }
TestResult >> unexpectedErrorCount [
	^ self unexpectedErrors size
]

{ #category : 'accessing' }
TestResult >> unexpectedErrors [
	^ errors ifNil: [ errors := OrderedCollection new ] ifNotNil: [ errors select: [:each | each shouldPass] ]
]

{ #category : 'accessing' }
TestResult >> unexpectedFailureCount [
	^ self unexpectedFailures size
]

{ #category : 'accessing' }
TestResult >> unexpectedFailures [
	^ failures ifNil: [ failures := OrderedCollection new ] ifNotNil: [ failures select: [:each | each shouldPass] ]
]

{ #category : 'accessing' }
TestResult >> unexpectedPassCount [
	^ self unexpectedPasses size
]

{ #category : 'accessing' }
TestResult >> unexpectedPasses [
	^ passed select: [:each | each shouldPass not] 
]
