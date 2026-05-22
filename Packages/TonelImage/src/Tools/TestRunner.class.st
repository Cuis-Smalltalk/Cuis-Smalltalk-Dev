"
This is a user interface for the SUnit TestCase and TestSuite classes.
It lets you run tests in the background, and you can select subsets to run.
"
Class {
	#name : 'TestRunner',
	#superclass : 'ActiveModel',
	#instVars : [
		'result',
		'details',
		'passFail',
		'failures',
		'errors',
		'tests',
		'lastPass',
		'selectedFailureTest',
		'selectedErrorTest',
		'selectedSuite',
		'filter',
		'selectedSuites',
		'running',
		'runSemaphore',
		'completedTests',
		'totalTests',
		'progressLabel'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'ui' }
TestRunner class >> open [
	^ TestRunnerWindow open: self new label: nil
]

{ #category : 'processing' }
TestRunner >> addTestsFor: testName toSuite: suite  [
	| cls |
	(testName indexOf: $() > 0
		ifFalse: [
			cls := SUnitNameResolver classNamed: testName asSymbol.
			cls isAbstract
				ifTrue: [cls allSubclasses
						do: [:each | each isAbstract
								ifFalse: [each addToSuiteFromSelectors: suite]]]
				ifFalse: [cls addToSuiteFromSelectors: suite]].
	^ suite
]

{ #category : 'processing' }
TestRunner >> changedRunning [
	
	self changed: #isRunning; changed: #isNotRunning
]

{ #category : 'accessing' }
TestRunner >> completedTests [
	^ completedTests
]

{ #category : 'processing' }
TestRunner >> debugErrorTest: anInteger [
	self terminateRun.
	selectedErrorTest := anInteger.  "added rew"
	selectedFailureTest := 0.                        "added rew"
	self changed: #selectedFailureTest.             "added rew"
	self changed: #selectedErrorTest.               "added rew"
	anInteger ~= 0 ifTrue: [(result errors at: anInteger) debug]
]

{ #category : 'processing' }
TestRunner >> debugFailureTest: anInteger [
	self terminateRun.
	anInteger ~= 0 ifTrue: [(self failures at: anInteger) debugAsFailure].
	selectedFailureTest := anInteger.
	selectedErrorTest := 0.
	self changed: #selectedErrorTest.
	self changed: #selectedFailureTest
]

{ #category : 'menus' }
TestRunner >> deselectAll [
	selectedSuites := tests collect: [ :ea | false ].
	selectedSuite := 0.
      self changed: #allSelections.
 
]

{ #category : 'accessing' }
TestRunner >> details [

        ^details
]

{ #category : 'updating' }
TestRunner >> displayDetails: aString [
        details := aString.
        self changed: #details
]

{ #category : 'updating' }
TestRunner >> displayErrors: anOrderedCollection [

        errors := anOrderedCollection.
        self changed: #errorsList
]

{ #category : 'updating' }
TestRunner >> displayFailures: anOrderedCollection [

        failures := anOrderedCollection.
        self changed: #failuresList
]

{ #category : 'updating' }
TestRunner >> displayPassFail: aString [
        passFail := aString.
        self changed: #passFail
]

{ #category : 'logging' }
TestRunner >> errorLog [
	^Transcript
]

{ #category : 'accessing' }
TestRunner >> errors [

        ^errors
]

{ #category : 'accessing' }
TestRunner >> errorsList [

        ^self errors collect: [:error | error printString]
]

{ #category : 'accessing' }
TestRunner >> failures [

        ^failures
]

{ #category : 'accessing' }
TestRunner >> failuresList [

        ^self failures collect: [:failure | failure printString]
]

{ #category : 'initialization' }
TestRunner >> gatherTestNames [
	^ (self testCases collect: [ :each |
		each name ]) asArray sort.
]

{ #category : 'initialization' }
TestRunner >> initialize [

	result := TestResult new.
	passFail := ''.
	details := ''.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	tests := self gatherTestNames.
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	selectedSuites := tests collect: [:ea | true].
	running := nil.
	runSemaphore := Semaphore new
]

{ #category : 'menus' }
TestRunner >> interruptRun [
	running ifNotNil: [:aProcess| aProcess debugWithTitle: 'Interrupted Test']
]

{ #category : 'menus' }
TestRunner >> invertSelections [
	selectedSuites := selectedSuites collect: [ :ea | ea not ].
	selectedSuite := selectedSuites indexOf: true ifAbsent: [0].
	self changed: #allSelections .

]

{ #category : 'testing' }
TestRunner >> isNotRunning [
	^ running isNil
]

{ #category : 'testing' }
TestRunner >> isRunning [
	^ running notNil
]

{ #category : 'accessing' }
TestRunner >> listSelectionAt: index [
	^selectedSuites at: index
]

{ #category : 'accessing' }
TestRunner >> listSelectionAt: index put: aBoolean [
	^selectedSuites at: index put: aBoolean

]

{ #category : 'accessing' }
TestRunner >> passFail [

        ^passFail
]

{ #category : 'accessing' }
TestRunner >> progressLabel [
	^progressLabel
]

{ #category : 'updating' }
TestRunner >> refreshTestRunner [
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: ''.
	self displayDetails: ''
]

{ #category : 'processing' }
TestRunner >> refreshTests [
	| preselected |
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	preselected := Set new.
	tests
		with: selectedSuites
		do: [ :t :f |
			f ifTrue: [ preselected add: t ]].
	tests := self gatherTestNames.
	selectedSuites := tests collect: [ :ea |
		preselected includes: ea ].
	self changed: #tests.
	self changed: #selectedFailureTest.		"added rew"
	self changed: #selectedErrorTest.			"added rew"
	self changed: #selectedSuite.
]

{ #category : 'accessing' }
TestRunner >> result [
	^result

]

{ #category : 'processing' }
TestRunner >> runOneTest [
	| testSuite |
	self runWindow.
	selectedSuite isZero ifTrue: [ ^ self displayPassFail: 'No Test Suite Selected' ].
	testSuite :=  TestSuite new name: 'TestRunner Suite'.
	self addTestsFor: (tests at: selectedSuite) toSuite: testSuite.
	runSemaphore initSignals.
	self runSuite: testSuite
]

{ #category : 'processing' }
TestRunner >> runProfiledTests [
	| suite |
	suite := TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuiteProfiled: suite
]

{ #category : 'processing' }
TestRunner >> runSuite: suite [
	
	self runSuite: suite with: [ suite run ]
]

{ #category : 'processing' }
TestRunner >> runSuite: suite with: aBlock [
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests := suite tests size.
	completedTests := 0.
	running := [
	[ result := aBlock value ] ensure: [
		running := nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self suiteRan. ]]] newProcess.
	self runWindow.
	self suiteRan.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.
]

{ #category : 'processing' }
TestRunner >> runSuiteProfiled: suite [
	
	self runSuite: suite with: [ TimeProfileBrowser onBlock: [ suite run ]]
]

{ #category : 'processing' }
TestRunner >> runTests [
	| suite |
	suite := TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuite: suite
]

{ #category : 'updating' }
TestRunner >> runWindow [
	
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: 'Running...'.
	self displayDetails: ' '
]

{ #category : 'menus' }
TestRunner >> selectAll [
	| sel |
	sel := self selectedSuite.
	selectedSuites := selectedSuites collect: [ :ea | true ].
	selectedSuites size isZero ifFalse: [
		sel isZero ifTrue: [ self selectedSuite: 1 ]
			ifFalse: [ self changed: #allSelections ]].

]

{ #category : 'processing' }
TestRunner >> selectedErrorTest [
        ^selectedErrorTest
]

{ #category : 'processing' }
TestRunner >> selectedFailureTest [

        ^selectedFailureTest
]

{ #category : 'processing' }
TestRunner >> selectedSuite [

        ^selectedSuite
]

{ #category : 'processing' }
TestRunner >> selectedSuite: anInteger [
	anInteger > 0 ifTrue: [ | selected |
		selected := selectedSuite ~= anInteger.
		selectedSuites at: anInteger put: selected.
	] ifFalse: [
		"selectedSuite > 0 ifTrue: [ selectedSuites at: selectedSuite put: false ]."
	].
	selectedSuite := anInteger.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	self changed: #selectedFailureTest.             "added rew"
	self changed: #selectedErrorTest.               "added rew" 
	self changed: #selectedSuite.
	self changed: #allSelections.

]

{ #category : 'accessing' }
TestRunner >> selectedTests [
	| retval |
	retval := OrderedCollection new.
	tests with: selectedSuites do: [ :str :sel | sel ifTrue: [ retval add: str ]].
	^retval

]

{ #category : 'menus' }
TestRunner >> setFilter [
	self
		request: 'Add test cases by pattern (# and * allowed)'
		initialAnswer: '*'
		do: [:aString|
			filter := aString.
			(filter endsWith: '*') ifFalse: [ filter := filter, '*' ].
			selectedSuites := (tests asOrderedCollection with: selectedSuites collect: [ :ea :sel |
				sel or: [ filter match: ea asString ]
			]).
			selectedSuite := selectedSuites indexOf: true ifAbsent: [0].
			self changed: #allSelections]
]

{ #category : 'logging' }
TestRunner >> showResult [

	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.
	self errorLog print: self result.
	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine.
]

{ #category : 'processing' }
TestRunner >> suiteRan [

	self changed: #runTests; changedRunning
	
]

{ #category : 'menus' }
TestRunner >> terminateRun [
	running ifNotNil: [:aProcess|
		aProcess terminate. running := nil.
		self changedRunning]
]

{ #category : 'initialization' }
TestRunner >> testCases [
	^ TestCase allSubclasses reject: [:cls | cls isAbstract]
]

{ #category : 'accessing' }
TestRunner >> tests [
        ^ tests
]

{ #category : 'accessing' }
TestRunner >> timeSinceLastPassAsString: aResult [
        (lastPass isNil or: [aResult hasPassed not]) ifTrue: [^ ''].
        ^ String newLineString , (DateAndTime now - lastPass) printString , ' since last Pass'
]

{ #category : 'accessing' }
TestRunner >> totalTests [
	^ totalTests
]

{ #category : 'updating' }
TestRunner >> update: aParameter  [
	"updates come in from another thread"
	(aParameter is: #TestResult) ifTrue:
		[UISupervisor whenUIinSafeState:
			[result := aParameter.
"			self updatePassFail: aParameter."
			self updateDetails: aParameter.
			self updateFailures: aParameter.
			self updateErrors: aParameter].
		^ self].
	(aParameter is: #TestCase) ifTrue:
		[UISupervisor whenUIinSafeState:
			[completedTests := completedTests + 1.
			progressLabel := aParameter printString.
			self changed: #progress].
		^ self].
	super update: aParameter
]

{ #category : 'updating' }
TestRunner >> updateDetails: aTestResult  [
	self displayDetails: aTestResult printString
			, (self timeSinceLastPassAsString: aTestResult).
	aTestResult hasPassed
		ifTrue: [lastPass := DateAndTime now]
]

{ #category : 'updating' }
TestRunner >> updateErrors: aTestResult [

        self displayErrors: aTestResult errors
]

{ #category : 'updating' }
TestRunner >> updateFailures: aTestResult [

        self displayFailures: aTestResult failures
]

{ #category : 'updating' }
TestRunner >> updatePassFail: aTestResult [
        | message |
        message := aTestResult hasPassed
                                ifTrue: ['Pass']
                                ifFalse: ['Fail'].
        self displayPassFail: message
]

{ #category : 'updating' }
TestRunner >> updateWindow: aTestResult [
	self updatePassFail: aTestResult.
	self updateDetails: aTestResult.
	self updateFailures: aTestResult.
	self updateErrors: aTestResult
]
