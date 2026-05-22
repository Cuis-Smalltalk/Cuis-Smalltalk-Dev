Class {
	#name : 'ProgressiveTestRunner',
	#superclass : 'Object',
	#instVars : [
		'testSuite',
		'testsStream',
		'progressBar',
		'testResult',
		'testRunIncrement'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'instance creation' }
ProgressiveTestRunner class >> for: aTestSuite [

	^self new initializeFor: aTestSuite
]

{ #category : 'progress bar - private' }
ProgressiveTestRunner >> calculateTestRunIncrement [
	
	testRunIncrement := 1/testsStream size
]

{ #category : 'progress bar - private' }
ProgressiveTestRunner >> createProgressBar [
		
	progressBar := ProgressMorph label: testSuite name.
	progressBar setProperty: #layerNumber toValue: 100.
	self calculateTestRunIncrement.
	self updateProgressBarSubLabel.
	
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> createProgressBarAndRun [

	self createProgressBar.
	[ self runSuiteShowingProgress ]
		forkAt: Processor userSchedulingPriority
		named: 'ProgressiveTestRunner'.
]

{ #category : 'show result - private' }
ProgressiveTestRunner >> informAllTestPassed  [
	
	PopUpMenu inform: testResult printString.
	
]

{ #category : 'show result - private' }
ProgressiveTestRunner >> informNoTestToRun [
		
	 PopUpMenu inform: 'No test to run'
]

{ #category : 'initialization' }
ProgressiveTestRunner >> initializeFor: aTestSuite [

	testSuite := aTestSuite.
	
]

{ #category : 'show result - private' }
ProgressiveTestRunner >> openTestResultWindow [

	TestResultWindow openFor: testResult 
	
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> registerTestSuiteAction [
		
	testSuite when: #changed: send: #testRun: to: self
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> runSuite [
		
	testResult := testSuite run.	
	testResult hasPassed 
		ifTrue: [ self informAllTestPassed ] 
		ifFalse: [self showDefects ]
	
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> runSuiteShowingProgress [

	[
		self registerTestSuiteAction. 
		progressBar openInWorld.
		self runSuite ] ensure: [
			self unregisterTestSuiteAction.
			UISupervisor whenUIinSafeState: [progressBar dismissMorph] ].
]

{ #category : 'show result - private' }
ProgressiveTestRunner >> showDefects [
	
	 | defects |
			
	defects := testResult defects.
	"defects size = 1" false
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> testRun: aTest [

	self updateProgressBarSubLabel.
	self updateDoneIncrement 
	
		
]

{ #category : 'evaluating - private' }
ProgressiveTestRunner >> unregisterTestSuiteAction [
	
	testSuite removeAllActions
]

{ #category : 'progress bar - private' }
ProgressiveTestRunner >> updateDoneIncrement  [

 	progressBar incrDone: testRunIncrement 
	
]

{ #category : 'progress bar - private' }
ProgressiveTestRunner >> updateProgressBarSubLabel [

	testsStream atEnd ifFalse: [
		progressBar subLabel: testsStream next printString, ' (', testsStream position printString, '/', testsStream size printString, ')' ].
]

{ #category : 'evaluating' }
ProgressiveTestRunner >> value [

	Utilities authorInitials.
	testsStream := ReadStream on: testSuite tests.
	testsStream atEnd
		ifTrue: [ self informNoTestToRun ]
	 	ifFalse:[ self createProgressBarAndRun ]
]
