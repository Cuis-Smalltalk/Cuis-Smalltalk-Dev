'From Cuis 4.2 of 25 July 2013 [latest update: #2212] on 27 March 2015 at 11:41:14.50128 am'!
!classDefinition: #TestCase category: #SUnit!
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!


!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:45'!
errored: anError
	raisedError _ anError! !

!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:48'!
failureString
	^failureString! !

!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:49'!
raisedError
	^raisedError! !


!TestRunner methodsFor: 'processing' stamp: 'jmv 3/27/2015 11:25'!
runOneTest
	| testSuite |
	self runWindow.
	selectedSuite isZero ifTrue: [ ^ self displayPassFail: 'No Test Suite Selected' ].
	testSuite _  TestSuite new name: 'TestRunner Suite'.
	self addTestsFor: (tests at: selectedSuite) toSuite: testSuite.
	runSemaphore initSignals.
	self runSuite: testSuite! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 3/27/2015 11:25'!
runProfiledTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuiteProfiled: suite! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 3/27/2015 11:25'!
runSuite: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ suite run ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				WorldState addDeferredUIMessage: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
	      priority: Processor userBackgroundPriority;
	      resume.
! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 3/27/2015 11:25'!
runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ MessageTally spyOn: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				WorldState addDeferredUIMessage: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
	      priority: Processor userBackgroundPriority;
	      resume.
! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 3/27/2015 11:25'!
runTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuite: suite! !

!TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 09:44'!
errorLog
	^Transcript! !

!TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'!
showResult

	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.
	self
		showResultSummary;
		showResultDefects.
	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine.! !

!TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'!
showResultDefects

	(self result failureCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ----- Failures ----'.
			self result failures do: [:failure |
				self errorLog newLine; tab; show: failure printString]].
	(self result errorCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ------- Errors ----'.
			self result errors do: [:error |
				self errorLog newLine; tab; show: error printString]].! !

!TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'!
showResultSummary

	| message summary |
	message := (self result runCount = self result correctCount)
		ifTrue: ['succeeded']
		ifFalse: ['failed'].
	Transcript newLine; tab; show: message.
	summary :=
		self result runCount printString, ' run, ',
		self result failureCount printString, ' failed, ',
		self result errorCount printString, ' errors'.
	Transcript newLine; tab; show: summary.! !


!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:41'!
assert: aBoolean description: aString
	aBoolean ifFalse: [
		failureString _ aString.
		self logFailure: aString.
		TestResult failure sunitSignalWith: aString]
			! !

!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:41'!
assert: aBoolean description: aString resumable: resumableBoolean 
	| exception |
	aBoolean
		ifFalse: [		
			failureString _ aString.
			self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]
			! !

!TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:42'!
signalFailure: aString

	failureString _ aString.
	TestResult failure sunitSignalWith: aString! !

!TestCase methodsFor: 'Running' stamp: 'jmv 3/27/2015 09:44'!
failureLog	
	^Transcript	! !


!TestResult methodsFor: 'Running' stamp: 'jmv 3/27/2015 10:45'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed _
		[
			[
				aTestCase runCase.
				true]
					on: self class failure
					do: [ :signal |
						(self failures isEmpty or: [ (failures last == aTestCase) not ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: self class error
							do: [ :signal |
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]! !

!classDefinition: #TestCase category: #SUnit!
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
!methodRemoval: SUnitNameResolver class #defaultLogDevice!
SUnitNameResolver class removeSelector: #defaultLogDevice!

!TestRunner reorganize!
('accessing' completedFraction details errors errorsList failures failuresList listSelectionAt: listSelectionAt:put: passFail progressLabel result selectedTests tests timeSinceLastPassAsString:)
('constants' runButtonState stopButtonState)
('menus' deselectAll invertSelections selectAll setFilter terminateRun)
('processing' addTestsFor:toSuite: debugErrorTest: debugFailureTest: refreshTests runOneTest runProfiledTests runSuite: runSuiteProfiled: runTests selectedErrorTest selectedFailureTest selectedSuite selectedSuite:)
('test processing')
('updating' displayDetails: displayErrors: displayFailures: displayPassFail: refreshTR runWindow update: updateDetails: updateErrors: updateFailures: updatePassFail: updateWindow:)
('initialization' gatherTestNames initialize testCases)
('logging' errorLog showResult showResultDefects showResultSummary)
!

