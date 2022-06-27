'From Cuis 6.0 [latest update: #5322] on 27 June 2022 at 12:30:56 pm'!

!TestRunner methodsFor: 'processing' stamp: 'jmv 6/27/2022 11:56:12'!
runSuite: suite
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
	[ result _ suite run ] ensure: [
		running _ nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self changed: #runTests.
			self changed: #runOneTest ]]] newProcess.
	self runWindow.
	self changed: #runTests.
	self changed: #runOneTest.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 6/27/2022 11:56:27'!
runSuiteProfiled: suite
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
	[ result _ TimeProfileBrowser onBlock: [ suite run ]] ensure: [
		running _ nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self changed: #runTests.
			self changed: #runOneTest ]]] newProcess.
	self runWindow.
	self changed: #runTests.
	self changed: #runOneTest.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.! !


!ProgressiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 6/27/2022 11:56:30'!
createProgressBarAndRun

	self createProgressBar.
	[ self runSuiteShowingProgress ]
		forkAt: Processor userSchedulingPriority
		named: 'ProgressiveTestRunner'.! !

!ProgressiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 6/27/2022 11:57:07'!
runSuiteShowingProgress

	[
		self registerTestSuiteAction. 
		progressBar openInWorld.
		self runSuite ] ensure: [
			self unregisterTestSuiteAction.
			UISupervisor whenUIinSafeState: [progressBar dismissMorph] ].! !


!TestSuite methodsFor: 'running' stamp: 'jmv 6/27/2022 12:03:31'!
run: aResult

	| t1 t2 |
	t1 _ Time localMillisecondClock.
	self tests do: [:each | 
		self changed: each.
		each run: aResult.
		"Allow for some responsivenes of the UI, or any other stuff that may need running"
		t2 _ Time localMillisecondClock.
		t2-t1 > 200 ifTrue: [
			Processor yield. 
			t1 _ t2 ]]! !

