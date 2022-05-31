'From Cuis 6.0 [latest update: #5207] on 31 May 2022 at 12:05:25 pm'!

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/31/2022 11:55:07'!
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
		priority: Processor userBackgroundPriority;
		resume.! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/31/2022 11:55:11'!
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
		priority: Processor userBackgroundPriority;
		resume.! !


!ProgressiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 5/31/2022 11:38:00'!
createProgressBarAndRun

	self createProgressBar.
	[ self runSuiteShowingProgress ]
		forkAt: Processor userBackgroundPriority
		named: 'ProgressiveTestRunner'.! !

