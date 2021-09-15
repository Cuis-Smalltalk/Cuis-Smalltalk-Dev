'From Haver 5.0 [latest update: #4638] on 18 June 2021 at 8:49:05 am'!

!TestRunner methodsFor: 'processing' stamp: 'KLG 6/18/2021 08:42:36'!
runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ TimeProfileBrowser onBlock: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				UISupervisor whenUIinSafeState: [
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
"	      priority: Processor userBackgroundPriority;"
	      resume.
! !

