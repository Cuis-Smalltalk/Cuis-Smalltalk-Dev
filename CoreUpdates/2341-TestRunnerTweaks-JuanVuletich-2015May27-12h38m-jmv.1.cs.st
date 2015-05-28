'From Cuis 4.2 of 25 July 2013 [latest update: #2338] on 27 May 2015 at 12:38:49.795362 pm'!

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/27/2015 10:02'!
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
"	      priority: Processor userBackgroundPriority;"
	      resume.
! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/27/2015 10:02'!
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
"	      priority: Processor userBackgroundPriority;"
	      resume.
! !

