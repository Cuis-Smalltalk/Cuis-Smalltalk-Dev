'From Cuis7.1 [latest update: #6427] on 7 August 2024 at 8:46:42 pm'!

!TestResult methodsFor: 'accessing' stamp: 'HAW 8/7/2024 20:39:53'!
addAll: tests asFailureBecause: aReason

	tests do: [ :aFailedTest |
		self reportFailed: aFailedTest because: aReason.
		self failures add: aFailedTest ]! !


!TestResource class methodsFor: 'creation' stamp: 'HAW 8/7/2024 20:40:19'!
signalInitializationError
	^TestResult signalFailureWith: 'Resource ' , self name , ' could not be initialized'
			! !


!TestSuite methodsFor: 'running' stamp: 'HAW 8/7/2024 20:39:09'!
run

	| result |
 
	result := TestResult new.
	
	[ self setUpResources ] 
		on: result class failure 
		do: [ :aFailure |
			result addAll: tests asFailureBecause: aFailure messageText.
			self tearDownResources.
			^result ].
		
	[self run: result] ensure: [self tearDownResources].
	
	^result
			! !

