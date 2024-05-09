'From Cuis6.3 [latest update: #6351] on 9 May 2024 at 4:31:52 pm'!

!ProgressiveTestRunner methodsFor: 'show result - private' stamp: 'jmv 5/9/2024 16:31:22'!
showDefects
	
	 | defects |
			
	defects := testResult defects.
	defects size = 1
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]! !


!ProgressiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 5/9/2024 16:31:29'!
runSuite
		
	testResult := testSuite run.	
	testResult hasPassed 
		ifTrue: [ self informAllTestPassed ] 
		ifFalse: [self showDefects ]
	! !

!methodRemoval: ProgressiveTestRunner #showDeffects stamp: 'jmv 5/9/2024 16:31:36'!
ProgressiveTestRunner removeSelector: #showDeffects!
