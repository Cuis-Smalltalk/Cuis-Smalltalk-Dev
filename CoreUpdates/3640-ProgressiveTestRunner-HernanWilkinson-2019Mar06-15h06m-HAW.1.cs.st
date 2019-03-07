'From Cuis 5.0 of 7 November 2016 [latest update: #3641] on 6 March 2019 at 4:12:06 pm'!

!ProgressiveTestRunner methodsFor: 'evaluating' stamp: 'HAW 3/6/2019 15:35:58'!
value

	Utilities authorInitials.
	testsStream _ ReadStream on: testSuite tests.
	testsStream atEnd
		ifTrue: [ self informNoTestToRun ]
	 	ifFalse:[ self createProgressBarAndRun ]! !

