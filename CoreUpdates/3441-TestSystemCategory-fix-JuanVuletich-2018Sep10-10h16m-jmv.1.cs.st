'From Cuis 5.0 of 7 November 2016 [latest update: #3437] on 10 September 2018 at 10:25:41 am'!

!TestSuite class methodsFor: 'Creation - Private' stamp: 'jmv 9/10/2018 10:24:54'!
forTestCaseClasses: testCaseClasses named: aName

	| suite |
	suite _ self named: aName.
	testCaseClasses do: [ :aTestCaseClass |
			aTestCaseClass isAbstract
				ifFalse: [ aTestCaseClass addToSuiteFromSelectors: suite ]].
		
	^suite! !

