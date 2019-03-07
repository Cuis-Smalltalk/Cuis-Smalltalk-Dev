'From Cuis 5.0 of 7 November 2016 [latest update: #3647] on 7 March 2019 at 5:16:10 pm'!

!TestSuite class methodsFor: 'instance creation - private' stamp: 'HAW 3/7/2019 17:15:17'!
forClasses: classes named: aName

	| suite |
	
	suite _ classes
		inject: (self named: aName)
		into: [ :partialSuite :aClass | partialSuite addTests: (self forClass: aClass) tests ].
		
	^suite
		
! !

