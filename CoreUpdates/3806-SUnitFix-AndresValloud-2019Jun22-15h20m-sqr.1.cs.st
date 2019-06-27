'From Cuis 5.0 of 7 November 2016 [latest update: #3805] on 22 June 2019 at 3:21:51 pm'!

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'sqr 6/22/2019 15:21:45'!
errorObject
	^UnhandledError! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 12:51:03'!
shouldntFail: aBlock

	self shouldnt: aBlock raise: TestResult exError! !