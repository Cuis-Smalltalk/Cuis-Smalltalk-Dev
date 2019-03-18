'From Cuis 5.0 of 7 November 2016 [latest update: #3665] on 17 March 2019 at 7:32:51 am'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 3/17/2019 07:08:22'!
shouldFail: aBlock
	
	self should: aBlock raise: Exception ! !

!methodRemoval: TestCase #shouldFix:!
TestCase removeSelector: #shouldFix:!
