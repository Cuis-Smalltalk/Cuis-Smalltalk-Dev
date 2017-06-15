'From Cuis 5.0 of 7 November 2016 [latest update: #3099] on 11 June 2017 at 8:11:06 pm'!

!TestCase class methodsFor: 'Testing' stamp: 'pb 6/11/2017 20:10:43'!
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"
	^ thisContext methodClass == self class.! !


!TestResource class methodsFor: 'Testing' stamp: 'pb 6/11/2017 20:10:52'!
isAbstract
	"Override to true if a TestResource subclass is Abstract and should not have
	TestCase instances built from it"
	^ thisContext methodClass == self class.! !

