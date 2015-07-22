'From Cuis 4.2 of 25 July 2013 [latest update: #2404] on 11 July 2015 at 2:26:10.688539 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 7/11/2015 14:25'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation withTranslation: 50@50.
	layoutNeeded _ false! !

