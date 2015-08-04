'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 29 July 2015 at 10:25:11.297735 am'!

!Morph methodsFor: 'initialization' stamp: 'jmv 1/7/2015 09:59'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.
	layoutNeeded _ false! !

!Morph methodsFor: 'initialization' stamp: 'jmv 7/29/2015 10:24'!
openInWorld: aWorld
	"Add this morph to the requested World."
	(location = MorphicTranslation new)
		ifTrue: [ aWorld addMorph: self position: 50@50 ]
		ifFalse: [ aWorld addMorph: self ].
	aWorld startSteppingSubmorphsOf: self! !

