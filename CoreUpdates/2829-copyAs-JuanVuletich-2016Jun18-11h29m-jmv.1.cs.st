'From Cuis 4.2 of 25 July 2013 [latest update: #2827] on 18 June 2016 at 11:29:30.07319 am'!

!Object methodsFor: 'converting' stamp: 'jmv 6/18/2016 11:29'!
copyAs: aSimilarClass
	"Answer an object of class aSimilarClass that has similar contents to the receiver.
	Always answer a new object, even if of same class as receiver"

	^ aSimilarClass newFrom: self! !

