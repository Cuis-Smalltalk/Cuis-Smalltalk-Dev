'From Cuis 4.2 of 25 July 2013 [latest update: #1937] on 15 January 2014 at 9:27:54.124634 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'ul 1/11/2014 16:18'!
allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system
	 excluding SmallIntegers."

	| object lastObject |
	object _ self someObject.
	lastObject _ Object new.
	[ lastObject == object ]
		whileFalse: [
			| nextObject |
			nextObject := object nextObject.
			aBlock value: object.
			object := nextObject ]! !

