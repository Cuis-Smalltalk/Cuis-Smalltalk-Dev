'From Cuis 4.2 of 25 July 2013 [latest update: #2198] on 26 February 2015 at 2:55:07.821714 pm'!

!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 2/26/2015 12:33'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]! !

!methodRemoval: ShortIntegerArray #bytesPerBasicElement!
ShortIntegerArray removeSelector: #bytesPerBasicElement!
