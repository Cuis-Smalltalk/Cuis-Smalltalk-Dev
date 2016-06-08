'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 10:49:07.575982 pm'!

!HaloMorph methodsFor: 'private' stamp: 'len 6/7/2016 09:55'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target isKindOf: SystemWindow) ifTrue: [target windowColor] ifFalse: [Color lightBlue alpha: 0.9]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color black.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	self addMorph: nameBackground position: namePosition - 2.
	self addMorph: nameMorph position: namePosition.
	^nameMorph! !

