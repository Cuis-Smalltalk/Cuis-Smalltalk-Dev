'From Cuis 4.0 of 21 April 2012 [latest update: #1479] on 25 October 2012 at 11:29:43 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 10/25/2012 23:09'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ (self morphWidth - nameMorph morphWidth // 2) @ (self morphHeight + (self handleSize * 2)).
	nameMorph morphPosition: namePosition.
	nameBackground morphPosition: nameMorph morphPosition - 2.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	^nameMorph! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 10/25/2012 23:09'!
addHandles
	| box |
	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	box _ self basicBox.

	target addHandlesTo: self box: box.

	self addNameString: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/25/2012 23:20'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m morphExtent // 2.
	m morphPosition: delta negated! !

!methodRemoval: HaloMorph #addNameBeneath:string:!
HaloMorph removeSelector: #addNameBeneath:string:!
!methodRemoval: HaloMorph #mouseMove:localPosition:!
HaloMorph removeSelector: #mouseMove:localPosition:!
