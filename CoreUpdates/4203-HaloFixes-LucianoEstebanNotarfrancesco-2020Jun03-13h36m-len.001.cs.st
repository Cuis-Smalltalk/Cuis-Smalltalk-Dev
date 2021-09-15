'From Cuis 5.0 [latest update: #4200] on 3 June 2020 at 1:37:27 pm'!

!HaloMorph methodsFor: 'private' stamp: 'len 6/3/2020 13:07:09'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target is: #SystemWindow) ifTrue: [target windowColor] ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition _ haloBox bottom + self class handleSize.
	self world ifNotNil: [:w| verticalNamePosition + nameMorph morphHeight > w morphHeight ifTrue: [verticalNamePosition _ haloBox bottom - nameMorph morphHeight - self class handleSize]].
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPositionInWorld: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPositionInWorld: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'stepping' stamp: 'len 6/3/2020 13:09:52'!
step
	self comeToFront.
	(target isNil or: [target isInWorld not]) ifTrue: [self delete]! !

