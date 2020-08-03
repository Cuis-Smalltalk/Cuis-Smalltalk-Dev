'From Cuis 5.0 [latest update: #4263] on 12 July 2020 at 8:59:50 pm'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 7/12/2020 20:59:04'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self
		invalidateDisplayRect:
			((self morphBoundsInWorld
				outsetBy: self class handleSize*2)
					extendBy: 0@(FontFamily defaultLineSpacing+self class handleSize))
		from: nil.! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/12/2020 20:54:19'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self class handleSize.
	anExtent _ extent + (self class handleSize*2) max: minSide@minSide.
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: `8@8`) ]! !

!HaloMorph methodsFor: 'stepping' stamp: 'jmv 7/12/2020 18:55:39'!
step
	(target isNil or: [target isInWorld not]) ifTrue: [self delete]! !

