'From Cuis 5.0 [latest update: #4479] on 26 November 2020 at 3:03:59 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 11/26/2020 15:03:12'!
haloEnclosesFullBounds
	^ self
		valueOfFlag: #haloEnclosesFullBounds
		ifAbsent: [ true ].! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 11/26/2020 13:05:43'!
basicBox
	"basicBox is in local coordinates"

	| minSide e hs box |
	hs _ Preferences haloHandleSize.
	minSide _ 4 * hs.
	e _ extent + (hs*2) max: minSide@minSide.
	box _ target displayBoundsForHalo.
	box _ Rectangle center: box center extent: e.
	self world ifNotNil: [ :w | box _ box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	^box translatedBy: self morphPosition negated.
! !

