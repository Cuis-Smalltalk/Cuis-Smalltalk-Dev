'From Cuis 5.0 [latest update: #4288] on 24 July 2020 at 10:43:56 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 10:21:33'!
displayBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 10:22:57'!
morphBounds: aRectangle
	self morphPosition: aRectangle topLeft.
	self morphExtent: aRectangle extent! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/24/2020 10:23:24'!
addHalo: evt
	| halo |
	halo _ HaloMorph new.
	halo popUpFor: self event: evt.
	halo morphBounds: self displayBoundsForHalo.
	^halo! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/24/2020 10:43:34'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e _ self class handleSize asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/24/2020 10:23:20'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	self morphBounds: target displayBoundsForHalo.  "update my size"
	haloBox _ self basicBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/24/2020 10:42:30'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target is: #SystemWindow)
			ifTrue: [target windowColor]
			ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition _ haloBox bottom + self class handleSize.
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/24/2020 10:42:12'!
basicBox
	"basicBox is in local coordinates"
	| aBox minSide anExtent w hs targetBounds |
	hs _ self class handleSize.
	minSide _ 4 * hs.
	anExtent _ extent + (hs*2) max: minSide@minSide.
	targetBounds _ target displayBounds.
	aBox _ Rectangle center: targetBounds center extent: anExtent.
	w _ self world ifNil: [ target world ].
	aBox _ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	^aBox translatedBy: self morphPosition negated! !

!methodRemoval: RectangleLikeMorph #morphBoundsInWorld: stamp: 'jmv 7/24/2020 10:27:38'!
RectangleLikeMorph removeSelector: #morphBoundsInWorld:!
!methodRemoval: Morph #worldBoundsForHalo stamp: 'jmv 7/24/2020 10:21:43'!
Morph removeSelector: #worldBoundsForHalo!
!methodRemoval: Morph #morphBounds: stamp: 'jmv 7/24/2020 10:23:01'!
Morph removeSelector: #morphBounds:!
