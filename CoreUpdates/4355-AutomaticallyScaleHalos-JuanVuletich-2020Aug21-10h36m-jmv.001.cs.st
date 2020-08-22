'From Cuis 5.0 [latest update: #4354] on 21 August 2020 at 10:39:26 am'!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox scaleOffset '
	classVariableNames: 'HandleSize Icons '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/21/2020 10:34:45'!
haloHandleSize
	^ Preferences standardListFont pointSize * 3 // 2 max: 16! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/21/2020 10:38:31'!
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
	e _ Preferences haloHandleSize asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form _ form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 1 ].
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/21/2020 10:38:43'!
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
	verticalNamePosition _ haloBox bottom + Preferences haloHandleSize.
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/21/2020 10:38:51'!
basicBox
	"basicBox is in local coordinates"
	| aBox minSide anExtent w hs targetBounds |
	hs _ Preferences haloHandleSize.
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


!HaloMorph class methodsFor: 'cached state access' stamp: 'jmv 8/21/2020 10:29:20'!
releaseClassCachedState

	Icons _ nil! !

!methodRemoval: HaloMorph class #handleSize stamp: 'jmv 8/21/2020 10:38:55'!
HaloMorph class removeSelector: #handleSize!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox scaleOffset'
	classVariableNames: 'Icons'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
