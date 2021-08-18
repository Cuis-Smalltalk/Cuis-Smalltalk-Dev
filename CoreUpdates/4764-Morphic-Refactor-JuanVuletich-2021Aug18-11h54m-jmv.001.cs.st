'From Cuis 5.0 [latest update: #4763] on 18 August 2021 at 11:56:50 am'!

!Morph methodsFor: 'private' stamp: 'jmv 8/18/2021 10:21:10'!
privateLocation: aGeometryTransformation! !


!MovableMorph methodsFor: 'private' stamp: 'jmv 8/18/2021 10:21:31'!
privateLocation: aGeometryTransformation
	location _ aGeometryTransformation.! !


!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 8/18/2021 11:42:50'!
initializeWithTranslation: aPoint

	"Set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	on draw error. See #displayWorldSafely"
	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:43:12'!
onForm: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: nil;
		initializeWithTranslation: aRectangle topLeft negated.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:45:55'!
onForm: aForm translation: aPoint

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: nil;
		initializeWithTranslation: aPoint.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:47:19'!
onFormWithSubPixelAntiAliasing: aForm translation: aPoint

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: true;
		initializeWithTranslation: aPoint.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:47:27'!
onFormWithWholePixelAntiAliasing: aForm translation: aPoint

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false;
		initializeWithTranslation: aPoint.! !


!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 8/18/2021 11:42:39'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	boundsFinderCanvas _ BitBltBoundsFinderCanvas new initializeWithTranslation: aPoint.! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 8/18/2021 11:18:46'!
placeEmbeddedObject: anchoredFormOrMorph
	"Place the anchoredMorph or return false if it cannot be placed.
	In any event, advance destX by its width."

	| w |
	w _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner width ]
		ifFalse: [ anchoredFormOrMorph width ].
	destX _ destX + w.
	(destX > rightMargin and: [ lastIndex ~= line first ])
		"Won't fit, but  not at start of a line. Start a new line with it"
		ifTrue: [ ^ false].
	lastIndex _ lastIndex + 1.
	^ true! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 8/18/2021 11:18:52'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	specialWidth _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner width ]
		ifFalse: [ anchoredFormOrMorph width ].
	^ true! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 8/18/2021 11:18:58'!
placeEmbeddedObject: anchoredFormOrMorph
	| descent h |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	descent _ lineHeight - baseline.
	h _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner height ]
		ifFalse: [ anchoredFormOrMorph height ].
	baseline _ baseline max: h.
	lineHeight _ baseline + descent.
	line stop: lastIndex.
	^ true! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 8/18/2021 11:19:24'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				(destX@ (lineY+line baseline)) + (anchoredFormOrMorph morphPosition-anchoredFormOrMorph fullBoundsInOwner corner) rounded.
			anchoredFormOrMorph show.
			canvas fullDraw: anchoredFormOrMorph ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !


!Morph methodsFor: 'updating' stamp: 'jmv 8/18/2021 11:35:21'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	"outsetBy: 1 due to anti aliasing in VectorCanvas"
	self invalidateDisplayRect: (self displayBoundsOrBogus outsetBy: 1) for: self.
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:45:10'!
depth: depth over: aRectangle

	^self onForm: (Form extent: aRectangle extent depth: depth) translation: aRectangle topLeft negated.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:48:01'!
onForm: aForm

	^ self onForm: aForm translation: `0@0`.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:48:27'!
onFormWithSubPixelAntiAliasing: aForm

	^ self onFormWithSubPixelAntiAliasing: aForm translation: `0@0`.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/18/2021 11:48:09'!
onFormWithWholePixelAntiAliasing: aForm

	^ self onFormWithWholePixelAntiAliasing: aForm translation: `0@0`.! !

!methodRemoval: BitBltCanvas #initializeWithOrigin: stamp: 'jmv 8/18/2021 11:56:25'!
BitBltCanvas removeSelector: #initializeWithOrigin:!
!methodRemoval: MorphicCanvas class #on:over: stamp: 'jmv 8/18/2021 11:56:23'!
MorphicCanvas class removeSelector: #on:over:!
!methodRemoval: MorphicCanvas #initializeWithOrigin: stamp: 'jmv 8/18/2021 11:56:32'!
MorphicCanvas removeSelector: #initializeWithOrigin:!
