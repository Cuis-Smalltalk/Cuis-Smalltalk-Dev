'From Cuis 5.0 [latest update: #4660] on 13 July 2021 at 9:59:40 am'!

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 7/12/2021 17:06:17'!
withRadians: radians around: rotationCenter

	^(AffineTransformation withTranslation: rotationCenter) composedWith:
		((AffineTransformation withRadians: radians) composedWith:
			(AffineTransformation withTranslation: rotationCenter negated)).! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/13/2021 09:58:42'!
wantsContour
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas.
	By default, any top morph that is drawn by VectorCanvas."

	^self requiresVectorCanvas and: [ self isOwnedByWorldOrHand ]! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 7/13/2021 09:40:08'!
referencePosition
	^self externalizeToWorld: self rotationCenter! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 7/13/2021 09:52:20'!
rotationCenter
	"In own coordinates.
	Subclasses might redefine if desired."

	self valueOfProperty: #rotationCenter ifPresentDo: [ :p | ^p ].
	self displayFullBounds ifNotNil: [ :r |
		^self setProperty: #rotationCenter toValue: (self internalizeFromWorld: r center) ].
	^`0@0`.! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 7/12/2021 16:02:51'!
rotationCenter
	"In own coordinates."

	^ extent / 2.0.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/12/2021 16:02:56'!
rotationCenter
	"In own coordinates."

	^ extent / 2.0.! !


!AffineTransformation methodsFor: 'composing' stamp: 'jmv 7/12/2021 16:26:13'!
composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a transformation whose application (transform:) is equivalent
	to first applying innerTransformation and then self.
	In other words
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

 	^innerTransformation innerComposedWithAffineTransformation: self! !


!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 7/12/2021 16:26:33'!
composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a transformation whose application (transform:) is equivalent
	to first applying innerTransformation and then self.
	In other words
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

	^innerTransformation innerComposedWithTranslation: self! !


!Morph methodsFor: 'accessing - properties' stamp: 'jmv 7/13/2021 09:51:15'!
setProperty: aSymbol toValue: anObject
	"change the receiver's property named aSymbol to anObject"

	"the properties dictionary never has nil as value.
	Asking for a nil value is the same as removing the property."

	anObject ifNil: [
		self removeProperty: aSymbol.
		^nil].
	properties ifNil: [ properties _ IdentityDictionary new ].
	properties at: aSymbol put: anObject.
	^anObject! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/13/2021 09:58:46'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				self isOrthoRectangularMorph
					ifTrue: [
						"In these cases, #drawOn: might not be able to find bounds (InnerTextMorph, for example),
						but it is OK to compute them from #morphLocalBounds. This is not true for non-orthoRectangular morphs!!"
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds ]
					ifFalse: [
						"In this cases, we use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self setProperty: #contour toValue: (
							self wantsContour ifTrue: [ aCanvas contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) ])]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/13/2021 09:58:48'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	To be called after #postDrawOn:. See also #displayBoundsSetFrom:
	This more complicated update is needed because the contour information from #drawOn: was possibly
	when drawing submorphs. So, the controur from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				self isOrthoRectangularMorph ifFalse: [				"Like in #displayBoundsSetFrom:"
					| oldTop oldBottom |
					oldTop _ privateDisplayBounds top max: 0.
					oldBottom _ privateDisplayBounds bottom-1.
					privateDisplayBounds _ privateDisplayBounds
						quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
					self setProperty: #contour toValue:
						(self wantsContour ifTrue: [
							aCanvas
								contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
								oldTop: oldTop
								oldBottom: oldBottom ])]]]].! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 7/12/2021 17:37:54'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil: [^#()].
	myRect := self displayFullBounds.
	^Array streamContents: [ :strm |
		myWorld allMorphsBut: self do: [ :m |
			(m isReallyVisible
			and: [ m isLocked not
				and: [(m displayFullBounds intersects: myRect)
					and: [(m ~= self)
						and: [(#(HaloMorph HaloHandleMorph) statePointsTo: m class name) not]]]])
		 ifTrue: [ strm nextPut: m ]]].! !


!MovableMorph methodsFor: 'accessing' stamp: 'jmv 7/8/2021 19:01:42'!
location: aGeometryTransformation
	location _ aGeometryTransformation.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/13/2021 09:43:07'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians prevLocation |
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta + angleOffset.
	radians _ radians detentBy: 0.05 atMultiplesOf: Float pi / 4 snap: false.
	rotHandle color: (radians = 0.0
		ifTrue: [`Color lightBlue`]
		ifFalse: [`Color blue`]).
	rotHandle submorphsDo: [ :m |
		m color: rotHandle color makeForegroundColor].
	prevLocation _ target location.
	target location: (prevLocation composedWith: (
		AffineTransformation withRadians: radians-prevLocation radians around: target rotationCenter)).
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2).	
	self redrawNeeded.! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 7/13/2021 09:48:52'!
crAction: aBlock
	"Sets the action to perform when user presses <Return> key"
	self setProperty: #crAction toValue: aBlock! !

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 7/13/2021 09:49:29'!
escAction: aBlock
	"Sets the action to perform when user presses <Esc> key"

	self setProperty: #escAction toValue: aBlock.! !

!methodRemoval: KernelMorph #referencePosition stamp: 'jmv 7/13/2021 09:35:48'!
KernelMorph removeSelector: #referencePosition!
!methodRemoval: MovableMorph #referencePosition: stamp: 'jmv 7/13/2021 09:41:49'!
MovableMorph removeSelector: #referencePosition:!
!methodRemoval: Morph #knowsContour stamp: 'jmv 7/13/2021 09:58:54'!
Morph removeSelector: #knowsContour!
!methodRemoval: Morph #rotationCenter stamp: 'jmv 7/13/2021 09:41:25'!
Morph removeSelector: #rotationCenter!
!methodRemoval: Morph #referencePosition stamp: 'jmv 7/13/2021 09:41:20'!
Morph removeSelector: #referencePosition!
