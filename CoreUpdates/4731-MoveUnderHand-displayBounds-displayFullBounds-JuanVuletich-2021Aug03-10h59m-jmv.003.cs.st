'From Cuis 5.0 [latest update: #4730] on 3 August 2021 at 11:12:20 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 10:00:10'!
basicDisplayBounds
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	Integer pixel coordinates!!"

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 11:04:36'!
basicDisplayBounds

	^ self world canvas displayBoundsForHand: self! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 11:04:44'!
basicDisplayBounds
	^ 0@0 extent: extent! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 09:33:14'!
displayBoundsSetFrom: aCanvas
	"Private for framework use. See super implementation."
	
	"Widgets don't need contour. Additionally, for InnerTextMorph and LayoutMorph, the bounds
	can not be deduced from #drawOn: and should be computed from the 'extent' instance variable."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				"In these cases, #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph, for example),
				but it is OK to compute them from #morphLocalBounds. This is not true for non-orthoRectangular morphs!!"
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds ]]].! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 09:35:25'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use. NOP here. See super implementation."! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 11:06:31'!
displayBounds
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	Integer pixel coordinates!!"

	| answer |	
	answer _ self basicDisplayBounds.
	answer ifNil: [ ^nil ].
	answer hasPositiveExtent ifTrue: [ ^answer ].
	^self displayFullBounds.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 10:21:33'!
displayBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 10:52:38'!
displayBoundsOrBogus
	"Never answer nil. A bogus value will trigger a draw operation, and bounds will later be corrected.
	Integer pixel coordinates!!"

	^self basicDisplayBounds ifNil: [
		Rectangle center: (self externalizeToWorld: `0@0`) extent: `2@2` ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 09:34:43'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
				self setProperty: #contour toValue: (
					self wantsContour ifTrue: [ aCanvas contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) ])]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 09:37:16'!
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
				| oldTop oldBottom |
				oldTop _ privateDisplayBounds top max: 0.
				oldBottom _ privateDisplayBounds bottom-1.
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					self setProperty: #contour toValue:
						(aCanvas
							contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
							oldTop: oldTop
							oldBottom: oldBottom)]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/3/2021 10:52:51'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!!"

	| answer |
	answer _ self basicDisplayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer encompassingIntegerRectangle! !


!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 8/3/2021 09:30:04'!
wantsContour
	"Kernel morphs don't need contour"

	^false! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/3/2021 11:09:33'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				bounds _ r translatedBy: grabbed morphPositionInWorld negated ]].
		bounds ifNil: [ bounds _ grabbed findBounds ].
		positionInHandCoordinates _ (bounds center + bounds bottomRight //2) negated.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/3/2021 09:59:35'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].

	Preferences halosShowCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.3`.
			b _ target displayBounds.
			b = fb ifFalse: [
				aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.3` ].
			self drawCoordinateSystemOn: aCanvas ]].! !


!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 8/3/2021 09:29:54'!
wantsContour
	"Widget morphs don't need contour"

	^false! !


!WindowEdgeAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/3/2021 09:54:38'!
drawOn: aCanvas

	| c |
	(owner is: #SystemWindow) ifFalse: [
		^super drawOn: aCanvas ].

	c _ owner windowFrameColor.
	selector caseOf: {
		[ #windowTopLeft: ] -> [
			aCanvas roundTopLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowTopRight: ] -> [
			aCanvas roundTopRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomLeft: ] -> [
			aCanvas roundBottomLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomRight: ] -> [
			aCanvas roundBottomRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."	
		aCanvas roundEdge: self morphLocalBounds color: c ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/3/2021 09:48:56'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			(currentMorph is: #WidgetMorph)
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]].
			(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not]) ifTrue: [
				currentMorph submorphsDo: [ :m |
					self fullUpdateProtrudingBounds: m ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/3/2021 09:50:14'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			(currentMorph is: #WidgetMorph)
				ifTrue: [currentMorph displayBoundsSetFrom: self]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph
						drawOn: self;
						displayBoundsSetFrom: self.
					(currentMorph postDrawOn: self) ifTrue: [
						currentMorph displayBoundsUpdateFrom: self ]]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 8/3/2021 10:00:12'!
morph: aMorph isAtPoint: aPoint
	aMorph basicDisplayBounds ifNotNil: [ :r |
		(r containsPoint: aPoint) ifFalse: [
			^false ]].
	^aMorph morphContainsPoint:
		(aMorph internalizeFromWorld: aPoint)! !

!methodRemoval: WorldMorph #displayBounds stamp: 'jmv 8/3/2021 11:05:39'!
WorldMorph removeSelector: #displayBounds!
!methodRemoval: HandMorph #displayBounds stamp: 'jmv 8/3/2021 11:05:37'!
HandMorph removeSelector: #displayBounds!
