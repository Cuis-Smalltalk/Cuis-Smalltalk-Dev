'From Cuis 5.0 [latest update: #4957] on 29 October 2021 at 5:45:19 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 15:05:41'!
morphExtent
	"In our own coordinates!!
	nil if unknown."

	^ nil! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 16:54:11'!
fullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	Note: Answers an integer rectangle"

	"Rethoric question. If it is ever true, call super."
	"(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifFalse: ["
	^ (self externalizeBoundingRectOf: self morphLocalBounds) encompassingIntegerRectangle! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 16:54:07'!
fullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	Note: Answers an integer rectangle"

	"Rethoric question. If it is ever true, call super."
	"(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifFalse: ["
	^ (self externalizeBoundingRectOf: self morphLocalBounds) encompassingIntegerRectangle! !


!Morph methodsFor: 'drawing' stamp: 'jmv 10/29/2021 15:59:23'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas bounds |
	self requiresVectorCanvas ifTrue: [	
		^ self imageFormVG: extentOrNil depth: depth ].

	bounds _ self fullBoundsInOwner.
	answerExtent _ bounds extent.
	extentOrNil ifNotNil: [ answerExtent _ answerExtent min: extentOrNil * 4 ].
	auxCanvas _ MorphicCanvas depth: depth over: (bounds origin extent: answerExtent ceiling).
	auxCanvas fullDraw: self.
	answer _ auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
	^answer! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 15:49:22'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Subclasses that (re)define #requiresVectorCanvas and #morphExtent should redefine this methods.
	See inheritance.
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w _ owner ]
	ifFalse: [
		w _ UISupervisor ui ].
	c _ w boundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner _ owner.
	owner _ w.
	prevNotVisibleFlag _ self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	c fullUpdateProtrudingBounds: self.
	answer _ self displayFullBounds.
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!!"
	prevOwner == owner ifFalse: [
		owner _ prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 15:34:45'!
fullBoundsInOwner
	"Answer fullBoundsInOwner. Cache the found bounds.
	No special care needed for property #fullBoundsInOwner. Cache is invalidated when appropriate.
	Note: Answers an integer rectangle"

	(self valueOfProperty: #fullBoundsInOwner) ifNotNil: [ :fullBoundsInOwner |
		^fullBoundsInOwner ].

	^self setProperty: #fullBoundsInOwner toValue: self findFullBoundsInOwner.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 17:39:50'!
morphExtentInOwner

	self morphExtent ifNotNil: [ :e |
		^ self externalizeDistance: e ].
	^self fullBoundsInOwner extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 17:40:09'!
topLeftInOwner

	^self fullBoundsInOwner origin! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 10/29/2021 15:23:44'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If False, we can be drawn by BitBltCanvas, and needs to implement:
		#morphExtent and #topLeftInOwner"

	^ true! !


!MovableMorph methodsFor: 'geometry testing' stamp: 'jmv 10/29/2021 16:34:26'!
isOrAnyOwnerIsRotated
	"True if we or any owner is rotated.
	A zoomed widget will answer false, but a rotated one will answer true (even if only
		some owner is rotated)."

	^ location doesNotRotate not or: [ owner notNil and: [ owner isOrAnyOwnerIsRotated ]].! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 15:05:53'!
morphExtent
	"In our own coordinates!!
	nil if unknown."

	^ extent! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/29/2021 15:59:42'!
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
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				bounds _ r translatedBy: grabbed morphPositionInWorld negated ]].
		bounds ifNil: [ bounds _ grabbed fullBoundsInOwner ].
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


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 10/29/2021 15:05:28'!
morphExtent
	"In our own coordinates!!
	nil if unknown."

	^ extent! !


!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 10/29/2021 17:38:15'!
scrollSelectionIntoView

	| r |
	selectedMorph ifNotNil: [
		r _ scroller externalizeBoundingRectOf: selectedMorph fullBoundsInOwner.
		self scrollToShow: r ]! !

!methodRemoval: WidgetMorph #morphExtentInOwner stamp: 'jmv 10/29/2021 17:44:41'!
WidgetMorph removeSelector: #morphExtentInOwner!
!methodRemoval: WidgetMorph #findFullBoundsInOwner stamp: 'jmv 10/29/2021 17:44:31'!
WidgetMorph removeSelector: #findFullBoundsInOwner!
!methodRemoval: KernelMorph #morphExtentInOwner stamp: 'jmv 10/29/2021 17:44:41'!
KernelMorph removeSelector: #morphExtentInOwner!
!methodRemoval: KernelMorph #findFullBoundsInOwner stamp: 'jmv 10/29/2021 17:44:31'!
KernelMorph removeSelector: #findFullBoundsInOwner!
