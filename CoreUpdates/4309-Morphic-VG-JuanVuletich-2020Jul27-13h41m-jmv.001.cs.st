'From Cuis 5.0 [latest update: #4305] on 27 July 2020 at 1:42:34 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 13:17:46'!
updateCurrentDisplayBounds
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	currentMorph drawOn: self.
	currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw.
	currentMorph clippedSubmorph ifNotNil: [ :clipped |
		self clippingByCurrentMorphDo: [ self updateDisplayBounds: clipped ]].
	currentMorph unclippedSubmorphsReverseDo: [ :m |
		self updateDisplayBounds: m ].! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/27/2020 13:24:09'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed delta |
	"#aboutToGrab: and #aboutToBeGrabbedBy: might mess wildly with our morphs.
	If we need it, prepare delta before that happens. Use 0@0 if we don't know."
	delta _ `0@0`.
	moveUnderHand ifFalse: [
		delta _ aMorph morphPositionInWorld - self morphPositionInWorld ].
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	(moveUnderHand or: [ (grabbed == aMorph) not ])
		ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				delta _ (r extent // 2) negated ]].
	^ self
		grabMorph: grabbed
		delta: delta! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 13:18:09'!
updateDisplayBounds: aMorph
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aMorph visible ifTrue: [
		aMorph displayBounds ifNil: [
			self into: aMorph.
			self canvasToUse updateCurrentDisplayBounds.
			self outOfMorph
		]]! !

