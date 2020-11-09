'From Cuis 5.0 [latest update: #4437] on 28 October 2020 at 10:16:36 am'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/28/2020 10:16:27'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	moveUnderHand
		ifFalse: [
			positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
				morphPositionInWorld - self morphPositionInWorld ]
		ifTrue: [
			"We can possibly do better, especially for non WidgetMorphs"
			positionInHandCoordinates _ -30 @ -10.
			grabbed isInWorld ifTrue: [
				grabbed displayBounds ifNotNil: [ :r |
					positionInHandCoordinates _ (r extent // 2) negated ]]].
	^ self
		grabMorph: grabbed
		delta: positionInHandCoordinates! !

