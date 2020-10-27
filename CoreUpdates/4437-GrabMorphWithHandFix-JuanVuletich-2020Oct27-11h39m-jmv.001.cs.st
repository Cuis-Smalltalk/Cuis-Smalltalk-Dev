'From Cuis 5.0 [latest update: #4434] on 27 October 2020 at 12:05:52 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/27/2020 12:05:33'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed delta |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	"If we find a case where this doesn't work, asking the position to aMorph instead of grabbed is possible."
	delta _ grabbed morphPositionInWorld - self morphPositionInWorld.
	grabbed displayBounds ifNotNil: [ :r |
		(moveUnderHand or: [ (r containsPoint: self morphPositionInWorld) not ])
			ifTrue: [
				delta _ (r extent // 2) negated ]].
	^ self
		grabMorph: grabbed
		delta: delta print! !

