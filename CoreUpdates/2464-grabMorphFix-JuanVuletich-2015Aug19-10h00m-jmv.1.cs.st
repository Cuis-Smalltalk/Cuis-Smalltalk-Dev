'From Cuis 4.2 of 25 July 2013 [latest update: #2463] on 19 August 2015 at 10:05:14.922598 am'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/19/2015 10:04'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed delta |
	"#aboutToGrab: and #aboutToBeGrabbedBy: might mess wildly with our morphs.
	If we need it, prepare delta before that happens"
	moveUnderHand ifFalse: [
		delta _ aMorph morphPositionInWorld - self morphPositionInWorld ].
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	(moveUnderHand or: [ (grabbed morphExtent >= aMorph morphExtent) not ])
		ifTrue: [ delta _ (grabbed morphExtent // 2) negated ].
	^ self
		grabMorph: grabbed
		delta: delta! !

