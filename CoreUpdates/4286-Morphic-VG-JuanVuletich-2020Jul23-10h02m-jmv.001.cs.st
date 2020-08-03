'From Cuis 5.0 [latest update: #4282] on 23 July 2020 at 10:29:47 am'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 10:19:48'!
morphBoundsInWorld: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.

	oldExtent _ self morphExtentInWorld.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPositionInWorld: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPositionInWorld: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPositionInWorld: newBounds topLeft.
	].! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 7/23/2020 10:04:22'!
readEvalPrint
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line _ self request: '>') ] whileFalse: [
		line caseOf: {
			[ 'revert' ] -> [].
			}
		otherwise: [
			self
				newLine;
				show:
					([ Compiler evaluate: line ] ifError: [ :err :ex | err ]) ]]! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/23/2020 10:29:28'!
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
	(moveUnderHand or: [ (grabbed == aMorph) not ])
		ifTrue: [
			delta _ (grabbed morphExtent // 2) negated ].
	^ self
		grabMorph: grabbed
		delta: delta! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/23/2020 10:20:27'!
basicBox
	| aBox minSide anExtent w hs |
	hs _ self class handleSize.
	minSide _ 4 * hs.
	anExtent _ extent + (hs*2) max: minSide@minSide.
	aBox _ Rectangle center: target displayBounds center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: hs@hs) ]! !

!methodRemoval: HaloMorph #redrawNeeded stamp: 'jmv 7/23/2020 10:25:42'!
HaloMorph removeSelector: #redrawNeeded!
!methodRemoval: Morph #morphBoundsInWorld: stamp: 'jmv 7/23/2020 10:19:57'!
Morph removeSelector: #morphBoundsInWorld:!
