'From Cuis 5.0 [latest update: #4271] on 22 July 2020 at 12:17:10 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 12:11:55'!
morphFullBoundsInWorld
"Usando los nombres viejos que estoy por hacer pelota. Repasar.
Ahora es displayFullBounds"
	| fullBounds |
	fullBounds _ self morphBoundsInWorld.
	self submorphsFullBoundsInWorld ifNotNil: [ :b |
		fullBounds _ b quickMerge: fullBounds ].
	^fullBounds! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 11:53:37'!
submorphsFullBoundsInWorld
"Usando los nombres viejos que estoy por hacer pelota. Repasar."
	"Current full bounds of submorphs carried by the hand.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	submorphs isEmpty ifTrue: [ ^nil ].
	fullBounds _ nil.
	(self valueOfProperty:#lastPos)
		ifNil: [						"Just once when a new morph (not in the world) is attached to the hand."
			self submorphsDo: [ :m |
				m morphFullBoundsInWorld ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]]]
		ifNotNil: [ :lastPos |						"When carrying morphs around."
			self submorphsDo: [ :m |
				m displayFullBounds ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]].
			fullBounds ifNil: [ ^nil ].						"bueeeenoooo"
			fullBounds _ fullBounds translatedBy: self morphPosition-lastPos ].
	^fullBounds! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/13/2020 09:50:36'!
fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
	self setProperty: #lastPos toValue: (submorphs notEmpty ifTrue: [self morphPosition] ifFalse: [nil])! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 12:03:21'!
displayFullBounds
"ver lo que habia hecho. #lastPos, reusar cuando se mueve.
Done.
Por ahora esta en #morphFullBoundsInWorld"
	^super displayFullBounds! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/13/2020 10:31:49'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		grabMorphData
			at: aMorph 
			put: { formerOwner. aMorph morphPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.
	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		self setProperty: #lastPos toValue: self morphPosition ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/22/2020 11:56:13'!
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
			grabbed displayBounds ifNotNil: [ :r |
				delta _ (r extent // 2) negated ]].
	^ self
		grabMorph: grabbed
		delta: delta! !

