'From Cuis 4.2 of 25 July 2013 [latest update: #2402] on 5 July 2015 at 8:05:18.928613 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/5/2015 20:04'!
grabMorph: aMorph from: formerOwner at: morphPos
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	grabMorphData
		at: aMorph 
		put: { formerOwner. morphPos. }.
	self addMorphBack: aMorph position: morphPos - self morphPositionInWorld.
	aMorph justGrabbedFrom: formerOwner! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/5/2015 20:00'!
attachMorph: aMorph
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self rememberGrabMorphDataFor: aMorph.
	delta _ aMorph morphExtent // 2.
	self addMorphBack: aMorph position: delta negated! !

!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 7/5/2015 20:04'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| grabbed  |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	^ self
		grabMorph: grabbed
		from: aMorph owner
		at: aMorph morphPositionInWorld! !

!methodRemoval: HandMorph #grabMorph:from:!
HandMorph removeSelector: #grabMorph:from:!
