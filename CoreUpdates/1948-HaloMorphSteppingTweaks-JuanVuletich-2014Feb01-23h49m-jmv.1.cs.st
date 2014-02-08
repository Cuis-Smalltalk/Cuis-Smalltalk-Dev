'From Cuis 4.2 of 25 July 2013 [latest update: #1947] on 2 February 2014 at 12:06:20.256312 am'!

!HaloMorph methodsFor: 'stepping' stamp: 'jmv 2/1/2014 23:51'!
updatePositionAndExtent
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target morphBoundsInWorld]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = self morphBoundsInWorld
		ifTrue: [^ self].
	newBounds extent = extent
		ifTrue: [^ self morphPositionInWorld: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self morphBoundsInWorld: newBounds! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 2/1/2014 23:51'!
step
	self updatePositionAndExtent! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 2/1/2014 23:51'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setTarget: (target duplicateMorph: evt).
	evt hand grabMorph: target.
	self updatePositionAndExtent.	"update position if necessary"! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 2/1/2014 23:51'!
doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	evt hand grabMorph: target.
	self updatePositionAndExtent. "update position if necessary"! !

!methodRemoval: HaloMorph #stepX!
HaloMorph removeSelector: #stepX!
