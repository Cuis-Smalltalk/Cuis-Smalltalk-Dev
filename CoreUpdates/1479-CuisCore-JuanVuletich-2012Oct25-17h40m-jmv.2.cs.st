'From Cuis 4.0 of 21 April 2012 [latest update: #1478] on 25 October 2012 at 5:54:57 pm'!

!DropEvent methodsFor: 'accessing' stamp: 'jmv 10/25/2012 17:54'!
wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/25/2012 17:42'!
justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	newOwnerMorph activateWindow.
	self isInWorld  ifTrue: [
		self world startSteppingSubmorphsOf: self ].
	"Note an unhappy inefficiency here:  the startStepping... call will often have already been called in the sequence leading up to entry to this method, but unfortunately the isPartsDonor: call often will not have already happened, with the result that the startStepping... call will not have resulted in the startage of the steppage."! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 10/25/2012 17:43'!
addHalo: evt
	| halo |
	halo _ HaloMorph new.
	halo popUpFor: self event: evt.
	halo morphBoundsInWorld: self worldBoundsForHalo.
	^halo! !


!MenuMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/25/2012 17:42'!
justDroppedInto: newOwnerMorph event: evt
	| halo |
	super justDroppedInto: newOwnerMorph event: evt.
	halo _ evt hand halo.
	(halo notNil and:[halo target hasOwner: self]) ifTrue: [
		"Grabbed single menu item"
		self addHalo: evt ].
	stayUp ifFalse: [ evt hand newMouseFocus: self ]! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/25/2012 17:44'!
acceptDroppingMorph: dropped event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph _ self morphToDropFrom: dropped.
	self isWorldMorph 
		ifTrue: [	
			"Add the given morph to this world and start stepping it if it wants to be."
			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Beeper beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m is: #HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 10/25/2012 17:42'!
justDroppedInto: newOwnerMorph event: anEvent
	isCollapsed
		ifTrue: [
			self morphPosition: (self morphPosition max: 0@0) ]
		ifFalse: [
			TopWindow ~~ self ifTrue: [ self activate ]].
	^super justDroppedInto: newOwnerMorph event: anEvent! !


!UserInputEvent methodsFor: 'accessing' stamp: 'jmv 10/25/2012 17:54'!
wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool! !

!methodRemoval: Morph #haloClass!
Morph removeSelector: #haloClass!
