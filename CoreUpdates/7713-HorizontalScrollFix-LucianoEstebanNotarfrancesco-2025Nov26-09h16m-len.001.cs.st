'From Cuis7.5 [latest update: #7710] on 26 November 2025 at 9:22:21 am'!
!classDefinition: #MouseScrollEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseScrollEvent
	instanceVariableNames: 'direction delta '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseScrollEvent methodsFor: 'accessing' stamp: 'len 11/15/2025 17:49:13'!
delta
	^ delta! !

!MouseScrollEvent methodsFor: 'private' stamp: 'len 11/15/2025 17:48:03'!
setPosition: evtPos direction: evtDir delta: deltaPoint buttons: evtButtons hand: evtHand stamp: stamp
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	direction := evtDir.
	delta := deltaPoint.
	timeStamp := stamp.! !


!MouseScrollEvent methodsFor: 'private' stamp: 'len 11/15/2025 17:48:30'!
setPosition: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp
	self setPosition: evtPos direction: evtDir delta: nil buttons: evtButtons hand: evtHand stamp: stamp! !


!HandMorph methodsFor: 'private events' stamp: 'len 11/17/2025 14:24:03'!
generateMouseScrollEvent: evtBuf
	"Generate the appropriate mouse scroll event for the given raw event buffer
	Copied from Sqeak 5.3 and modifed."
	| buttons modifiers stamp deltaX deltaY direction oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue ].
	deltaX := evtBuf third.
	deltaY := evtBuf fourth.
	(deltaX = 0 and: [deltaY = 0]) ifTrue: [^ nil]. "No movement, bail out"
	"For the 'direction', we chooses either horizontal or vertical movement (whichever is greater):"
	direction := deltaY abs >= deltaX abs
		ifTrue: [deltaY negative ifTrue: [#down] ifFalse: [#up]]
		ifFalse: [deltaX negative ifTrue: [#left] ifFalse: [#right]].
	modifiers := evtBuf fifth.
	buttons := (modifiers bitShift: 3) bitOr: (lastMouseEvent buttons bitAnd: 7).
	oldButtons := lastEventBuffer fifth 
		bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	^ MouseScrollEvent new
		setPosition: self morphPosition 
		direction: direction
		delta: deltaX @ deltaY "although we have choosen either a horizontal or vertical direction, we make available the original delta because it has legitimate use cases"
		buttons: (oldButtons bitXor: buttons)
		hand: self
		stamp: stamp! !

!classDefinition: #MouseScrollEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseScrollEvent
	instanceVariableNames: 'direction delta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
