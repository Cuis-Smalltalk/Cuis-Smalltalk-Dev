Class {
	#name : 'MouseButtonEvent',
	#superclass : 'MouseEvent',
	#instVars : [
		'buttonEventType',
		'whichButton'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
MouseButtonEvent >> dispatchIn: aWorldMorphOrFocusedMorph [
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."
	
	| topUnlockedMorph handlerMorph grabTheMorph |
	
	"Only for Button 1 and Button 2 MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].
	self mouseButton3Pressed ifTrue: [
		^super dispatchIn: aWorldMorphOrFocusedMorph ].

	topUnlockedMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	
	(self mouseButton1Pressed and: [ topUnlockedMorph is: #HaloMorph ]) ifTrue: [
		"Get rid of Halo on MouseDown, and find the new top morph under hand."
		topUnlockedMorph delete.
		topUnlockedMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position ].
	
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			
			"handlerMorph is in the owner chain of th top-most unlocked, visible morph."
			(handlerMorph handlesMouseDown: self)
				ifTrue: [
					self mouseButton2Pressed ifTrue: [
						"Don't propagate mouse button 2 up to the world.
						Getting the World menu when you right click on a morph is something you never want."
						(handlerMorph isWorldMorph and: [topUnlockedMorph isWorldMorph not]) ifTrue: [^self]].
					self sendEventTo: handlerMorph ]
				ifFalse: [
					grabTheMorph := false.
					"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
					self mouseButton1Pressed ifTrue: [
						handlerMorph owner ifNotNil: [ :o |
							handlerMorph isSticky
								ifTrue: [self wasHandled: true]
								ifFalse: [
									o allowsSubmorphDrag ifTrue: [
										grabTheMorph := true ]]]].
					grabTheMorph ifTrue: [
						self hand
							waitForClicksOrDrag: handlerMorph event: self
							dragSel: ((Preferences at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
							clkSel: ((Preferences at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
						"false ifTrue: [ self hand grabMorph: aMorph ]."
						(Preferences at: #clickGrabsMorphs) ifFalse: [
							self shiftPressed
								ifTrue: [ (handlerMorph is: #SystemWindow) ifTrue: [ handlerMorph sendToBack ] ]
								ifFalse: [ handlerMorph activateWindow ] ].
						self wasHandled: true ]]
			
		].
		handlerMorph := handlerMorph owner.
	].
]

{ #category : 'testing' }
MouseButtonEvent >> isMouseButton [
	^ true
]

{ #category : 'testing' }
MouseButtonEvent >> isMouseDown [
	^ buttonEventType == #mouseDown
]

{ #category : 'testing' }
MouseButtonEvent >> isMouseUp [
	^ buttonEventType == #mouseUp
]

{ #category : 'accessing' }
MouseButtonEvent >> mouseButton1Changed [
	"Answer true if the mouseButton1 has changed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton1Changed and mouseButton3Changed.
	The check for button change (instead of button press) is specially useful on buttonUp events.
	See also #mouseButton1Pressed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton1
]

{ #category : 'accessing' }
MouseButtonEvent >> mouseButton2Changed [
	"Answer true if the mouseButton2 has changed.
	Reported by the VM for right mouse button or option+click on the Mac.
	It is also emulated here with ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into2 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	self turnMouseButton2Into3 ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton2
]

{ #category : 'accessing' }
MouseButtonEvent >> mouseButton3Changed [
	"Answer true if the mouseButton3 has changed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or meta+click on Linux.
	It is also emulated here with shift-ctrl-click or ctrl-rightClick on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into3 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	(self turnMouseButton2Into3 and: [ whichButton anyMask: InputSensor mouseButton2 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton3
]

{ #category : 'private' }
MouseButtonEvent >> mouseDownPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp [
	
	buttonEventType := #mouseDown.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.
]

{ #category : 'private' }
MouseButtonEvent >> mouseUpPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp [
	
	buttonEventType := #mouseUp.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.
]

{ #category : 'dispatching' }
MouseButtonEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	self isMouseDown ifTrue: [
		aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	self isMouseUp ifTrue: [
		aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
]

{ #category : 'dispatching' }
MouseButtonEvent >> startDispatchFromHand: aHand [
	"double dispatch the event dispatch"

	aHand startMouseButtonDispatch: self
]
