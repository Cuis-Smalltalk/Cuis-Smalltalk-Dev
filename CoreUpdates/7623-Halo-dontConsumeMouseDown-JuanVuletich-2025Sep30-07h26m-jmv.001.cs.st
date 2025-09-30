'From Cuis7.5 [latest update: #7622] on 30 September 2025 at 7:31:15 am'!

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/30/2025 07:27:59'!
dispatchIn: aWorldMorphOrFocusedMorph
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
							(o allowsSubmorphDrag and: [ handlerMorph isSticky not ]) ifTrue: [
								grabTheMorph := true ]]].
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
	].! !


!HaloMorph methodsFor: 'event handling testing' stamp: 'jmv 9/30/2025 07:30:46'!
handlesMouseDown: aMouseButtonEvent

	^aMouseButtonEvent mouseButton3Pressed! !

!methodRemoval: HaloMorph #mouseButton1Down:localPosition: stamp: 'jmv 9/30/2025 07:26:18'!
HaloMorph removeSelector: #mouseButton1Down:localPosition:!
!methodRemoval: HaloMorph #mouseButton2Down:localPosition: stamp: 'jmv 9/30/2025 07:26:18'!
HaloMorph removeSelector: #mouseButton2Down:localPosition:!
