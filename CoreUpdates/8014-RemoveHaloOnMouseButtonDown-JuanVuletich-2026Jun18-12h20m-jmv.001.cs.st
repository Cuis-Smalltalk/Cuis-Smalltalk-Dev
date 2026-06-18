'From Cuis7.9 [latest update: #8013] on 18 June 2026 at 12:22:14 pm'!

!HaloHandleMorph methodsFor: 'testing' stamp: 'jmv 18/Jun/2026, 3:14:16 pm (UTC)'!
is: aSymbol
	^ aSymbol == #HaloHandleMorph or: [ super is: aSymbol ]! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 18/Jun/2026, 3:16:42 pm (UTC)'!
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
	(topUnlockedMorph is: #HaloMorph) ifTrue: [
		topUnlockedMorph := topUnlockedMorph target ].
	(topUnlockedMorph is: #HaloHandleMorph) ifFalse: [
		self hand halo: nil ].

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
	].! !


!Morph methodsFor: 'events' stamp: 'jmv 18/Jun/2026, 2:44:10 pm (UTC)'!
dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand grabMorph: self.! !

