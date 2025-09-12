'From Cuis7.5 [latest update: #7563] on 12 September 2025 at 3:35:00 pm'!

!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/12/2025 15:34:52'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to all morphs at event position, in strict front to back order.
	This event is not marked with `wasHandled: true`, so it is passed to multiple recipients
	across the owner chain.."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil ] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		"If we call #morphBehindAt: instead, MouseEnterEvent and MouseLeaveEvent are sent to morphs
		unrelated to the #topUnlockedMorphAt:, but that happen to be behind it."
		"handlerMorph := handlerMorph morphBehindAt: position."
		handlerMorph := handlerMorph owner ].! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/12/2025 15:19:44'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	"If we click to focus, mouse down wasn't processed as a regular event in #processMouseDown:localPosition:,
	because we'll set keyboard focus on mouse up. Then, also ignore mouse moves.
	See also #processMouseUp:localPosition:, where the focus is actually set."
	(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifFalse: [ self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].

	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ].! !

