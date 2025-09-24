'From Cuis7.5 [latest update: #7608] on 24 September 2025 at 2:09:48 pm'!

!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/24/2025 14:09:44'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to all morphs at event position, in strict front to back order.
	This event is not marked with `wasHandled: true`, so it is passed to multiple recipients
	across the owner chain.."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	(handlerMorph is: #HaloMorph) ifTrue: [ handlerMorph := handlerMorph target ].
	[ handlerMorph notNil ] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		"If we call #morphBehindAt: instead, MouseEnterEvent and MouseLeaveEvent are sent to morphs
		unrelated to the #topUnlockedMorphAt:, but that happen to be behind it."
		"handlerMorph := handlerMorph morphBehindAt: position."
		handlerMorph := handlerMorph owner ].! !

