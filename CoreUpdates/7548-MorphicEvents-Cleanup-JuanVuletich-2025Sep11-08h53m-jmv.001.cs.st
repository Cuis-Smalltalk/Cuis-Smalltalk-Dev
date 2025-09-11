'From Cuis7.5 [latest update: #7547] on 11 September 2025 at 8:57:11 am'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:37:46'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| mouseButton1DownHandled |

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/10/2025 16:20:12'!
dispatchIn: aWorldMorphOrFocusedMorph
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		handlerMorph := handlerMorph owner ].! !

!methodRemoval: EntryFieldMorph #mouseButton1Up:localPosition: stamp: 'jmv 9/11/2025 08:55:03'!
EntryFieldMorph removeSelector: #mouseButton1Up:localPosition:!
!methodRemoval: MouseScrollEvent #dispatchIn: stamp: 'jmv 9/11/2025 08:53:37'!
MouseScrollEvent removeSelector: #dispatchIn:!
