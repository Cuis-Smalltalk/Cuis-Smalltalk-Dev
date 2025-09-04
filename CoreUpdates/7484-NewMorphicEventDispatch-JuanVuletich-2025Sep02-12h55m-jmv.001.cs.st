'From Cuis7.5 [latest update: #7481] on 2 September 2025 at 2:13:47 pm'!

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 13:00:49'!
dispatchIn: aWorldMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the event position.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	self subclassResponsibility! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 14:12:23'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| handlerMorph dropped |
	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
			wasHandled
				ifFalse: [ handlerMorph invalidDrop: self ]
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 14:12:30'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it."

	| handlerMorph |
	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			handlerMorph allowsFilesDrop ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!UserInputEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 14:12:37'!
dispatchIn: aWorldMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the event position.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			self sendEventTo: handlerMorph.
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 14:12:42'!
dispatchIn: aWorldMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the event position.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."
	
	| handlerMorph grabTheMorph |
	
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchIn: aWorldMorph ].

	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			"aMorph is in the top-most unlocked, visible morph in the chain."
			(self mouseButton3Pressed or: [handlerMorph handlesMouseDown: self])
				ifTrue: [ self sendEventTo: handlerMorph ]
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


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 14:12:46'!
dispatchIn: aWorldMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the event position.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			(handlerMorph handlesMouseScroll: self) ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 11:15:43'!
dispatchIn: aWorldMorph
	"Host window events do not have a position and are only dispatched to the World"

	self sendEventTo: aWorldMorph.! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 11:28:15'!
dispatchEvent: aMorphicEvent
	"This is the central entry for dispatching events in morphic. Given some event, find the right receiver and let him handle it."

	(self rejectsEventFully: aMorphicEvent)
		ifFalse: [ aMorphicEvent dispatchIn: self ].! !


!MorphicEvent reorganize!
('accessing' dispatchWith: eventPosition hand timeStamp wasHandled wasHandled:)
('dispatching' dispatchIn: sendEventTo: startDispatchFrom:)
('testing' is: isDraggingEvent isDropEvent isKeyboard isKeystroke isMouse isMouseMove isMouseOver isWindowEvent)
('private' setTimeStamp:)
!


!UserInputEvent reorganize!
('accessing' buttons eventPosition wasHandled wasHandled:)
('dispatching' dispatchIn:)
('modifier state' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
('printing' printOn:)
!

