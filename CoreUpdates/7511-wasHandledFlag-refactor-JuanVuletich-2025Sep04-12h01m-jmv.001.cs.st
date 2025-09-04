'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 12:13:40 pm'!

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:04:33'!
dispatchIn: focusedMorph
	"Dispatch me.
	The event will be passed to focusedMorph.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner.
	Note: event position is usually irrelevant for KeyboardEvents."

	| handlerMorph |
	handlerMorph := focusedMorph.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		(handlerMorph rejectsEvent: self) not ifTrue: [
			
			self sendEventTo: handlerMorph.
			
		].
		handlerMorph := handlerMorph owner.
	].! !

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:12:05'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."
"
Revisar senders de
#definesKeyboardShortcuts.
"
	keyEventType == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			self wasHandled: true.
			^self ].
		self isCloseWindowShortcut ifTrue: [
			self closeCurrentWindowOf: aMorph.
			self wasHandled: true.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].! !

