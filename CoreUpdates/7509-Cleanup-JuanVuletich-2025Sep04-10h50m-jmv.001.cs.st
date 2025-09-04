'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 11:36:37 am'!

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:31'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"
	"An event of an unknown type was sent. What shall we do?!!"

	Smalltalk beep. 
	self printString displayAt: `0@0`.
	self wasHandled: true! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:35'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startDropEventDispatch: self! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:38'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startDropFilesEventDispatch: self! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:33:54'!
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

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:41'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startKeyboardDispatch: self! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:34:04'!
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

!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:44'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startMouseDispatch: self! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:35:47'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startWindowEventDispatch: self! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 11:04:15'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
"
Revisar senders de
#definesKeyboardShortcuts.
"
	keyEventType == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			^self ].
		self isCloseWindowShortcut ifTrue: [
			self closeCurrentWindowOf: aMorph.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].
	super sendEventTo: aMorph.! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 11:35:59'!
startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent startDispatchFromHand: self.! !

!methodRemoval: WindowEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:07'!
WindowEvent removeSelector: #startDispatchFrom:!
!methodRemoval: MouseEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:08'!
MouseEvent removeSelector: #startDispatchFrom:!
!methodRemoval: KeyboardEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:09'!
KeyboardEvent removeSelector: #startDispatchFrom:!
!methodRemoval: DropFilesEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:10'!
DropFilesEvent removeSelector: #startDispatchFrom:!
!methodRemoval: DropEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:10'!
DropEvent removeSelector: #startDispatchFrom:!
!methodRemoval: MorphicEvent #startDispatchFrom: stamp: 'jmv 9/4/2025 11:36:20'!
MorphicEvent removeSelector: #startDispatchFrom:!
