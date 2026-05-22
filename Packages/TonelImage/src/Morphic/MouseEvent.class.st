Class {
	#name : 'MouseEvent',
	#superclass : 'UserInputEvent',
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'comparing' }
MouseEvent >> = aMorphicEvent [

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true
]

{ #category : 'button state' }
MouseEvent >> anyButtonPressed [
	"Answer true if any mouse button is being pressed."

	^ buttons anyMask: InputSensor anyMouseButton
]

{ #category : 'converting' }
MouseEvent >> asMouseEnter [

	^ MouseEnterEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"
]

{ #category : 'converting' }
MouseEvent >> asMouseLeave [

	^ MouseLeaveEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"
]

{ #category : 'converting' }
MouseEvent >> asMouseMove [

	^ MouseMoveEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"
]

{ #category : 'converting' }
MouseEvent >> asMouseOver [
	"Convert the receiver into a mouse over event"

	^ MouseOverEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"
]

{ #category : 'dispatching' }
MouseEvent >> dispatchIn: aWorldMorphOrFocusedMorph [
	"Dispatch me.
	The event will be passed to the front-most visible submorph that contains the event position.
	Search usually starts with the WorldMorph, but it could be another morph if it is the hand's #mouseFocus.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		handlerMorph := handlerMorph owner ].
]

{ #category : 'comparing' }
MouseEvent >> hash [
	^ self class name hash bitXor: (position hash bitXor: buttons hash)
]

{ #category : 'testing' }
MouseEvent >> isMouse [
	^true
]

{ #category : 'testing' }
MouseEvent >> isMouseButton [
	^ false
]

{ #category : 'testing' }
MouseEvent >> isMouseDown [
	^ false
]

{ #category : 'testing' }
MouseEvent >> isMouseOver [
	^false
]

{ #category : 'testing' }
MouseEvent >> isMouseScroll [
	^ false
]

{ #category : 'testing' }
MouseEvent >> isMouseUp [
	^ false
]

{ #category : 'button state' }
MouseEvent >> mouseButton1Pressed [
	"Answer true if the mouseButton1 is being pressed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton2Pressed and mouseButton3Pressed.
	See also #mouseButton1Changed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton1
]

{ #category : 'button state' }
MouseEvent >> mouseButton2Pressed [
	"Answer true if the mouseButton2 is being pressed.
	Reported by the VM for right mouse button or option+click on the Mac, ctrl-click on Windows, or ctrl-click or alt-click on Linux.
	It is also emulated here with ctrl-click on any platform."

	(self turnMouseButton1Into2 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	self turnMouseButton2Into3 ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton2
]

{ #category : 'button state' }
MouseEvent >> mouseButton3Pressed [
	"Answer true if the mouseButton3 is being pressed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or win/meta+click on Windows and Linux.
	It is also emulated here with on any platform with:	
		shift - ctrl - click
		ctrl - rightClick"

	(self turnMouseButton1Into3 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	(self turnMouseButton2Into3 and: [ buttons anyMask: InputSensor mouseButton2 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton3
]

{ #category : 'dispatching' }
MouseEvent >> startDispatchFromHand: aHand [
	"double dispatch the event dispatch
	Generic MouseEvent behavior. See inheritance."

	aHand startMouseDispatch: self
]

{ #category : 'button state' }
MouseEvent >> turnMouseButton1Into2 [
	"Answer true if modifier keys are such that button 1 should be considered as button 2.
	ctrl - click -> right click
	"

	(self controlKeyPressed and: [self shiftPressed not]) ifTrue: [ ^ true ].
	^ false
]

{ #category : 'button state' }
MouseEvent >> turnMouseButton1Into3 [
	"Answer true if modifier keys are such that button 1 should be considered as button 3.
	ctrl - shift - click -> center click
	alt -> click -> center click (effective only on Windows,
						the vm on Mac already reports center click, and on Linux right click)
	"

	(self controlKeyPressed and: [self shiftPressed]) ifTrue: [ ^ true ].
	self commandAltKeyPressed ifTrue: [ ^ true ].
	^ false
]

{ #category : 'button state' }
MouseEvent >> turnMouseButton2Into3 [
	"Answer true if modifier keys are such that button 2 should be considered as button 3.
	ctrl - click right -> center click
	"

	self controlKeyPressed ifTrue: [ ^ true ].
	^ false
]
