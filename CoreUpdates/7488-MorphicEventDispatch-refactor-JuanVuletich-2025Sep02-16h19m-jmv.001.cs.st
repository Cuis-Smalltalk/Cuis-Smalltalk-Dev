'From Cuis7.5 [latest update: #7485] on 2 September 2025 at 4:22:56 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 15:38:29'!
handleKeyboardFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	aMorphicEvent sendEventTo: self.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 15:38:33'!
handleMouseFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	aMorphicEvent sendEventTo: self.! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:19:20'!
handleKeyboardFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	self dispatchEvent: aMorphicEvent.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sendEventTo: self ].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sendEventTo: self ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:19:09'!
handleMouseFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	self dispatchEvent: aMorphicEvent.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sendEventTo: self ].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sendEventTo: self ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:20:44'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement := self keyboardFocus ifNil: [ self world ].
	focusedElement handleKeyboardFocusEvent: aKeyboardEvent.
	
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:20:35'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent ].
		^self].

	"any mouse event but mouseOver"
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [
						"Want to drop on mouseUp, NOT mouseDown"
						aMouseEvent isMouseUp ifTrue: [
							self dropMorphs: aMouseEvent ]
					]
				] ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!methodRemoval: MenuMorph #handleFocusEvent: stamp: 'jmv 9/2/2025 16:20:55'!
MenuMorph removeSelector: #handleFocusEvent:!
!methodRemoval: Morph #handleFocusEvent: stamp: 'jmv 9/2/2025 16:20:56'!
Morph removeSelector: #handleFocusEvent:!
