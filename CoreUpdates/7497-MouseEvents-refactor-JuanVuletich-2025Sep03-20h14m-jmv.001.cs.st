'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 8:30:53 pm'!

!MouseButtonEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:06:13'!
isMouseDown
	^ type == #mouseDown! !

!MouseButtonEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:07:19'!
isMouseUp
	^ type == #mouseUp! !


!MouseMoveEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:05:10'!
isMouseMove
	^true! !


!MouseOverEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:03:37'!
isMouseOver
	^true! !


!MouseScrollEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:08:24'!
isMouseScroll
	^ true! !


!MouseEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:06:18'!
isMouseDown
	^ false! !

!MouseEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:03:41'!
isMouseOver
	^false! !

!MouseEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:08:20'!
isMouseScroll
	^ false! !

!MouseEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:07:28'!
isMouseUp
	^ false! !


!Morph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:27:11'!
keyDown: aKeyboardEvent
	"Handle a key down event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler | handler value: aKeyboardEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:28:00'!
handleMouseFocusEvent: aMouseEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	aMouseEvent sendEventTo: self.! !


!HaloHandleMorph methodsFor: 'act' stamp: 'jmv 9/3/2025 20:26:17'!
send: selector withEvent: aMouseEvent
	| arity |
	owner ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	arity := selector numArgs.
	arity = 0 ifTrue: [ ^ owner perform: selector ].
	arity = 1 ifTrue: [
		^ owner
			perform: selector
			with: aMouseEvent ].
	arity = 2 ifTrue: [
		^ owner
			perform: selector
			with: aMouseEvent
			with: self ].
	self error: 'Event handling selectors must be Symbols and take 0-2 arguments'! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 20:25:33'!
handleMouseFocusEvent: aMouseEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	"#dispatchIn: and not #sendEventTo: so it is handled by the appropriate menu item."
	aMouseEvent dispatchIn: self.
	
	"Handle clicks outside us, so we are closed. This will only be done if the event wasn't handled above,
	meaning that it is a click outside of us."
	aMouseEvent isMouseButton ifTrue: [
		aMouseEvent sendEventTo: self.
		^self ].
	
	"On MouseMove maybe activate submenu, or owner."
	aMouseEvent isMouseMove ifTrue: [
		"On #mouseMove we might transfer control to *either* the currently active submenu
		or the owner menu, if any.
		Since the active sub menu is always displayed upfront check it first."
		selectedItem ifNotNil: [
			(selectedItem activateSubmenu: aMouseEvent) ifTrue: [
				^self]].
		"Note: The following does not traverse upwards but it's the best I can do for now"
		popUpOwner ifNotNil: [
			(popUpOwner activateOwnerMenu: aMouseEvent) ifTrue: [
				^self]]].! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 9/3/2025 20:25:05'!
windowEvent: aWindowEvent

	aWindowEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 16:05:22'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
			ifNil: [ aMouseEvent dispatchIn: owner ].
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

	aMouseEvent isMouseMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			aMouseEvent dispatchIn: owner
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setPosition: aMouseEvent eventPosition
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
						ifNil: [ aMouseEvent dispatchIn: owner ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent.! !

!methodRemoval: MouseMoveEvent #isMove stamp: 'jmv 9/3/2025 20:19:39'!
MouseMoveEvent removeSelector: #isMove!
!methodRemoval: MouseEvent #isMouseMove stamp: 'jmv 9/3/2025 20:30:06'!
MouseEvent removeSelector: #isMouseMove!
!methodRemoval: MouseEvent #isMove stamp: 'jmv 9/3/2025 20:19:39'!
MouseEvent removeSelector: #isMove!
!methodRemoval: MorphicEvent #isMouseOver stamp: 'jmv 9/3/2025 20:22:46'!
MorphicEvent removeSelector: #isMouseOver!

!MouseButtonEvent reorganize!
('accessing' mouseButton1Changed mouseButton2Changed mouseButton3Changed)
('dispatching' dispatchIn: sendEventTo:)
('testing' isMouseButton isMouseDown isMouseUp)
('private' mouseDownPosition:which:buttons:hand:stamp: mouseUpPosition:which:buttons:hand:stamp:)
!


!MouseOverEvent reorganize!
('dispatching' sendEventTo:)
('testing' isMouseOver)
('private' setPosition:buttons:hand:stamp:)
!


!MouseScrollEvent reorganize!
('accessing' direction)
('comparing' = hash)
('dispatching' dispatchIn: sendEventTo:)
('testing' isMouseScroll)
('private' setPosition:direction:buttons:hand:stamp:)
!

