'From Cuis7.5 [latest update: #7633] on 30 September 2025 at 5:58:03 pm'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/30/2025 17:56:01'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu.
	Answer wether some action was actually taken."

	| h |
	h := evt ifNotNil: [ evt hand ].
	h ifNil: [
		self world ifNotNil: [ :w | h := h activeHand ]].
	h ifNil: [ h := self activeHand ].
	h ifNotNil: [
		h keyboardFocus == self ifTrue: [
			h newKeyboardFocus: prevKbdFocus ].
		h mouseFocus == self ifTrue: [
			"h newMouseFocus: prevMouseFocus."
			h releaseMouseFocus ]].
	stayUp ifFalse: [
		self delete.
		popUpOwner ifNotNil: [
			popUpOwner isSelected: false.
			popUpOwner deleteIfPopUp: evt ].
		^true ].
	^false! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 9/30/2025 17:56:18'!
deleteIfPopUp: evt
	"Recurse up for nested pop ups"
	owner ifNotNil: [^owner deleteIfPopUp: evt].
	^false! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/30/2025 17:56:44'!
startMouseButtonDispatch: aMouseEvent

	| closedSomeMenu |
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.
	
	closedSomeMenu := false.
	aMouseEvent mouseButton1Pressed ifTrue: [
		owner submorphsDo: [ :m |
			((m is: #MenuMorph) and: [m activeSubMenu isNil]) ifTrue: [
				(m includesPixel: aMouseEvent eventPosition) ifFalse: [
					closedSomeMenu := m deleteIfPopUp: aMouseEvent ]]].
		closedSomeMenu ifTrue: [
			"Used this click to close some menu(s). Do not further process this event."
			aMouseEvent wasHandled: true.
			^self ]].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			aMouseEvent wasHandled: true.
			^self ]].

	"Issue a synthetic move event if we're not at the position of the event"
	aMouseEvent eventPosition = self morphPosition ifFalse: [
		"Issue a mouse move event to make the receiver appear at the given position"
		(MouseMoveEvent new
			setPosition: aMouseEvent eventPosition
			buttons: aMouseEvent buttons
			hand: self
			stamp: aMouseEvent timeStamp) startDispatchFromHand: self ].

	"Drop submorphs on button events"
	self hasSubmorphs
		ifTrue: [
			"Not if we are grabbing them"
			mouseClickState ifNil: [
				"Want to drop on mouseUp, NOT mouseDown"
				aMouseEvent isMouseUp ifTrue: [
					self dropMorphs: aMouseEvent ]]]
		ifFalse: [
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]].

	self hasSubmorphs ifFalse: [
		self dispatchMouseOverEvent ].! !

