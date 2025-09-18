'From Cuis7.5 [latest update: #7584] on 18 September 2025 at 12:22:13 pm'!

!Morph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:44:35'!
keyboardUnfocused
	"Just lost keyboard focus via a keyboard event.
	See also #keyboardFocused"! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/18/2025 12:15:41'!
activeSubMenu
	^activeSubMenu ! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/18/2025 11:44:42'!
keyboardUnfocused
	"Just lost keyboard focus via a keyboard event.
	See also #keyboardFocused"
	
	self deleteIfPopUp: nil.! !

!MenuMorph methodsFor: 'testing' stamp: 'jmv 9/18/2025 11:53:03'!
is: aSymbol
	^ aSymbol == #MenuMorph or: [ super is: aSymbol ]! !


!Morph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:46:06'!
keyboardFocused
	"Just given focus via a keyboard event (not a MouseMove or MouseClick).
	See also #keyboardUnfocused"! !


!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/18/2025 11:30:32'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	self redrawNeeded! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/18/2025 12:20:10'!
startMouseButtonDispatch: aMouseEvent

	| closedSomeMenu |
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.
	
	closedSomeMenu := false.
	aMouseEvent mouseButton1Pressed ifTrue: [
		owner submorphsDo: [ :m |
			((m is: #MenuMorph) and: [m activeSubMenu isNil]) ifTrue: [
				(m includesPixel: aMouseEvent eventPosition) ifFalse: [
					m deleteIfPopUp: aMouseEvent.
					closedSomeMenu := true ]]].
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

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:45:37'!
keyboardFocusNext

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusPrevious"
	true ifTrue: [
		^self keyboardFocusNextWithinWindow ].

	m := self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m.
		keyboardFocus keyboardUnfocused.
		m keyboardFocused ].! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:45:40'!
keyboardFocusNextWithinWindow
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus previousMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			keyboardFocus keyboardUnfocused.
			aMorph keyboardFocused.
			^true ].
	^false! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:45:44'!
keyboardFocusPrevious

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusNext"
	true ifTrue: [
		^self keyboardFocusPreviousWithinWindow ].

	m := self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m.
		keyboardFocus keyboardUnfocused.
		m keyboardFocused ].! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/18/2025 11:45:47'!
keyboardFocusPreviousWithinWindow
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus nextMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			keyboardFocus keyboardUnfocused.
			aMorph keyboardFocused.
			^true ].
	^false! !

