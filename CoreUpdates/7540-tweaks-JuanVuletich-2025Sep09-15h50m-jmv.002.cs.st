'From Cuis7.5 [latest update: #7538] on 9 September 2025 at 3:55:46 pm'!

!Rectangle methodsFor: 'transforming' stamp: 'jmv 9/9/2025 15:51:19'!
newRect2From: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect aHand delay |
	delay := Delay forMilliseconds: 10.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[Sensor isAnyButtonPressed] whileTrue: 
		[delay wait.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		releaseMouseFocus;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !

!Rectangle methodsFor: 'transforming' stamp: 'jmv 9/9/2025 15:51:23'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		releaseMouseFocus;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 9/9/2025 15:51:39'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

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
			popUpOwner deleteIfPopUp: evt ]].! !

