'From Cuis7.3 [latest update: #6911] on 12 December 2024 at 3:49:02 pm'!

!Rectangle methodsFor: 'transforming' stamp: 'jmv 12/12/2024 15:46:55'!
newRect2From: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect aHand delay |
	delay := Delay forMilliseconds: 10.
	rect := self.
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	[Sensor isAnyButtonPressed] whileTrue: 
		[delay wait.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			Display border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			rect := newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 3/25/2024 11:43:34'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			Display border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			rect := newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 12/12/2024 15:47:53'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preferences at: #cheapWindowReframe) or: [millisecondSinceLast > 100]]) ifTrue: [
			owner displayBounds newRect2From: [ :f |
				self stopStepping.
				hand := nil.
				Cursor defaultCursor activateCursor.
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner extentInWorld ]].! !

