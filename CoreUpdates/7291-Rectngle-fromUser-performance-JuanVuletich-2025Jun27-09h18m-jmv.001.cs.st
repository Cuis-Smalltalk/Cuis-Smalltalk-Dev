'From Cuis7.5 [latest update: #7289] on 27 June 2025 at 9:24:56 am'!

!Rectangle methodsFor: 'transforming' stamp: 'jmv 6/27/2025 09:23:56'!
newRect2From: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect aHand delay offDisplay |
	delay := Delay forMilliseconds: 10.
	rect := self.
	offDisplay := Display copyToDisableImmediateUpdates.
	offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	offDisplay forceToScreen: rect.
	[Sensor isAnyButtonPressed] whileTrue: 
		[delay wait.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			offDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			offDisplay forceToScreen: (rect merge: newRect).
			rect := newRect]].
	offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	offDisplay forceToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !

!Rectangle methodsFor: 'transforming' stamp: 'jmv 6/27/2025 09:23:09'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay offDisplay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	offDisplay := Display copyToDisableImmediateUpdates.
	offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	offDisplay forceToScreen: rect.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			offDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			offDisplay forceToScreen: (rect merge: newRect).
			rect := newRect]].
	offDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	offDisplay forceToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !

