'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 1:12:45 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:18'!
processKeyDown: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self handlesKeyboard ifFalse: [^self].
	self keyDown: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:27'!
processKeyUp: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self handlesKeyboard ifFalse: [^self].
	self keyUp: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:34'!
processKeystroke: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self handlesKeyboard ifTrue: [
		self keyStroke: aKeyboardEvent.
		self 
			valueOfProperty: #'keyStroke:'
			ifPresentDo: [ :handler |
				handler value: aKeyboardEvent.
				aKeyboardEvent wasHandled: true ]].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:44'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:50'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	aMouseEnterEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:55'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	aMouseLeaveEvent hand removePendingBalloonFor: self.
	aMouseLeaveEvent isDraggingEvent ifTrue: [
		^self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:10:59'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:11:04'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseOverEvent) not and: [self fullIncludesPixel: aMouseOverEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseOverEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:11:09'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopStepping: #processMouseStillDown! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:12:19'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	
	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue:[
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:12:24'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	
	aMouseLeaveEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ]! !

