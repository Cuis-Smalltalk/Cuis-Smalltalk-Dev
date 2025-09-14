'From Cuis7.5 [latest update: #7566] on 13 September 2025 at 6:09:25 pm'!

!HierarchicalListMorph methodsFor: 'event handling testing' stamp: 'jmv 9/13/2025 17:32:05'!
clickToFocusConsumesEvent
	"We don't react to a focus setting click if we have a selection, so we don't change selection just for focusing."
	
	^selectedMorph notNil! !


!PluggableListMorph methodsFor: 'event handling testing' stamp: 'jmv 9/13/2025 17:32:09'!
clickToFocusConsumesEvent
	"We don't react to a focus setting click if we have a selection, so we don't change selection just for focusing."
	
	^self getCurrentSelectionIndex > 0! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 9/13/2025 17:32:14'!
clickToFocusConsumesEvent
	"We don't react to a focus setting click if we have a selection, so we don't change selection just for focusing."
	
	^editor hasSelection! !


!Morph methodsFor: 'event handling testing' stamp: 'jmv 9/13/2025 17:32:40'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	Some subclasses do not. See inheritance."
	
	^false! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/13/2025 17:53:33'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
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

	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(Preferences at: #focusFollowsMouse)
		ifTrue: [" MouseEnterToFocus. Clicks have no relation to keyboard focus. Just deliver the event."
			self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ "ClickToFocus. Still two different cases. Some morphs 'ignore' focus setting MouseDown events."
			self clickToFocusConsumesEvent
				ifTrue: [
					"#clickToFocusConsumesEvent means: Don't deliver a MouseDown if the
					soon-to-be MouseUp will set keyboard focus on us (see #processMouseUp:localPosition:).
					So, deliver it if we already have keyboard focus."
					self hasKeyboardFocus ifTrue: [
						self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]]
				ifFalse: [
					"(#clickToFocusConsumesEvent not) means: A keyboard focus setting MouseDown is also a
					regular MouseDown (after setting keyboard focus)"
					self hasKeyboardFocus ifFalse: [
						aMouseButtonEvent hand newKeyboardFocus: self ].
					self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/13/2025 17:55:50'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [ "ClickToFocus, and focus setting MouseDown was ignored. (see #processMouseDown:localPosition:)"
			self hasKeyboardFocus ifTrue: [
				"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
				self mouseMove: aMouseMoveEvent localPosition: localEventPosition  ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseMove."
			self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].

	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/13/2025 18:09:11'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"If we click to focus, don't process this event as a regular click.
	See also #processMouseDown:localPosition:"
	"(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifTrue: [ aMouseButtonEvent hand newKeyboardFocus: self ]
		ifFalse: [ self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]."
	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [
			self hasKeyboardFocus
				ifFalse: [
					"Haven't set keyboard focus but must. Do it.
					MouseDown and any MouseMove were not delivered. Do not deliver #mouseButton1Up:localPosition:"
					aMouseButtonEvent hand newKeyboardFocus: self ]
				ifTrue: [
					"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
					self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseUp."
			self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ].

	self stopStepping: #processMouseStillDown.! !

!methodRemoval: ResizeMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
ResizeMorph removeSelector: #clickToFocusConsumesEvent!
!methodRemoval: LayoutAdjustingMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
LayoutAdjustingMorph removeSelector: #clickToFocusConsumesEvent!
!methodRemoval: PasteUpMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
PasteUpMorph removeSelector: #clickToFocusConsumesEvent!
!methodRemoval: PluggableButtonMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
PluggableButtonMorph removeSelector: #clickToFocusConsumesEvent!
!methodRemoval: MenuItemMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
MenuItemMorph removeSelector: #clickToFocusConsumesEvent!
!methodRemoval: HaloHandleMorph #clickToFocusConsumesEvent stamp: 'jmv 9/13/2025 17:29:57'!
HaloHandleMorph removeSelector: #clickToFocusConsumesEvent!
