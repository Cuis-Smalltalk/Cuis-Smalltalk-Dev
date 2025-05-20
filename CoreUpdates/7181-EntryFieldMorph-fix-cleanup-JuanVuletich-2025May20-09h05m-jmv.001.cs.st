'From Cuis7.3 [latest update: #7180] on 20 May 2025 at 9:06:46 am'!

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 5/20/2025 09:00:36'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self editor clickAndHalf.
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 5/20/2025 09:02:17'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		contents := self editor string.
		self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
		self redrawNeeded.
		^self ].

	self pauseBlinking.

	"Return - check for special action"
	aKeyboardEvent isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			^action value ]].

	"Esc - check for special action"
	aKeyboardEvent isEsc ifTrue: [			
		self escAction ifNotNil: [ :action |
			^action value ]].

	self editor processKeystrokeEvent: aKeyboardEvent.
	contents := self editor string.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 5/20/2025 09:01:00'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent hand newKeyboardFocus: self.

	self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition).
	self redrawNeeded.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 5/20/2025 09:01:09'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: localEventPosition).
	self redrawNeeded.! !

!methodRemoval: EntryFieldMorph #updateFromContents stamp: 'jmv 5/20/2025 09:05:54'!
EntryFieldMorph removeSelector: #updateFromContents!
!methodRemoval: EntryFieldMorph #handleInteraction: stamp: 'jmv 5/20/2025 09:05:54'!
EntryFieldMorph removeSelector: #handleInteraction:!
