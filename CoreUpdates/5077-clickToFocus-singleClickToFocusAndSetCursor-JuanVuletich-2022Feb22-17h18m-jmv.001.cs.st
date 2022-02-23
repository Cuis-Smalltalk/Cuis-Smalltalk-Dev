'From Cuis 6.0 [latest update: #5075] on 22 February 2022 at 5:21:29 pm'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 2/22/2022 17:20:38'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	"If we don't focus, Get focus, and continue (i.e. position the cursor)"
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'events' stamp: 'HAW 11/30/2021 14:54:59'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| mouseButton1DownHandled |

	self pauseBlinking.

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

