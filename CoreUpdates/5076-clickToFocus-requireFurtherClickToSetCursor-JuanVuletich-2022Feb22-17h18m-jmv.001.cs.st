'From Cuis 6.0 [latest update: #5075] on 22 February 2022 at 5:19:27 pm'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 2/22/2022 17:18:57'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	"If we don't focus, do nothing. Focus will be got at button up."
	self hasKeyboardFocus ifFalse: [
		^self ].

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

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 2/22/2022 17:19:14'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| mouseButton1DownHandled |

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^ aMouseButtonEvent hand newKeyboardFocus: self ].

	self pauseBlinking.

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

