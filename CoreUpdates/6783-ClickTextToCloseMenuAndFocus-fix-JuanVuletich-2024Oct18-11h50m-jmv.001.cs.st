'From Cuis7.1 [latest update: #6782] on 18 October 2024 at 11:52:58 am'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 10/18/2024 11:52:05'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| mouseButton1DownHandled |

	"If we don't focus, Get focus, and continue (i.e. position the cursor)
	(It is possible that we didn't get the #mouseButton1Down:localPosition:, maybe because
	we were in a menu, and clicking on us closed the it)."
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].
	self pauseBlinking.

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

