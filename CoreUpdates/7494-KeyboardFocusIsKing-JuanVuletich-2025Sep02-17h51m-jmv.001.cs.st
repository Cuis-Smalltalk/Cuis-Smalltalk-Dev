'From Cuis7.5 [latest update: #7490] on 2 September 2025 at 5:59:00 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 17:58:33'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement := self keyboardFocus ifNil: [ self world ].
	aKeyboardEvent sendEventTo: focusedElement.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!methodRemoval: MenuMorph #handleKeyboardFocusEvent: stamp: 'jmv 9/2/2025 17:57:34'!
MenuMorph removeSelector: #handleKeyboardFocusEvent:!
!methodRemoval: Morph #handleKeyboardFocusEvent: stamp: 'jmv 9/2/2025 17:58:38'!
Morph removeSelector: #handleKeyboardFocusEvent:!
