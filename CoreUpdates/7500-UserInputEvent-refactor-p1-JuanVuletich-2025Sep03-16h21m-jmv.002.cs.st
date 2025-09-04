'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 4:23:22 pm'!
!classDefinition: #KeyboardEvent category: #'Morphic-Events'!
UserInputEvent subclass: #KeyboardEvent
	instanceVariableNames: 'keyValue keyEventType '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
!classDefinition: #MouseButtonEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseButtonEvent
	instanceVariableNames: 'whichButton buttonEventType '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!KeyboardEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:22:54'!
setType: aSymbol buttons: anInteger position: pos keyValue: aValue hand: aHand stamp: stamp
	type := aSymbol.
	keyEventType := aSymbol.
	buttons := anInteger.
	position := pos.
	keyValue := aValue.
	source := aHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:23:10'!
mouseDownPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	type := #mouseDown.
	buttonEventType := #mouseDown.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !

!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:23:16'!
mouseUpPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	type := #mouseUp.
	buttonEventType := #mouseUp.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !

!classDefinition: #KeyboardEvent category: #'Morphic-Events'!
UserInputEvent subclass: #KeyboardEvent
	instanceVariableNames: 'keyEventType keyValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
!classDefinition: #MouseButtonEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseButtonEvent
	instanceVariableNames: 'buttonEventType whichButton'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
