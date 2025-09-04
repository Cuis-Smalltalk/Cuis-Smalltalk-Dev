'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 4:25:12 pm'!

!KeyboardEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:24:20'!
setType: aSymbol buttons: anInteger position: pos keyValue: aValue hand: aHand stamp: stamp
	
	keyEventType := aSymbol.
	buttons := anInteger.
	position := pos.
	keyValue := aValue.
	source := aHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:24:24'!
mouseDownPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	
	buttonEventType := #mouseDown.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !

!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:24:28'!
mouseUpPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	
	buttonEventType := #mouseUp.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !

