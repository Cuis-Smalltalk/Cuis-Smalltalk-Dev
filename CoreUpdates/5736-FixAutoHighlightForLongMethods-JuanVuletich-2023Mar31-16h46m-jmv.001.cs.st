'From Cuis 6.0 [latest update: #5735] on 31 March 2023 at 4:52:37 pm'!

!TextModel methodsFor: 'misc' stamp: 'jmv 3/31/2023 16:50:55'!
autoSelectString
	^nil! !


!TextProvider methodsFor: 'user interface support' stamp: 'jmv 3/31/2023 16:51:16'!
autoSelectString
	^nil! !


!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 3/31/2023 16:51:47'!
stylerStyled
	model autoSelectString notNil ifTrue: [
		self highlightMessage
			ifFalse: [ self highlightString ]].
	self textComposition composeAll.
	needsFit := true.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

