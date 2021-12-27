'From Cuis 5.0 [latest update: #5004] on 27 December 2021 at 2:17:56 pm'!

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 12/27/2021 14:12:47'!
stylerStyled

	model allowStylingWithEmphasis ifTrue: [
		self textComposition composeAll.
		needsFit _ true ].
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

