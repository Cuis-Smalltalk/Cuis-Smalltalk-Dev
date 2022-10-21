'From Cuis 6.0 [latest update: #5499] on 20 October 2022 at 6:49:10 pm'!

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 10/20/2022 18:46:51'!
stylerStyled

	self textComposition composeAll.
	needsFit _ true.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 10/20/2022 18:47:37'!
formatWorkspace: text
	"Do first stage of styling.
	Afterwards, call #styleWorkspaceFrom:to: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	
	parser ifNil: [ parser _ SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil! !

!methodRemoval: SHTextStylerST80 #privateFormatAndConvertWorkspace stamp: 'jmv 10/20/2022 18:48:51'!
SHTextStylerST80 removeSelector: #privateFormatAndConvertWorkspace!
!methodRemoval: SHTextStyler #privateFormatAndConvertWorkspace stamp: 'jmv 10/20/2022 18:48:50'!
SHTextStyler removeSelector: #privateFormatAndConvertWorkspace!
