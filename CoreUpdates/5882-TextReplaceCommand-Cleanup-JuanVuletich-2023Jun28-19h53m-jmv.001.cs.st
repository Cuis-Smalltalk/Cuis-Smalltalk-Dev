'From Cuis 6.0 [latest update: #5880] on 28 June 2023 at 7:55:20 pm'!
!classDefinition: #TextReplaceCommand category: #'System-Text'!
TextCommand subclass: #TextReplaceCommand
	instanceVariableNames: 'old new position newSizeShouted '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/28/2023 19:53:35'!
convertAndStyleAllWith: anSHTextStyler
	"Convert and style all contents as a single method or script.
	Do it in a background process if too big."

	anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.
	self basicActualContents: anSHTextStyler convertedAndStyledText.! !


!TextReplaceCommand methodsFor: 'actions' stamp: 'jmv 9/21/2011 10:38'!
undoOn: aTextModel
	"Undo the command, bringing the text model to the state it had prior to doing it.
	Answer a new position for the text cursor"

	aTextModel basicReplaceFrom: position to: position + new size-1 with: old.
	^position + old size! !

!methodRemoval: TextReplaceCommand #newSizeShouted: stamp: 'jmv 6/28/2023 19:54:01'!
TextReplaceCommand removeSelector: #newSizeShouted:!
!methodRemoval: TextCommand #newSizeShouted: stamp: 'jmv 6/28/2023 19:54:02'!
TextCommand removeSelector: #newSizeShouted:!
!classDefinition: #TextReplaceCommand category: #'System-Text'!
TextCommand subclass: #TextReplaceCommand
	instanceVariableNames: 'old new position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
