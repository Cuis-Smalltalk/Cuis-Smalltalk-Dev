'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 7:46:23 am'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 27/Nov/2025 17:27:44'!
convertAndStyleIfNeededWith: anSHTextStyler

	self aboutToStyleWith: anSHTextStyler.
	self styleByParagraphs
		ifTrue: [ self convertAndStyleIByParagraphsWith: anSHTextStyler ]
		ifFalse: [ self convertAndStyleAllWith: anSHTextStyler ].! !


!SHTextStyler methodsFor: 'accessing' stamp: 'jmv 27/Nov/2025 17:06:13'!
textModel: aTextModel
	self terminateBackgroundStylingProcess.
	textModel := aTextModel! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 27/Nov/2025 17:06:28'!
workspaceContents: text

	self terminateBackgroundStylingProcess.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].! !

