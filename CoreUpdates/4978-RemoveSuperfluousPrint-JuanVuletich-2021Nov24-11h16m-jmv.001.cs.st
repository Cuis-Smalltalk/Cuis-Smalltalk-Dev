'From Cuis 5.0 [latest update: #4976] on 24 November 2021 at 11:16:57 am'!

!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 11/24/2021 11:16:33'!
contents: aString wrappedTo: wordWrapWidthOrNil
	"If wordWrapWidthOrNil is nil, don't do wordwrap, and make the morph as wide as needed"

	| width |
	contents _ aString.
	width _ wordWrapWidthOrNil ifNil: [9999999].
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 	width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.! !

