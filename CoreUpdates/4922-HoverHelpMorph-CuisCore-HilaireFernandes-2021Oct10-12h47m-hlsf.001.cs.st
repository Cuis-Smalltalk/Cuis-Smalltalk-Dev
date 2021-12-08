'From Cuis 5.0 [latest update: #4895] on 10 October 2021 at 12:58:06 pm'!

!HoverHelpMorph methodsFor: 'accessing' stamp: 'hlsf 10/10/2021 12:55:12'!
contents: aString
	| width |
	contents _ aString.
	width _ (contents includes: Character lf)
		ifTrue: [9999999] ifFalse: [300].
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 	width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8! !

