'From Cuis 5.0 [latest update: #4965] on 4 November 2021 at 5:14:49 pm'!
!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 11/4/2021 17:11:01'!
contents: aString wrappedTo: wordWrapWidthOrNil
	"If wordWrapWidthOrNil is nil, don't do wordwrap, and make the morph as wide as needed"

	| width |
	contents _ aString.
	width _ wordWrapWidthOrNil ifNil: [9999999].
wordWrapWidthOrNil print.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 	width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.! !

!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 11/4/2021 17:07:51'!
contentsWrapped: aString
	
	self contents: aString wrappedTo: FontFamily defaultLineSpacing * 13! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2021 17:12:39'!
contents: aString wrappedTo: wordWrapWidthOrNil
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.'
		wrappedTo: 150) openInHand
	"

	^self new contents: aString wrappedTo: wordWrapWidthOrNil! !

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2021 17:11:35'!
contentsWrapped: aString
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contentsWrapped: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.') openInHand
	"

	^self new contentsWrapped: aString! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 11/4/2021 17:13:39'!
contents: aString

	self contents: aString wrappedTo: nil! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2021 17:08:47'!
contents: aString
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph') openInHand
	"

	^self new contents: aString! !
