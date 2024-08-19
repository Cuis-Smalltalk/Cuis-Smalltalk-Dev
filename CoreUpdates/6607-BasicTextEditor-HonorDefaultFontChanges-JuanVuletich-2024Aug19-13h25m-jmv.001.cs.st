'From Cuis7.1 [latest update: #6606] on 19 August 2024 at 1:26:05 pm'!

!TextEditor methodsFor: 'accessing' stamp: 'jmv 8/19/2024 13:21:30'!
defaultFont
	^defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].! !

!TextEditor methodsFor: 'accessing' stamp: 'jmv 8/19/2024 13:21:38'!
lastFont
	"Answer the Font for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents fontIfApplying: emphasisHere default: self defaultFont ]
		ifFalse: [ model actualContents fontAt: model textSize +1 default: self defaultFont ]! !

!TextEditor methodsFor: 'attributes' stamp: 'jmv 8/19/2024 13:21:44'!
offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| currentFont attr startIndex |
	startIndex := self startIndex.
	currentFont := model actualContents fontAt: startIndex default: self defaultFont.
	(FontFamily promptUser: currentFont familyName) ifNotNil: [ :fontFamily |
		attr := TextFontFamily familyName: fontFamily familyName.
		self applyAttribute: attr.
		morph updateFromTextComposition ].! !

!TextEditor methodsFor: 'attributes' stamp: 'jmv 8/19/2024 13:21:48'!
offerFontSizeMenu
	| startIndex currentFont fontFamily attr |
	startIndex := self startIndex.
	currentFont := model actualContents fontAt: startIndex default: self defaultFont.
	fontFamily := currentFont family.
	(FontFamily
		promptUserForSize: currentFont pointSize
		fontFamily: fontFamily) ifNotNil: [ :ptSize |
			attr := TextFontSize pointSize: ptSize.
			self applyAttribute: attr.
			morph updateFromTextComposition ].! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 8/19/2024 13:22:06'!
initialize
	emphasisHere := #().
	initialSelectionStart := nil.
	initialSelectionStop := nil.
	doWordSelection := false.
	doParagraphSelection := false.
	defaultFont := nil.! !

