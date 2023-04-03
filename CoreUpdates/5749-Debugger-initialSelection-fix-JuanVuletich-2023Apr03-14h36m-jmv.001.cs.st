'From Cuis 6.0 [latest update: #5748] on 3 April 2023 at 2:47:10 pm'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 11:49:12'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents."
	^false! !


!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 11:49:47'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents."
	^textProvider shoutMustKeepContentsSize! !


!TextProvider methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 11:50:50'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents."
	^false! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 11:51:25'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents."
	^true! !


!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 11:50:54'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents."
	^false! !


!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 4/3/2023 14:40:54'!
zeroWidthSpace
	^self  codePoint: 16r200B! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 4/3/2023 14:41:23'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkUpArrow asString` with: '^'.
	^answer! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 4/3/2023 14:45:31'!
isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."

	^ `#(32 9 10 13 12), { UnicodeCodePoint zeroWidthSpace codePoint }` statePointsTo: value! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 4/3/2023 12:36:08'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint asciiValue ifNotNil: [ :ascii |
		^aTypeTable at: ascii ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/3/2023 14:41:39'!
privateFormatAndConvert

	| attr cs s start found hs |
	textModel shouldAutoHighlight ifTrue: [
		textModel textProvider autoHighlightString ifNotNil: [ :autoHighlight |
			"Message sends. May include, for instance, arguments of keyword selectors."
			attr := `TextBackgroundColor color: (Color red alpha: 0.2)`.
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: attr
					from: interval first
					to: interval last ].
			"Searched text. Senders of symbols, including nested in literal arrays."
			textModel textProvider autoHighlightAllOccurrences ifTrue: [
				cs := Preferences at: #caseSensitiveFinds.
				s := formattedText size.
				hs := autoHighlight size.
				start := 1.
				[start < s] whileTrue: [
					found := formattedText findString: autoHighlight startingAt: start caseSensitive: cs.
					start := s.
					found = 0 ifFalse: [
						formattedText addAttribute: attr from: found to: found+hs-1.
						start := found+hs ]]]
			].
		textModel textProvider justDidAutoHighlight ].

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: (textModel shoutMustKeepContentsSize
					ifTrue: [`Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`]
					ifFalse: [`Character smalltalkLeftArrow asString`]).
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 4/3/2023 14:46:34'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes carriageReturn |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.

	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := #(8592 8593 8594 8595 8617) collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	mapSize > 8617
		ifTrue: [
			carriageReturn := glyphs at: (characterMap at: 8617+1)+1.
			carriageReturn == unknownGlyph ifTrue: [ carriageReturn := FallbackArrowGlyphs at: 5 ]]
		ifFalse: [ carriageReturn := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ UnicodeCodePoint zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOfUnicodeCodePoint: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturn.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: FallbackArrowGlyphs! !

!methodRemoval: Character class #nbsp stamp: 'jmv 4/3/2023 14:37:54'!
Character class removeSelector: #nbsp!

!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('misc' getSelection refetch)
('testing' canBindVariables is: isDirty isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom: saveOn:)
('shout support' privateAddBoldAt: privateRemoveBoldAt:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: commandForReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttributes:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: startNewUndoRedoCommand undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
('copying' postCopy)
('user interface support' allowStylingWithEmphasis autoCompleterClass editorClass formatAndStyleIfNeededWith: fullPrintIt shouldAutoHighlight shoutMustKeepContentsSize textStylerClass)
('shout styling' shouldStyle:with:)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
!


!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: isDirty refusesToAccept wantsFrameAdornments)
('misc' getSelection refetch releaseCachedState)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass shouldAutoHighlight shoutMustKeepContentsSize textStylerClass)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('binding' bindingOf:)
!

