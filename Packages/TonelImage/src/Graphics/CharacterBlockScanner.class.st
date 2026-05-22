"
My instances are used to scan text to compute the CharacterBlock for a character specified by its index in the text or its proximity to the cursor location.
"
Class {
	#name : 'CharacterBlockScanner',
	#superclass : 'CharacterScanner',
	#instVars : [
		'characterPoint',
		'characterIndex',
		'specialWidth',
		'lastCharacterWidth',
		'lastSpaceOrTabWidth',
		'forCursorPosition'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'scanning' }
CharacterBlockScanner >> characterBlockAtIndex: index in: textLine [
	"This method is the Morphic characterBlock finder.
	Answer the CharacterBlock containing the character at index."
	| runLength lineStop stopCondition string |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := index.  "nil means scanning for point"
	characterPoint := line bottomRight.
	(text isEmpty or: [ characterIndex < line first ]) ifTrue:	 [
		^ CharacterBlock
			stringIndex: line first
			text: text
			topLeft: line leftMargin@line top
			extent: 0 @ line lineHeight
			textLine: line ].

	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := characterIndex.	"scanning for index"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth := specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			"Result for characterBlockForIndex: "
			^ CharacterBlock
				stringIndex: characterIndex
				text: text
				topLeft: characterPoint x@line top
				extent: lastCharacterWidth @ line lineHeight
				textLine: line ]
		] repeat
]

{ #category : 'scanning' }
CharacterBlockScanner >> characterBlockAtPoint: aPoint in: textLine forCursorPosition: aBoolean [
	"This method is the Morphic characterBlock finder.

	If aBoolean, find the CharacterBlock for the character at the right of where
	the text cursor should be positioned if user clicked in aPoint.
	This is useful for positioning the text cursor, and also for selecting text.

	If aBoolean is false, find the CharacterBlock that actually contains aPoint.
	This is useful, for example, for selecting enclosed text between delimiters."

	| runLength lineStop stopCondition string |
	forCursorPosition := aBoolean.
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := nil.  "nil means scanning for point"
	characterPoint := aPoint.
	characterPoint y > line bottom
		ifTrue: [ characterPoint := line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])]) ifTrue:	 [
		^ CharacterBlock
			stringIndex: line first
			text: text
			topLeft: line leftMargin@line top
			extent: 0 @ line lineHeight
			textLine: line ].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := line last.			"scanning for point"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth := specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			"Result for characterBlockAtPoint: 
			Not actually the character we hit with the mouse,
			but the one at the right of the desired cursor position."
			^ CharacterBlock
				stringIndex: lastIndex
				text: text
				topLeft: characterPoint x@line top
				extent: lastCharacterWidth @ line lineHeight
				textLine: line ]
		] repeat
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> crossedX [
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| lastChar currentX delta |
	"Scanning for index"
	characterIndex ifNotNil: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex := characterIndex.
			characterPoint := leftMargin @ (destY + line lineHeight).
			^true ]].

	"Pointing before middle of a character.
	This is to know where to place cursor."
	delta := forCursorPosition ifTrue: [lastCharacterWidth // 2] ifFalse: [lastCharacterWidth].
	characterPoint x <= (destX + delta) ifTrue: [
		characterPoint := destX - (font isBold ifTrue: [1] ifFalse: [0]) @ destY.
		^true ].

	"Pointing past the end"
	lastIndex >= line last ifTrue: [
		characterPoint := destX @ destY.
		^true ].

	"Pointing past middle of a character, return the next character.
	This is to know where to place cursor."
	lastIndex := lastIndex + 1.
	currentX := destX + lastCharacterWidth - (font isBold ifTrue: [2] ifFalse: [0]).
	lastChar := text at: lastIndex.
	lastCharacterWidth := font widthOf: lastChar.
	characterPoint := currentX @ destY.
	lastChar = Character space ifFalse: [
		^ true].

	"Yukky if next character is space or tab."
	alignment = CharacterScanner justifiedCode ifTrue: [
		lastCharacterWidth := lastCharacterWidth + (line justifiedPadFor: (spaceCount + 1)).
		^ true].

	^ true
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> doNewLine [
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a newLine stop condition that has just been 
	encountered. The convention is to denote selections by 
	CharacterBlocks, sometimes including the newLine character (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue: [
			"When off end of string, give data for next character"
			destY := destY +  line lineHeight.
			characterPoint := leftMargin @ destY.
			lastIndex := lastIndex + 1.
			lastCharacterWidth := 0.
			^ true ].
		characterPoint := destX @ destY.
		lastCharacterWidth := rightMargin - destX.
		^true
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> endOfRun [
	"Before arriving at the cursor location, the selection has encountered an 
	end of run. Answer false if the selection continues, true otherwise. Set 
	up indexes for building the appropriate CharacterBlock."

	| runLength lineStop lastChar |
	((characterIndex notNil and: [
		runStopIndex < characterIndex and: [ runStopIndex < text size ]])
			or:	[ characterIndex isNil and: [ lastIndex < line last ]])
		ifTrue:	[
			"We're really at the end of a real run."
			runLength := (text runLengthFor: (lastIndex := lastIndex + 1)).
			lineStop := characterIndex		"scanning for index"
				ifNil: [ line last ].			"scanning for point"
			(runStopIndex := lastIndex + (runLength - 1)) > lineStop
				ifTrue: [ runStopIndex := lineStop ].
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^ false].

	lastChar := text at: lastIndex.
	characterPoint := destX @ destY.
	(( lastChar = Character space and: [ alignment = CharacterScanner justifiedCode ])
		or: [ lastChar = Character tab and: [ lastSpaceOrTabWidth notNil ]])
		ifTrue: [
			lastCharacterWidth := lastSpaceOrTabWidth ].
	characterIndex
		ifNotNil: [
			"If scanning for an index and we've stopped on that index,
			then we back destX off by the width of the character stopped on
			(it will be pointing at the right side of the character) and return"
			runStopIndex = characterIndex
				ifTrue: [
					characterPoint := destX - lastCharacterWidth @ characterPoint y.
					^ true ].
			"Otherwise the requested index was greater than the length of the
			string.  Return string size + 1 as index, indicate further that off the
			string by setting character to nil and the extent to 0."
			lastIndex :=  lastIndex + 1.
			lastCharacterWidth := 0.
			^ true ].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue: [
			"off end of string"
			lastIndex :=  lastIndex + 1.
			lastCharacterWidth := 0.
			^ true ].
	"just off end of line without crossing x"
	lastIndex := lastIndex + 1.
	^true
]

{ #category : 'initialization' }
CharacterBlockScanner >> initialize [
	super initialize.
	forCursorPosition := true.
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> paddedSpace [
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	spaceCount := spaceCount + 1.
	pad := line justifiedPadFor: spaceCount.
	lastSpaceOrTabWidth := spaceWidth + pad.
	(destX + lastSpaceOrTabWidth) >= characterPoint x
		ifTrue: [
			lastCharacterWidth := lastSpaceOrTabWidth.
			^ self crossedX ].
	lastIndex := lastIndex + 1.
	destX := destX + lastSpaceOrTabWidth.
	^ false
]

{ #category : 'scanning' }
CharacterBlockScanner >> placeEmbeddedObject: anchoredFormOrMorph [

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	specialWidth := (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner width ]
		ifFalse: [ anchoredFormOrMorph width ].
	^ true
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> setFont [
	specialWidth := nil.
	super setFont
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> setStopConditions [
	"Set the font and the stop conditions for the current run."

	stopConditions := alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]
]

{ #category : 'stop conditions' }
CharacterBlockScanner >> tab [
	| currentX |
	currentX := self tabDestX.
	lastSpaceOrTabWidth := currentX - destX max: 0.
	currentX >= characterPoint x
		ifTrue: [
			lastCharacterWidth := lastSpaceOrTabWidth.
			^ self crossedX ].
	destX := currentX.
	lastIndex := lastIndex + 1.
	tabCount := tabCount + 1.
	^false
]
