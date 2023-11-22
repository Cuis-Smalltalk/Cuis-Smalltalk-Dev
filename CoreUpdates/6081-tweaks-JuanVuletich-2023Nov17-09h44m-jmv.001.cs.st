'From Cuis 6.0 [latest update: #6073] on 17 November 2023 at 9:49:38 am'!

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 11/17/2023 09:45:36'!
crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| lastChar currentX |
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
	characterPoint x <= (destX + (lastCharacterWidth // 2)) ifTrue: [
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

	^ true! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 11/17/2023 09:47:28'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition string |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := index.  "nil means scanning for point"
	characterPoint := aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint := line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
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
			^characterIndex
				ifNil: [
					"Result for characterBlockAtPoint: 
					Not actually the character we hit with the mouse,
					but the one at the right of the desired cursor position."
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [
					"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 11/17/2023 08:25:18'!
characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
			characterBlockAtPoint: aPoint
			index: nil
				in: line.! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 11/17/2023 08:24:58'!
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line := lines at: (self lineIndexFor: index).
	t := model actualContents.
	^ (CharacterBlockScanner new text: t)
		defaultFont: self defaultFont;
			characterBlockAtPoint: nil
			index: ((index max: line first) min: t size+1)
				in: line! !

