'From Cuis 6.0 [latest update: #6080] on 18 November 2023 at 9:51:48 am'!

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 11/18/2023 09:42:49'!
characterBlockAtIndex: index in: textLine
	"This method is the Morphic characterBlock finder."
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
		] repeat! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 11/18/2023 09:46:45'!
characterBlockAtPoint: aPoint in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition string |
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
		] repeat! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 11/18/2023 09:48:31'!
characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint in: line.! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 11/18/2023 09:49:02'!
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line := lines at: (self lineIndexFor: index).
	t := model actualContents.
	^ (CharacterBlockScanner new text: t)
		defaultFont: self defaultFont;
		characterBlockAtIndex: ((index max: line first) min: t size+1) in: line.! !

!methodRemoval: CharacterBlockScanner #characterBlockAtPoint:index:in: stamp: 'jmv 11/18/2023 09:51:34'!
CharacterBlockScanner removeSelector: #characterBlockAtPoint:index:in:!
