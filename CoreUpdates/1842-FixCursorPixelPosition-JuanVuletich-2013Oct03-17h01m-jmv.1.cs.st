'From Cuis 4.2 of 25 July 2013 [latest update: #1840] on 3 October 2013 at 5:06:29.28099 pm'!

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/3/2013 17:05'!
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
			lastIndex _ characterIndex.
			characterPoint _ leftMargin @ (destY + line lineHeight).
			^true ]].

	"Pointing before middle of a character"
	characterPoint x <= (destX + (lastCharacterWidth // 2)) ifTrue: [
		characterPoint _ destX - (font isBold ifTrue: [1] ifFalse: [0]) @ destY.
		^true ].

	"Pointing past the end"
	lastIndex >= line last ifTrue: [
		characterPoint _ destX @ destY.
		^true ].

	"Pointing past middle of a character, return the next character."
	lastIndex _ lastIndex + 1.
	currentX _ destX + lastCharacterWidth - (font isBold ifTrue: [2] ifFalse: [0]).
	lastChar _ text at: lastIndex.
	lastCharacterWidth _ font widthOf: lastChar.
	characterPoint _ currentX @ destY.
	lastChar = Character space ifFalse: [
		^ true].

	"Yukky if next character is space or tab."
	alignment = CharacterScanner justifiedCode ifTrue: [
		lastCharacterWidth _ lastCharacterWidth + (line justifiedPadFor: (spaceCount + 1)).
		^ true].

	^ true! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 10/3/2013 17:05'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastIndex _ line first.
	self setStopConditions.		"also sets font"
	characterIndex _ index.  "nil means scanning for point"
	characterPoint _ aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint _ line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	destX _ leftMargin _ line leftMarginForAlignment: alignment.
	destY _ line top.
	runLength _ text runLengthFor: line first.
	lineStop _ characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex _ lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth _ 0.
	spaceCount _ 0.

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x
			stopConditions: stopConditions kern: kern.
		"see setStopConditions for stopping conditions for character block 	operations."
		lastCharacterWidth _ specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !

