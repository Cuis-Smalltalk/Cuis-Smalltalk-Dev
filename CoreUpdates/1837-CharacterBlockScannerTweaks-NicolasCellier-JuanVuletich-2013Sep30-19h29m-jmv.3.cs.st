'From Cuis 4.2 of 25 July 2013 [latest update: #1836] on 30 September 2013 at 8:18:28.70994 pm'!
!classDefinition: #CharacterBlockScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CharacterBlockScanner
	instanceVariableNames: 'characterPoint characterIndex lastCharacter lastCharacterExtent lastSpaceOrTabExtent specialWidth lastCharacterWidth lastSpaceOrTabWidth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/30/2013 20:14'!
crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| lastChar currentX |
	characterIndex ifNotNil: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex _ characterIndex.
			characterPoint _ leftMargin @ (destY + line lineHeight).
			^true ]].
	characterPoint x <= (destX + (lastCharacterWidth // 2))
		ifTrue:	[
			characterPoint _ destX @ destY.
			^true ].
	lastIndex >= line last 
		ifTrue:	[
			characterPoint _ destX @ destY.
			^true ].

	"Pointing past middle of a character, return the next character."
	lastIndex _ lastIndex + 1.
	currentX _ destX + lastCharacterWidth + kern.
	lastChar _ text at: lastIndex.
	lastCharacterWidth _ font widthOf: lastChar.
	characterPoint _ currentX @ destY.
	lastChar = Character space ifFalse: [^ true].

	"Yukky if next character is space or tab."
	alignment = CharacterScanner justifiedCode ifTrue: [
		lastCharacterWidth _ lastCharacterWidth + (line justifiedPadFor: (spaceCount + 1)).
		^ true].

	^ true! !

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/30/2013 20:14'!
doNewLine
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a carriage return stop condition that has just been 
	encountered. The ParagraphEditor convention is to denote selections by 
	CharacterBlocks, sometimes including the carriage return (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue: [
			"When off end of string, give data for next character"
			destY _ destY +  line lineHeight.
			characterPoint _ leftMargin @ destY.
			lastIndex _ lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].
		characterPoint _ destX @ destY.
		lastCharacterWidth _ rightMargin - destX.
		^true! !

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/30/2013 20:18'!
endOfRun
	"Before arriving at the cursor location, the selection has encountered an 
	end of run. Answer false if the selection continues, true otherwise. Set 
	up indexes for building the appropriate CharacterBlock."

	| runLength lineStop lastChar |
	((characterIndex notNil and: [
		runStopIndex < characterIndex and: [ runStopIndex < text size ]])
			or:	[ characterIndex isNil and: [ lastIndex < line last ]])
		ifTrue:	[
			"We're really at the end of a real run."
			runLength _ (text runLengthFor: (lastIndex _ lastIndex + 1)).
			lineStop _ characterIndex		"scanning for index"
				ifNil: [ line last ].			"scanning for point"
			(runStopIndex _ lastIndex + (runLength - 1)) > lineStop
				ifTrue: [ runStopIndex _ lineStop ].
			self setStopConditions.
			self placeEmbeddedObject.
			^ false].

	lastChar _ text at: lastIndex.
	characterPoint _ destX @ destY.
	(( lastChar = Character space and: [ alignment = CharacterScanner justifiedCode ])
		or: [ lastChar = Character tab and: [ lastSpaceOrTabWidth notNil ]])
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth ].
	characterIndex
		ifNotNil: [
			"If scanning for an index and we've stopped on that index,
			then we back destX off by the width of the character stopped on
			(it will be pointing at the right side of the character) and return"
			runStopIndex = characterIndex
				ifTrue: [
					characterPoint _ destX - lastCharacterWidth @ characterPoint y.
					^ true ].
			"Otherwise the requested index was greater than the length of the
			string.  Return string size + 1 as index, indicate further that off the
			string by setting character to nil and the extent to 0."
			lastIndex _  lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue: [
			"off end of string"
			lastIndex _  lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].
	"just off end of line without crossing x"
	lastIndex _ lastIndex + 1.
	^true! !

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/30/2013 20:17'!
paddedSpace
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	pad _ 0.
	spaceCount _ spaceCount + 1.
	pad _ line justifiedPadFor: spaceCount.
	lastSpaceOrTabWidth _ spaceWidth + pad.
	(destX + lastSpaceOrTabWidth) >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	lastIndex _ lastIndex + 1.
	destX _ destX + lastSpaceOrTabWidth.
	^ false
! !

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/30/2013 20:17'!
tab
	| currentX |
	currentX _ self tabDestX.
	lastSpaceOrTabWidth _ currentX - destX max: 0.
	currentX >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	destX _ currentX.
	lastIndex _ lastIndex + 1.
	^false! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 9/30/2013 20:15'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop done stopCondition |
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
	done  _ false.
	[ done ] whileFalse: [
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
						topLeft: characterPoint - (kern @ 0)
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]]! !

!methodRemoval: CharacterBlockScanner #characterPointSetX:!
CharacterBlockScanner removeSelector: #characterPointSetX:!
!methodRemoval: CharacterBlockScanner #lastCharacterExtentSetX:!
CharacterBlockScanner removeSelector: #lastCharacterExtentSetX:!
!methodRemoval: CharacterBlockScanner #lastSpaceOrTabExtentSetX:!
CharacterBlockScanner removeSelector: #lastSpaceOrTabExtentSetX:!
!methodRemoval: CharacterBlockScanner #lastSpaceOrTabExtentX:!
CharacterBlockScanner removeSelector: #lastSpaceOrTabExtentX:!
!classDefinition: #CharacterBlockScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CharacterBlockScanner
	instanceVariableNames: 'characterPoint characterIndex specialWidth lastCharacterWidth lastSpaceOrTabWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
