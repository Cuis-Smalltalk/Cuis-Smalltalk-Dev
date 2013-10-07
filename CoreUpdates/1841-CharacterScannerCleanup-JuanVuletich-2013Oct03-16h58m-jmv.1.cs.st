'From Cuis 4.2 of 25 July 2013 [latest update: #1840] on 3 October 2013 at 5:01:17.08099 pm'!

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/2/2013 08:09'!
paddedSpace
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	spaceCount _ spaceCount + 1.
	pad _ line justifiedPadFor: spaceCount.
	lastSpaceOrTabWidth _ spaceWidth + pad.
	(destX + lastSpaceOrTabWidth) >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	lastIndex _ lastIndex + 1.
	destX _ destX + lastSpaceOrTabWidth.
	^ false! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 10/2/2013 08:10'!
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
						topLeft: characterPoint - (kern @ 0)
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 10/3/2013 16:59'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastIndex _ startIndex.	"scanning sets last index"
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setStopConditions.	"also sets font, style, etc"

	"Set up margins"
	leftMargin _ lineRectangle left.
	rightMargin _ lineRectangle right.
	xtraSpaceBefore _ 0.
	spaceAfterParagraph _ 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin _ leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin _ rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore _ paragraphStyle spaceBefore ].
		spaceAfterParagraph _ paragraphStyle spaceAfter ].
	destX _ spaceX _ leftMargin.

	runLength _ text runLengthFor: startIndex.
	runStopIndex _ (lastIndex _ startIndex) + (runLength - 1).
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: kern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/2/2013 21:09'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ runX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				""
				foregroundColor _ paragraphColor.
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ 0 - font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				stopConditions _ DefaultStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
				""
			]
		].
		self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.

	lastIndex _ line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: kern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat.! !

!methodRemoval: CharacterScanner #leadingTab!
CharacterScanner removeSelector: #leadingTab!
