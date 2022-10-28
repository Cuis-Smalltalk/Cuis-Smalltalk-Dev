'From Cuis 6.0 [latest update: #5527] on 26 October 2022 at 4:06:08 pm'!

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 10/26/2022 16:05:50'!
scanCharactersFrom: startIndex to: stopIndex in: anUtf8String rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	anUtf8String from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil).
		It is OK to just use the first byte in UTF-8"
		(stops at: byte1 + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX _ destX + (font widthOfUtf8Byte1: byte1 b2: byte2 b3: byte3 b4: byte4).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 10/26/2022 16:04:57'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition string |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastIndex _ line first.
	tabCount _ 0.
	self setFont.
	self setStopConditions.
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
	string _ text string asUtf8String.

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth _ specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 10/26/2022 16:04:13'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph string |
	
	lastIndex _ startIndex.	"scanning sets last index"
	tabCount _ 0.
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

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
	runStopIndex _ lastIndex + runLength - 1.
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	lastLineBreakingSpace _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.
	string _ text string asUtf8String.

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: string rightX: rightMargin stopConditions: stopConditions.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/26/2022 16:03:20'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	tabCount _ 0.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string asUtf8String.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 _ destX.
			((Preferences at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((Preferences at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !

!methodRemoval: CharacterScanner #scanUtf8StringFrom:to:in:rightX:stopConditions: stamp: 'jmv 10/26/2022 16:06:00'!
CharacterScanner removeSelector: #scanUtf8StringFrom:to:in:rightX:stopConditions:!
!methodRemoval: Utf8String #scanCharactersFrom:to:with:rightX:stopConditions: stamp: 'jmv 10/26/2022 16:01:16'!
Utf8String removeSelector: #scanCharactersFrom:to:with:rightX:stopConditions:!

!Utf8String reorganize!
('accessing' at: bytes findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with: copyWith: shallowCopy)
('comparing' < <= = > >= basicHash caseInsensitiveLessOrEqual: computeHash hash is:substringAt: sameAs:)
('converting' asByteString asHex asUtf32Words asUtf8Bytes asUtf8String reversed substrings surroundedBySingleQuotes thatCanBeModified withLineEndings:)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('printing' isLiteral)
('private' boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: privateBytes:codePointCount: stride)
('services' numArgs)
('initialization' initialize)
('dispatch' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!

