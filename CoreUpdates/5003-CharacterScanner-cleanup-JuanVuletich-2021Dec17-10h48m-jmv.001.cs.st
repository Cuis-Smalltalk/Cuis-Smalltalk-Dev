'From Cuis 5.0 [latest update: #5002] on 17 December 2021 at 10:51:53 am'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth tabWidth kern paragraphStyle defaultFont tabCount '
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 12/17/2021 10:50:38'!
scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value. Determine
	width of each character from xTable, indexed by map.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX char |
	lastIndex _ startIndex.
	[ lastIndex <= stopIndex ]
		whileTrue: [
			char _ sourceString at: lastIndex.
			"stops are only defined for the first 256 characters.
			If we (ever) handle Character like objects beyond those in ISO-8859-15,
			then #iso8859s15Code shound answer nil!!"
			char iso8859s15Code ifNotNil: [ :code |
				(stops at: code + 1) ifNotNil: [ :stop | ^stop ]].
			nextDestX _ destX + (font widthOf: char).
			nextDestX > rightX ifTrue: [
				^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 12/17/2021 10:41:13'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex default: defaultFont).
	attributes _ text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment _ alignmentInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	tabWidth _ (font widthOf: $0) * 4.! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 12/17/2021 10:43:27'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
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

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x
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


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 12/17/2021 10:43:34'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
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

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/17/2021 10:43:40'!
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
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 _ destX.
			(Preferences backgroundColorFillsAllBackground and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			(Preferences backgroundColorFillsAllBackground and: [stopCondition = #tab]) ifTrue: [
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

!methodRemoval: CharacterScanner #scanCharactersFrom:to:in:rightX:stopConditions:kern: stamp: 'jmv 12/17/2021 10:50:03'!
CharacterScanner removeSelector: #scanCharactersFrom:to:in:rightX:stopConditions:kern:!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth tabWidth kern paragraphStyle defaultFont tabCount'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!
