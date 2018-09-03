'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 2 September 2018 at 11:47:54 pm'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle tabWidth defaultFont lastTabIndex lastTabX '
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterScanner methodsFor: 'private' stamp: 'jmv 9/2/2018 23:46:54'!
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
	tabWidth _ font pointSize * 5 // 2.
	xTable _ font xTable! !

!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 9/2/2018 23:46:57'!
tabDestX
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			"Add the width of a tab for every two chars since last tab, to last tab x position."
			(lastIndex - lastTabIndex // 3 + 1) * tabWidth + lastTabX min: rightMargin ]! !

!CharacterScanner methodsFor: 'initialization' stamp: 'jmv 9/1/2018 20:32:04'!
initialize
	lastTabIndex _ lastTabX _ destX _ destY _ leftMargin _ rightMargin _ 0.! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 9/2/2018 23:40:08'!
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
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^false! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 9/2/2018 23:30:31'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastTabIndex _ lastIndex _ line first.
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
	lastTabX _ destX _ leftMargin _ line leftMarginForAlignment: alignment.
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
			stopConditions: stopConditions kern: font baseKern.
		"see setStopConditions for stopping conditions for character block operations."
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


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 9/2/2018 23:27:51'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastTabIndex _ lastIndex _ startIndex.	"scanning sets last index"
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
	lastTabX _ destX _ spaceX _ leftMargin.

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
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: font baseKern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 9/2/2018 23:40:01'!
tab
	"Advance destination x according to tab settings in the current
	ParagraphStyle. Answer whether the character has crossed the right edge of 
	the composition rectangle of the TextComposition."

	destX _ self tabDestX.
	destX > rightMargin ifTrue: [^self crossedX].
	lastIndex _ lastIndex + 1.
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^false
! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/2/2018 23:30:05'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastTabIndex _ lastIndex _ line first.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	lastTabX _ destX _ leftMargin.
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
				xTable _ font xTable.
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
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: font baseKern.
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

!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 9/2/2018 23:39:42'!
tab
	destX _ self tabDestX.
	lastIndex _ lastIndex + 1.
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^ false! !

!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth tabWidth kern paragraphStyle defaultFont lastTabX lastTabIndex'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!
