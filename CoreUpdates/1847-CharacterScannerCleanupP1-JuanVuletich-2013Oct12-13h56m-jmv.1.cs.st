'From Cuis 4.2 of 25 July 2013 [latest update: #1846] on 12 October 2013 at 2:02:10.393117 pm'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle '
	classVariableNames: 'DefaultStopConditions StopConditionsWithSpace StopConditionsWithPaddedSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterScanner methodsFor: 'private' stamp: 'jmv 10/12/2013 14:01'!
setFont
	| priorFont attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"
	
	priorFont _ font.
	self setActualFont: (text fontAt: lastIndex).
	attributes _ text attributesAt: lastIndex.
	text
		withFormattingAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :kernInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			alignment _ alignmentInText.
			kern _ kernInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info | self textColor: TextAction textActionColor ]].
	
	priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
	destX _ destX - font descentKern.
	kern _ kern - font baseKern.

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	xTable _ font xTable.
	map _ font characterToGlyphMap! !


!CharacterScanner class methodsFor: 'class initialization' stamp: 'jmv 10/11/2013 18:34'!
initialize
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions _ Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space asciiValue + 1 put: nil.
	stopConditions at: Character tab asciiValue + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lf asciiValue + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character cr asciiValue + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions _ stopConditions.

	StopConditionsWithSpace _ DefaultStopConditions copy.
	StopConditionsWithSpace at: Character space asciiValue + 1 put: #space.

	StopConditionsWithPaddedSpace _ DefaultStopConditions copy.
	StopConditionsWithPaddedSpace at: Character space asciiValue + 1 put: #paddedSpace.
	
	! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:15'!
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
			self setFont.
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

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/12/2013 13:59'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	self setFont.
	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ StopConditionsWithPaddedSpace ]
		ifFalse: [ DefaultStopConditions ]! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 10/11/2013 18:14'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastIndex _ line first.
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


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 10/11/2013 18:25'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastIndex _ startIndex.	"scanning sets last index"
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
	runStopIndex _ (lastIndex _ startIndex) + (runLength - 1).
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
				kern: kern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:27'!
endOfRun
	"Answer true if scanning has reached the end of the paragraph. 
	Otherwise step conditions (mostly install potential new font) and answer 
	false."

	| runLength |
	lastIndex = text size
		ifTrue:	[
			line stop: lastIndex.
			spaceX _ destX.
			line paddingWidth: rightMargin - destX - 1.
			^true ]
		ifFalse:	[
			runLength _ (text runLengthFor: (lastIndex _ lastIndex + 1)).
			runStopIndex _ lastIndex + (runLength - 1).
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^false ]! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/12/2013 14:00'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	self setFont.
	stopConditions _ StopConditionsWithSpace! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:01'!
space
	"Record left x and character index of the space character just encounted. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the paragraph."
	spaceX _ destX.
	destX _ spaceX + spaceWidth.
	spaceIndex _ lastIndex.
	lineHeightAtSpace _ lineHeight.
	baselineAtSpace _ baseline.
	lastIndex _ lastIndex + 1.
	spaceCount _ spaceCount + 1.
	destX > rightMargin ifTrue: [ ^ self crossedX ].
	^ false! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/12/2013 13:54'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
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
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
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

!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:15'!
endOfRun
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastIndex = line last ifTrue: [^true].
	runLength _ text runLengthFor: (lastIndex _ lastIndex + 1).
	runStopIndex _ lastIndex + (runLength - 1) min: line last.
	self setFont.
	self setStopConditions.
	self placeEmbeddedObject.
	^ false! !

!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 10/12/2013 14:00'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	self setFont.
	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ StopConditionsWithPaddedSpace ]
		ifFalse: [ DefaultStopConditions ]! !

!methodRemoval: CompositionScanner #setFont!
CompositionScanner removeSelector: #setFont!
CharacterScanner initialize!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!
