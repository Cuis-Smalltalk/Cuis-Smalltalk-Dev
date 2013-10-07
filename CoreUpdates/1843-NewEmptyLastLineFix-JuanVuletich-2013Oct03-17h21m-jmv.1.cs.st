'From Cuis 4.2 of 25 July 2013 [latest update: #1840] on 3 October 2013 at 5:34:48.22399 pm'!
!classDefinition: #CompositionScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CompositionScanner
	instanceVariableNames: 'spaceX spaceIndex lineHeight baseline lineHeightAtSpace baselineAtSpace lastLineBreakingSpace '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CompositionScanner methodsFor: 'accessing' stamp: 'jmv 10/3/2013 17:30'!
lastLineBreakingSpace
	^lastLineBreakingSpace! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2013 17:28'!
addEmptyTrailingLine: newParagraph
	"The line to add is usually the first line of a new paragraph (if last char in text was newLine),
	but it can be a new line in same paragraph (if enough spaces ended last line)."
	| ts f h bs r lm rm w a leftMarginForAlignment s |
	s _ theText size+1.
	f _ editor
		ifNotNil: [ editor lastFont ]
		ifNil: [ theText fontAt: s ].
	ts _ editor
		ifNotNil: [ editor lastParagraphStyleOrNil ]
		ifNil: [ theText paragraphStyleOrNilAt: s].

	h _ f height.
	bs _ f ascent.
	lm _ 0.
	rm _ 0.
	w _ extentForComposing x.
	a _ 0.
	ts ifNotNil: [
		newParagraph ifTrue: [
			h _ h + ts spaceBefore.
			bs _ bs + ts spaceBefore ].
		lm _ ((newParagraph and: [ ts isListStyle not ])
			ifTrue: [ ts firstIndent ]
			ifFalse: [ ts restIndent ]).
		rm _ ts rightIndent.
		a _ ts alignment ].
	
	leftMarginForAlignment _ a = CharacterScanner rightFlushCode
		ifTrue: [ w - rm]
		ifFalse: [
			a = CharacterScanner centeredCode
				ifTrue: [ (w - rm - lm) //2 + lm]
				ifFalse: [ lm ]].
	r _ leftMarginForAlignment @ currentY extent: 0@h.

	lines addLast: (
		EmptyLine new
			firstIndex: s lastIndex: s - 1;
			rectangle: r;
			lineHeight: h baseline: bs;
			paragraphStyle: ts)! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 10/3/2013 17:30'!
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

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/3/2013 17:31'!
crossedX
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition."

	spaceCount >= 1 ifTrue: [
		"The common case. First back off to the space at which we wrap."
		line stop: spaceIndex.
		lastLineBreakingSpace _ spaceIndex.
		lineHeight _ lineHeightAtSpace.
		baseline _ baselineAtSpace.
		spaceCount _ spaceCount - 1.
		spaceIndex _ spaceIndex - 1.

		"Check to see if any spaces preceding the one at which we wrap.
			Double space after punctuation, most likely."
		[(spaceCount > 1 and: [(text at: spaceIndex) = Character space])]
			whileTrue: [
				spaceCount _ spaceCount - 1.
				"Account for backing over a run which might
					change width of space."
				font _ text fontAt: spaceIndex.
				spaceIndex _ spaceIndex - 1.
				spaceX _ spaceX - (font widthOf: Character space)].
		line paddingWidth: rightMargin - spaceX - 1.
		line internalSpaces: spaceCount]
	ifFalse: [
		"Neither internal nor trailing spaces -- almost never happens."
		lastIndex _ lastIndex - 1.
		[ destX <= rightMargin or: [ lastIndex = 0]]
			whileFalse: [
				destX _ destX - (font widthOf: (text at: lastIndex)).
				lastIndex _ lastIndex - 1].
		lastLineBreakingSpace _ lastIndex.
		spaceX _ destX.
		line paddingWidth: rightMargin - destX - 1.
		line stop: (lastIndex max: line first)].
	^true! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2013 17:33'!
composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

	lines _ argLinesCollection.
	theText _ argText.
	extentForComposing _ argExtentForComposing.
	deltaCharIndex _ argDelta.
	currCharIndex _ startCharIndex _ argStart.
	stopCharIndex _ argStop.
	prevLines _ argPriorLines.
	currentY _ argStartY.
	maxRightX _ 0.
	possibleSlide _ stopCharIndex < theText size.
	nowSliding _ false.
	prevIndex _ 1.
	scanner _ CompositionScanner new text: theText.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed _ false.
	self composeAllLines.
	(anythingComposed not or: [ theText last isLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine: true ]
		ifFalse: [
			scanner lastLineBreakingSpace = theText size ifTrue: [
				self addEmptyTrailingLine: false ]].
	^ {lines asArray. maxRightX}

! !

!methodRemoval: TextComposer #addEmptyTrailingLine!
TextComposer removeSelector: #addEmptyTrailingLine!
!classDefinition: #CompositionScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CompositionScanner
	instanceVariableNames: 'spaceX spaceIndex lineHeight baseline lineHeightAtSpace baselineAtSpace lastLineBreakingSpace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
