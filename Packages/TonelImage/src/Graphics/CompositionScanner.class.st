"
CompositionScanners are used to measure text and determine where line breaks and space padding should occur.
"
Class {
	#name : 'CompositionScanner',
	#superclass : 'CharacterScanner',
	#instVars : [
		'spaceX',
		'spaceIndex',
		'lineHeight',
		'baseline',
		'lineHeightAtSpace',
		'baselineAtSpace',
		'lastLineBreakingSpace',
		'lineSpacingFactor'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'scanning' }
CompositionScanner >> composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide [

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph string |
	
	lastIndex := startIndex.	"scanning sets last index"
	tabCount := 0.
	destY := lineRectangle top.
	lineHeight := baseline := 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

	"Set up margins"
	leftMargin := lineRectangle left.
	rightMargin := lineRectangle right.
	xtraSpaceBefore := 0.
	spaceAfterParagraph := 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin := leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin := rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore := paragraphStyle spaceBefore ].
		spaceAfterParagraph := paragraphStyle spaceAfter ].
	destX := spaceX := leftMargin.

	runLength := text runLengthFor: startIndex.
	runStopIndex := lastIndex + runLength - 1.
	line := (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount := 0.
	lastLineBreakingSpace := 0.
	leftMargin := destX.
	line leftMargin: leftMargin.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
			stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
				in: string rightX: rightMargin stopConditions: stopConditions.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat
]

{ #category : 'stop conditions' }
CompositionScanner >> crossedX [
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition."

	spaceCount >= 1 ifTrue: [
		"The common case. First back off to the space at which we wrap."
		line stop: spaceIndex.
		lastLineBreakingSpace := spaceIndex.
		lineHeight := lineHeightAtSpace.
		baseline := baselineAtSpace.
		spaceCount := spaceCount - 1.
		spaceIndex := spaceIndex - 1.

		"Check to see if any spaces preceding the one at which we wrap.
			Double space after punctuation, most likely."
		[(spaceCount > 1 and: [(text at: spaceIndex) = Character space])]
			whileTrue: [
				spaceCount := spaceCount - 1.
				"Account for backing over a run which might
					change width of space."
				font := text fontAt: spaceIndex default: defaultFont.
				spaceIndex := spaceIndex - 1.
				spaceX := spaceX - (font widthOf: Character space)].
		line paddingWidth: rightMargin - spaceX - 1.
		line internalSpaces: spaceCount]
	ifFalse: [
		"Neither internal nor trailing spaces -- almost never happens."
		lastIndex := lastIndex - 1.
		[ destX <= rightMargin or: [ lastIndex = 0]]
			whileFalse: [
				destX := destX - (font widthOf: (text at: lastIndex)).
				lastIndex := lastIndex - 1].
		lastLineBreakingSpace := lastIndex.
		spaceX := destX.
		line paddingWidth: rightMargin - destX - 1.
		line stop: (lastIndex max: line first)].
	^true
]

{ #category : 'stop conditions' }
CompositionScanner >> doNewLine [
	"Answer true. Set up values for the text line interval currently being 
	composed."

	line stop: lastIndex.
	spaceX := destX.
	line paddingWidth: rightMargin - spaceX - 1.
	^true
]

{ #category : 'stop conditions' }
CompositionScanner >> endOfRun [
	"Answer true if scanning has reached the end of the TextComposition. 
	Otherwise step conditions (mostly install potential new font) and answer 
	false."

	| runLength |
	lastIndex = text size
		ifTrue:	[
			line stop: lastIndex.
			spaceX := destX.
			line paddingWidth: rightMargin - destX - 1.
			^true ]
		ifFalse:	[
			runLength := (text runLengthFor: (lastIndex := lastIndex + 1)).
			runStopIndex := lastIndex + (runLength - 1).
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^false ]
]

{ #category : 'initialization' }
CompositionScanner >> initialize [
	super initialize.
	lineSpacingFactor := Preferences at: #defaultLineSpacingFactor.
]

{ #category : 'accessing' }
CompositionScanner >> lastLineBreakingSpace [
	^lastLineBreakingSpace
]

{ #category : 'accessing' }
CompositionScanner >> lineSpacingFactor: aNumber [
	lineSpacingFactor := aNumber
]

{ #category : 'stop conditions' }
CompositionScanner >> placeEmbeddedObject: anchoredFormOrMorph [
	| prevBaseline h |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	prevBaseline := baseline.
	h := (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner height ]
		ifFalse: [ anchoredFormOrMorph height ].
	baseline := baseline max: h.
	lineHeight := lineHeight + baseline - prevBaseline.
	line stop: lastIndex.
	^ true
]

{ #category : 'accessing' }
CompositionScanner >> rightX [
	"Meaningful only when a line has just been composed -- refers to the line most recently composed.  This is a subtrefuge to allow for easy resizing of a composition rectangle to the width of the maximum line.  Useful only when there is only one line in the form or when each line is terminated by a carriage return.  Handy for sizing menus and lists."

	^spaceX
]

{ #category : 'scanning' }
CompositionScanner >> setFont  [

	| oldBaseline oldLineHeightMinusBaseline newBaseline newLineHeight |
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor".
	"Split the extra leading due to lineSpacingFactor > 1, 70% above ascent, 30% below descent."
	newBaseline := font ascent + (lineSpacingFactor - 1 * 0.7 * font lineSpacing).
	newLineHeight := font lineSpacing * lineSpacingFactor.
	lineHeight
		ifNil: [
			baseline := newBaseline.
			lineHeight := newLineHeight ]
		ifNotNil: [
			oldBaseline := baseline.
			oldLineHeightMinusBaseline := lineHeight - oldBaseline.
			baseline := baseline max: newBaseline.
			lineHeight := lineHeight max: baseline + (oldLineHeightMinusBaseline max: newLineHeight -newBaseline) ].
]

{ #category : 'stop conditions' }
CompositionScanner >> setStopConditions [
	"Set the font and the stop conditions for the current run."

	stopConditions := self class stopConditionsWithSpace
]

{ #category : 'stop conditions' }
CompositionScanner >> space [
	"Record left x and character index of the space character just encounted. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the paragraph."

	spaceX := destX.
	destX := spaceX + spaceWidth.
	spaceIndex := lastIndex.
	lineHeightAtSpace := lineHeight.
	baselineAtSpace := baseline.
	lastIndex := lastIndex + 1.
	spaceCount := spaceCount + 1.
	destX > rightMargin ifTrue: 	[^self crossedX].
	^false

]

{ #category : 'stop conditions' }
CompositionScanner >> tab [
	"Advance destination x according to tab settings in the current
	ParagraphStyle. Answer whether the character has crossed the right edge of 
	the composition rectangle of the TextComposition."

	destX := self tabDestX.
	destX > rightMargin ifTrue: [^self crossedX].
	lastIndex := lastIndex + 1.
	tabCount := tabCount + 1.
	^false

]
