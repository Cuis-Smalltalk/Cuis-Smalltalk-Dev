"
Aux class for TextComposition. My instances are usually transient, and therefore don't need a TextModel, but just a Text with the current contents.
"
Class {
	#name : 'TextComposer',
	#superclass : 'Object',
	#instVars : [
		'lines',
		'maxRightX',
		'currentY',
		'scanner',
		'possibleSlide',
		'nowSliding',
		'prevIndex',
		'prevLines',
		'currCharIndex',
		'startCharIndex',
		'stopCharIndex',
		'deltaCharIndex',
		'theText',
		'extentForComposing',
		'isFirstLine',
		'actualHeight',
		'anythingComposed',
		'editor'
	],
	#category : 'Morphic-Text Support',
	#package : 'Morphic',
	#tag : 'Text Support'
}

{ #category : 'as yet unclassified' }
TextComposer >> addEmptyTrailingLine: isANewParagraph [
	"The line to add is usually the first line of a new paragraph (if last char in text was newLine),
	but it can be a new line in same paragraph (if enough spaces ended last line)."
	| ts f h bs r lm rm w a leftMarginForAlignment s |
	s := theText size+1.
	f := editor
		ifNotNil: [ editor lastFont ]
		ifNil: [ theText fontAt: s default: self defaultFont ].
	ts := editor
		ifNotNil: [ editor lastParagraphStyleOrNil ]
		ifNil: [ theText paragraphStyleOrNilAt: s].

	h := f lineSpacing.
	bs := f ascent.
	lm := 0.
	rm := 0.
	w := extentForComposing x.
	a := 0.
	ts ifNotNil: [
		isANewParagraph ifTrue: [
			h := h + ts spaceBefore.
			bs := bs + ts spaceBefore ].
		lm := ((isANewParagraph and: [ ts isListStyle not ])
			ifTrue: [ ts firstIndent ]
			ifFalse: [ ts restIndent ]).
		rm := ts rightIndent.
		a := ts alignment ].
	
	leftMarginForAlignment := a = CharacterScanner rightFlushCode
		ifTrue: [ w - rm]
		ifFalse: [
			a = CharacterScanner centeredCode
				ifTrue: [ (w - rm - lm) //2 + lm]
				ifFalse: [ lm ]].
	r := leftMarginForAlignment @ currentY extent: 0@h.

	lines addLast: (
		EmptyLine new
			firstIndex: s lastIndex: s - 1;
			rectangle: r;
			lineHeight: h baseline: bs;
			paragraphStyle: ts)
]

{ #category : 'as yet unclassified' }
TextComposer >> checkIfReadyToSlide [

	"Check whether we are now in sync with previously composed lines"

	(possibleSlide and: [currCharIndex > stopCharIndex]) ifFalse: [^self].

	[prevIndex < prevLines size
		and: [(prevLines at: prevIndex) first < (currCharIndex - deltaCharIndex)]]
			whileTrue: [prevIndex := prevIndex + 1].

	(prevLines at: prevIndex) first = (currCharIndex - deltaCharIndex) ifTrue: [
		"Yes -- next line will have same start as prior line."
		prevIndex := prevIndex - 1.
		possibleSlide := false.
		nowSliding := true
	] ifFalse: [
		prevIndex = prevLines size ifTrue: [
			"Weve reached the end of prevLines, so no use to keep looking for lines to slide."
			possibleSlide := false
		]
	]
]

{ #category : 'as yet unclassified' }
TextComposer >> composeAllLines [

	[currCharIndex <= theText size and: [
			currentY < extentForComposing y ]] whileTrue: [

		nowSliding ifTrue: [
			self slideOneLineDown ifNil: [^nil].
		] ifFalse: [
			self composeOneLine ifNil: [^nil].
		]
	].

]

{ #category : 'as yet unclassified' }
TextComposer >> composeAllRectangles: rectangles [

	| charIndexBeforeLine numberOfLinesBefore |

	actualHeight := 1.
	charIndexBeforeLine := currCharIndex.
	numberOfLinesBefore := lines size.
	self composeEachRectangleIn: rectangles.

	currentY := currentY + actualHeight.
	currentY > extentForComposing y ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex := charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [ lines removeLast ].
		^ self ].
	
	"It's OK -- the line still fits."
	maxRightX := maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline ].

	anythingComposed := true.
	isFirstLine := currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator]
]

{ #category : 'as yet unclassified' }
TextComposer >> composeEachRectangleIn: rectangles  [
	| myLine lastChar |
	1 to: rectangles size
		do: 
			[:i | 
			currCharIndex <= theText size ifFalse: [^false].
			myLine := scanner 
						composeFrom: currCharIndex
						inRectangle: (rectangles at: i)
						firstLine: isFirstLine
						leftSide: i = 1
						rightSide: i = rectangles size.
			lines addLast: myLine.
			actualHeight := actualHeight max: myLine lineHeight.	"includes font changes"
			currCharIndex := myLine last + 1.
			lastChar := theText at: myLine last.
			lastChar isLineSeparator ifTrue: [^#newLine]].
	^false
]

{ #category : 'as yet unclassified' }
TextComposer >> composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing [

	lines := argLinesCollection.
	theText := argText.
	extentForComposing := argExtentForComposing.
	deltaCharIndex := argDelta.
	currCharIndex := startCharIndex := argStart.
	stopCharIndex := argStop.
	prevLines := argPriorLines.
	currentY := argStartY.
	maxRightX := 0.
	possibleSlide := stopCharIndex < theText size.
	nowSliding := false.
	prevIndex := 1.
	scanner := CompositionScanner new text: theText.
	scanner defaultFont: self defaultFont.
	isFirstLine := currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed := false.
	self composeAllLines.
	(anythingComposed not or: [ theText isLastLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine: true ]
		ifFalse: [
			scanner lastLineBreakingSpace = theText size ifTrue: [
				self addEmptyTrailingLine: false ]].
	^ {lines asArray. maxRightX}


]

{ #category : 'as yet unclassified' }
TextComposer >> composeOneLine [
	| rectangles |
	rectangles := self rectanglesAt: currentY.
	rectangles notEmpty 
		ifTrue: [(self composeAllRectangles: rectangles) ifNil: [^nil]]
		ifFalse: [currentY := currentY + 1].
	self checkIfReadyToSlide
]

{ #category : 'accessing' }
TextComposer >> defaultFont [
	^editor ifNil: [ FontFamily defaultFamilyAndPointSize ] ifNotNil: [ editor defaultFont ]
]

{ #category : 'accessing' }
TextComposer >> editor: anEditor [
	editor := anEditor
]

{ #category : 'as yet unclassified' }
TextComposer >> rectanglesAt: y [
	(y+1) > extentForComposing y ifTrue: [^ #()].
	^ Array with: (0 @ y corner: extentForComposing x @ (y+1))
]

{ #category : 'as yet unclassified' }
TextComposer >> slideOneLineDown [
	"Having detected the end of rippling recoposition, we are only sliding old lines"

	| priorLine |
	prevIndex < prevLines size 
		ifFalse: 
			["There are no more prevLines to slide."

			^nowSliding := possibleSlide := false].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex) slideIndexBy: deltaCharIndex
				andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1
]
