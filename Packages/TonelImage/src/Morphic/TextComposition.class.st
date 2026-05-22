"
A TextComposition represents text that has been laid out, or composed, in some container.
(This class was formerly known as Paragraph).
	text 		A Text with encoded per-character emphasis.
	lines		An Array of TextLines comprising the final layout of the text
				after it has been composed within its container.
Lines are ordered vertically. However, for a given y, there may be several lines in left to right order. Lines must never be empty, even if text is empty.
"
Class {
	#name : 'TextComposition',
	#superclass : 'Object',
	#instVars : [
		'model',
		'extentForComposing',
		'lines',
		'maxRightX',
		'editor',
		'showTextCursor',
		'lastTextCursorRect',
		'selectionStartBlock',
		'selectionStopBlock'
	],
	#category : 'Morphic-Text Support',
	#package : 'Morphic',
	#tag : 'Text Support'
}

{ #category : 'selection' }
TextComposition >> addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: aStream [
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w line |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 := characterBlock1.  cb2 := characterBlock2]
		ifFalse: [cb2 := characterBlock1.  cb1 := characterBlock2].
	cb1 = cb2 ifTrue: [
		w := 6.
		^ aStream nextPut: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 := self lineIndexFor: cb1 stringIndex.
	line2 := self lineIndexFor: cb2 stringIndex.
	line1 = line2 ifTrue: [
		^ aStream nextPut: (cb1 topLeft corner: cb2 bottomRight)].
	rects := OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		line := lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect := rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].
	aStream nextPutAll: rects.
	aStream nextPut: ((lines at: line2) topLeft corner: cb2 bottomLeft)
]

{ #category : 'selection' }
TextComposition >> characterBlockAtPoint: aPoint  [
	"Answer the CharacterBlock for the character at the right of where
	the text cursor should be positioned if user clicked in aPoint.
	This is useful for positioning the text cursor, and also for selecting text.
	See also #characterBlockIncludingPoint:"
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint in: line forCursorPosition: true.
]

{ #category : 'selection' }
TextComposition >> characterBlockForIndex: index  [
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line := lines at: (self lineIndexFor: index).
	t := model actualContents.
	^ (CharacterBlockScanner new text: t)
		defaultFont: self defaultFont;
		characterBlockAtIndex: ((index max: line first) min: t size+1) in: line.
]

{ #category : 'selection' }
TextComposition >> characterBlockIncludingPoint: aPoint  [
	"Answer the CharacterBlock that actually contains aPoint.
	This is useful, for example, for selecting enclosed text between delimiters.
	See also #characterBlockAtPoint:"
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint in: line forCursorPosition: false.
]

{ #category : 'editing' }
TextComposition >> clickAt: clickPoint [
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| action startBlock t |
	action := false.
	startBlock := self characterBlockAtPoint: clickPoint.
	t := model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue: [ | target range boxes box |
				(target := model) ifNil: [ target := editor morph].
				range := t rangeOf: att startingAt: startBlock stringIndex.
				boxes := self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box := boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box := editor morph displayBounds.
					editor morph allOwnersDo: [ :m | box := box intersect: (m displayBounds) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action := true]].
				]]].
	^ action
]

{ #category : 'composition' }
TextComposition >> composeAll [
	"Full recomposition computes actual right border of text,
	only limited by longest line and (extentForComposing x)"
	maxRightX := self
		composeLinesFrom: 1
		to: model textSize
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: 0
]

{ #category : 'composition' }
TextComposition >> composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY  [
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult composer |
	composer := TextComposer new.
	editor ifNotNil: [
		composer editor: editor ].
	newResult := composer
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				text: model actualContents
				extentForComposing: extentForComposing.
	lines := newResult first asArray.

	"If there is any justified, rightFlush or centered text, assume all available width has been used."
	model actualContents hasAnyTextAlignment ifTrue: [ ^extentForComposing x ].

	^newResult second "right border of recomposed lines (bounded by extentForComposing x)"
]

{ #category : 'selection' }
TextComposition >> defaultCharacterBlock [
	^ CharacterBlock
		stringIndex: 1
		text: model actualContents
		topLeft: lines first topLeft
		extent: `0 @ 0`
		textLine: lines first
]

{ #category : 'access' }
TextComposition >> defaultFont [
	^editor ifNil: [ FontFamily defaultFamilyAndPointSize ] ifNotNil: [ editor defaultFont ]
]

{ #category : 'display' }
TextComposition >> displaySelectionInLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc [

	"textTopLeft is relative to the morph currently being drawn"
	selectionStartBlock ifNotNil: [
		self
			displaySelectionStartBlock: selectionStartBlock
			stopBlock: selectionStopBlock
			InLine: line
			on: aCanvas
			textTopLeft: textTopLeft
			selectionColor: sc ].
]

{ #category : 'display' }
TextComposition >> displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc [
	"textTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx textCursorFont t b textCursorAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show text cursor on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showTextCursor ifTrue: [
				leftX := textTopLeft x + startBlock left.
				idx := startBlock stringIndex.
				textCursorAttributes := editor ifNotNil: [ editor currentAttributes ].
				textCursorFont := textCursorAttributes
					ifNil: [ model actualContents fontAt: idx default: self defaultFont ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes default: self defaultFont ].
				b := textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t := textTopLeft y + line top + line baseline - textCursorFont ascent+1.
				lastTextCursorRect := nil.
				self
					displayTextCursorAtX: leftX
					top: t
					bottom: b
					emphasis: textCursorFont emphasis
					on: aCanvas
					textLeft: textTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastTextCursorRect := nil.
			leftX :=  textTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX :=  textTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  textTopLeft y) corner: rightX @ (line bottom +  textTopLeft y))
				color: sc ].	"Selection begins on line below"
]

{ #category : 'display' }
TextComposition >> displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft [
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d extraW corner origin |
	isBold := emphasis allMask: 1.
	isItalic := emphasis allMask: 2.
	textCursorColor := Theme current textCursor.
	h := bottom - top.
	w := isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW := w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d := isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 := x- (h*5//24) + d.
			x1 := x + d ]
		ifFalse: [
			x0 := x - halfW.
			x1 := x - halfW].
	x0-textLeft < -1 ifTrue: [
		x1 := x1 - x0 + textLeft.
		x0 := textLeft ].
	r := extentForComposing x-halfW-1.
	r < (x1-textLeft) ifTrue: [
		x0 := x0 + r - x1+textLeft.
		x1 := r +textLeft].
	extraW := isBold
		ifTrue: [ 3 ]
		ifFalse: [ 2 ].
	origin := x0-halfW@ top.
	corner := x1+halfW+extraW @ (bottom+2).
	lastTextCursorRect
		ifNil: [ lastTextCursorRect := origin corner: corner ]
		ifNotNil: [lastTextCursorRect
			setOrigin: (lastTextCursorRect origin min: origin)
			corner: (lastTextCursorRect corner max: corner)].
	aCanvas
		line: x0+halfW@(bottom-w-w+halfW) to: x1+halfW@top
		width: w color: textCursorColor
]

{ #category : 'access' }
TextComposition >> editor: anEditor [
	"Can be nil.
	But if not nil, must be the same editor used in any TextMorph that references us."
	editor := anEditor
]

{ #category : 'composition' }
TextComposition >> extentForComposing: aPoint [
	extentForComposing := aPoint
]

{ #category : 'private' }
TextComposition >> fastFindFirstLineSuchThat: lineBlock [
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."

	^lines
		findBinaryIndex: [ :each | 
			(lineBlock value: each)
				ifTrue: [ -1 ]
				ifFalse: [ 1 ] ]
		do: [ :found | found ]
		ifNone: [ :lower :upper | upper ]
]

{ #category : 'private' }
TextComposition >> indentationOfLineIndex: lineIndex ifBlank: aBlock [
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last str |
	str := model actualContents string.
	arrayIndex := lineIndex.
	[
		first := (lines at: arrayIndex) first.
		 first > 1 and: [(str at: first - 1) isLineSeparator not ] ] whileTrue: [ "word wrap"
			arrayIndex := arrayIndex - 1].
	last := (lines at: arrayIndex) last.
	
	^(str copyFrom: first to: last) indentationIfBlank: aBlock
]

{ #category : 'initialization' }
TextComposition >> initialize [
	showTextCursor := false.
	selectionStartBlock := nil.
	selectionStopBlock := nil.
]

{ #category : 'private' }
TextComposition >> lastLine [
	^lines last
]

{ #category : 'access' }
TextComposition >> lastTextCursorRect [
	"Warning: Could be not updated, it is the one for when the text cursor was last drawn."
	^lastTextCursorRect
]

{ #category : 'private' }
TextComposition >> lineIndexFor: characterIndex [
	"Answer the index of the line in which to select the character at index."
	^ (self fastFindFirstLineSuchThat: [:line | line first > characterIndex]) - 1 max: 1
]

{ #category : 'private' }
TextComposition >> lineIndexForPoint: aPoint [
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py := aPoint y truncated.

	"Find the first line at this y-value"
	i := (self fastFindFirstLineSuchThat: [ :line | line bottom > py]) min: lines size.

	"Now find the first line at this x-value"
	[ i < lines size and: [ (lines at: i+1) top = (lines at: i) top
				and: [ aPoint x >= (lines at: i+1) left ]]]
		whileTrue: [ i := i + 1 ].
	^ i
]

{ #category : 'private' }
TextComposition >> lines [
	^ lines
]

{ #category : 'access' }
TextComposition >> numberOfLines [

	^lines size
]

{ #category : 'composition' }
TextComposition >> recomposeFrom: startArg to: stopArg delta: delta [
	"Recompose this text.  The altered portion is between start and stop.
	Recomposition may continue to the end of the text, due to a ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed.
	Expand the requested interval to include whole paragraphs because there could be paragraph attributes."
	| intervalToFix start stop startLine newLines partialMaxRightX |
	"Have to recompose line above in case a word-break was affected."
	intervalToFix := model actualContents encompassParagraph: (startArg to: stopArg).
	start := intervalToFix first.
	stop := intervalToFix last.
	startLine := (self lineIndexFor: start) - 1 max: 1.
	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]
		whileTrue: [startLine := startLine - 1].  "Find leftmost of line pieces"
	newLines := OrderedCollection new.
	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].
	partialMaxRightX := self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top.
	"Partial recomposition computes actual right border only of recompsed text,
	so, it could make maxRightX larger but not smaller.
	This means that if the longest line gets shortened, we won't know, and maxRightX will be erroneously large"
	maxRightX := maxRightX max: partialMaxRightX
]

{ #category : 'selection' }
TextComposition >> selectionRects [
	"Return an array of rectangles representing the selection regions."
	^ Array streamContents: [ :strm |
		selectionStartBlock ifNotNil: [
			self addSelectionRectsFrom: selectionStartBlock to: selectionStopBlock to: strm ]].
]

{ #category : 'selection' }
TextComposition >> selectionRectsFrom: characterBlock1 to: characterBlock2  [
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	^ Array streamContents: [ :strm |
		self addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: strm ]
]

{ #category : 'selection' }
TextComposition >> selectionStartBlock: startBlock selectionStopBlock: stopBlockBlock [
	selectionStartBlock := startBlock.
	selectionStopBlock := stopBlockBlock.
]

{ #category : 'composition' }
TextComposition >> setModel: aTextModel [
	model := aTextModel
]

{ #category : 'access' }
TextComposition >> showTextCursor [
	^showTextCursor
]

{ #category : 'access' }
TextComposition >> showTextCursor: aBool [
	showTextCursor := aBool
]

{ #category : 'access' }
TextComposition >> textComposed [
	"Not named #text just to avoid false polyphormism"
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
]

{ #category : 'access' }
TextComposition >> usedExtent [
	"If there is any justified, rightFlush or centered text, assume all available width has been used."

	^ self usedWidth @ self usedHeight
]

{ #category : 'access' }
TextComposition >> usedHeight [
	^ lines last bottom - lines first top
]

{ #category : 'access' }
TextComposition >> usedWidth [
	"If there is any justified, rightFlush or centered text, assume all available width has been used."

	^ maxRightX
]
