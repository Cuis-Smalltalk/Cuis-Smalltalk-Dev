'From Cuis 6.0 [latest update: #6088] on 22 November 2023 at 3:12:19 pm'!
!classDefinition: #CharacterBlockScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CharacterBlockScanner
	instanceVariableNames: 'characterPoint characterIndex specialWidth lastCharacterWidth lastSpaceOrTabWidth forCursorPosition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/22/2023 12:35:16'!
delimitedRangeAt: index hitOnRightHalf: hitOnRightHalf leftDelimiters: leftDelimiters rightDelimiters: rightDelimiters simpleDelimiters: simpleDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction leftMatch rightMatch simpleMatch includeDelimiters level string here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := index min: sSize max: 2.
	initialDelimiter := string at: here.
	leftMatch := leftDelimiters indexOf: initialDelimiter.
	rightMatch := rightDelimiters indexOf: initialDelimiter.
	simpleMatch := simpleDelimiters  indexOf: initialDelimiter.
	leftMatch + rightMatch + simpleMatch = 0 ifTrue: [
		"no delimiters, then just select word."
		^self wordRangeIncluding: index ].

	(simpleMatch > 0 & hitOnRightHalf or: [leftMatch > 0])
		ifTrue: [	"Hit on right half of simple delimiter or is a left delimiter -- match to the right"
			start := here.
			direction := 1 ]
		ifFalse: [ 	"Hit on left half of simple delimiter or is a right delimiter-- match to the left"
			stop := here.
			direction :=  -1 ].
	
	simpleMatch > 0
		ifTrue: [
			finalDelimiter := initialDelimiter.
			includeDelimiters := false ]
		ifFalse: [
			leftMatch > 0
				ifTrue: [
					finalDelimiter := rightDelimiters at: leftMatch.
					includeDelimiters := hitOnRightHalf not ]
				ifFalse: [
					finalDelimiter := leftDelimiters at: rightMatch.
					includeDelimiters := hitOnRightHalf ]].

	level := 1.
	[ level > 0 and: [ direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			here := here + direction.
			hereChar := string at: here.
			"delimiter match just counts nesting level"
			hereChar = finalDelimiter
				ifTrue: [level := level - 1"leaving nest"]
				ifFalse: [
					hereChar = initialDelimiter 
						ifTrue: [level := level + 1"entering deeper nest" ]]].
	level > 0 ifTrue: [
		"If we failed to find final delimiter, then just select word."
		^self wordRangeIncluding: index ].
	direction > 0
		ifTrue: [ stop := here ]
		ifFalse: [ start := here ].
	^includeDelimiters
		ifTrue: [ start to: stop ]
		ifFalse: [ start+1 to: stop-1 ]! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 11/22/2023 15:00:55'!
characterBlockAtPoint: aPoint in: textLine forCursorPosition: aBoolean
	"This method is the Morphic characterBlock finder.

	If aBoolean, find the CharacterBlock for the character at the right of where
	the text cursor should be positioned if user clicked in aPoint.
	This is useful for positioning the text cursor, and also for selecting text.

	If aBoolean is false, find the CharacterBlock that actually contains aPoint.
	This is useful, for example, for selecting enclosed text between delimiters."

	| runLength lineStop stopCondition string |
	forCursorPosition := aBoolean.
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := nil.  "nil means scanning for point"
	characterPoint := aPoint.
	characterPoint y > line bottom
		ifTrue: [ characterPoint := line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])]) ifTrue:	 [
		^ CharacterBlock
			stringIndex: line first
			text: text
			topLeft: line leftMargin@line top
			extent: 0 @ line lineHeight
			textLine: line ].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := line last.			"scanning for point"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth := specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			"Result for characterBlockAtPoint: 
			Not actually the character we hit with the mouse,
			but the one at the right of the desired cursor position."
			^ CharacterBlock
				stringIndex: lastIndex
				text: text
				topLeft: characterPoint x@line top
				extent: lastCharacterWidth @ line lineHeight
				textLine: line ]
		] repeat! !

!CharacterBlockScanner methodsFor: 'initialization' stamp: 'jmv 11/22/2023 11:36:31'!
initialize
	super initialize.
	forCursorPosition := true.! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 11/22/2023 15:01:34'!
characterBlockIncludingPoint: aPoint 
	"Answer the CharacterBlock that actually contains aPoint.
	This is useful, for example, for selecting enclosed text between delimiters.
	See also #characterBlockAtPoint:"
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint in: line forCursorPosition: false.! !


!TextEditor methodsFor: 'new selection' stamp: 'jmv 11/22/2023 11:48:51'!
selectWordOrDelimitedTextAt: localEventPosition
	"Select delimited text or word--the result of double-clicking."

	| hitCharacterBlock range |
	hitCharacterBlock := textComposition characterBlockIncludingPoint: localEventPosition.
	range := self
		delimitedRangeAt: hitCharacterBlock stringIndex
		hitOnRightHalf: localEventPosition x  > hitCharacterBlock center x
		leftDelimiters: '([{<'
		rightDelimiters: ')]}>'
		simpleDelimiters: '|''"`'.
	self selectFrom: range first to: range last.! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 11/22/2023 15:03:12'!
crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| lastChar currentX delta |
	"Scanning for index"
	characterIndex ifNotNil: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex := characterIndex.
			characterPoint := leftMargin @ (destY + line lineHeight).
			^true ]].

	"Pointing before middle of a character.
	This is to know where to place cursor."
	delta := forCursorPosition ifTrue: [lastCharacterWidth // 2] ifFalse: [lastCharacterWidth].
	characterPoint x <= (destX + delta) ifTrue: [
		characterPoint := destX - (font isBold ifTrue: [1] ifFalse: [0]) @ destY.
		^true ].

	"Pointing past the end"
	lastIndex >= line last ifTrue: [
		characterPoint := destX @ destY.
		^true ].

	"Pointing past middle of a character, return the next character.
	This is to know where to place cursor."
	lastIndex := lastIndex + 1.
	currentX := destX + lastCharacterWidth - (font isBold ifTrue: [2] ifFalse: [0]).
	lastChar := text at: lastIndex.
	lastCharacterWidth := font widthOf: lastChar.
	characterPoint := currentX @ destY.
	lastChar = Character space ifFalse: [
		^ true].

	"Yukky if next character is space or tab."
	alignment = CharacterScanner justifiedCode ifTrue: [
		lastCharacterWidth := lastCharacterWidth + (line justifiedPadFor: (spaceCount + 1)).
		^ true].

	^ true! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 11/22/2023 09:53:03'!
characterBlockAtIndex: index in: textLine
	"This method is the Morphic characterBlock finder.
	Answer the CharacterBlock containing the character at index."
	| runLength lineStop stopCondition string |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := index.  "nil means scanning for point"
	characterPoint := line bottomRight.
	(text isEmpty or: [ characterIndex < line first ]) ifTrue:	 [
		^ CharacterBlock
			stringIndex: line first
			text: text
			topLeft: line leftMargin@line top
			extent: 0 @ line lineHeight
			textLine: line ].

	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := characterIndex.	"scanning for index"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth := specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			"Result for characterBlockForIndex: "
			^ CharacterBlock
				stringIndex: characterIndex
				text: text
				topLeft: characterPoint x@line top
				extent: lastCharacterWidth @ line lineHeight
				textLine: line ]
		] repeat! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 11/22/2023 15:01:21'!
characterBlockAtPoint: aPoint 
	"Answer the CharacterBlock for the character at the right of where
	the text cursor should be positioned if user clicked in aPoint.
	This is useful for positioning the text cursor, and also for selecting text.
	See also #characterBlockIncludingPoint:"
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint in: line forCursorPosition: true.! !

!methodRemoval: CharacterBlockScanner #characterBlockAtPoint:in: stamp: 'jmv 11/22/2023 14:52:26'!
CharacterBlockScanner removeSelector: #characterBlockAtPoint:in:!
!methodRemoval: Editor #wordRangeLeftDelimiters:rightDelimiters: stamp: 'jmv 11/22/2023 14:55:34'!
Editor removeSelector: #wordRangeLeftDelimiters:rightDelimiters:!
!classDefinition: #CharacterBlockScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CharacterBlockScanner
	instanceVariableNames: 'characterPoint characterIndex specialWidth lastCharacterWidth lastSpaceOrTabWidth forCursorPosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
