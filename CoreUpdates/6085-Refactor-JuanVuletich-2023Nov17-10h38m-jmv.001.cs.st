'From Cuis 6.0 [latest update: #6073] on 17 November 2023 at 10:53:39 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:50:13'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := self pointIndex min: sSize max: 2.
	initialDelimiter := string at: here - 1.
	match := leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start := here.
			direction := 1.
			here := here - 1.
			finalDelimiter := rightDelimiters at: match]
		ifFalse: [
			initialDelimiter := string at: here.
			match := rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop := here - 1.
					direction := -1.
					finalDelimiter := leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction := -1]].
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start := 1.
									"go right if hit string start"
									direction := 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start := here + 1.
									"go right if hit non-token"
									direction := 1]
								ifFalse: [level := 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level := level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level := level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		leftDelimiters size + rightDelimiters size = 0 ifFalse: [
			"If we failed to find final delimiter, then just select word."
			^self wordRangeIncluding: self pointIndex ].
		here := here + direction ].
	^ direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here := here + 1].
			start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			here + 1 to: stop]! !


!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:46:18'!
selectWord
	"Select exactly one word.
	See also #selectWordOrDelimitedText"

	| wordRange |
	wordRange := self wordRangeIncluding: self pointIndex.
	self selectFrom: wordRange first to: wordRange last! !

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:52:13'!
selectWordOrDelimitedText
	"Select delimited text or word--the result of double-clicking."

	| wordRange |
	wordRange := self
		wordRangeLeftDelimiters: '([{<|''"`'
				rightDelimiters: ')]}>|''"`'.
	self selectFrom: wordRange first to: wordRange last.! !

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:43:10'!
wordRangeIncluding: aPositionInText
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter direction level string here hereChar start sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := aPositionInText min: sSize max: 2.
	initialDelimiter := string at: here.
	"no delimiters -- select a token"
	direction := -1.
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			"token scan goes left, then right"
			hereChar isValidInIdentifiers 	"Consider $: as a word separator"
				ifTrue: [
					here = 1
						ifTrue: [
							start := 1.
							"go right if hit string start"
							direction := 1]]
				ifFalse: [
					direction < 0
						ifTrue: [
							start := here + 1.
							"go right if hit non-token"
							direction := 1]
						ifFalse: [level := 0]]].
	level > 0 ifTrue: [
		here := here + direction ].
	"If a word ends with $: (a keyword), consider it part of the word"
	hereChar = $: ifTrue: [here := here + 1].
	^start to: here - 1! !


!TextEditor methodsFor: 'events' stamp: 'jmv 11/17/2023 10:46:45'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| newPointBlock goingBackwards newStartBlock newStopBlock interval i1 i2 |
	newPointBlock := textComposition characterBlockAtPoint: localEventPosition.
	goingBackwards := newPointBlock stringIndex < markBlock stringIndex.

	doWordSelection ifTrue: [
		pointBlock := newPointBlock.
		self selectWord.
		newStartBlock := self startBlock min: initialSelectionStart.
		newStopBlock := self stopBlock max: initialSelectionStop.
		markBlock := goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock := goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	doParagraphSelection ifTrue: [
		i1 := newPointBlock stringIndex min: initialSelectionStart stringIndex.
		i2 := newPointBlock stringIndex max: initialSelectionStop stringIndex-1.
		interval := self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		newStartBlock := self startBlock min: initialSelectionStart.
		newStopBlock := self stopBlock max: initialSelectionStop.
		markBlock := goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock := goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	pointBlock := newPointBlock.
	self storeSelectionInComposition! !

!methodRemoval: Editor #wordRangeLeftDelimiters:rightDelimiters:startingAt: stamp: 'jmv 11/17/2023 10:50:28'!
Editor removeSelector: #wordRangeLeftDelimiters:rightDelimiters:startingAt:!
!methodRemoval: Editor #selectWordLeftDelimiters:rightDelimiters: stamp: 'jmv 11/17/2023 10:52:17'!
Editor removeSelector: #selectWordLeftDelimiters:rightDelimiters:!
