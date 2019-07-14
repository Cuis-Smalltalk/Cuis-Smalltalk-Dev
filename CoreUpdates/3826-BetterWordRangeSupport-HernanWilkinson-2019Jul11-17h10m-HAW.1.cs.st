'From Cuis 5.0 [latest update: #3823] on 11 July 2019 at 5:44:59 pm'!

!Editor methodsFor: 'new selection' stamp: 'HAW 7/11/2019 17:31:59'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters startingAt: pointIndex
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^1 to: 1].
	here _ pointIndex min: string size max: 2.
	initialDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			finalDelimiter _ rightDelimiters at: match]
		ifFalse: [
			initialDelimiter _ string at: here.
			match _ rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					finalDelimiter _ leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction _ -1]].
	level _ 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar _ string at: (here _ here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start _ 1.
									"go right if hit string start"
									direction _ 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start _ here + 1.
									"go right if hit non-token"
									direction _ 1]
								ifFalse: [level _ 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level _ level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		leftDelimiters size + rightDelimiters size = 0 ifFalse: [
			"If we failed to find final delimiter, then just select word."
			^self wordRangeLeftDelimiters: '' rightDelimiters: '' ].
		here _ here + direction ].
	^ direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			here + 1 to: stop]! !

!Editor methodsFor: 'new selection' stamp: 'HAW 7/11/2019 17:37:54'!
wordRangeUnder: aPositionInText
	
	^self wordRangeLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters startingAt: aPositionInText ! !


!TextEditor methodsFor: 'model access' stamp: 'HAW 7/11/2019 17:37:01'!
wordUnder: aPositionInText
	
	| wordUnderCursorRange word indexOfSpace |
	
	wordUnderCursorRange := self wordRangeUnder: aPositionInText.
	word := (model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asString.
	
	"I have to handle the edge case where the cursor is for example between a ' and the first letter of the word.
	In that case the range will include words with spaces - Hernan"
	indexOfSpace := word indexOf: $  ifAbsent: [ ^word ].
	
	^word first: indexOfSpace -1 
	
			! !


!Editor methodsFor: 'new selection' stamp: 'HAW 7/11/2019 17:32:34'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	^self wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters  startingAt: self pointIndex ! !


!TextEditor methodsFor: 'model access' stamp: 'HAW 7/11/2019 17:39:22'!
wordUnderCursor
	
	^self wordUnder: self pointIndex! !

!methodRemoval: Editor #wordUnderCursorRange!
Editor removeSelector: #wordUnderCursorRange!
