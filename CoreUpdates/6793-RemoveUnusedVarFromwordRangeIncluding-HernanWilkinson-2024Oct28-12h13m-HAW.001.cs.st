'From Cuis7.1 [latest update: #6792] on 28 October 2024 at 12:14:10 pm'!

!Editor methodsFor: 'new selection' stamp: 'HAW 10/28/2024 12:13:31'!
wordRangeIncluding: aPositionInText
	"Select delimited text or word--the result of double-clicking."

	| direction level string here hereChar start sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := aPositionInText min: sSize max: 2.
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

