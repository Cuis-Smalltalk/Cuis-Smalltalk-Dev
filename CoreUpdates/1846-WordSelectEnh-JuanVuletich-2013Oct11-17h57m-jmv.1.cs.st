'From Cuis 4.2 of 25 July 2013 [latest update: #1845] on 11 October 2013 at 5:58:56.65961 pm'!

!Editor methodsFor: 'new selection' stamp: 'jmv 10/9/2013 18:38'!
selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| openDelimiter closeDelimiter direction match level
	string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^self selectFrom: 1 to: 1].
	here _ self pointIndex min: string size max: 2.
	openDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: openDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			closeDelimiter _ rightDelimiters at: match]
		ifFalse: [
			openDelimiter _ string at: here.
			match _ rightDelimiters indexOf: openDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					closeDelimiter _ leftDelimiters at: match]
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
				ifTrue: ["token scan goes left, then right"
					hereChar isGenerallyValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [here = 1
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
				ifFalse: ["bracket match just counts nesting level"
					hereChar = closeDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [hereChar = openDelimiter 
									ifTrue: [level _ level + 1"entering deeper nest"]]]].

	level > 0 ifTrue: ["in case ran off string end"	here _ here + direction].
	direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			self selectFrom: start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			self selectFrom: here + 1 to: stop]! !

