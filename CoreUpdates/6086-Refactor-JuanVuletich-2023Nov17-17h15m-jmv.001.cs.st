'From Cuis 6.0 [latest update: #6078] on 17 November 2023 at 5:15:40 pm'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 17:10:35'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction leftMatch rightMatch level string here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := self pointIndex min: sSize max: 2.
	initialDelimiter := string at: here - 1.
	leftMatch := leftDelimiters indexOf: initialDelimiter.
	rightMatch := rightDelimiters indexOf: initialDelimiter.
	leftMatch + rightMatch = 0 ifTrue: [
		"no delimiters, then just select word."
		^self wordRangeIncluding: self pointIndex ].
	leftMatch > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start := here.
			direction := 1.
			here := here - 1.
			finalDelimiter := rightDelimiters at: leftMatch]
		ifFalse: [
			initialDelimiter := string at: here.
			rightMatch := rightDelimiters indexOf: initialDelimiter.
			"delimiter is on right -- match to the left"
			stop := here - 1.
			direction := -1.
			finalDelimiter := leftDelimiters at: rightMatch].
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			"delimiter match just counts nesting level"
			hereChar = finalDelimiter
				ifTrue: [level := level - 1"leaving nest"]
				ifFalse: [
					hereChar = initialDelimiter 
						ifTrue: [level := level + 1"entering deeper nest"]]].
	level > 0 ifTrue: [
		"If we failed to find final delimiter, then just select word."
		^self wordRangeIncluding: self pointIndex ].
	^ direction > 0
		ifTrue: [start to: here - 1]
		ifFalse: [here + 1 to: stop]! !

