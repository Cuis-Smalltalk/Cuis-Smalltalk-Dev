'From Cuis 6.0 [latest update: #6101] on 29 November 2023 at 10:09:13 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/29/2023 10:03:26'!
delimitedRangeAt: index hitOnRightHalf: hitOnRightHalfOfDelimiter leftDelimiters: leftDelimiters rightDelimiters: rightDelimiters simpleDelimiters: simpleDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction leftMatch rightMatch simpleMatch includeDelimiters level string hitOnRightHalf here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	hitOnRightHalf := hitOnRightHalfOfDelimiter.
	here := index min: sSize max: 2.
	((string at: here) isLineSeparator and: [ here > 2 ]) ifTrue: [
		here := here - 1.
		hitOnRightHalf := true ].
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

