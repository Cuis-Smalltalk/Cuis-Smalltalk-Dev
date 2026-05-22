"
Count := 0.
"
Class {
	#name : 'DifferenceFinder',
	#superclass : 'Object',
	#instVars : [
		'base',
		'case',
		'x',
		'y',
		'map',
		'tally',
		'matrix',
		'differences'
	],
	#category : 'Tools-Code Differ',
	#package : 'Tools',
	#tag : 'Code Differ'
}

{ #category : 'instance creation' }
DifferenceFinder class >> base: aCollection case: anotherCollection [
	^self new base: aCollection case: anotherCollection

]

{ #category : 'instance creation' }
DifferenceFinder class >> charactersOf: aString and: anotherString [
	| finder |
	finder := self base: aString case: anotherString.
	finder compareCharacters.
	^finder

]

{ #category : 'constants' }
DifferenceFinder class >> d [
	^1
]

{ #category : 'compatibility' }
DifferenceFinder class >> displayPatchFrom: srcString to: dstString tryWords: aBoolean [
	| finder |

	aBoolean ifTrue: [
		(self wordsDisplayPatchFrom: srcString to: dstString)
			ifNotNil: [ :answer | ^answer ] ].

	finder := self base: srcString case: dstString.
	finder compareLines; compute.
	^finder differences anyOne asText
]

{ #category : 'compatibility' }
DifferenceFinder class >> displayPatchFrom: srcString to: dstString tryWords: aBoolean prettyPrintedIn: aClass [
	| formattedSrcString formattedDstString |
	formattedSrcString := aClass
		ifNil: [ srcString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: srcString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					srcString ]].
	formattedDstString := aClass
		ifNil: [ dstString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: dstString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					dstString ]].
	^ self
		displayPatchFrom: formattedSrcString
		to: formattedDstString
		tryWords: aBoolean
]

{ #category : 'constants' }
DifferenceFinder class >> l [
	^3
]

{ #category : 'instance creation' }
DifferenceFinder class >> linesOf: aString and: anotherString [
	| finder |
	finder := self base: aString case: anotherString.
	finder compareLines.
	^finder

]

{ #category : 'bibliography' }
DifferenceFinder class >> references [
	^'http://en.wikipedia.org/wiki/Longest_common_subsequence'

]

{ #category : 'constants' }
DifferenceFinder class >> u [
	^2
]

{ #category : 'constants' }
DifferenceFinder class >> ul [
	^4
]

{ #category : 'compatibility' }
DifferenceFinder class >> wordsDisplayPatchFrom: srcStringOrText to: dstStringOrText [

	| finder answer src1 dst1 changedCount dstString srcString |
	srcString := srcStringOrText asPlainString.
	dstString := dstStringOrText asPlainString.
	srcString class == dstString class ifFalse: [
		^self wordsDisplayPatchFrom: srcString asUnicodeString to: dstString asUnicodeString ].

	finder := self base: srcString case: dstString.
	finder compareLines; compute.
	answer := srcString class new asText.
	src1 := srcString class writeStream.
	dst1 := srcString class writeStream.
	changedCount := 0.
	finder differences sort first do: [:item :condition |
		condition caseOf: {
			[ #unchanged ] -> [
				changedCount > 0 ifTrue: [
					"If the sequence of changed lines is large, comparing words gets too slow and less useful"
					changedCount > 30 ifTrue: [
						^nil ].
					"Compare the just ended sequence of changed lines"
					finder base: src1 contents case: dst1 contents.
					finder compareWords; compute: true.
					finder differences ifNil: [ ^nil ].
					answer := answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount := 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer := answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount := changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount := changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer := answer append: finder differences anyOne asText.

	^answer
]

{ #category : 'instance creation' }
DifferenceFinder class >> wordsOf: aString and: anotherString [
	| finder |
	finder := self base: aString case: anotherString.
	finder compareWords.
	^finder

]

{ #category : 'accessing' }
DifferenceFinder >> base: aCollection case: anotherCollection [
	base := aCollection.
	case := anotherCollection.
	x := aCollection.
	y := anotherCollection

]

{ #category : 'configuration' }
DifferenceFinder >> compareCharacters [
	x := base.
	y := case

]

{ #category : 'configuration' }
DifferenceFinder >> compareLines [
	x := self linesIn: base.
	y := self linesIn: case

]

{ #category : 'configuration' }
DifferenceFinder >> compareWords [
	x := self keywordsAndBlanksFrom: base.
	y := self keywordsAndBlanksFrom: case

]

{ #category : 'computing' }
DifferenceFinder >> compute [
	^self compute: false
]

{ #category : 'computing' }
DifferenceFinder >> compute: abortIfTooExpensive [
	"If abortIfTooExpensive, we might abort, and then differences could be nil."
	| longestSequences |
	self initializeMap; initializeMatrix; computeMap.
	longestSequences := self longestSequences: abortIfTooExpensive.
	"If decided computation was too expensive..."
	longestSequences ifNil: [
		differences := nil.
		^self ].
	differences := longestSequences asArray collect: [ :lcs |
		SequenceDifference x: x y: y lcs: lcs].
	differences sort
]

{ #category : 'computing' }
DifferenceFinder >> computeLcsAt: i at: j [
	| mij cij pair left up |
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		cij := self lcsAt: i - 1 at: j - 1.
		pair := Array with: i with: j.
		^cij collect: [:s | s copyWith: pair]].
	mij = self class u ifTrue: [^self lcsAt: i - 1 at: j].
	mij = self class l ifTrue: [^self lcsAt: i at: j - 1].
	mij = self class ul ifTrue: [
		left := self lcsAt: i at: j - 1.
		up := self lcsAt: i - 1 at: j.
		^left copy addAll: up; yourself].
	self assert: false
]

{ #category : 'computing' }
DifferenceFinder >> computeMap [
	| m |
	tally := Array2D height: x size width: y size.
	1 to: x size do: [ :i | 
		1 to: y size do: [ :j | 
			m := self computeMapAt: i at: j.
			map i: i j: j put: m ]]
]

{ #category : 'computing' }
DifferenceFinder >> computeMapAt: i at: j [
	| xi yj left up t |
	xi := x at: i.
	yj := y at: j.
	xi = yj ifTrue: [
		t := ((j = 1 or: [i = 1])
			ifTrue: [0]
			ifFalse: [tally i: i - 1 j: j - 1])
			+ 1.
		tally i: i j: j put: t.
		^self class d].
	left := j = 1 ifTrue: [0] ifFalse: [tally i: i j: j - 1].
	up := i = 1 ifTrue: [0] ifFalse: [tally i: i - 1 j: j].
	left < up ifTrue: [
		tally i: i j: j put: up.
		^self class u].
	tally i: i j: j put: left.
	^up < left ifTrue: [self class l] ifFalse: [self class ul]

]

{ #category : 'outputs' }
DifferenceFinder >> differences [
	^differences

]

{ #category : 'private' }
DifferenceFinder >> initializeMap [
	map := Array2D height: x size width: y size
]

{ #category : 'private' }
DifferenceFinder >> initializeMatrix [
	matrix := Array2D height: x size width: y size
]

{ #category : 'private' }
DifferenceFinder >> keywordsAndBlanksFrom: aString [
	^Array streamContents: [:strm | | read keyword tail |
		read := aString readStream.
		[read atEnd] whileFalse: [
			keyword := read nextKeyword.
			keyword notEmpty ifTrue: [
				strm nextPut: keyword ].
			tail := read untilAnySatisfying: [:ch | ch isValidInIdentifiers].
			tail notEmpty ifTrue: [strm nextPut: tail]]]

]

{ #category : 'computing' }
DifferenceFinder >> lcsAt: i at: j [
	| lcs |
	(i = 0 or: [j = 0]) ifTrue: [^Set with: #() "EmptyLCS"].
	lcs := matrix i: i j: j.
	lcs ifNil: [
		lcs := self computeLcsAt: i at: j.
		matrix i: i j: j put: lcs].
	^lcs

]

{ #category : 'private' }
DifferenceFinder >> linesIn: aString [
	"
	LongestCommonSequenceFinder linesIn: 'x y'
	"
	^Array streamContents: [:strm | | cr read |
		cr := '
'.
		read := aString readStream.
		[read atEnd] whileFalse: [| line |
			line := read nextLine.
			read skipBack.
			read peek = cr last ifTrue: [line := line , cr].
			read skip.
			strm nextPut: line]]

]

{ #category : 'computing' }
DifferenceFinder >> longestSequences: abortIfTooExpensive [
	| maxs points answer |
	maxs := self maxLengthPoints.
	points := self unfold: maxs.
	abortIfTooExpensive ifTrue: [
		points size > 500 ifTrue: [ ^nil ].	"maybe a bit too much..."
	].
	points
		sort: [:p :q | p x < q x or: [p x = q x and: [p y <= q y]]];
		do: [:p | self lcsAt: p x at: p y].
	answer := Set new.
	maxs do: [ :p | | lcs |
		lcs := self lcsAt: p x at: p y.
		lcs do: [ :s | 
			answer add: s]].
	^answer
]

{ #category : 'private' }
DifferenceFinder >> maxLength [

	(tally width = 0 or: [ tally height = 0 ]) ifTrue: [ ^0 ].
	^tally i: x size j: y size
]

{ #category : 'private' }
DifferenceFinder >> maxLengthPoints [
	| max points |
	max := self maxLength.
	max = 0 ifTrue: [^Array with: `0 @ 0`].
	points := OrderedCollection new.
	tally withIndexesDo: [:i :j :t | t = max ifTrue: [points add: i @ j]].
	^ points
]

{ #category : 'private' }
DifferenceFinder >> similitudeProportion [
	^self maxLength / (x size + y size / 2)
]

{ #category : 'private' }
DifferenceFinder >> unfold [
	| points |
	points := OrderedCollection with: x size @ y size.
	^self unfold: points

]

{ #category : 'private' }
DifferenceFinder >> unfold: pointCollection [
	| pending visited point |
	pending := OrderedCollection withAll: pointCollection.
	visited := OrderedCollection new.
	[pending notEmpty] whileTrue: [
		point := pending removeFirst.
		(visited includes: point) ifFalse: [
			self unfold: point on: pending.
			visited add: point]].
	^visited

]

{ #category : 'private' }
DifferenceFinder >> unfold: aPoint on: pending [
	| i j mij |
	i := aPoint x.
	j := aPoint y.
	(i = 0 or: [j = 0]) ifTrue: [^self].
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		pending add: i - 1 @ (j - 1).
		^self].
	mij = self class u ifTrue: [
		pending add: i - 1 @ j.
		^self].
	mij = self class l ifTrue: [
		pending add: i @ (j - 1).
		^self].
	mij = self class ul ifTrue: [
		pending add: i - 1 @ j; add: i @ (j - 1).
		^self].
	self assert: false
]
