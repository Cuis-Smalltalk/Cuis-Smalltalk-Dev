'From Cuis 5.0 [latest update: #4708] on 27 July 2021 at 9:11:16 pm'!

!DifferenceFinder methodsFor: 'accessing' stamp: 'LC 1/24/2010 15:18'!
base: aCollection case: anotherCollection
	base := aCollection.
	case := anotherCollection.
	x := aCollection.
	y := anotherCollection
! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 8/23/2014 23:23'!
initializeMap
	map _ Array2D height: x size width: y size! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 8/23/2014 23:23'!
initializeMatrix
	matrix _ Array2D height: x size width: y size! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 11/16/2015 14:51'!
keywordsAndBlanksFrom: aString
	^Array streamContents: [:strm | | read keyword tail |
		read := aString readStream.
		[read atEnd] whileFalse: [
			keyword := read nextKeyword.
			keyword notEmpty ifTrue: [
				strm nextPut: keyword ].
			tail := read untilAnySatisfying: [:ch | ch isValidInIdentifiers].
			tail notEmpty ifTrue: [strm nextPut: tail]]]
! !

!DifferenceFinder methodsFor: 'private' stamp: 'LC 1/24/2010 15:18'!
linesIn: aString
	"
	LongestCommonSequenceFinder linesIn: 'x y'
	"
	^Array streamContents: [:strm | | cr read |
		cr := '
'.
		read := aString readStream.
		[read atEnd] whileFalse: [| line |
			line := read nextLine.
			read skip: -1.
			read peek = cr last ifTrue: [line := line , cr].
			read skip: 1.
			strm nextPut: line]]
! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 2/22/2010 11:36'!
maxLength

	(tally width = 0 or: [ tally height = 0 ]) ifTrue: [ ^0 ].
	^tally i: x size j: y size! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 3/12/2018 15:48:12'!
maxLengthPoints
	| max points |
	max := self maxLength.
	max = 0 ifTrue: [^Array with: `0 @ 0`].
	points := OrderedCollection new.
	tally withIndexesDo: [:i :j :t | t = max ifTrue: [points add: i @ j]].
	^ points! !

!DifferenceFinder methodsFor: 'private' stamp: 'LC 1/26/2010 10:21'!
similitudeProportion
	^self maxLength / (x size + y size / 2)! !

!DifferenceFinder methodsFor: 'private' stamp: 'LC 1/24/2010 15:18'!
unfold
	| points |
	points := OrderedCollection with: x size @ y size.
	^self unfold: points
! !

!DifferenceFinder methodsFor: 'private' stamp: 'LC 1/24/2010 15:18'!
unfold: pointCollection
	| pending visited point |
	pending := OrderedCollection withAll: pointCollection.
	visited := OrderedCollection new.
	[pending notEmpty] whileTrue: [
		point := pending removeFirst.
		(visited includes: point) ifFalse: [
			self unfold: point on: pending.
			visited add: point]].
	^visited
! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 4/17/2015 16:00'!
unfold: aPoint on: pending
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
	self assert: false! !

!DifferenceFinder methodsFor: 'configuration' stamp: 'LC 1/24/2010 15:18'!
compareCharacters
	x := base.
	y := case
! !

!DifferenceFinder methodsFor: 'configuration' stamp: 'LC 1/24/2010 15:18'!
compareLines
	x := self linesIn: base.
	y := self linesIn: case
! !

!DifferenceFinder methodsFor: 'configuration' stamp: 'LC 1/24/2010 15:18'!
compareWords
	x := self keywordsAndBlanksFrom: base.
	y := self keywordsAndBlanksFrom: case
! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 12/21/2012 13:51'!
compute
	^self compute: false! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 12/21/2012 13:53'!
compute: abortIfTooExpensive
	"If abortIfTooExpensive, we might abort, and then differences could be nil."
	| longestSequences |
	self initializeMap; initializeMatrix; computeMap.
	longestSequences _ self longestSequences: abortIfTooExpensive.
	"If decided computation was too expensive..."
	longestSequences ifNil: [
		differences _ nil.
		^self ].
	differences _ longestSequences asArray collect: [ :lcs |
		SequenceDifference x: x y: y lcs: lcs].
	differences sort! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:00'!
computeLcsAt: i at: j
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
	self assert: false! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 8/23/2014 23:23'!
computeMap
	| m |
	tally _ Array2D height: x size width: y size.
	1 to: x size do: [ :i | 
		1 to: y size do: [ :j | 
			m _ self computeMapAt: i at: j.
			map i: i j: j put: m ]]! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:00'!
computeMapAt: i at: j
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
! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:35'!
lcsAt: i at: j
	| lcs |
	(i = 0 or: [j = 0]) ifTrue: [^Set with: #() "EmptyLCS"].
	lcs := matrix i: i j: j.
	lcs ifNil: [
		lcs := self computeLcsAt: i at: j.
		matrix i: i j: j put: lcs].
	^lcs
! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 12/21/2012 13:50'!
longestSequences: abortIfTooExpensive
	| maxs points answer |
	maxs _ self maxLengthPoints.
	points _ self unfold: maxs.
	abortIfTooExpensive ifTrue: [
		points size > 500 ifTrue: [ ^nil ].	"maybe a bit too much..."
	].
	points
		sort: [:p :q | p x < q x or: [p x = q x and: [p y <= q y]]];
		do: [:p | self lcsAt: p x at: p y].
	answer _ Set new.
	maxs do: [ :p | | lcs |
		lcs _ self lcsAt: p x at: p y.
		lcs do: [ :s | 
			answer add: s]].
	^answer! !

!DifferenceFinder methodsFor: 'outputs' stamp: 'LC 1/24/2010 15:18'!
differences
	^differences
! !


!DifferenceFinder class methodsFor: 'instance creation' stamp: 'LC 1/24/2010 15:18'!
base: aCollection case: anotherCollection
	^self new base: aCollection case: anotherCollection
! !

!DifferenceFinder class methodsFor: 'instance creation' stamp: 'LC 1/24/2010 15:18'!
charactersOf: aString and: anotherString
	| finder |
	finder := self base: aString case: anotherString.
	finder compareCharacters.
	^finder
! !

!DifferenceFinder class methodsFor: 'instance creation' stamp: 'LC 1/24/2010 15:18'!
linesOf: aString and: anotherString
	| finder |
	finder := self base: aString case: anotherString.
	finder compareLines.
	^finder
! !

!DifferenceFinder class methodsFor: 'instance creation' stamp: 'LC 1/24/2010 15:18'!
wordsOf: aString and: anotherString
	| finder |
	finder := self base: aString case: anotherString.
	finder compareWords.
	^finder
! !

!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 2/22/2010 13:08'!
displayPatchFrom: srcString to: dstString tryWords: aBoolean
	| finder |

	aBoolean ifTrue: [
		(self wordsDisplayPatchFrom: srcString to: dstString)
			ifNotNil: [ :answer | ^answer ] ].

	finder _ self base: srcString case: dstString.
	finder compareLines; compute.
	^finder differences anyOne asText! !

!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 8/2/2016 16:45:19'!
displayPatchFrom: srcString to: dstString tryWords: aBoolean prettyPrintedIn: aClass
	| formattedSrcString formattedDstString |
	formattedSrcString _ aClass
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
	formattedDstString _ aClass
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
		tryWords: aBoolean! !

!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 9/13/2016 17:28:22'!
wordsDisplayPatchFrom: srcString to: dstString
	| finder answer src1 dst1 changedCount |
	finder _ self base: srcString case: dstString.
	finder compareLines; compute.
	answer _ '' asText.
	src1 _ String new writeStream.
	dst1 _ String new writeStream.
	changedCount _ 0.
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
					answer _ answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount _ 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount _ changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount _ changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer _ answer append: finder differences anyOne asText.

	^answer! !

!DifferenceFinder class methodsFor: 'bibliography' stamp: 'LC 1/24/2010 16:30'!
references
	^'http://en.wikipedia.org/wiki/Longest_common_subsequence'
! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
d
	^1! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
l
	^3! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
u
	^2! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
ul
	^4! !


!SequenceDifference methodsFor: 'accessing' stamp: 'jmv 12/21/2012 12:32'!
lcsSize
	^lcs size! !

!SequenceDifference methodsFor: 'accessing' stamp: 'LC 1/24/2010 15:18'!
partsSize
	| count last |
	count := 0.
	self do: [:item :condition | 
		last = condition ifFalse: [
			count := count + 1.
			last := condition]].
	^count
! !

!SequenceDifference methodsFor: 'accessing' stamp: 'jmv 8/23/2010 10:31'!
x: aCollection y: anotherCollection lcs: pairCollection
	x := aCollection.
	y := anotherCollection.
	lcs := pairCollection sort: [ :a :b | a first < b first ]! !

!SequenceDifference methodsFor: 'printing' stamp: 'LC 1/24/2010 15:18'!
asText
	^Text streamContents: [:rtf | self printTextOn: rtf]! !

!SequenceDifference methodsFor: 'printing' stamp: 'pb 1/9/2020 23:51:15'!
attributesFor: condition 
	condition == #unchanged
		ifTrue: [ 
			^ {TextEmphasis normal} ].
	condition == #removed
		ifTrue: [ 
			^ {TextEmphasis struckThrough. TextColor red} ].
	condition == #inserted
		ifTrue: [ 
			^ {TextColor green} ]! !

!SequenceDifference methodsFor: 'printing' stamp: 'LC 1/24/2010 15:13'!
printTextOn: rtf
	self do: [:item :condition | | attributes |
		attributes := self attributesFor: condition.
		rtf withAttributes: attributes do: [rtf nextPutAll: item asString]]! !

!SequenceDifference methodsFor: 'enumerating' stamp: 'LC 1/24/2010 15:18'!
do: aBlock
	| j i item |
	i := j := 1.
	lcs do: [:pair | 
		[i < pair first] whileTrue: [
			item := x at: i.
			aBlock value: item value: #removed.
			i := i + 1].
		[j < pair second] whileTrue: [
			item := y at: j.
			aBlock value: item value: #inserted.
			j := j + 1].
		item := x at: i.
		aBlock value: item value: #unchanged.
		i := i + 1.
		j := j + 1].
	i to: x size do: [:k | 
		item := x at: k.
		aBlock value: item value: #removed].
	j to: y size do: [:k | 
		item := y at: k.
		aBlock value: item value: #inserted]
! !

!SequenceDifference methodsFor: 'services' stamp: 'LC 1/24/2010 15:18'!
invert
	| swap |
	swap := x.
	x := y.
	y := swap.
	lcs := lcs collect: [:pair | pair copy swap: 1 with: 2]
! !

!SequenceDifference methodsFor: 'services' stamp: 'LC 1/24/2010 15:18'!
size
	^lcs sum: [:pair | (x at: pair first) size]
! !

!SequenceDifference methodsFor: 'testing' stamp: 'jmv 12/21/2012 12:38'!
<= sequence
	^lcs size <= sequence lcsSize
! !


!SequenceDifference class methodsFor: 'instance creation' stamp: 'LC 1/24/2010 15:18'!
x: x y: y lcs: sequence
	^self new x: x y: y lcs: sequence
! !

