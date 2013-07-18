'From Cuis 4.1 of 12 December 2012 [latest update: #1519] on 21 December 2012 at 1:53:55 pm'!

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


!SequenceDifference methodsFor: 'accessing' stamp: 'jmv 12/21/2012 12:32'!
lcsSize
	^lcs size! !


!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 12/21/2012 13:51'!
compute
	^self compute: false! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 12/21/2012 13:52'!
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
	answer append: finder differences anyOne asText.

	^answer! !


!SequenceDifference methodsFor: 'testing' stamp: 'jmv 12/21/2012 12:38'!
<= sequence
	^lcs size <= sequence lcsSize
! !

!methodRemoval: DifferenceFinder #longestSequences!
DifferenceFinder removeSelector: #longestSequences!
