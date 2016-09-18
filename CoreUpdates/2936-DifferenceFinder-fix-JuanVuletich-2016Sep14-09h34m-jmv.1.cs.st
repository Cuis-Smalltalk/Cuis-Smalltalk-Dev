'From Cuis 4.2 of 25 July 2013 [latest update: #2935] on 14 September 2016 at 9:35:22 am'!

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

