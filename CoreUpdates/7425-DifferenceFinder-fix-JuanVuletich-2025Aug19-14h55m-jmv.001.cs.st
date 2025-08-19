'From Cuis7.5 [latest update: #7424] on 19 August 2025 at 3:00:33 pm'!

!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 8/19/2025 14:59:30'!
wordsDisplayPatchFrom: srcStringOrText to: dstStringOrText

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

	^answer! !

