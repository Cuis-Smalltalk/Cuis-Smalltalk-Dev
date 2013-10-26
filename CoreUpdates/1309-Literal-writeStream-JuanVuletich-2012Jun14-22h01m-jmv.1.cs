'From Cuis 4.0 of 21 April 2012 [latest update: #1308] on 14 June 2012 at 10:02:25 pm'!

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 6/14/2012 22:01'!
headerDescription
	"Answer a description containing the information about the form of the 
	receiver and the form of the context needed to run the receiver."

	| s |
	s _ String new writeStream.
	self header printOn: s.
	s newLine; nextPutAll: '"primitive: '.
	self primitive printOn: s.
	s newLine; nextPutAll: ' numArgs: '.
	self numArgs printOn: s.
	s newLine; nextPutAll: ' numTemps: '.
	self numTemps printOn: s.
	s newLine; nextPutAll: ' numLiterals: '.
	self numLiterals printOn: s.
	s newLine; nextPutAll: ' frameSize: '.
	self frameSize printOn: s.
	s newLine; nextPutAll: ' isClosureCompiled: '.
	self isBlueBookCompiled not printOn: s.
	s nextPut: $"; newLine.
	^ s contents! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 6/14/2012 22:01'!
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
					finder compareWords; compute.
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
	finder compareWords; compute.
	answer append:  finder differences anyOne asText.

	^answer! !


!String methodsFor: 'converting' stamp: 'jmv 6/14/2012 22:01'!
squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str := self.
	desiredSize := n.
	str size <= n ifTrue: [^str].
	str := str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix := ''.
	desiredSize := n - postFix size.
	vowelCount := str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels := vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels := 0.
			postFix := '...'.
			n - postFix size < 5 ifTrue: [postFix := ''].
			desiredSize := n - postFix size].

	read := str readStream.
	write := String new writeStream.
	i := 0.
	j := 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char := read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j := j + 1]].
	str := write contents , postFix.
	^ str! !


!Transcript class methodsFor: 'class initialization' stamp: 'jmv 6/14/2012 22:02'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	innerRectangle _ 20@20 extent: 300@500.
	logToFile _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String new writeStream.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !

Transcript initialize!
Transcript showOnDisplay: true; logToFile: true; bounds: (0@0 extent: (700@900 min: Display extent))!
