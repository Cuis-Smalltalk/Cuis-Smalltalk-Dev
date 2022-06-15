'From Cuis 6.0 [latest update: #5280] on 13 June 2022 at 3:30:12 pm'!

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 6/13/2022 15:23:00'!
alike: aString
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize _ (j _ self size) min: (k _ aString size).
	bonus _ (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].

	i _ 1.
	[(i <= minSize) and: [(self at: i) asUppercase = (aString at: i) asUppercase]]
		whileTrue: [ i _ i + 1 ].

	[(j > 0) and: [(k > 0) and:
		[(self at: j) asUppercase = (aString at: k) asUppercase]]]
			whileTrue: [ j _ j - 1.  k _ k - 1. ].

	^ i - 1 + self size - j + bonus.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 15:16:43'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in char rest did |
	in _ self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek = $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [(char _ in next) = $&
					ifTrue: [
						rest _ in upTo: $;.
						did _ out position.
						rest = 'lt' ifTrue: [out nextPut: $<].
						rest = 'gt' ifTrue: [out nextPut: $>].
						rest = 'amp' ifTrue: [out nextPut: $&].
						rest = 'deg' ifTrue: [out nextPut: $¡].
						rest = 'quot' ifTrue: [out nextPut: $"].
						rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest findPositiveInteger) ].
						did = out position ifTrue: [
							out nextPut: $&; nextPutAll: rest.
							"self error: 'unknown encoded HTML char'."
							"Please add it to this method"]]
					ifFalse: [out nextPut: char]].
			]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
correctAgainst: wordList
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results _ self correctAgainst: wordList continuedFrom: nil.
	results _ self correctAgainst: nil continuedFrom: results.
	^ results! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 3/2/2010 16:36'!
correctAgainst: wordList continuedFrom: oldCollection
	"Like correctAgainst:.  Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList."

	^ wordList
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordList do: action without: nil]
				continuedFrom: oldCollection ]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 3/2/2010 16:37'!
correctAgainstDictionary: wordDict continuedFrom: oldCollection
	"Like correctAgainst:continuedFrom:.  Use when you want to correct against a dictionary."

	^ wordDict
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordDict keysDo: action ]
				continuedFrom: oldCollection ]! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 3/2/2010 16:38'!
correctAgainstEnumerator: wordBlock continuedFrom: oldCollection
	"The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it."

	| choices scoreMin results score maxChoices |
	scoreMin _ self size // 2 min: 3.
	maxChoices _ 10.
	choices _ oldCollection
		ifNil: [ SortedCollection sortBlock: [ :x :y | x value > y value ] ].
	wordBlock
		ifNil: [
			results _ OrderedCollection new.
			1 to: (maxChoices min: choices size) do: [ :i | results add: (choices at: i) key ] ]
		ifNotNil: [
			wordBlock value: [ :word |
				(score _ self alike: word) >= scoreMin ifTrue: [
					choices add: (Association key: word value: score).
						(choices size >= maxChoices) ifTrue: [ scoreMin _ (choices at: maxChoices) value] ] ].
			results _ choices ].
	^ results! !

!methodRemoval: String #correctAgainstEnumerator:continuedFrom: stamp: 'jmv 6/13/2022 15:26:21'!
String removeSelector: #correctAgainstEnumerator:continuedFrom:!
!methodRemoval: String #correctAgainstDictionary:continuedFrom: stamp: 'jmv 6/13/2022 15:28:06'!
String removeSelector: #correctAgainstDictionary:continuedFrom:!
!methodRemoval: String #asUnHtml stamp: 'jmv 6/13/2022 15:15:29'!
String removeSelector: #asUnHtml!
!methodRemoval: String #alike: stamp: 'jmv 6/13/2022 15:25:52'!
String removeSelector: #alike:!
!methodRemoval: String #correctAgainst:continuedFrom: stamp: 'jmv 6/13/2022 15:26:10'!
String removeSelector: #correctAgainst:continuedFrom:!
!methodRemoval: String #correctAgainst: stamp: 'jmv 6/13/2022 15:26:04'!
String removeSelector: #correctAgainst:!
