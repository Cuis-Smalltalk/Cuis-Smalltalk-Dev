'From Cuis7.3 [latest update: #6946] on 23 December 2024 at 11:11:28 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:18:25'!
untilAnySatisfying: aBlock
	| c |
	^self collectionSpecies streamContents: [ :write |
		[
			self atEnd not and: [
				c := self peek.
				(aBlock value: c) not]]
			whileTrue: [write nextPut: self next]].! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/19/2022 11:57:58'!
backChunk
	"Answer the contents of the receiver back to the previous terminator character.
	Doubled delimiter indicate an embedded delimiter character."
	
	| answer ch firstPosition delimiter prevCh count delimiterCount |
	delimiter := $!!.
	"Go to end of chunk, not including delimiter."
	ch := self back.
	ch = delimiter ifTrue: [ ch := self back ].

	count := 1.
	prevCh := $$.
	"Go back until non-doubled delimiter, or beginning."
	[ch notNil and: [ ch ~= delimiter or: [self peekBack = delimiter or: [prevCh = delimiter]]]] whileTrue: [
		count := count + 1.
		prevCh := ch.
		ch := self back. ].
	"Skip initial delimiter"
	ch = delimiter ifTrue: [
		self next.
		count := count-1 ].

	"This is where chunk starts. Undouble doubled delimiters."
	firstPosition := self position.
	answer := String streamContents: [ :strm |
		prevCh := $$.
		delimiterCount := 0.
		count timesRepeat: [
			ch := self next.
			delimiterCount \\ 2 = 0 ifTrue: [
				strm nextPut: ch ].
			delimiterCount := ch = delimiter
				ifTrue: [ delimiterCount+1]
				ifFalse: [0]]].

	"Re position at start."
	self position: firstPosition.
	^ answer! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/23/2022 17:47:39'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator ch done |
	terminator := $!!.
	^self collectionSpecies streamContents: [ :strm |
		self skipSeparators.
		done := false.
		[done not and: [(ch := self next) ~~ nil]] whileTrue: [
			done := false.
			(ch = terminator) ifTrue: [
				self peek = terminator ifTrue: [
					self next.  "skip doubled terminator"
				] ifFalse: [
					done := true  "terminator is not doubled; we're done!!"
				].
			].
			done ifFalse: [
				strm nextPut: ch ].
		]
	]! !

