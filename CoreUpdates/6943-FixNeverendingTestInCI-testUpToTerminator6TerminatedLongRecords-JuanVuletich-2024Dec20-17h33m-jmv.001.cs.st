'From Cuis7.3 [latest update: #6942] on 20 December 2024 at 5:34:45 pm'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 12/20/2024 17:33:07'!
untilAnySatisfying: aBlock
1 print.
	^self collectionSpecies streamContents: [ :write |
		[
			self atEnd not and: [
				(aBlock value: self peek) not]]
			whileTrue: [write nextPut: self next]].! !

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 10/25/2021 11:18:50'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"Answer a subcollection from the current access position to the 
	occurrence of delimiter in the receiver. 

	If delimiterIsTerminator is false, delimiter is considered a separator:
		- Skip delimiter, but don't include it in the answer.
		- If delimiter is not found, answer the entire rest of the receiver.

	If delimiterIsTerminator is true, answer will end with delimeter:
		- Read delimiter, include it in the answer.
		- If delimiter is not found, answer nil and don't advance receiver at all.
	This is especially useful if we are appended new stuff while simultaneusly being read."

	| prevPosition element answer |
	prevPosition := self position.
	answer := self collectionSpecies streamContents: [ :newStream |
		[self atEnd or: [(element := self next) = delimiter]]
			whileFalse: [newStream nextPut: element].
		delimiterIsTerminator ifTrue: [
			element = delimiter
				ifTrue: [newStream nextPut: element]
				ifFalse: [
					self position: prevPosition.
					^ nil ]]].
	^answer.! !

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 12/20/2024 17:33:18'!
upToAny: aCollection
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
3 print.
	^self collectionSpecies streamContents: [ :strm | | element prevPos |
		[self atEnd or: [
			prevPos := self position.
			element := self next.
			aCollection includes: element ]]
				whileFalse: [strm nextPut: element].
		(aCollection includes: element)
			ifTrue: [self position: prevPos]].! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 12/20/2024 17:33:24'!
backChunk
	"Answer the contents of the receiver back to the previous terminator character.
	Doubled delimiter indicate an embedded delimiter character."
	
	| answer ch firstPosition delimiter prevCh count delimiterCount |
4 print.
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

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 12/20/2024 17:33:30'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator |
5 print.
	terminator := $!!.
	^self collectionSpecies streamContents: [ :strm |
		| ch done |
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

