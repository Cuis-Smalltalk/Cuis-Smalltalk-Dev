'From Cuis 6.0 [latest update: #5767] on 16 April 2023 at 5:37:42 pm'!

!String methodsFor: 'copying' stamp: 'jmv 4/16/2023 17:37:20'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| rep |
	rep := replacementCollection string.
	rep isUnicodeString ifTrue: [
		^self asUnicodeString copyReplaceFrom: start to: stop with: rep  ].
	^super copyReplaceFrom: start to: stop with: rep.! !


!UnicodeString methodsFor: 'copying' stamp: 'jmv 4/16/2023 17:19:35'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex := (self byteIndexAt: startIndex)-1.
	epilogFirstIndex := self byteIndexAt: endIndex+1.
	replacementBytes := replacement string asUtf8BytesOrByteString.
	newBytes := (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^UnicodeString fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).! !

