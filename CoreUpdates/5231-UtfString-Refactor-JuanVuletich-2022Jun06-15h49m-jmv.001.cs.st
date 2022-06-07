'From Cuis 6.0 [latest update: #5230] on 6 June 2022 at 3:49:35 pm'!
!classDefinition: #Utf8String category: #'System-Text'!
Object subclass: #Utf8String
	instanceVariableNames: 'bytes count index hash strideIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
beAscii
	"Mark me as being pure 7-bit ASCII, which means each character is encoded as one byte"
	
	count := bytes size.
	strideIndex := #().! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
byteIndexAt: codePointIndex
	| byteIndex |
	self isAscii ifTrue: [ ^ codePointIndex ].
	codePointIndex = (self size+1) ifTrue: [
		"The position a hypotetical extra code point would start at.
		Useful to find the last byte position for some codePointIndex. See senders."
		^bytes size+1 ].
	byteIndex := codePointIndex > self stride
		ifTrue: [
			strideIndex at: codePointIndex - 1 // self stride ]
		ifFalse: [ 1 ].
	codePointIndex - 1 \\ self stride timesRepeat: [ 
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ].
	^ byteIndex! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
codePointIndexAt: byteIndex
	| codePointIndex byteCursor |
	self isAscii ifTrue: [ ^ byteIndex ].
	codePointIndex := byteCursor := 1.
	strideIndex ifNil: [ self computeCountAndIndex ].
	strideIndex ifNotEmpty: [  
		strideIndex 
			findBinaryIndex: [ :x | byteIndex - x ] 
			do: [ :found | ^ (found * self stride) + 1 ] 
			ifNone: [ :lower :upper |
				lower = 0 
			 		ifFalse: [ 
						byteCursor := strideIndex at: lower. 
						codePointIndex := (lower * self stride) + 1 ] ] ].
	[ byteCursor <= bytes size ] whileTrue: [
		byteCursor = byteIndex 
			ifTrue: [ ^ codePointIndex ].
		byteCursor := byteCursor + (self encodedByteCountAt: byteCursor).
		codePointIndex := codePointIndex + 1 ].
	^ 0! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
computeCountAndIndex
	self isEmpty ifTrue: [ count := 0. strideIndex := #(). ^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
computeCountAndIndexFull
	count := 0.
	strideIndex := Array streamContents: [ :out | | byteIndex |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 15:49:28'!
computeCountNoIndex
	| byteIndex |
	count := 0.
	strideIndex := #().
	byteIndex := 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count := count + 1.
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ]! !

!classDefinition: #Utf8String category: #'System-Text'!
Object subclass: #Utf8String
	instanceVariableNames: 'bytes count strideIndex hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Utf8String reorganize!
('accessing' at: at:ifAbsent: bytes bytes: isLastLineSeparator isLastSeparator last size)
('copying' , appendToString: appendToText: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = commonPartWith:startAt:stopAt:applying: computeHash computeHashForAscii hash)
('converting' asByteString asPlainString asString asSymbol asText asUtf32String asUtf32Words asUtf8Bytes asUtf8String contractTo: displayStringOrText string)
('enumerating' collect: collectInArray: do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' canHoldCodePoints is: isAscii isEmpty isString isUtf8String notEmpty)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('user interface' edit editLabel:)
('private' beAscii boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: stride)
('as yet unclassified' doesNotUnderstand:)
('initialization' initialize)
('printing' print printOn: storeOn:)
('compatibility' contents)
!

