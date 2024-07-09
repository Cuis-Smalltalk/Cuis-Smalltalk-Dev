'From Cuis7.1 [latest update: #6509] on 9 July 2024 at 6:26:22 pm'!

!UnicodeString methodsFor: 'private' stamp: 'jmv 7/9/2024 18:17:29'!
computeCountAndIndex
	self isEmpty ifTrue: [
		count := 0.
		strideIndex := #[].
		^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!UnicodeString methodsFor: 'private' stamp: 'jmv 7/9/2024 18:20:35'!
computeCountAndIndexFull
	| strideIndexClass byteIndex |
	count := 0.
	strideIndexClass := bytes size < 256
		ifTrue: [ ByteArray ]
		ifFalse: [ bytes size < 65536
			ifTrue: [ DoubleByteArray ]
			ifFalse: [ bytes size < 4294967296
				ifTrue: [ WordArray ]
				ifFalse: [DoubleWordArray ]]].
	strideIndex := strideIndexClass streamContents: [ :out |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!UnicodeString methodsFor: 'private' stamp: 'jmv 7/9/2024 18:20:44'!
computeCountNoIndex
	| byteIndex |
	count := 0.
	strideIndex := #[].
	byteIndex := 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count := count + 1.
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk garbageCollect.
UnicodeString allInstancesDo: [ :s |
	(s instVarNamed: 'strideIndex') class == Array
		ifTrue: [ s computeCountAndIndex ]].
Smalltalk garbageCollect.!

