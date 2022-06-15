'From Cuis 6.0 [latest update: #5265] on 12 June 2022 at 3:46:42 pm'!

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/12/2022 15:36:54'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	arg _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].

	"Do UTF-8 encoding as needed."
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ '' findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/12/2022 15:38:35'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	| startByteIndex byteIndex |
	caseSensitive ifTrue: [
		^self findString: key startingAt: start ].

	self isAscii ifTrue: [
		key isAscii
			ifTrue: [
				"Fast lane"
				startByteIndex _ self byteIndexAt: start.
				byteIndex _ '' findStringIgnoreCase: key in: bytes startingAt: startByteIndex.
				^self codePointIndexAt: byteIndex ]
			ifFalse: [
				"Won't be found"
				^0 ]].

"caseSensitive = false is very expensive. Do better."
self flag: #slowUnicode.
	^self asLowercase findString: key asLowercase startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/12/2022 15:45:08'!
indexOf: anUnicodeCodePoint  startingAt: start  ifAbsent: aBlock
	| codePointBytes startByteIndex foundByteIndex answer |
	anUnicodeCodePoint isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes _ Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint.
	startByteIndex _ self byteIndexAt: start.
	foundByteIndex _ '' findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer _ self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/12/2022 15:39:57'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock

	| index arg startByteIndex byteIndex |
	arg _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ '' findString: arg in: bytes startingAt: startByteIndex.
	index _ self codePointIndexAt: byteIndex.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/12/2022 15:41:57'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ subString asUtf8Bytes.
	subStringBytes _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	byteIndex _ self byteIndexAt: index.
	(bytes at: byteIndex) = (subStringBytes byteAt: 1) ifFalse: [ ^false ].
	^('' findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex! !

