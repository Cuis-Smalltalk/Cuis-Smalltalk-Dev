'From Cuis 6.0 [latest update: #5295] on 15 June 2022 at 4:26:11 pm'!

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/15/2022 16:07:35'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	(self isAscii and: [subString isByteString])
		ifTrue: [ "Fast lane"
			^'' findString: subString in: bytes startingAt: start ]
		ifFalse: [
			arg _ subString asUtf8BytesOrByteString.
			"Do UTF-8 encoding as needed."
			startByteIndex _ self byteIndexAt: start.
			byteIndex _ '' findString: arg in: bytes startingAt: startByteIndex.
			^self codePointIndexAt: byteIndex].! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/15/2022 16:09:23'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive ifTrue: [
		^self findString: key startingAt: start ].

	self isAscii ifTrue: [
		key isAscii
			ifTrue: [ "Fast lane"
				^'' findStringIgnoreCase: key in: bytes startingAt: start ]
			ifFalse: [ "Won't be found"
				^0 ]].

"caseSensitive = false is very expensive. Do better."
self flag: #slowUnicode.
	^self asLowercase findString: key asLowercase startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/15/2022 16:15:02'!
indexOf: anUnicodeCodePoint startingAt: start ifAbsent: aBlock
	| codePointBytes startByteIndex foundByteIndex answer |
	anUnicodeCodePoint isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes _ Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint.
	startByteIndex _ self byteIndexAt: start.
	foundByteIndex _ '' findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer _ self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

