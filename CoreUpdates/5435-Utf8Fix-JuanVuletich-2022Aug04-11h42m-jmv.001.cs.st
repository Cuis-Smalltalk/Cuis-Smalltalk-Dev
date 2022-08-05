'From Cuis 6.0 [latest update: #5434] on 4 August 2022 at 11:45:07 am'!

!Utf8String methodsFor: 'accessing' stamp: 'jmv 8/4/2022 11:43:22'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findString: subString in: bytes startingAt: start ].
		subString isUtf8String ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findString: subString bytes in: bytes startingAt: start ]]].

	arg _ subString asUtf8BytesOrByteString.
	"Do UTF-8 encoding as needed."
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ String findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 8/4/2022 11:42:58'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findStringIgnoreCase: subString in: bytes startingAt: start ].
		subString isUtf8String ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findStringIgnoreCase: subString bytes in: bytes startingAt: start ]]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: subString asLowercase startingAt: start! !

