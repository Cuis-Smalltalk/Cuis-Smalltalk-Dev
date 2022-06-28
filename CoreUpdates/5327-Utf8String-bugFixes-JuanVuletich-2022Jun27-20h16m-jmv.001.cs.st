'From Cuis 6.0 [latest update: #5323] on 27 June 2022 at 8:17:39 pm'!

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/27/2022 16:16:16'!
caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^self asByteString caseInsensitiveLessOrEqual: aString ].
		(aString isUtf8String and: [ aString isAscii ]) ifTrue: [
			^self asByteString caseInsensitiveLessOrEqual: aString asByteString ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/27/2022 16:09:00'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	byteIndex _ self byteIndexAt: index.
	(bytes at: byteIndex) = (subStringBytes byteAt: 1) ifFalse: [ ^false ].
	^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex! !

