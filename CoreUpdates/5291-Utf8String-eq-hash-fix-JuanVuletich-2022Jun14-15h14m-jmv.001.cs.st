'From Cuis 6.0 [latest update: #5284] on 14 June 2022 at 3:17:50 pm'!

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/14/2022 13:18:55'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [
		"Fast lane."
		^'' is: bytes equalTo: aString ].
	
	aString isUtf8String ifTrue: [
		self isAscii = aString isAscii ifFalse: [
			^false ]].

	"May need to do UTF-8 encoding."
	^ bytes = aString asUtf8Bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:51:14'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !

