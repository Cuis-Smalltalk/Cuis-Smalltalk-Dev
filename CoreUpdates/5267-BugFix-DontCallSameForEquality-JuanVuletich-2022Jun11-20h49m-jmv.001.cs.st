'From Cuis 6.0 [latest update: #5263] on 11 June 2022 at 8:58:45 pm'!

!String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:50:20'!
is: aStringOrByteArray equalTo: another
	"Take adventage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: CaseSensitiveOrder) = 2! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:52:28'!
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
	^ bytes = aString.! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:50:36'!
= another
	"Make good use of String primitives"

	self == another ifTrue: [ ^ true ].
	self species == another species ifFalse: [^ false].
	^'' is: self equalTo: another ! !

!methodRemoval: String #is:sameAs: stamp: 'jmv 6/11/2022 20:53:03'!
String removeSelector: #is:sameAs:!
