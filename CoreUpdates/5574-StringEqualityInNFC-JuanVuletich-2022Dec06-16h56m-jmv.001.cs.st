'From Cuis 6.0 [latest update: #5572] on 6 December 2022 at 5:20:36 pm'!

!String methodsFor: 'testing' stamp: 'jmv 12/6/2022 17:15:51'!
isInCannonicalCompositionForm

	^true! !


!Utf8String methodsFor: 'testing' stamp: 'jmv 12/6/2022 17:15:07'!
isInCannonicalCompositionForm
	"Answer if we are in NFC"

	self isAscii ifTrue: [ ^true ].

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	"See findCannonicalCompositionAndHash"
	^hash isNumber! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:18:50'!
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

	self hash = aString hash ifFalse: [
		^false ].

	(self isInCannonicalCompositionForm and: [ aString isInCannonicalCompositionForm ]) ifFalse: [
		^self cannonicalComposition = aString cannonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString isUtf8String ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:02:40'!
computeHash
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"

	| byteString |
	self isAscii ifTrue: [
		hash _ self basicHash.
		^self ].

	"If there's a String that is equal to us (i.e. no characters were lost during conversion)"
	byteString _ self asByteString.
	byteString size = self size ifTrue: [
		hash _ byteString hash.
		^self ].

	"Otherwise, use a hash that doesn't need to equal that of a String"
	hash _ self basicHash.! !

