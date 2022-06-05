'From Cuis 6.0 [latest update: #5214] on 2 June 2022 at 7:13:45 pm'!
!classDefinition: #Utf32String category: #'System-Text'!
Object subclass: #Utf32String
	instanceVariableNames: 'codePoints hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf8String category: #'System-Text'!
Object subclass: #Utf8String
	instanceVariableNames: 'bytes count index hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Object methodsFor: 'testing' stamp: 'jmv 6/2/2022 15:04:50'!
isUtf32String
	"Overridden to return true in Utf32String, natch"
	^ false! !


!String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 14:43:31'!
is: aStringOrByteArray sameAs: another 
	"Take adventage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: CaseInsensitiveOrder) = 2! !


!Utf32String methodsFor: 'testing' stamp: 'jmv 6/2/2022 15:04:42'!
isUtf32String

	^true! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 14:24:51'!
computeHash
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"
	| hashForAscii byteString |
	hashForAscii _ self computeHashForAscii.
	self isAscii ifTrue: [
		^hashForAscii ].
	"If there's a String that is equal to us (i.e. no NCRs were needed to represent us)"
	byteString _ self asByteString.
	byteString size = self size ifTrue: [
		^byteString hash ].
	"Otherwise, use a hash that doesn't need to equal that of a String"
	^hashForAscii
! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 14:14:52'!
computeHashForAscii
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present."
	bytes size > 256 ifTrue: [ ^ bytes hashQuick ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !


!String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:03:08'!
= aString 
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isStringLike ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isString ifFalse: [
		^aString = self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2.! !

!String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:01:55'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol, Utf32String and Text
	if they hold same Characters."

	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ByteArray
		hashBytes: self
		startingWith: String hash! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 6/2/2022 14:47:56'!
= another
	"Make good use of String primitives"

	self == another ifTrue: [ ^ true ].
	self species == another species ifFalse: [^ false].
	^'' is: self sameAs: another ! !


!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:06:02'!
= aString
	"Answer whether the receiver sorts equally as anUtf32String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isStringLike ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Expensive. Assume it is extremely unlikely."
	aString isUtf32String ifFalse: [
		^self asUtf8String = aString ].

	^ codePoints = aString words! !

!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:06:54'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol, Utf32String and Text
	if they hold same Characters.
	This implementation is expensive. We assume that String and Utf8String are much more common."

	hash ifNil: [ hash _ self asUtf8String hash ].
	^hash! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:03:08'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isStringLike ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	aString isString ifTrue: [
		"If aString contains NCRs wit will never equal self. If this ever changes, don't forget about #hash,
		and String >> #="
		^bytes = (aString asUtf8Bytes: false) ].

	^ bytes = aString asUtf8Bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/2/2022 15:01:38'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol, Utf32String and Text
	if they hold same Characters."

	hash ifNil: [ hash _ self computeHash ].
	^hash! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/2/2022 14:11:48'!
computeCountAndIndex
	self isEmpty ifTrue: [ count := 0. index := #(). ^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!classDefinition: #Utf32String category: #'System-Text'!
Object subclass: #Utf32String
	instanceVariableNames: 'codePoints hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf8String category: #'System-Text'!
Object subclass: #Utf8String
	instanceVariableNames: 'bytes count index hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
