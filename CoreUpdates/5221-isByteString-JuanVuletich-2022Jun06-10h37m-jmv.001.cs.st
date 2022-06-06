'From Cuis 6.0 [latest update: #5220] on 6 June 2022 at 10:37:32 am'!

!Object methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:20:29'!
isByteString
	"Overridden to return true in String, natch"
	^ false! !


!String methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:21:36'!
isByteString
	^ true! !


!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 10:24:19'!
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
	aString isByteString ifFalse: [
		^aString = self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2.! !

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 10:29:16'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary"
	"'MElViN' uncapitalized"
	"#Will uncapitalized"
	| answer |
	self isEmpty ifTrue: [^ self].
	answer _ self asPlainString.
	answer == self ifTrue: ["don't modify receiver"
		answer _ self copy ].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ answer! !


!WriteStream methodsFor: 'private' stamp: 'jmv 6/6/2022 10:33:40'!
isCompatibleWithContents: aCollection

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isByteString and: [ (collection is: #Text) or: [ collection is: #ArrayOfCharactersAndCodePoints ]])
		ifTrue: [ ^ true ].

	^ false! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 10:35:18'!
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

	aString isByteString ifTrue: [
		"If aString contains NCRs wit will never equal self. If this ever changes, don't forget about #hash,
		and String >> #="
		^bytes = (aString asUtf8Bytes: false) ].

	^ bytes = aString asUtf8Bytes.! !

