'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 11:44:56 am'!

!String methodsFor: 'system primitives' stamp: 'jmv 6/6/2022 20:10:40'!
compare: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: CaseSensitiveOrder! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:20:13'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	^ ('' compare: bytes with: aString asUtf8Bytes) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:20:18'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	^ ('' compare: bytes with: aString asUtf8Bytes) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:20:25'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	^ ('' compare: bytes with: aString asUtf8Bytes) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:20:28'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	^ ('' compare: bytes with: aString asUtf8Bytes) >= 2! !


!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:07:22'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString > self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 1! !

!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:07:18'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString >= self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:07:14'!
= aString
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
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

!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:07:10'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString < self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 3! !

!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 20:07:05'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString <= self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) >= 2! !

