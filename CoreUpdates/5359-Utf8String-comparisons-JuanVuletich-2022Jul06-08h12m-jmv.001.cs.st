'From Cuis 6.0 [latest update: #5358] on 6 July 2022 at 8:32:45 am'!

!Utf8String class methodsFor: 'services' stamp: 'jmv 7/6/2022 08:29:15'!
compare: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2.
	Case sensitive.
	Can handle any kind of Character sequences.
	Slower than implementation in String class"

	| len1 len2 c1 c2 |

	len1 _ string1 size.
	len2 _ string2 size.
	1 to: (len1 min: len2) do: [ :i |
		c1 _ string1 at: i.
		c2 _ string2 at: i.
		c1 = c2 ifFalse: [
			c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 7/6/2022 08:32:19'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (Utf8String compare: self with: aString) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 7/6/2022 08:32:15'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) <= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (Utf8String compare: self with: aString) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 7/6/2022 08:32:11'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 3].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (Utf8String compare: self with: aString) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 7/6/2022 08:32:08'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) >= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (Utf8String compare: self with: aString) >= 2! !

