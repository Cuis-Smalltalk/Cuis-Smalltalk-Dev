'From Cuis7.1 [latest update: #6526] on 25 July 2024 at 10:39:59 am'!

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:38:29'!
< another
	"Answer whether the receiver sorts before another.
	The collation order is case sensitive.
	Note: Behavior the same for String, Symbol, UnicodeString, UnicodeSymbol and Text,
	both as receiver and as argument, if they hold same Characters."

	self subclassResponsibility.! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:29:56'!
<= another
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	"Comparing for equality may is faster in most casts. Try it first."
	self = another ifTrue: [ ^true ].

	^self < another.! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:38:14'!
= another
	"Note: Behavior the same for String, Symbol, UnicodeString, UnicodeSymbol and Text,
	both as receiver and as argument, if they hold same Characters."

	self subclassResponsibility! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:29:53'!
> another
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	"Comparing for equality may is faster in most casts. Try it first."
	self = another ifTrue: [ ^false ].

	^(self < another) not.! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:23:43'!
>= another
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	^(self < another) not.! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:36:22'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self subclassResponsibility! !


!String methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:29:37'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Comparing for equality may is faster in most casts. Try it first."
	self = aString ifTrue: [ ^false ].

	"Let them handle us (already knowing we are not equal)"
	aString isByteString ifFalse: [ ^(aString < self) not ].

	^ (String compare: self with: aString) = 1.! !


!UnicodeString methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:29:46'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Comparing for equality may is faster in most casts. Try it first."
	self = aString ifTrue: [ ^false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1.! !

!methodRemoval: UnicodeString #>= stamp: 'jmv 7/25/2024 10:19:58'!
UnicodeString removeSelector: #>=!
!methodRemoval: UnicodeString #> stamp: 'jmv 7/25/2024 10:24:24'!
UnicodeString removeSelector: #>!
!methodRemoval: UnicodeString #<= stamp: 'jmv 7/25/2024 10:27:11'!
UnicodeString removeSelector: #<=!
!methodRemoval: String #>= stamp: 'jmv 7/25/2024 10:20:03'!
String removeSelector: #>=!
!methodRemoval: String #> stamp: 'jmv 7/25/2024 10:24:20'!
String removeSelector: #>!
!methodRemoval: String #<= stamp: 'jmv 7/25/2024 10:27:05'!
String removeSelector: #<=!
