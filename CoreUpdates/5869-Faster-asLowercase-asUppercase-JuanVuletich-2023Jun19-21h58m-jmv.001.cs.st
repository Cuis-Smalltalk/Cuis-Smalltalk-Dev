'From Cuis 6.0 [latest update: #5868] on 19 June 2023 at 10:06:47 pm'!

!String class methodsFor: 'services' stamp: 'jmv 6/19/2023 22:02:28'!
translate: aStringOrByteArray table: table
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: table! !

!String class methodsFor: 'services' stamp: 'jmv 6/19/2023 22:04:10'!
translateToLowercase: aStringOrByteArray
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: LowercasingTable! !

!String class methodsFor: 'services' stamp: 'jmv 6/19/2023 22:03:58'!
translateToUppercase: aStringOrByteArray
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: UppercasingTable! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 6/19/2023 22:06:43'!
asLowercase
	| b |
	self isAscii ifFalse: [ ^super asLowercase ].
	b := bytes copy.
	String translateToLowercase: b.
	^UnicodeString fromUtf8Bytes: b! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 6/19/2023 22:06:15'!
asUppercase
	| b |
	self isAscii ifFalse: [ ^super asUppercase ].
	b := bytes copy.
	String translateToUppercase: b.
	^UnicodeString fromUtf8Bytes: b! !


!String methodsFor: 'converting' stamp: 'jmv 6/19/2023 22:04:22'!
translateToLowercase
	"Translate all characters to lowercase, in place"

	String translateToLowercase: self! !

!String methodsFor: 'converting' stamp: 'jmv 6/19/2023 22:04:36'!
translateToUppercase
	"Translate all characters to lowercase, in place"

	String translateToUppercase: self! !

!methodRemoval: String #translateWith: stamp: 'jmv 6/19/2023 22:03:25'!
String removeSelector: #translateWith:!
!methodRemoval: String #translateFrom:to:table: stamp: 'jmv 6/19/2023 22:01:36'!
String removeSelector: #translateFrom:to:table:!
