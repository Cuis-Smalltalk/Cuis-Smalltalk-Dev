Class {
	#name : 'UnicodeStringsTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'testing' }
UnicodeStringsTest >> test01 [

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!' asUnicodeString.

	self assert: asciiString hash = asciiUtf8String hash.
	self assert: asciiString = asciiUtf8String.
	self assert: asciiUtf8String = asciiString.

	self assert: latinString hash = latinUtf8String hash.
	self assert: latinString = latinUtf8String.
	self assert: latinUtf8String = latinString.
]

{ #category : 'testing' }
UnicodeStringsTest >> test02 [

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!' asUnicodeString.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).
]

{ #category : 'testing' }
UnicodeStringsTest >> test03 [

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!' asUnicodeString.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).
]

{ #category : 'testing' }
UnicodeStringsTest >> test04 [

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!' asUnicodeString.
	asciiString _ asciiString asLowercase.
	latinString _ latinString asLowercase.

	self assert: (asciiString sameAs: asciiUtf8String).
	self assert: (asciiUtf8String sameAs: asciiString).

	self assert: (latinString sameAs: latinUtf8String).
	self assert: (latinUtf8String sameAs: latinString).
]

{ #category : 'testing' }
UnicodeStringsTest >> test05Utf8 [

	self deny: (UnicodeString fromUtf8Bytes: #[64 65 66]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 32]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 9]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 10]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 13]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 12]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 194 133]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 168 ]) isLastSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 169 ]) isLastSeparator.

	self deny: (UnicodeString fromUtf8Bytes: #[64 65 66]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 10]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 13]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 12]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 194 133]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 168 ]) isLastLineSeparator.
	self assert: (UnicodeString fromUtf8Bytes: #[64 65 66 226 128 169 ]) isLastLineSeparator.
]

{ #category : 'testing' }
UnicodeStringsTest >> test06isAscii [

	| asciiString asciiUtf8String latinString latinUtf8String |
	asciiString _ 'Hello world' asByteString.
	latinString _ '¡Tomá agüita, Ñandú!' asByteString.
	asciiUtf8String _ 'Hello world' asUnicodeString.
	latinUtf8String _ '¡Tomá agüita, Ñandú!' asUnicodeString.

	self assert: asciiString isAscii.
	self assert: asciiUtf8String isAscii.
	self deny: latinString isAscii.
	self deny: latinUtf8String isAscii.

	self assert: asciiString asLowercase isAscii.
	self assert: asciiUtf8String asLowercase isAscii.
	self deny: latinString asLowercase isAscii.
	self deny: latinUtf8String asLowercase isAscii.
]

{ #category : 'testing' }
UnicodeStringsTest >> test07 [
	| s |

	self assert: ('Hello World' asByteString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.
	self assert: ('Hello World' asUnicodeString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.
	self assert: ('Hello World' asUnicodeString copyReplaceFrom: 3 to: 4 with: '') =  'Heo World'.

	s _ '¿El Ñandú toma agüita?' asByteString.
	self assert: (s copyReplaceFrom: 3 to: 4 with: 'xxx') =  '¿ExxxÑandú toma agüita?'.
	self assert: (s copyReplaceFrom: 3 to: 2 with: 'xxx') =  '¿Exxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: 1 to: 2 with: 'xxx') =  'xxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: s size to: s size with: 'xxx') =  '¿El Ñandú toma agüitaxxx'.
	self assert: (s copyReplaceFrom: s size to: 2 with: 'xxx') =  '¿El Ñandú toma agüitaxxxl Ñandú toma agüita?'.

	s _ '¿El Ñandú toma agüita?' asUnicodeString.
	self assert: (s copyReplaceFrom: 3 to: 4 with: 'xxx') =  '¿ExxxÑandú toma agüita?'.
	self assert: (s copyReplaceFrom: 3 to: 2 with: 'xxx') =  '¿Exxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: 1 to: 2 with: 'xxx') =  'xxxl Ñandú toma agüita?'.
	self assert: (s copyReplaceFrom: s size to: s size with: 'xxx') =  '¿El Ñandú toma agüitaxxx'.
	self assert: (s copyReplaceFrom: s size to: 2 with: 'xxx') =  '¿El Ñandú toma agüitaxxxl Ñandú toma agüita?'.
]

{ #category : 'testing' }
UnicodeStringsTest >> test08a [
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!"

	| asciiString0 asciiString1 asciiString2 |
	asciiString0 _ 'Helle world!'.
	asciiString1 _ 'Hello world'.
	asciiString2 _ 'Hellu world!'.

	self deny: asciiString0 < asciiString0.
	self assert: asciiString0 < asciiString1.
	self assert: asciiString0 < asciiString2.
	self deny: asciiString1 < asciiString0.
	self deny: asciiString1 < asciiString1.
	self assert: asciiString1 < asciiString2.
	self deny: asciiString2 < asciiString0.
	self deny: asciiString2 < asciiString1.
	self deny: asciiString2 < asciiString2.

	self assert: asciiString0 <= asciiString0.
	self assert: asciiString0 <= asciiString1.
	self assert: asciiString0 <= asciiString2.
	self deny: asciiString1 <= asciiString0.
	self assert: asciiString1 <= asciiString1.
	self assert: asciiString1 <= asciiString2.
	self deny: asciiString2 <= asciiString0.
	self deny: asciiString2 <= asciiString1.
	self assert: asciiString2 <= asciiString2.

	self deny: asciiString0 > asciiString0.
	self deny: asciiString0 > asciiString1.
	self deny: asciiString0 > asciiString2.
	self assert: asciiString1 > asciiString0.
	self deny: asciiString1 > asciiString1.
	self deny: asciiString1 > asciiString2.
	self assert: asciiString2 > asciiString0.
	self assert: asciiString2 > asciiString1.
	self deny: asciiString2 > asciiString2.

	self assert: asciiString0 >= asciiString0.
	self deny: asciiString0 >= asciiString1.
	self deny: asciiString0 >= asciiString2.
	self assert: asciiString1 >= asciiString0.
	self assert: asciiString1 >= asciiString1.
	self deny: asciiString1 >= asciiString2.
	self assert: asciiString2 >= asciiString0.
	self assert: asciiString2 >= asciiString1.
	self assert: asciiString2 >= asciiString2.
]

{ #category : 'testing' }
UnicodeStringsTest >> test08b [
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiString0 < asciiUtf8String0.
	self assert: asciiString0 < asciiUtf8String1.
	self assert: asciiString0 < asciiUtf8String2.
	self deny: asciiString1 < asciiUtf8String0.
	self deny: asciiString1 < asciiUtf8String1.
	self assert: asciiString1 < asciiUtf8String2.
	self deny: asciiString2 < asciiUtf8String0.
	self deny: asciiString2 < asciiUtf8String1.
	self deny: asciiString2 < asciiUtf8String2.

	self assert: asciiString0 <= asciiUtf8String0.
	self assert: asciiString0 <= asciiUtf8String1.
	self assert: asciiString0 <= asciiUtf8String2.
	self deny: asciiString1 <= asciiUtf8String0.
	self assert: asciiString1 <= asciiUtf8String1.
	self assert: asciiString1 <= asciiUtf8String2.
	self deny: asciiString2 <= asciiUtf8String0.
	self deny: asciiString2 <= asciiUtf8String1.
	self assert: asciiString2 <= asciiUtf8String2.

	self deny: asciiString0 > asciiUtf8String0.
	self deny: asciiString0 > asciiUtf8String1.
	self deny: asciiString0 > asciiUtf8String2.
	self assert: asciiString1 > asciiUtf8String0.
	self deny: asciiString1 > asciiUtf8String1.
	self deny: asciiString1 > asciiUtf8String2.
	self assert: asciiString2 > asciiUtf8String0.
	self assert: asciiString2 > asciiUtf8String1.
	self deny: asciiString2 > asciiUtf8String2.

	self assert: asciiString0 >= asciiUtf8String0.
	self deny: asciiString0 >= asciiUtf8String1.
	self deny: asciiString0 >= asciiUtf8String2.
	self assert: asciiString1 >= asciiUtf8String0.
	self assert: asciiString1 >= asciiUtf8String1.
	self deny: asciiString1 >= asciiUtf8String2.
	self assert: asciiString2 >= asciiUtf8String0.
	self assert: asciiString2 >= asciiUtf8String1.
	self assert: asciiString2 >= asciiUtf8String2.
]

{ #category : 'testing' }
UnicodeStringsTest >> test08c [
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiUtf8String0 < asciiString0.
	self assert: asciiUtf8String0 < asciiString1.
	self assert: asciiUtf8String0 < asciiString2.
	self deny: asciiUtf8String1 < asciiString0.
	self deny: asciiUtf8String1 < asciiString1.
	self assert: asciiUtf8String1 < asciiString2.
	self deny: asciiUtf8String2 < asciiString0.
	self deny: asciiUtf8String2 < asciiString1.
	self deny: asciiUtf8String2 < asciiString2.

	self assert: asciiUtf8String0 <= asciiString0.
	self assert: asciiUtf8String0 <= asciiString1.
	self assert: asciiUtf8String0 <= asciiString2.
	self deny: asciiUtf8String1 <= asciiString0.
	self assert: asciiUtf8String1 <= asciiString1.
	self assert: asciiUtf8String1 <= asciiString2.
	self deny: asciiUtf8String2 <= asciiString0.
	self deny: asciiUtf8String2 <= asciiString1.
	self assert: asciiUtf8String2 <= asciiString2.

	self deny: asciiUtf8String0 > asciiString0.
	self deny: asciiUtf8String0 > asciiString1.
	self deny: asciiUtf8String0 > asciiString2.
	self assert: asciiUtf8String1 > asciiString0.
	self deny: asciiUtf8String1 > asciiString1.
	self deny: asciiUtf8String1 > asciiString2.
	self assert: asciiUtf8String2 > asciiString0.
	self assert: asciiUtf8String2 > asciiString1.
	self deny: asciiUtf8String2 > asciiString2.

	self assert: asciiUtf8String0 >= asciiString0.
	self deny: asciiUtf8String0 >= asciiString1.
	self deny: asciiUtf8String0 >= asciiString2.
	self assert: asciiUtf8String1 >= asciiString0.
	self assert: asciiUtf8String1 >= asciiString1.
	self deny: asciiUtf8String1 >= asciiString2.
	self assert: asciiUtf8String2 >= asciiString0.
	self assert: asciiUtf8String2 >= asciiString1.
	self assert: asciiUtf8String2 >= asciiString2.
]

{ #category : 'testing' }
UnicodeStringsTest >> test08d [
"Need a similar test for non-ascii stuff. But we also need to implement the correct behavior!"

	| asciiString0 asciiString1 asciiString2 asciiUtf8String0 asciiUtf8String1 asciiUtf8String2 |
	asciiString0 _ 'Helle world!' asByteString.
	asciiString1 _ 'Hello world' asByteString.
	asciiString2 _ 'Hellu world!' asByteString.
	asciiUtf8String0 _ asciiString0 asUnicodeString.
	asciiUtf8String1 _ asciiString1 asUnicodeString.
	asciiUtf8String2 _ asciiString2 asUnicodeString.

	self deny: asciiUtf8String0 < asciiUtf8String0.
	self assert: asciiUtf8String0 < asciiUtf8String1.
	self assert: asciiUtf8String0 < asciiUtf8String2.
	self deny: asciiUtf8String1 < asciiUtf8String0.
	self deny: asciiUtf8String1 < asciiUtf8String1.
	self assert: asciiUtf8String1 < asciiUtf8String2.
	self deny: asciiUtf8String2 < asciiUtf8String0.
	self deny: asciiUtf8String2 < asciiUtf8String1.
	self deny: asciiUtf8String2 < asciiUtf8String2.

	self assert: asciiUtf8String0 <= asciiUtf8String0.
	self assert: asciiUtf8String0 <= asciiUtf8String1.
	self assert: asciiUtf8String0 <= asciiUtf8String2.
	self deny: asciiUtf8String1 <= asciiUtf8String0.
	self assert: asciiUtf8String1 <= asciiUtf8String1.
	self assert: asciiUtf8String1 <= asciiUtf8String2.
	self deny: asciiUtf8String2 <= asciiUtf8String0.
	self deny: asciiUtf8String2 <= asciiUtf8String1.
	self assert: asciiUtf8String2 <= asciiUtf8String2.

	self deny: asciiUtf8String0 > asciiUtf8String0.
	self deny: asciiUtf8String0 > asciiUtf8String1.
	self deny: asciiUtf8String0 > asciiUtf8String2.
	self assert: asciiUtf8String1 > asciiUtf8String0.
	self deny: asciiUtf8String1 > asciiUtf8String1.
	self deny: asciiUtf8String1 > asciiUtf8String2.
	self assert: asciiUtf8String2 > asciiUtf8String0.
	self assert: asciiUtf8String2 > asciiUtf8String1.
	self deny: asciiUtf8String2 > asciiUtf8String2.

	self assert: asciiUtf8String0 >= asciiUtf8String0.
	self deny: asciiUtf8String0 >= asciiUtf8String1.
	self deny: asciiUtf8String0 >= asciiUtf8String2.
	self assert: asciiUtf8String1 >= asciiUtf8String0.
	self assert: asciiUtf8String1 >= asciiUtf8String1.
	self deny: asciiUtf8String1 >= asciiUtf8String2.
	self assert: asciiUtf8String2 >= asciiUtf8String0.
	self assert: asciiUtf8String2 >= asciiUtf8String1.
	self assert: asciiUtf8String2 >= asciiUtf8String2.
]

{ #category : 'testing' }
UnicodeStringsTest >> test09 [

	self assert: 'Hello ñandú' asByteString = 'Hello ñandú' asByteString.
	self deny: 'Hello ñandú' asByteString = 'hello ñandú' asByteString.
	self assert: ('Hello ñandú' asByteString sameAs: 'hello ñandú' asByteString).

	self assert: 'Hello ñandú' asUnicodeString = 'Hello ñandú' asUnicodeString.
	self deny: 'Hello ñandú' asUnicodeString = 'hello ñandú' asUnicodeString.
	self assert: ('Hello ñandú' asUnicodeString sameAs: 'hello ñandú' asUnicodeString).

	self assert: 'Hello ñandú' asUnicodeString bytes = 'Hello ñandú' asUnicodeString bytes.
	self deny: 'Hello ñandú' asUnicodeString bytes = 'hello ñandú' asUnicodeString bytes.
]

{ #category : 'testing' }
UnicodeStringsTest >> test10 [

	self assert: ('ñandúX' asByteString findString: 'X' asByteString startingAt: 1) = 6.
	self assert: ('ñandúX' asUnicodeString findString: 'X' asByteString startingAt: 1) = 6.
	
	self assert: ('ñandúX' asByteString findString: 'X' startingAt: 7 caseSensitive: false) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'X' startingAt: 7 caseSensitive: false) = 0.
	self assert: ('ñandúX' asByteString findString: 'X' startingAt: 7 caseSensitive: true) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'X' startingAt: 7 caseSensitive: true) = 0.
	self assert: ('ñandúX' asByteString findString: 'x' startingAt: 1 caseSensitive: true) = 0.
	self assert: ('ñandúX' asUnicodeString findString: 'x' startingAt: 1 caseSensitive: true) = 0.
]

{ #category : 'testing' }
UnicodeStringsTest >> test11 [
	self assert: ('ñandúes' asByteString indexOfSubCollection: 'úe' startingAt: 1 ifAbsent: []) = 5.
	self assert: ('ñandúes' asUnicodeString indexOfSubCollection: 'úe' startingAt: 1 ifAbsent: []) = 5.
	self assert: ('ñandúes' asByteString indexOfSubCollection: 'úe' startingAt: 6 ifAbsent: []) = nil.
	self assert: ('ñandúes' asUnicodeString indexOfSubCollection: 'úe' startingAt: 6 ifAbsent: []) = nil.
]

{ #category : 'testing' }
UnicodeStringsTest >> test12 [
	self deny: ('ñandúes' asByteString is: 'úe' substringAt: 4).
	self assert: ('ñandúes' asByteString is: 'úe' substringAt: 5).
	self deny: ('ñandúes' asByteString is: 'úe' substringAt: 6).
	self deny: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 4).
	self assert: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 5).
	self deny: ('ñandúes' asByteString is: 'úe' asUnicodeString substringAt: 6).
	self deny: ('ñandúes' asUnicodeString is: 'úe' substringAt: 4).
	self assert: ('ñandúes'asUnicodeString  is: 'úe' substringAt: 5).
	self deny: ('ñandúes' asUnicodeString is: 'úe' substringAt: 6).
	self deny: ('ñandúes' asUnicodeString is: 'úe' asUnicodeString substringAt: 4).
	self assert: ('ñandúes'asUnicodeString  is: 'úe' asUnicodeString substringAt: 5).
	self deny: ('ñandúes' asUnicodeString is: 'úe' asUnicodeString substringAt: 6).
]

{ #category : 'testing' }
UnicodeStringsTest >> testAsUtf8 [
	"
	 UnicodeStringsTest new testAsUtf8
	"
	| subject |
	subject := 'A¢€'.
	self assert: subject asUtf8Bytes hex = '41C2A2E282AC'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.
	subject := '¿Ñandú, agüita?'.
	self assert: subject asUtf8Bytes hex = 'C2BFC391616E64C3BA2C206167C3BC6974613F'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.
	self assert: subject asByteString asUtf8Bytes = subject asUtf8Bytes.
	self assert: subject asByteString utf8BytesSize = subject asUtf8Bytes size.
]

{ #category : 'testing' }
UnicodeStringsTest >> testAtPut [
	| a b c |
	a _ 'Agüita'.

	b _ a copy.
	self assert: a = b.
	self assert: a hash = b hash.

	c _ b at: 1 put: $Ñ.
	self assert: c = $Ñ.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: (b at: 1) = $Ñ.
	self assert: b = 'Ñgüita'.

	b _ a copy.
	c _ b at: 3 put: $u.
	self assert: c = $u.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: (b at: 3) = $u.
	self assert: b = 'Aguita'.

	b _ a copy.
	c _ b at: b size put: $á.
	self assert: c = $á.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: b last = $á.
	self assert: b = 'Agüitá'.

	b _ a copy.
	c _ b at: b size put: $π.
	self assert: c = $π.
	self deny: a = b.
	self deny: a hash = b hash.
	self assert: a size = b size.
	self assert: b last = $π.
	self assert: b = 'Agüitπ'.
]

{ #category : 'testing' }
UnicodeStringsTest >> testEmpty [

	| s1 us1 us2 |
	s1 := String new.
	us1 := UnicodeString new.
	us2 := UnicodeString fromUtf8Bytes:#[].

	self assert: s1 hash = us1 hash.
	self assert: s1 = us1.
	self assert: us1 = s1.

	self assert: us2 hash = us1 hash.
	self assert: us2 = us1.
	self assert: us1 = us2.
]

{ #category : 'testing' }
UnicodeStringsTest >> testUtf8InvalidSequences [
	"
	 UnicodeStringsTest new testUtf8InvalidSequences
	"
	
	self deny: (Character isValidUtf8: #[16rE2 16r28 16rA1]).
	self deny: (Character isValidUtf8: #[16rF8]).
	self deny: (Character isValidUtf8: #[16r92]).
	self deny: (Character isValidUtf8: #[16rF5]).
	self deny: (Character isValidUtf8: #[16rF0 16r90 16r8D]).

	"Cuis correctly allows overlong sequences...
	(UnicodeString fromUtf8Bytes: #[16rC0  16r80]) first asString asUnicodeString bytes"
	"self deny: (Character isValidUtf8: #[16rC0  16r80])."
]

{ #category : 'testing' }
UnicodeStringsTest >> testUtf8ValidSequences [
	"
	 UnicodeStringsTest new testUtf8ValidSequences
	"
	self assert: (Character isValidUtf8: 'Hello world' asUnicodeString bytes).
	self assert: (Character isValidUtf8: '€' bytes).
	self assert: (Character isValidUtf8: '🐱' bytes).
	self assert: (Character isValidUtf8: '🐱🐱🐱€🐱 ¡Tomá agüita gato!' bytes).

]

{ #category : 'testing' }
UnicodeStringsTest >> testfromBytesStream [

	| s large |
	s := 'αℤ⇉⊕§☃§'.
	self assert: s = (UnicodeString fromBytesStream: s asUtf8Bytes readStream).
	large := UnicodeString streamContents: [ :strm |
		1000 timesRepeat: [ strm nextPutAll: s ]].
	self assert: large = (UnicodeString fromBytesStream: large asUtf8Bytes readStream).
]
