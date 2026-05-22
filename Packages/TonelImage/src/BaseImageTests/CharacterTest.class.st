Class {
	#name : 'CharacterTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'testing' }
CharacterTest >> testCaseConversion [
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' collect: [ :c | c asLowercase ]) = 'año comé tomá camión'.
	self assert:('año Comé tomá Camión' collect: [ :c | c asUppercase ]) = 'AÑO COMÉ TOMÁ CAMIÓN'
]

{ #category : 'testing' }
CharacterTest >> testCompareIsTransitive [
	| c1 c2 c3 |
	1 to: 512 by: 8 do: [ :n1 |
		c1 := Character codePoint: n1.
		1 to: 512 by: 8 do: [ :n2 |
			c2 := Character codePoint: n2.
			1 to: 512 by: 8 do: [ :n3 |
				c3 := Character codePoint: n3.
				(c1 < c2) & (c2 < c3) ifTrue: [ self assert: (c1 < c3) ]]]]
]

{ #category : 'UTF-8 conversion' }
CharacterTest >> testFromUtf8 [
	"
	CharacterTest new testFromUtf8
	"
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: '24') into: nil into: [ :codePoint | self assert: codePoint hex = '16r24' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'C2A2') into: nil into: [ :codePoint | self assert: codePoint hex = '16rA2' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'E282AC') into: nil into: [ :codePoint | self assert: codePoint hex = '16r20AC' ].
	Character utf8BytesAndCodePointAt: 1 in: (ByteArray readHexFrom: 'F0A4ADA2') into: nil into: [ :codePoint | self assert: codePoint hex = '16r24B62' ].
]

{ #category : 'testing' }
CharacterTest >> testIsCharacter [
	self assert: $a isCharacter.
	self assert: $X isCharacter.
	self assert: Character cr isCharacter.
]

{ #category : 'UTF-8 conversion' }
CharacterTest >> testSomeLatinCharsFromUtf8 [
	"
	CharacterTest new testSomeLatinCharsFromUtf8
	"
	| bytes string |
	bytes _ ByteArray readHexFrom: 'C3A141C3A541C3A641C3B141C38641C2A541C3BC'.
	string _ String streamContents: [ :strm | | s byteIndex n |
		s _ bytes size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes
				into: nil
				into: [ :codePoint |
					strm nextPut: (Character codePoint: codePoint) ].
			byteIndex _ byteIndex + n
		]].
	self assert: string = 'áAåAæAñAÆA¥Aü'
]

{ #category : 'UTF-8 conversion' }
CharacterTest >> testSomeLatinCharsToUtf8 [
	"
	CharacterTest new testSomeLatinCharsToUtf8
	"
	| characters bytes |
	characters := 'áAåAæAñAÆA¥Aü' readStream.
	bytes := ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOf: characters next codePoint ]].
	self assert: bytes hex = 'C3A141C3A541C3A641C3B141C38641C2A541C3BC'
]

{ #category : 'testing' }
CharacterTest >> testSorting [
	"
	self new testSorting
	"
	self assert: $a < $á.
	self deny: $a < $Á.
	self assert: $A < $á.
	self assert: $A < $Á.
	self assert: $á < $b.
	self deny: $á < $B.
	self assert: $Á < $b.
	self assert: $Á < $B.
]

{ #category : 'testing' }
CharacterTest >> testTestingMethods [
	"
	self new testTestingMethods
	"
	self assert:
		('año Comé tomá Camión' allSatisfy: [ :c |
			c = $  or: [ c isLetter ]]).
	self assert:
		('año comé tomá camión' allSatisfy: [ :c |
			c = $  or: [ c isLowercase ]]).
	self assert:
		('AÑO COMÉ TOMÁ CAMIÓN' allSatisfy: [ :c |
			c = $  or: [ c isUppercase ]]).
	self assert:
		('AaÀàÁáÂâÃãÄäÅåEeÈèÉéÊêËëIiÌìÍíÎîÏïOoÒòÓóÔôÕõÖöUuÙùÚúÛûÜü' allSatisfy: [ :c |
			c isVowel ]).
]

{ #category : 'UTF-8 conversion' }
CharacterTest >> testToUtf8 [
	"
	CharacterTest new testToUtf8
	"
	self assert: (Character utf8BytesOf: 16r0024) hex =  '24'.
	self assert: (Character utf8BytesOf: 16r00A2) hex =  'C2A2'.
	self assert: (Character utf8BytesOf: 16r20AC) hex = 'E282AC'.
	self assert: (Character utf8BytesOf: 16r024B62) hex = 'F0A4ADA2'
]
