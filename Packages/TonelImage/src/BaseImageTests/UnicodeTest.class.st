Class {
	#name : 'UnicodeTest',
	#superclass : 'TestCase',
	#instVars : [
		'bytesOfExample1'
	],
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'as yet unclassified' }
UnicodeTest class >> fileName [
	^'UTF8abc-test.txt'
]

{ #category : 'as yet unclassified' }
UnicodeTest class >> fileName2 [
	^'UTF8abc-test2.txt'
]

{ #category : 'set up' }
UnicodeTest >> setUp [
	"
	self new setUp
	"
	super setUp.
	bytesOfExample1 := #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
						16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3].

	"see UnicodeNotes.md"

	"write the bytes of an UFT8 encoded string in binary mode to a file"
	self class fileName asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: bytesOfExample1 ]
]

{ #category : 'testing' }
UnicodeTest >> test1ReadBinary [
	
	"see UnicodeNotes.md"
     "
	self new setUp test1ReadBinary
	"
     | content |
	content := self class fileName asFileEntry binaryContents.
	self assert: content = bytesOfExample1
]

{ #category : 'testing' }
UnicodeTest >> test2ReadWithOutBinary [
	
	"see UnicodeNotes.md"
     "
	self new setUp test2ReadWithOutBinary
	"
     | content |
	content := self class fileName asFileEntry textContents.
	self deny: content = bytesOfExample1
]

{ #category : 'testing' }
UnicodeTest >> test3ReadUtf8 [
	
	"see UnicodeNotes.md"
     "
	self new setUp test3ReadUtf8
	"
	| content byteArray |
	byteArray := self class fileName asFileEntry binaryContents.
	content := UnicodeString fromUtf8Bytes: byteArray.
	self assert: content = 'abc àè€ αβγ'.
]

{ #category : 'testing' }
UnicodeTest >> test4BackConversion [
	"see UnicodeNotes.md"
	"
	self new setUp test4BackConversion
	"
	| contentInternalString contentByteArray |
	contentInternalString := UnicodeString fromUtf8Bytes: self class fileName asFileEntry binaryContents.
	contentByteArray := self class fileName asFileEntry binaryContents.
	self assert: contentByteArray = contentInternalString asUtf8Bytes.
]

{ #category : 'testing' }
UnicodeTest >> test5ReadWriteUtf8 [
	"see UnicodeNotes.md"
	"
	self new setUp test5ReadWriteUtf8
	"
	| content byteArray byteArray2 |
	byteArray := self class fileName asFileEntry binaryContents.
	content := UnicodeString fromUtf8Bytes: byteArray.
	self class fileName2 asFileEntry forceWriteStreamDo: [ :stream |
		stream binary.
		stream nextPutAll: content asUtf8Bytes ].
	"compare the two versions: what is in file 'fileName' with what is in file 'fileName2'"
	byteArray := self class fileName asFileEntry binaryContents.
	byteArray2 := self class fileName2 asFileEntry binaryContents.
	self assert: byteArray = byteArray2.
]

{ #category : 'testing' }
UnicodeTest >> testTextConcatenation [
	"
	UnicodeTest new testTextConcatenation
	"

	| s1 s2 u1 u2 |
	s1 := 'Hello' asByteString.
	s2 := ' world' asByteString.
	u1 := s1 asUnicodeString.
	u2 := s2 asUnicodeString.

	self assert: s1, u2 equals: s1, s2.
	self assert: u1, s2 equals: s1, s2.
	self assert: u1, u2 equals: s1, s2.

	self assert: s1, u2 bold equals: s1, s2 bold.
	self assert: u1, s2 bold equals: s1, s2 bold.
	self assert: u1, u2 bold equals: s1, s2 bold.

	self assert: s1 bold, u2 equals: s1 bold, s2.
	self assert: u1 bold, s2 equals: s1 bold, s2.
	self assert: u1 bold, u2 equals: s1 bold, s2.

	self assert: s1, u2 bold equals: s1, s2 bold.
	self assert: u1, s2 bold equals: s1, s2 bold.
	self assert: u1, u2 bold equals: s1, s2 bold.
]

{ #category : 'testing utf16' }
UnicodeTest >> testUtf16BytesFromUtf8 [
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf16Bytes utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: false includeBOM: false.
	self assert: utf16Bytes = #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: true includeBOM: false.
	self assert: utf16Bytes = #[16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: false includeBOM: true.
	self assert: utf16Bytes = #[16rFF 16rFE 16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF].

	utf16Bytes := UnicodeString utf16BytesFromUtf8: utf8 bigEndian: true includeBOM: true.
	self assert: utf16Bytes = #[16rFE 16rFF 16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62].
]

{ #category : 'testing utf16' }
UnicodeTest >> testUtf16WordsFromUtf8 [
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf16Words utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].

	utf16Words := UnicodeString utf16WordsFromUtf8: utf8 includeBOM: true.
	self assert: utf16Words asArray = {16rFEFF. 16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62}.

	utf16Words := UnicodeString utf16WordsFromUtf8: utf8 includeBOM: false.
	self assert: utf16Words asArray = {16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62}.
]

{ #category : 'testing utf16' }
UnicodeTest >> testUtf8FromUtf16Bytes [
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16rFF 16rFE 16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16rFE 16rFF 16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF])
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r24 16r0 16rAC 16r20 16r1 16rD8 16r37 16rDC 16r52 16rD8 16r62 16rDF] bigEndianHint: false)
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[16r0 16r24 16r20 16rAC 16rD8 16r1 16rDC 16r37 16rD8 16r52 16rDF 16r62] bigEndianHint: true)
		equals: utf8.

	self
		assert: (UnicodeString utf8FromUtf16Bytes: #[])
		equals: #[].
]

{ #category : 'testing utf16' }
UnicodeTest >> testUtf8FromUtf16Words [
	"Four code point sample from https://en.wikipedia.org/wiki/UTF-16
	CodePoint: 0x24

	UTF-8 Encoding:	0x24
	UTF-16 Encoding:	0x0024
	UTF-32 Encoding:	0x00000024

	CodePoint: 0x20AC
	UTF-8 Encoding:	0xE2 0x82 0xAC
	UTF-16 Encoding:	0x20AC
	UTF-32 Encoding:	0x000020AC

	CodePoint: 0x10437
	UTF-8 Encoding:	0xF0 0x90 0x90 0xB7
	UTF-16 Encoding:	0xD801 0xDC37
	UTF-32 Encoding:	0x00010437

	CodePoint: 0x24B62
	UTF-8 Encoding:	0xF0 0xA4 0xAD 0xA2
	UTF-16 Encoding:	0xD852 0xDF62
	UTF-32 Encoding:	0x00024B62"

	| codePoints utf8 |
	codePoints := {16r24. 16r20AC. 16r10437. 16r24B62}.
	utf8 := UnicodeString utf8FromCodePoints: codePoints.
	"Just in case."
	self assert: utf8 equals: #[16r24 16rE2 16r82 16rAC 16rF0 16r90 16r90 16rB7 16rF0 16rA4 16rAD 16rA2].
	
	self
		assert: (UnicodeString utf8FromUtf16Words: {16rFEFF. 16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62})
		equals: utf8.
	
	self
		assert: (UnicodeString utf8FromUtf16Words: {16r24. 16r20AC. 16rD801. 16rDC37. 16rD852. 16rDF62})
		equals: utf8.

	self
		assert: (UnicodeString utf8FromUtf16Words: {})
		equals: #[].
]
