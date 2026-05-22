Class {
	#name : 'StringTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'tests - grammar' }
StringTest >> testArticle [
	self
		assert: 'euphemism' withArticle equals: 'a euphemism';
		assert: 'European' withArticle equals: 'a European';
		assert: 'Euclidean space' withArticle equals: 'a Euclidean space';
		assert: 'university' withArticle equals: 'a university';
		assert: 'anagram' withArticle equals: 'an anagram';
		assert: 'apple' withArticle equals: 'an apple';
		assert: 'Ukranian' withArticle equals: 'a Ukranian';
		assert: 'Argentine' withArticle equals: 'an Argentine';
		assert: 'user' withArticle equals: 'a user'
]

{ #category : 'UTF-8 conversion' }
StringTest >> testAsUtf8 [
	"
	 StringTest new testAsUtf8
	"
	| subject |
	subject := '¿Ñandú, agüita?' asByteString.
	self assert: subject asUtf8Bytes hex = 'C2BFC391616E64C3BA2C206167C3BC6974613F'.
	self assert: subject utf8BytesSize = subject asUtf8Bytes size.
]

{ #category : 'tests - converting' }
StringTest >> testBase64 [

	| bytes |
	bytes _ ('Hello World' collect: [ :char | char asciiValue ]) asByteArray.
	self 
		assert: 'SGVsbG8gV29ybGQ=' base64Decoded = bytes;
		assert: bytes base64Encoded = 'SGVsbG8gV29ybGQ=';
		assert: (String new: 100 withAll: $x) asByteArray base64Encoded  =
'eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4
eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eA=='
]

{ #category : 'testing' }
StringTest >> testBeginsWith [

	self assert: ('Elvis' asByteString beginsWith: 'El' asByteString).
	self deny: ('Elvis' asByteString beginsWith: 'Paul' asByteString).
	self assert: ('Elvis' asByteString beginsWith: 'El' asUnicodeString).
	self deny: ('Elvis' asByteString beginsWith: 'Paul' asUnicodeString).
	self assert: ('Elvis' asUnicodeString beginsWith: 'El' asByteString).
	self deny: ('Elvis' asUnicodeString beginsWith: 'Paul' asByteString).
	self assert: ('Elvis' asUnicodeString beginsWith: 'El' asUnicodeString).
	self deny: ('Elvis' asUnicodeString beginsWith: 'Paul' asUnicodeString).
]

{ #category : 'testing' }
StringTest >> testCaseConversion [
	"
	self new testCaseConversion
	"
	self assert: ('año Comé tomá Camión' asLowercase) = 'año comé tomá camión'.
	self assert: ('año Comé tomá Camión' asUppercase) = 'AÑO COMÉ TOMÁ CAMIÓN'
]

{ #category : 'testing' }
StringTest >> testEncompassParagraph1 [

	self assert: ('a' encompassParagraph: (1 to: 0)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (1 to: 1)) = (1 to: 1).
	self assert: ('a' encompassParagraph: (2 to: 1)) = (1 to: 1).
]

{ #category : 'testing' }
StringTest >> testEncompassParagraph2 [

	self assert: ('ab' encompassParagraph: (1 to: 0)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (1 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 1)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (2 to: 2)) = (1 to: 2).
	self assert: ('ab' encompassParagraph: (3 to: 2)) = (1 to: 2).
]

{ #category : 'testing' }
StringTest >> testEncompassParagraph3 [

self assert: ('a
' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
' encompassParagraph: (3 to: 2)) = (3 to: 2).

self assert: ('a
zcxv' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
zcxv' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
zxcv' encompassParagraph: (3 to: 2)) = (3 to: 6).
]

{ #category : 'testing' }
StringTest >> testEncompassParagraph4 [

self assert: ('
b' encompassParagraph: (1 to: 0)) = (1 to: 1).
self assert: ('
b' encompassParagraph: (1 to: 1)) = (1 to: 1).
self assert: ('
b' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('
b' encompassParagraph: (2 to: 1)) = (2 to: 2).
self assert: ('
b' encompassParagraph: (2 to: 2)) = (2 to: 2).
self assert: ('
b' encompassParagraph: (3 to: 2)) = (2 to: 2).
]

{ #category : 'testing' }
StringTest >> testEncompassParagraph5 [

self assert: ('a
b' encompassParagraph: (1 to: 0)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 1)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 2)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (1 to: 3)) = (1 to: 3).
self assert: ('a
b' encompassParagraph: (2 to: 1)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (2 to: 2)) = (1 to: 2).
self assert: ('a
b' encompassParagraph: (2 to: 3)) = (1 to: 3).
self assert: ('a
b' encompassParagraph: (3 to: 2)) = (3 to: 3).
self assert: ('a
b' encompassParagraph: (3 to: 3)) = (3 to: 3).
self assert: ('a
b' encompassParagraph: (4 to: 3)) = (3 to: 3).
]

{ #category : 'testing' }
StringTest >> testFindSelector  [

	self assert: '"self printOn:' findSelector equals: #printOn:.
	self assert: 'self printOn:"' findSelector equals: #printOn:.
	self assert: '"self printOn:"' findSelector equals: #printOn:.
	self assert: 'self printOn:' findSelector equals: #printOn:.

]

{ #category : 'testing' }
StringTest >> testFirstNonSeparator [

	self assert: 'abc' firstNonSeparator equals: 1.
	self assert: ' abc' firstNonSeparator equals: 2.
	self assert: '' firstNonSeparator equals: 0.
]

{ #category : 'testing' }
StringTest >> testIsString [
	self assert: ('Hello World' isString).
	self assert: ('' isString).
]

{ #category : 'testing' }
StringTest >> testLastNonSeparator [

	self assert: 'abc' lastNonSeparator equals: 3.
	self assert: 'abc ' lastNonSeparator equals: 3.
	self assert: ' abc ' lastNonSeparator equals: 4.
	self assert: '' lastNonSeparator equals: 0
]

{ #category : 'testing' }
StringTest >> testLineSeparators [
	"
	Test that #newLineCharacter is considered a line separator and not a line terminator.
	This means that the last line never ends with a #newLineCharacter (although it might be empty!)
	StringTest new testLineSeparators
	"
	| justAnLf linesBounds |
	linesBounds _ OrderedCollection new.
	justAnLf _ '
'.
	justAnLf lineIndicesDo: [ :start :endWithoutDelimiters :end |
		linesBounds add: { start . endWithoutDelimiters. end }.
		].

	self assert: linesBounds size = 2 description: 'There should be two lines.'.

	self assert: linesBounds first first = 1 description: 'First line starts at position 1'.
	self assert: linesBounds first second = (linesBounds first first-1) description: 'First line is empty'.
	self assert: linesBounds first third = (linesBounds first second+1) description: 'First line is terminated by ab Lf'.

	self assert: linesBounds second first = ( linesBounds first third+1) description: 'Second line starts after end of first line'.
	self assert: linesBounds second second = (linesBounds second first-1) description: 'Second line is empty'.
	self assert: linesBounds second third = (linesBounds second second+0) description: 'Second line is not terminated by ab Lf'.
]

{ #category : 'tests - padding' }
StringTest >> testPaddingCenterAddCharsToBothSides [

	self 
		assert: ('hi' padded: #center to: 4 with: $.)
		equals: '.hi.'.
		
	self 
		assert: ('hi' padded: #center to: 5 with: $.)
		equals: '.hi..'.
]

{ #category : 'tests - padding' }
StringTest >> testPaddingLeftAddCharsToTheLeft [

	self 
		assert: ('hi' padded: #left to: 4 with: $.)
		equals: '..hi'.
		
	self 
		assert: ('hi' padded: #left to: 1 with: $.)
		equals: 'hi'.
]

{ #category : 'tests - padding' }
StringTest >> testPaddingRightAddCharsToTheLeft [

	self 
		assert: ('hi' padded: #right to: 4 with: $.)
		equals: 'hi..'.
		
	self 
		assert: ('hi' padded: #right to: 1 with: $.)
		equals: 'hi'.
]

{ #category : 'testing' }
StringTest >> testSorting [
"
self new testSorting
"

	self assert: 'a' asByteString < 'á' asByteString.
	self deny: 'a' asByteString < 'Á' asByteString.
	self assert: 'A' asByteString < 'á' asByteString.
	self assert: 'A' asByteString < 'Á' asByteString.
	self assert: 'á' asByteString < 'b' asByteString.
	self deny: 'á' asByteString < 'B' asByteString.
	self assert: 'Á' asByteString < 'b' asByteString.
	self assert: 'Á' asByteString < 'B' asByteString.

	self assert: ('a' asByteString caseInsensitiveLessOrEqual: 'á' asByteString).
	self assert: ('a' asByteString caseInsensitiveLessOrEqual: 'Á' asByteString).
	self assert: ('A' asByteString caseInsensitiveLessOrEqual: 'á' asByteString).
	self assert: ('A' asByteString caseInsensitiveLessOrEqual: 'Á' asByteString).
	self assert: ('á' asByteString caseInsensitiveLessOrEqual: 'b' asByteString).
	self assert: ('á' asByteString caseInsensitiveLessOrEqual: 'B' asByteString).
	self assert: ('Á' asByteString caseInsensitiveLessOrEqual: 'b' asByteString).
	self assert: ('Á' asByteString caseInsensitiveLessOrEqual: 'B' asByteString).
]

{ #category : 'testing' }
StringTest >> testWithBlanksTrimmed [

	self assert: ' abc' withBlanksTrimmed equals: 'abc'.
	self assert: 'abc ' withBlanksTrimmed equals: 'abc'.
	self assert: ' abc ' withBlanksTrimmed equals: 'abc'.
	self assert: ' a b c ' withBlanksTrimmed equals: 'a b c'.
	self assert: '' equals: '' withBlanksTrimmed.
	
]

{ #category : 'testing' }
StringTest >> testWithoutLeadingBlanks [

	self assert: ' abc' withoutLeadingBlanks equals: 'abc'.
	self assert: 'abc' withoutLeadingBlanks equals: 'abc'.
	self assert: '' withoutLeadingBlanks isEmpty

]

{ #category : 'testing' }
StringTest >> testWithoutPrefix [

	self assert: ('Elvis' asByteString withoutPrefix: 'El' asByteString) = 'vis'.
	self assert: ('Elvis' asByteString withoutPrefix: 'Paul' asByteString) isNil.
	self assert: ('Elvis' asByteString withoutPrefix: 'El' asUnicodeString) = 'vis'.
	self assert: ('Elvis' asByteString withoutPrefix: 'Paul' asUnicodeString) isNil.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'El' asByteString) = 'vis'.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'Paul asByteString') isNil.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'El' asUnicodeString) = 'vis'.
	self assert: ('Elvis' asUnicodeString withoutPrefix: 'Paul' asUnicodeString) isNil.
]

{ #category : 'testing' }
StringTest >> testWithoutSeparators [

	self assert: 'abc' withoutSeparators equals: 'abc'.
	self assert: ' a b c ' withoutSeparators equals: 'abc'
]

{ #category : 'testing' }
StringTest >> testWithoutTrailingBlanks [

	self assert: 'abc ' withoutTrailingBlanks equals: 'abc'.
	self assert: 'abc' withoutTrailingBlanks equals: 'abc'.
	self assert: '' withoutTrailingBlanks isEmpty

]
