Class {
	#name : 'TrieUnicodeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Text',
	#package : 'BaseImageTests',
	#tag : 'System-Text'
}

{ #category : 'testing' }
TrieUnicodeTest >> test01 [
	| t all |
	t _ Trie new.
	t add: #toto.
	t at: #toton put: 47.
	t add: 'carrr' asByteString.
	t at: 'car' asByteString put: Float pi.
	t at: 'cat' asByteString put: Date today.
	self assert: (t includesKey: 'car').
	self deny: (t includes: 'car').
	self assert: (t includesKey: 'cat').
	self deny: (t includes: 'cat').
	self assert: (t includes: #toto).
	self assert: (t includes: 'toto').
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 5.
	self assert: (all includes: {#toto. #toto class. #toto. #toto class}).
	self assert: (all includes: {#toton. #toton class. 47. SmallInteger}).
	self assert: (all includes: {'carrr'. String. 'carrr'. String}).
	self assert: (all includes: {'car'. String. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. String. Date today. Date}).
]

{ #category : 'testing' }
TrieUnicodeTest >> test02 [
	| t all |
	t _ Trie new.
	t add: #toto.
	t at: #toton put: 47.
	t add: 'carrrs' asByteString.
	t add: 'carrr' asUnicodeString.
	t at: 'car' asUnicodeString put: Float pi.
	t at: 'cat' asByteString put: Date today.
	self assert: (t includesKey: 'carrrs' asByteString).
	self assert: (t includes: 'carrrs' asByteString).
	self assert: (t includesKey: 'carrrs' asUnicodeString).
	self assert: (t includes: 'carrrs' asUnicodeString).
	self assert: (t includesKey: 'carrr' asByteString).
	self assert: (t includes: 'carrr' asByteString).
	self assert: (t includesKey: 'carrr' asUnicodeString).
	self assert: (t includes: 'carrr' asUnicodeString).
	self assert: (t includesKey: 'car' asByteString).
	self deny: (t includes: 'car' asByteString).
	self assert: (t includesKey: 'cat' asByteString).
	self deny: (t includes: 'cat' asByteString).
	self assert: (t includes: #toto).
	self assert: (t includes: 'toto' asByteString).
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 6.
	self assert: (all includes: {#toto. #toto class. #toto. #toto class}).
	self assert: (all includes: {#toton. #toton class. 47. SmallInteger}).
	self assert: (all includes: {'carrrs'. String. 'carrrs'. String}).
	self assert: (all includes: {'carrr'. UnicodeString. 'carrr'. UnicodeString}).
	self assert: (all includes: {'car'. UnicodeString. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. String. Date today. Date}).
]

{ #category : 'testing' }
TrieUnicodeTest >> test03 [
	| t all s1 s2 |
	t _ Trie new.
	s1 _ ('totoU' asByteString, Random next mantissaPart printString) asUnicodeString asSymbol.
	t add: s1.
	s2 _ ('totóUn' asByteString, Random next mantissaPart printString) asUnicodeString asSymbol.
	t at: s2 put: 47.
	t add: 'carrr' asUnicodeString.
	t at: 'car' asByteString put: Float pi.
	t at: 'cat' asUnicodeString put: Date today.
	self assert: (t includesKey: 'car' asByteString).
	self deny: (t includes: 'car' asByteString).
	self assert: (t includesKey: 'cat' asByteString).
	self deny: (t includes: 'cat' asByteString).
	self assert: (t includes: s1).
	self assert: (t includes: s1 asString).
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 5.
	self assert: (all includes: {s1. Symbol. s1. Symbol}).
	self assert: (all includes: {s2. UnicodeSymbol. 47. SmallInteger}).
	self assert: (all includes: {'carrr'. UnicodeString. 'carrr'. UnicodeString}).
	self assert: (all includes: {'car'. String. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. UnicodeString. Date today. Date}).
]
