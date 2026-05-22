Class {
	#name : 'UnicodeSymbolsTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'testing' }
UnicodeSymbolsTest >> test01AsciiSymbolFirst [
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ 'Stuff' asByteString, Random next mantissaPart printString.
	symbol _ str asSymbol.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.
]

{ #category : 'testing' }
UnicodeSymbolsTest >> test02LatinSymbolFirst [
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ '¡Tomá agüita, Ñandú!' asByteString, Random next mantissaPart printString.
	symbol _ str asSymbol.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.
]

{ #category : 'testing' }
UnicodeSymbolsTest >> test03AsciiUtf8SymbolFirst [
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ 'Stuff' asByteString, Random next mantissaPart printString.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.
	symbol _ str asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.
]

{ #category : 'testing' }
UnicodeSymbolsTest >> test04LatinUtf8SymbolFirst [
	"Try a few things with pure ASCII.
	Do #asSymbol before doing #asUtf8Symbol. The symbol created will be instance of Symbol."
	
	| str symbol utf8 utf8Symbol |
	str _ '¡Tomá agüita, Ñandú!' asByteString, Random next mantissaPart printString.
	utf8 _ str asUnicodeString.
	utf8Symbol _ utf8 asSymbol.
	symbol _ str asSymbol.

	self assert: str = str.
	self assert: str == str.
	self assert: symbol = symbol.
	self assert: symbol == symbol.
	self assert: utf8 = utf8.
	self assert: utf8 == utf8.
	self assert: utf8Symbol = utf8Symbol.
	self assert: utf8Symbol == utf8Symbol.

	self assert: str = symbol.
	self assert: symbol = str.
	self assert: utf8 = utf8Symbol.
	self assert: utf8Symbol = utf8.

	self assert: str = utf8.
	self assert: utf8 = str.
	self assert: symbol = utf8Symbol.
	self assert: utf8Symbol = symbol.

	self assert: str = utf8Symbol.
	self assert: utf8Symbol = str.
	self assert: symbol = utf8.
	self assert: utf8 = symbol.

	self assert: utf8Symbol == symbol.
	self assert: symbol == utf8Symbol.
]

{ #category : 'testing' }
UnicodeSymbolsTest >> testSerialization [
	self assert: (Object unStream: (ReferenceStream streamedRepresentationOf: 'hello' asUnicodeString asSymbol)) = 'hello' asUnicodeString asSymbol
]
