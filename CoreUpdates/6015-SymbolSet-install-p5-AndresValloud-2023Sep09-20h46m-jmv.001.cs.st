'From Cuis 6.0 [latest update: #6013] on 9 September 2023 at 8:58:30 pm'!
!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'NewSymbols OneCharacterSymbols SymbolTable Symbols '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character methodsFor: 'converting' stamp: 'jmv 9/9/2023 20:48:15'!
asSymbol
	^self asString asSymbol! !


!Symbol class methodsFor: 'access' stamp: 'jmv 9/9/2023 20:57:06'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList |

	selectorList := OrderedCollection new.
	(size := aString size) = 0 ifTrue: [^selectorList].

	aString first isValidInIdentifiers ifFalse: [
		aString size = 2 ifTrue: [
			Symbol hasInterned: aString ifTrue: [ :s | selectorList add: s]].
		^selectorList ].

	self allSymbolTablesDo: [ :each |
		each size >= size ifTrue: [
			(each findString: aString startingAt: 1 caseSensitive: false) > 0
				ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asPlainString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !

!Symbol class methodsFor: 'class initialization' stamp: 'sqr 9/9/2023 20:47:31'!
initialize

	"
	Symbol initialize.
	SystemDictionary initialize.
	"
	Symbol rehash.! !

!methodRemoval: Symbol class #internCharacter: stamp: 'jmv 9/9/2023 20:48:17'!
Symbol class removeSelector: #internCharacter:!
!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'Symbols'
	poolDictionaries: ''
	category: 'Kernel-Text'!
Symbol initialize!
