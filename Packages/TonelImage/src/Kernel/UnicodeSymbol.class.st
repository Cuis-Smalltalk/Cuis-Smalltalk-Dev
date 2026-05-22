Class {
	#name : 'UnicodeSymbol',
	#superclass : 'UnicodeString',
	#category : 'Kernel-Text',
	#package : 'Kernel',
	#tag : 'Text'
}

{ #category : 'private' }
UnicodeSymbol class >> basicFromString: aString [
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	| canonicalComposition utf8Bytes |
	canonicalComposition := aString canonicalComposition.
	utf8Bytes := canonicalComposition asUtf8Bytes.
	^self basicNew privateBytes: utf8Bytes codePointCount: canonicalComposition size.
]

{ #category : 'instance creation' }
UnicodeSymbol class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."

	^ (UnicodeString newFrom: aCollection) asSymbol

"
	UnicodeSymbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: UnicodeSymbol
"
]

{ #category : 'instance creation' }
UnicodeSymbol class >> readFrom: strm [
	"
	UnicodeSymbol readFrom: '#abc' readStream
	"

	strm peek == $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"
]

{ #category : 'testing' }
UnicodeSymbol class >> representsSymbols [
	^true
]

{ #category : 'comparing' }
UnicodeSymbol >> = another [

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another isSymbol ifTrue: [
		^ false].  "Was not ==, therefore can't be = "

	"Otherwise use string =..."
	^ super = another
]

{ #category : 'converting' }
UnicodeSymbol >> asPlainString [
	"Answer a plain String, not a Symbol."

	^ self species fromUtf8Bytes: bytes codePointCount: self size.
]

{ #category : 'converting' }
UnicodeSymbol >> asString [
	^ self asPlainString
]

{ #category : 'converting' }
UnicodeSymbol >> asSymbol [

]

{ #category : 'accessing' }
UnicodeSymbol >> at: anInteger put: anObject [
	"you can not modify the receiver."

	self errorNoModification
]

{ #category : 'converting' }
UnicodeSymbol >> capitalized [
	^ self asPlainString capitalized asSymbol
]

{ #category : 'private' }
UnicodeSymbol >> errorNoModification [

	self error:  'symbols can not be modified.'
]

{ #category : 'system primitives' }
UnicodeSymbol >> flushCache [
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>

]

{ #category : 'testing' }
UnicodeSymbol >> isInfix [
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2
]

{ #category : 'testing' }
UnicodeSymbol >> isInitializePvtSelector [
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ bytes beginsWith: `'initializePvt' asUtf8Bytes`
]

{ #category : 'testing' }
UnicodeSymbol >> isKeyword [
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3
]

{ #category : 'printing' }
UnicodeSymbol >> isLiteral [
	"Answer whether the receiver is a valid Smalltalk literal."

	^ true
]

{ #category : 'testing' }
UnicodeSymbol >> isPvtSelector [
	"Answer whether the receiver is a private message selector, that is,
	begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash."

	^ (bytes beginsWith: `'pvt' asUtf8Bytes` ) and: [bytes size >= 4 and: [(self at: 4) isUppercase]]
]

{ #category : 'testing' }
UnicodeSymbol >> isSymbol [
	^ true
]

{ #category : 'testing' }
UnicodeSymbol >> isUnary [
	"Answer whether the receiver is an unary message selector."

	^ self precedence = 1
]

{ #category : 'testing' }
UnicodeSymbol >> isValidBinarySelector [

	^ self isInfix and: [ self allSatisfy: [ :character | character isValidInBinarySelectors ] ]
]

{ #category : 'testing' }
UnicodeSymbol >> isValidGlobalName [

	^self notEmpty and: [
		self first canBeGlobalVarInitial and: [
			self allSatisfy: [ :character | character isValidInIdentifiers ]]].

]

{ #category : 'testing' }
UnicodeSymbol >> isValidKeywordSelector [

	^ self isKeyword and: [ self keywords allSatisfy: [ :keywordString | keywordString allButLast asSymbol isValidSelector ] ]
]

{ #category : 'testing' }
UnicodeSymbol >> isValidSelector [

	^ self isValidUnarySelector
		or: [ self isValidBinarySelector ]
		or: [ self isValidKeywordSelector ]
]

{ #category : 'testing' }
UnicodeSymbol >> isValidUnarySelector [

	^ self isUnary and: [ self allSatisfy: [ :character | character isValidInIdentifiers ] ]
]

{ #category : 'converting' }
UnicodeSymbol >> numArgs: n [
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		| offs |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol.
]

{ #category : 'accessing' }
UnicodeSymbol >> precedence [
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise. The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c := self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last == $: ifTrue: [^ 3].
	^ 1
]

{ #category : 'printing' }
UnicodeSymbol >> print [
	Transcript show: self printString; newLine
]

{ #category : 'accessing' }
UnicodeSymbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [

	self errorNoModification
]

{ #category : 'uCompletion' }
UnicodeSymbol >> separateKeywords [

	^self isKeyword
		ifFalse: [ self, ' ' ]
		ifTrue: [
			self species streamContents: [ :strm |
				self keywords
					do: [ :keyword | strm nextPutAll: keyword ]
					separatedBy: [ strm space; space ].
				strm space]]
]

{ #category : 'copying' }
UnicodeSymbol >> shallowCopy [
	"Answer with me, because Symbols are unique."
]

{ #category : 'private' }
UnicodeSymbol >> species [
	^UnicodeString
]

{ #category : 'printing' }
UnicodeSymbol >> storeOn: aStream  [

	aStream nextPut: $#.
	(Symbol isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]
]

{ #category : 'converting' }
UnicodeSymbol >> uncapitalized [
	^ self asPlainString uncapitalized asSymbol
]

{ #category : 'evaluating' }
UnicodeSymbol >> value: anObject [
	^ anObject perform: self
]

{ #category : 'evaluating' }
UnicodeSymbol >> value: anObject value: anObject2 [
	^ anObject perform: self with: anObject2
]

{ #category : 'evaluating' }
UnicodeSymbol >> value: anObject value: anObject2 value: anObject3 [
	^ anObject perform: self with: anObject2 with: anObject3
]

{ #category : 'evaluating' }
UnicodeSymbol >> value: anObject value: anObject2 value: anObject3 value: anObject4 [
	^ anObject perform: self with: anObject2 with: anObject3 with: anObject4
]

{ #category : 'evaluating' }
UnicodeSymbol >> valueWithArguments: anArray [
	^ anArray first perform: self withArguments: anArray allButFirst
]
