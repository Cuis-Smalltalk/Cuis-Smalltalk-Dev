"
I represent Strings that are created uniquely.
So, if aString = anotherString, then we know aString asSymbol == anotherString asSymbol
"
Class {
	#name : 'Symbol',
	#superclass : 'String',
	#classVars : [
		'AccessProtect',
		'Symbols'
	],
	#category : 'Kernel-Strings',
	#package : 'Kernel',
	#tag : 'Strings'
}

{ #category : 'private' }
Symbol class >> accessProtect [
	AccessProtect ifNil: [
		AccessProtect := Semaphore forMutualExclusion ].
	^AccessProtect
]

{ #category : 'access' }
Symbol class >> allSymbolTablesDo: aBlock [

	Symbols do: aBlock
]

{ #category : 'access' }
Symbol class >> allSymbols [
	"Answer all interned symbols"

	Symbols ifNil: [ ^#() ].
	^Array streamContents:
		[:s | Symbols do: [:each | s nextPut: each]]
]

{ #category : 'access' }
Symbol class >> allSymbolsSelect: aCondition [

	^Array streamContents:
		[:stream | Symbols do: [:symbol | (aCondition value: symbol) ifTrue: [ stream nextPut: symbol]]]
]

{ #category : 'private' }
Symbol class >> basicFromString: aString [
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	^(self basicNew: aString size) string: aString
]

{ #category : 'services' }
Symbol class >> estimatedCount [
	^Symbols estimatedSize
]

{ #category : 'access' }
Symbol class >> findInterned: aString [

	self hasInterned: aString ifTrue: [:symbol | ^symbol].
	^nil.
]

{ #category : 'services' }
Symbol class >> hasInterned: aString and: oneArgBlock  [
	"Answer with false if aString hasnt been interned (into a Symbol), 
	otherwise supply the symbol to symBlock and return the result of the evaluation."

	| symbol |

	^ (symbol := self lookup: aString)
		ifNil: [ false ]
		ifNotNil: [ oneArgBlock value: symbol ]
]

{ #category : 'services' }
Symbol class >> hasInterned: aString ifTrue: symBlock  [
	"Answer with false if aString hasnt been interned (into a Symbol), 
	otherwise supply the symbol to symBlock and return true."

	| symbol |

	^(symbol := self lookup: aString)
		ifNil: [false]
		ifNotNil: [symBlock value: symbol. true]
]

{ #category : 'services' }
Symbol class >> hasInternedAndImplemented: aString [
	"Answer with false if aString hasnt been interned (into a Symbol), 
	or if there are no implemetors of the selector."

	"Use optimized implementation if available,"
	Smalltalk at: #SmalltalkCompleter ifPresent: [ :cls | 
		^self hasInterned: aString and: [ :symbol | cls isThereAnImplementorOf: symbol ]].

	^self hasInterned: aString and: [ :symbol | Smalltalk isThereAnImplementorOf: symbol ]
]

{ #category : 'services' }
Symbol class >> hasInternedAndImplementedOrReferenced: aString [
	"Answer with false if aString hasnt been interned (into a Symbol), 
	or if there are no implemetors of the selector."

	^self hasInterned: aString and: [ :symbol |
		(Smalltalk isThereAnImplementorOf: symbol) or: [
			Smalltalk isThereAReferenceTo: symbol ]]
]

{ #category : 'class initialization' }
Symbol class >> initialize [
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[
			Symbols := WeakSet new.
			CharacterSequence allSubclassesDo: [ :cls |
				cls representsSymbols ifTrue: [
					cls allInstancesDo: [ :sym | Symbols add: sym ]]]].
]

{ #category : 'instance creation' }
Symbol class >> intern: aStringOrSymbol [

	^self accessProtect critical:
		[
			Symbols lookup: aStringOrSymbol ifAbsentAddFrom:
				[
					| preferByteStringForAscii |
					aStringOrSymbol isSymbol
						ifTrue: [ aStringOrSymbol ]
						ifFalse: [
							preferByteStringForAscii := aStringOrSymbol.
							(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
								ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
							preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ].
				]
			].
]

{ #category : 'services' }
Symbol class >> isLiteralSymbol: aSymbol  [
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| char |
	aSymbol size = 0 ifTrue: [^ false].
	aSymbol = ':' ifTrue: [ ^true ].
	aSymbol = '|' ifTrue: [ ^true ].
	(aSymbol allSatisfy: [ :c | Character binarySelectorCharacters includes: c ])
		ifTrue: [ ^true ].
	char := aSymbol at: 1.
	(char isLetter or: [ char = $_ ]) ifTrue: [
		(aSymbol allSatisfy: [ :c |
			c isLetter or: [ c = $_ or: [ c isDigit or: [ c = $: ]]]])
				ifTrue: [ ^true ]].		
	^false
]

{ #category : 'instance creation' }
Symbol class >> lookup: aStringOrSymbol [

	^self accessProtect critical: [
		Symbols lookup: aStringOrSymbol ].
]

{ #category : 'instance creation' }
Symbol class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."

	^ (String newFrom: aCollection) asSymbol

"
	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"
]

{ #category : 'services' }
Symbol class >> possibleSelectorsFor: misspelled  [
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first ss |
	lookupString := misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs := lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new].
	first := lookupString first.
	short := lookupString size - (lookupString size // 4 max: 3) max: 2.
	long := lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates := OrderedCollection new.
	self allSymbolTablesDo: [:s | (((ss := s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best := lookupString correctAgainst: candidates.
	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [
		binary := misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best
]

{ #category : 'instance creation' }
Symbol class >> readFrom: strm [
	"
	Symbol readFrom: '#abc' readStream
	"

	strm peek == $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"
]

{ #category : 'private' }
Symbol class >> rehash [
	"Rebuild the hash table"
	"
	Symbol rehash
	"

	
	self accessProtect critical: [
		Symbols rehash ].
]

{ #category : 'testing' }
Symbol class >> representsSymbols [
	^true
]

{ #category : 'access' }
Symbol class >> selectorsContaining: aString [
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

"Symbol selectorsContaining: 'scon'"
]

{ #category : 'access' }
Symbol class >> selectorsMatching: aStringPattern [
	"Answer a list of selectors that match aStringPattern within them. Case-insensitive.
	 Does return symbols that begin with a capital letter."

	| selectorList |

	selectorList := OrderedCollection new.

	aStringPattern isEmpty ifTrue: [^selectorList].

	self allSymbolTablesDo:
		[:each | (aStringPattern match: each) ifTrue: [selectorList add: each]].

	^selectorList reject: "reject non-selectors, but keep ones that begin with an uppercase"
		[:each | each numArgs < 0 and: [each asPlainString uncapitalized numArgs < 0]]

	"Symbol selectorsMatching: 'parse:*'"
]

{ #category : 'access' }
Symbol class >> symbolCount [

	^self accessProtect critical: [ Symbols size ].
]

{ #category : 'access' }
Symbol class >> thatStartsCaseSensitive: leadingCharacters [
	"Same as thatStarts:skipping: but caseSensitive"
	"Note: Only answers symbols for which there is some implementor"
	| size firstMatch key |

	"Use optimized implementation if available,"
	Smalltalk at: #SmalltalkCompleter ifPresent: [ :cls | ^cls thatStartsCaseSensitive: leadingCharacters ].

	size := leadingCharacters size.
	size = 0 ifTrue: [^#''].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [ key := leadingCharacters copyFrom: 2 to: size ].
	self allSymbolTablesDo: [ :each |
			each size >= size ifTrue: [
				((each at: 1) == firstMatch and: [
					key == nil or: [
						(each findString: key startingAt: 2 caseSensitive: true) = 2]])
							ifTrue: [
								(Smalltalk isThereAnImplementorOf: each) ifTrue: [
									^each]]]].
	^nil
]

{ #category : 'comparing' }
Symbol >> = another [

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another isSymbol ifTrue: [
		^ false].  "Was not ==, therefore can't be = "

	"Otherwise use string =..."
	^ super = another
]

{ #category : 'converting' }
Symbol >> asPlainString [
	"Answer a plain String, not a Symbol."
	| newString |
	newString := self species new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString
]

{ #category : 'converting' }
Symbol >> asString [
	^ self asPlainString
]

{ #category : 'converting' }
Symbol >> asSymbol [

]

{ #category : 'accessing' }
Symbol >> at: anInteger put: anObject  [
	"you can not modify the receiver."

	self errorNoModification
]

{ #category : 'converting' }
Symbol >> capitalized [
	^ self asPlainString capitalized asSymbol
]

{ #category : 'private' }
Symbol >> errorNoModification [
	self error:  'symbols can not be modified.'
]

{ #category : 'private' }
Symbol >> flushCache [
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>

]

{ #category : 'testing' }
Symbol >> isInfix [
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2
]

{ #category : 'testing' }
Symbol >> isInitializePvtSelector [
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ self beginsWith: 'initializePvt'
]

{ #category : 'testing' }
Symbol >> isKeyword [
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3
]

{ #category : 'testing' }
Symbol >> isLiteral [
	"Answer whether the receiver is a valid Smalltalk literal."

	^ true
]

{ #category : 'testing' }
Symbol >> isPvtSelector [
	"Answer whether the receiver is a private message selector, that is,
	begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash."

	^ (self beginsWith: 'pvt') and: [self size >= 4 and: [(self at: 4) isUppercase]]
]

{ #category : 'testing' }
Symbol >> isSymbol [
	^ true 
]

{ #category : 'testing' }
Symbol >> isUnary [
	"Answer whether the receiver is an unary message selector."

	^ self precedence = 1
]

{ #category : 'testing' }
Symbol >> isValidBinarySelector [

	^ self isInfix and: [ self allSatisfy: [ :character | character isValidInBinarySelectors ] ]
]

{ #category : 'testing' }
Symbol >> isValidGlobalName [

	^self notEmpty and: [
		self first canBeGlobalVarInitial and: [
			self allSatisfy: [ :character | character isValidInIdentifiers ]]].

]

{ #category : 'testing' }
Symbol >> isValidKeywordSelector [

	^ self isKeyword and: [ self keywords allSatisfy: [ :keywordString | keywordString allButLast asSymbol isValidSelector ] ]
]

{ #category : 'testing' }
Symbol >> isValidSelector [

	^ self isValidUnarySelector
		or: [ self isValidBinarySelector ]
		or: [ self isValidKeywordSelector ]
]

{ #category : 'testing' }
Symbol >> isValidUnarySelector [

	^ self isUnary and: [ self allSatisfy: [ :character | character isValidInIdentifiers ] ]
]

{ #category : 'converting' }
Symbol >> numArgs: n [
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		| offs |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol
]

{ #category : 'accessing' }
Symbol >> precedence [
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
Symbol >> print [
	Transcript show: self printString; newLine
]

{ #category : 'accessing' }
Symbol >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	self errorNoModification
]

{ #category : 'uCompletion' }
Symbol >> separateKeywords [

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
Symbol >> shallowCopy [
	"Answer with me, because Symbols are unique."
]

{ #category : 'private' }
Symbol >> species [
	^String
]

{ #category : 'printing' }
Symbol >> storeOn: aStream  [

	aStream nextPut: $#.
	(Symbol isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]
]

{ #category : 'private' }
Symbol >> string: aString  [
	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self
]

{ #category : 'converting' }
Symbol >> uncapitalized [
	^ self asPlainString uncapitalized asSymbol
]

{ #category : 'evaluating' }
Symbol >> value: anObject [
	^ anObject perform: self
]

{ #category : 'evaluating' }
Symbol >> value: anObject value: anObject2 [
	^ anObject perform: self with: anObject2
]

{ #category : 'evaluating' }
Symbol >> value: anObject value: anObject2 value: anObject3 [
	^ anObject perform: self with: anObject2 with: anObject3
]

{ #category : 'evaluating' }
Symbol >> value: anObject value: anObject2 value: anObject3 value: anObject4 [
	^ anObject perform: self with: anObject2 with: anObject3 with: anObject4
]

{ #category : 'evaluating' }
Symbol >> valueWithArguments: anArray [
	^ anArray first perform: self withArguments: anArray allButFirst
]
