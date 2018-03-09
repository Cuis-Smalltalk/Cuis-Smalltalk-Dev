'From Cuis 5.0 of 7 November 2016 [latest update: #3266] on 6 March 2018 at 5:42:11 pm'!

!String methodsFor: 'converting' stamp: 'jmv 3/6/2018 17:40:38'!
asValidInstanceVariableName
	| answer |
	answer _ self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer _ answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
"! !

!String methodsFor: 'converting' stamp: 'jmv 3/6/2018 17:32:02'!
asValidSelector
	^ self asIdentifier: false

"
'234znak 43 ) 2' asValidSelector
"! !

!String methodsFor: 'converting' stamp: 'jmv 3/6/2018 17:15:13'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary"
	"'MElViN' uncapitalized"
	"#Will uncapitalized"
	| answer |
	self isEmpty ifTrue: [^ self].
	answer _ self isString
				ifTrue: ["don't modify receiver"
					self copy]
				ifFalse: [self asString].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ self isString
		ifTrue: [answer]
		ifFalse: [answer as: self class]! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 3/6/2018 17:38:01'!
isValidInstanceVariableName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((self isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self pseudoVariableNames includes:  aString) not]! !

!Scanner class methodsFor: 'testing' stamp: 'jmv 3/6/2018 17:36:55'!
pseudoVariableNames
	"Answer a list of Smalltalk pseudo-varialbes"
	^ #('nil' 'true' 'false' 'self' 'super' 'thisContext')! !


!Workspace methodsFor: 'variable declarations' stamp: 'jmv 3/6/2018 17:28:10'!
nameForObject: object
	"Answer a name suitable for a Workspace variable"
	^ (object class name, object identityHash asString) asIdentifier: false! !


!String methodsFor: 'converting' stamp: 'jmv 3/6/2018 17:19:56'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	(aString size = 0 or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString _ 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"! !


!Symbol class methodsFor: 'access' stamp: 'jmv 3/6/2018 17:15:26'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList _ OrderedCollection new.
	(size _ aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii _ aString first numericValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	aString first isValidInIdentifiers ifFalse:
		[
			aString size = 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList _ selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !

!Symbol class methodsFor: 'access' stamp: 'jmv 3/6/2018 17:15:29'!
selectorsMatching: aStringPattern
	"Answer a list of selectors that match aStringPattern within them. Case-insensitive.
	 Does return symbols that begin with a capital letter."

	| selectorList |

	selectorList := OrderedCollection new.

	aStringPattern isEmpty ifTrue: [^selectorList].

	self allSymbolTablesDo:
		[:each | (aStringPattern match: each) ifTrue: [selectorList add: each]].

	^selectorList reject: "reject non-selectors, but keep ones that begin with an uppercase"
		[:each | each numArgs < 0 and: [each asString uncapitalized numArgs < 0]]

	"Symbol selectorsMatching: 'parse:*'"! !


!Morph methodsFor: 'menus' stamp: 'jmv 3/6/2018 16:58:49'!
addCopyItemsTo: aMenu 
	"Add copy-like items to the halo menu"

	aMenu add: 'copy to clipboard (c)' action: #copyToClipboard:! !

!methodRemoval: Utilities class #inviolateInstanceVariableNames!
Utilities class removeSelector: #inviolateInstanceVariableNames!
!methodRemoval: Utilities class #isLegalInstVarName:!
Utilities class removeSelector: #isLegalInstVarName:!
!methodRemoval: Utilities class #wellFormedInstanceVariableNameFrom:!
Utilities class removeSelector: #wellFormedInstanceVariableNameFrom:!
!methodRemoval: Scanner class #inviolateInstanceVariableNames!
Scanner class removeSelector: #inviolateInstanceVariableNames!
!methodRemoval: Scanner class #isLegalInstVarName:!
Scanner class removeSelector: #isLegalInstVarName:!
!methodRemoval: Scanner class #wellFormedInstanceVariableNameFrom:!
Scanner class removeSelector: #wellFormedInstanceVariableNameFrom:!
!methodRemoval: String #asLegalSelector!
String removeSelector: #asLegalSelector!
!methodRemoval: String #withFirstCharacterDownshifted!
String removeSelector: #withFirstCharacterDownshifted!
