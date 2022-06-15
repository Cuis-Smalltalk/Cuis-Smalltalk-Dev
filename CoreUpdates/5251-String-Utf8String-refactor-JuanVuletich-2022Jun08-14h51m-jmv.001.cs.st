'From Cuis 6.0 [latest update: #5252] on 8 June 2022 at 3:31:59 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/8/2022 15:31:27'!
tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isValidInIdentifiers or: [self = $:]! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 15:01:25'!
withNewLines
	"Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf)."

	^ self collect: [ :c | c = $\ ifTrue: [ Character newLineCharacter ] ifFalse: [ c ]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 14:58:47'!
withoutSuffix: aString
	"
	'Elvis' withoutSuffix: 'vis'
	'Elvis' withoutSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]! !


!Utf8String methodsFor: 'as yet unclassified' stamp: 'jmv 6/8/2022 15:31:07'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Reject if any chars are non-alphanumeric"
		self do: [ :codePoint | codePoint tokenish ifFalse: [ ^-1 ]].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[ (ix _ self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.! !


!String methodsFor: 'system primitives' stamp: 'jmv 6/8/2022 15:29:29'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(self
			findSubstring: '~'
			in: self
			startingAt: 1
			matchTable: Tokenish) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[ (ix _ self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.! !


!Symbol class methodsFor: 'access' stamp: 'jmv 6/8/2022 15:23:10'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList _ OrderedCollection new.
	(size _ aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue: [
		ascii _ aString first numericValue.
		ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]].

	aString first isValidInIdentifiers ifFalse: [
		aString size = 2 ifTrue: [
			Symbol hasInterned: aString ifTrue: [ :s | selectorList add: s]].
		^selectorList ].

	selectorList _ selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [ :each |
		each size >= size ifTrue: [
			(each findString: aString startingAt: 1 caseSensitive: false) > 0
				ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asPlainString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/8/2022 15:23:54'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	index _ '' findString: subString asUtf8Bytes in: bytes startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/8/2022 15:24:39'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize subStringBytes |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ subString asUtf8Bytes.
	(bytes at: index) = (subStringBytes at: 1) ifFalse: [ ^false ].
	^('' findString: subStringBytes in: bytes startingAt: index) = index! !

!methodRemoval: String #withoutSuffix: stamp: 'jmv 6/8/2022 14:58:47'!
String removeSelector: #withoutSuffix:!
!methodRemoval: String #withNewLines stamp: 'jmv 6/8/2022 15:01:25'!
String removeSelector: #withNewLines!
