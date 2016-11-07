'From Cuis 4.2 of 25 July 2013 [latest update: #2963] on 7 November 2016 at 1:05:38 pm'!

!SmallFloat64 class methodsFor: 'fileIn/Out' stamp: 'jmv 11/7/2016 13:03:19'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Float immediateSubclass: #SmallFloat64
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Kernel-Numbers'''! !


!SmallInteger class methodsFor: 'fileIn/Out' stamp: 'jmv 11/7/2016 13:03:56'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Integer immediateSubclass: #SmallInteger
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Kernel-Numbers'''! !


!Character class methodsFor: 'fileIn/Out' stamp: 'jmv 11/7/2016 13:02:31'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit''
	poolDictionaries: ''''
	category: ''Kernel-Text'''! !


!Class methodsFor: 'subclass creation' stamp: 'jmv 11/7/2016 13:05:34'!
immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new
	 immediate class as a subclass of an existing class (the receiver).
	
	In Spur, use no ivars and make it really immediate.
	In preSpur, use supplied ivars, and make it non-immediate.
	
	Any classes created like this should also implement #definition.
	See Character, SmallInteger and SmallFloat64"
	^Smalltalk isSpur ifTrue: [
		ClassBuilder new
			superclass: self
			immediateSubclass: t
			instanceVariableNames: ''
			classVariableNames: d
			poolDictionaries: s
			category: cat ]
	ifFalse: [
		ClassBuilder new
			superclass: self
			subclass: t
			instanceVariableNames: f
			classVariableNames: d
			poolDictionaries: s
			category: cat ]! !


!SmallFloat64 class reorganize!
('instance creation' basicNew basicNew:)
('fileIn/Out' definition)
!


!SmallInteger class reorganize!
('instance creation' basicNew new)
('constants' maxVal minVal)
('documentation' guideToDivision)
('fileIn/Out' definition)
!


!Character reorganize!
('accessing' asciiValue codePoint digitValue immediateNumericValue iso8859s15Code nonImmediateNumericValue numericValue)
('comparing' < <= = > >= hash identityHash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isDigit isLetter isLineSeparator isLowercase isSeparator isSpecial isUnaccented isUppercase isValidInIdentifiers isValidStartOfIdentifiers isVowel tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printOn: storeOn:)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('object fileIn' comeFullyUpOnReload:)
('*fileman-core-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!Character class reorganize!
('class initialization' accentedLetters initClassCachedState initialize initializeClassificationTable initializeUnicodeCodePoints releaseClassCachedState uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' asciiValue: codePoint: digitValue: immediateNumericValue: new nonImmediateNumericValue: numericValue: safeCodePoint: separators)
('accessing untypeable characters' arrowDown arrowLeft arrowRight arrowUp backspace cr crCharacter delete end enter escape euro home insert lf lfCharacter nbsp newLineCharacter newPage null pageDown pageUp space tab)
('constants' characterTable)
('converting' evaluate:withUtf8BytesOfUnicodeCodePoint: iso8859s15CodeForUnicodeCodePoint: latin1ToMacRoman: macRomanToLatin1: nextUnicodeCodePointFromUtf8: unicodeCodePointOfUtf8Bytes: utf8BytesOfUnicodeCodePoint:)
('accessing mathematical symbols' CC HH NN PP QQ RR ZZ aleph bullet circle contourIntegral degree doesNotExist dot emptySet exists forAll greaterNotEqual greaterOrEqual greaterOverEqual identical infinity integral lessNotEqual lessOrEqual lessOverEqual notEqual notIdentical odot oplus otimes partial strictlyEquivalent summation times)
('fileIn/Out' definition)
!

