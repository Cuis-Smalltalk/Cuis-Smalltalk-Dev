'From Cuis 5.0 of 7 November 2016 [latest update: #3214] on 21 November 2017 at 3:34:57 am'!
!Character class methodsFor: 'class initialization' stamp: 'pb 11/21/2017 03:33:45'!
initClassCachedState
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeLookupTables .
	self initializeUnicodeCodePoints.! !

!methodRemoval: Character class #initializeClassificationTable!
Character class removeSelector: #initializeClassificationTable!

!Character class methodsFor: 'fileIn/Out' stamp: 'pb 11/21/2017 03:34:29'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable UnaccentedTable UnicodeCodePoints LowercaseMappingTable LowercaseTruthTable UppercaseMappingTable UppercaseTruthTable LetterTruthTable''
	poolDictionaries: ''''
	category: ''Kernel-Text'''! !

!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'CharacterTable UnaccentedTable UnicodeCodePoints LowercaseMappingTable LowercaseTruthTable UppercaseMappingTable UppercaseTruthTable LetterTruthTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!

Character initialize!
Smalltalk recreateSpecialObjectsArray!
