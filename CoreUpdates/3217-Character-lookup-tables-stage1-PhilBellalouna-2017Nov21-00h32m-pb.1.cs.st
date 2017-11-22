'From Cuis 5.0 of 7 November 2016 [latest update: #3212] on 21 November 2017 at 3:19:55 am'!
!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: 'value '
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit LetterTruthTable UppercaseTruthTable LowercaseTruthTable LowercaseMappingTable UppercaseMappingTable '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character class methodsFor: 'class initialization' stamp: 'pb 11/21/2017 02:38:24'!
initializeLookupTables
	LowercaseMappingTable _ Array new: 256.
	LowercaseTruthTable _ Array new: 256.
	UppercaseMappingTable _ Array new: 256.
	UppercaseTruthTable _ Array new: 256.
	LetterTruthTable _ Array new: 256.
	UnaccentedTable _ ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char _ self numericValue: idx.
			LowercaseMappingTable
				at: idx + 1
				put: char.
			LowercaseTruthTable
				at: idx + 1
				put: false.
			UppercaseMappingTable
				at: idx + 1
				put: char.
			UppercaseTruthTable
				at: idx + 1
				put: false.
			LetterTruthTable
				at: idx + 1
				put: false.
			UnaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase _ group first numericValue.
				lowercase _ group second numericValue.
				lowercaseChar _ self numericValue: lowercase.
				uppercaseChar _ self numericValue: uppercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				LowercaseMappingTable
					at: uppercase + 1
					put: lowercaseChar.
				LowercaseTruthTable
					at: lowercase + 1
					put: true.
				LetterTruthTable
					at: lowercase + 1
					put: true.
				UppercaseMappingTable
					at: lowercase + 1
					put: uppercaseChar.
				UppercaseMappingTable
					at: uppercase + 1
					put: uppercaseChar.
				UppercaseTruthTable
					at: uppercase + 1
					put: true.
				LetterTruthTable
					at: uppercase + 1
					put: true.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase _ group third numericValue.
						unaccentedLowercase _ group fourth numericValue.
						UnaccentedTable at: uppercase+1 put: unaccentedUppercase.
						UnaccentedTable at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase _ group first numericValue.
				lowercaseChar _ self numericValue: lowercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				UppercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				LowercaseTruthTable
					at: lowercase + 1
					put: true.
				UppercaseTruthTable
					at: lowercase + 1
					put: false.
				LetterTruthTable
					at: lowercase + 1
					put: true ]].! !

!Character class methodsFor: 'class initialization' stamp: 'pb 11/21/2017 02:24:51'!
initClassCachedState
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeClassificationTable.
	self initializeLookupTables .
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'fileIn/Out' stamp: 'pb 11/21/2017 02:26:23'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit LowercaseMappingTable LowercaseTruthTable UppercaseMappingTable UppercaseTruthTable LetterTruthTable''
	poolDictionaries: ''''
	category: ''Kernel-Text'''! !

!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit LowercaseMappingTable LowercaseTruthTable UppercaseMappingTable UppercaseTruthTable LetterTruthTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!

Character initialize!

