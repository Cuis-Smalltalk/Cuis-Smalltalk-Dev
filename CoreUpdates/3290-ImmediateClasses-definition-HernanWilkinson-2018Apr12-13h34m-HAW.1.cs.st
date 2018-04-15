'From Cuis 5.0 of 7 November 2016 [latest update: #3285] on 12 April 2018 at 1:37:31 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 4/12/2018 13:36:40'!
definition
	"Answer a String that defines the receiver."

	^String streamContents: [ :strm |
		strm
			nextPutAll: (superclass ifNotNil: [ superclass name ] ifNil: [ 'ProtoObject' ]);
			nextPutAll: self kindOfSubclass;
			store: self name.
		strm
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
		strm
			newLine;
			tab;
			nextPutAll: 'category: ';
			store: self category asString.

		superclass ifNil: [ 
			strm nextPutAll: '.'; newLine.
			strm nextPutAll: self name.
			strm space; nextPutAll: 'superclass: nil' ]]! !


!SmallFloat64 class methodsFor: 'fileIn/Out' stamp: 'HAW 4/12/2018 13:35:41'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Float immediateSubclass: #SmallFloat64
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''', self category, ''''
! !


!SmallInteger class methodsFor: 'fileIn/Out' stamp: 'HAW 4/12/2018 13:35:04'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Integer immediateSubclass: #SmallInteger
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''', self category, ''''! !


!Character class methodsFor: 'fileIn/Out' stamp: 'HAW 4/12/2018 13:36:49'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable UnaccentedTable UnicodeCodePoints LowercaseMappingTable LowercaseTruthTable UppercaseMappingTable UppercaseTruthTable LetterTruthTable''
	poolDictionaries: ''''
	category: ''', self category, ''''
! !

