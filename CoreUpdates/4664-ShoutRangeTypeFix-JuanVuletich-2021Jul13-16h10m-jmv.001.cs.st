'From Cuis 5.0 [latest update: #4663] on 13 July 2021 at 4:16:54 pm'!
!classDefinition: #SHST80RangeType category: #'Tools-Syntax Highlighting'!
Object subclass: #SHST80RangeType
	instanceVariableNames: 'sourceCode messageRange receiverRange messageRangeType receiverRangeType classOrMetaClass lastRange '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHST80RangeType methodsFor: 'range type - private' stamp: 'jmv 7/13/2021 16:11:41'!
receiverAsNumber

	"if the user typed 1xe for example, asNumber will throw an exception because it is not a valid number
	even though the SHParser recognized it as such. I return nil because it is not really a number.
	Thank you Luciano for reporting the bug - Hernan"
	^[ (self sourceCodeIn: lastRange) asNumber ]
		on: Error
		do: [ :anError | anError return: nil ]! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'jmv 7/13/2021 16:11:41'!
receiverAsSymbol

	^ (self sourceCodeIn: lastRange) asSymbol! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'jmv 7/13/2021 16:11:41'!
typeWhenSendToClassVar

	| classVarValue |
	
	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ 
			classVarValue := classOrMetaClass theNonMetaClass classVarValueOf: (self sourceCodeIn: lastRange).
			self typeWhenSendTo: classVarValue ]! !

!SHST80RangeType methodsFor: 'range type' stamp: 'jmv 7/13/2021 16:11:41'!
lastRange: aRange

	lastRange := aRange ! !

!SHST80RangeType methodsFor: 'range type' stamp: 'jmv 7/13/2021 16:15:55'!
ofCurrentRangeOrMessageSendIn: aPotentialMessageRange

	| potentialMessageRangeType lastRangeType |

	potentialMessageRangeType := aPotentialMessageRange rangeType.

	potentialMessageRangeType = #unary ifFalse: [ ^potentialMessageRangeType ].
	lastRange ifNil: [ ^potentialMessageRangeType ].

	messageRange := aPotentialMessageRange.
	messageRangeType := potentialMessageRangeType.
	lastRangeType := lastRange rangeType.

	lastRangeType = #number ifTrue: [ ^self typeWhenSendToNumber ].
	lastRangeType = #string ifTrue: [ ^self typeWhenSendToInstanceOf: String ].
	lastRangeType = #symbol ifTrue: [ ^self typeWhenSendToInstanceOf: Symbol ].
	lastRangeType = #arrayEnd ifTrue: [ ^self typeWhenSendToInstanceOf: Array ].
	lastRangeType = #rightBrace ifTrue: [ ^self typeWhenSendToInstanceOf: Array ].
	lastRangeType = #blockEnd ifTrue: [ ^self typeWhenSendToInstanceOf: BlockClosure ].
	lastRangeType = #character ifTrue: [ ^self typeWhenSendToInstanceOf: Character ].
	lastRangeType = #nil ifTrue: [ ^self typeWhenSendToInstanceOf: nil class ].
	lastRangeType = #true ifTrue: [ ^self typeWhenSendToInstanceOf: true class ].
	lastRangeType = #false ifTrue: [ ^self typeWhenSendToInstanceOf: false class ].
	lastRangeType = #self ifTrue: [^self typeWhenSendToSelf ].
	lastRangeType = #super ifTrue: [^self typeWhenSendToSuper ].		
	lastRangeType = #globalVar ifTrue: [^self typeWhenSendToGlobal ].
	lastRangeType = #classVar ifTrue: [^self typeWhenSendToClassVar ].

	^messageRangeType.! !

!classDefinition: #SHST80RangeType category: #'Tools-Syntax Highlighting'!
Object subclass: #SHST80RangeType
	instanceVariableNames: 'sourceCode messageRange messageRangeType lastRange classOrMetaClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHST80RangeType reorganize!
('initialize' initializeFor:in:)
('range type - private' doesReceiverRespondsToMessageIn: messageName messageUndefinedType receiverAsNumber receiverAsSymbol sourceCodeIn: typeWhenSendTo: typeWhenSendToClassVar typeWhenSendToGlobal typeWhenSendToInstanceOf: typeWhenSendToNumber typeWhenSendToSelf typeWhenSendToSuper)
('range type' lastRange: ofCurrentRangeOrMessageSendIn:)
!

