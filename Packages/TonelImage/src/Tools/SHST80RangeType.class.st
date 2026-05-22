Class {
	#name : 'SHST80RangeType',
	#superclass : 'Object',
	#instVars : [
		'sourceCode',
		'messageRange',
		'messageRangeType',
		'lastRange',
		'classOrMetaClass'
	],
	#category : 'Tools-Syntax Highlighting',
	#package : 'Tools',
	#tag : 'Syntax Highlighting'
}

{ #category : 'instance creation' }
SHST80RangeType class >> for: aSourceCode in: aClassOrMetaclass [

	^self new initializeFor: aSourceCode in: aClassOrMetaclass
]

{ #category : 'range type - private' }
SHST80RangeType >> doesReceiverRespondsToMessageIn: aReceiver  [

	^aReceiver respondsTo: self messageName
]

{ #category : 'initialize' }
SHST80RangeType >> initializeFor: aSourceCode in: aClassOrMetaclass [

	sourceCode := aSourceCode.
	classOrMetaClass := aClassOrMetaclass 
]

{ #category : 'range type' }
SHST80RangeType >> lastRange: aRange [

	lastRange := aRange 
]

{ #category : 'range type - private' }
SHST80RangeType >> messageName [

	^ (self sourceCodeIn: messageRange) asSymbol
]

{ #category : 'range type - private' }
SHST80RangeType >> messageUndefinedType [
	
	messageRangeType = #unary ifTrue: [ ^#undefinedUnary ].
	messageRangeType = #binary ifTrue: [ ^#undefinedBinary ].
	
	^#invalid
	
]

{ #category : 'range type' }
SHST80RangeType >> ofCurrentRangeOrMessageSendIn: aPotentialMessageRange [

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

	^messageRangeType.
]

{ #category : 'range type - private' }
SHST80RangeType >> receiverAsNumber [

	"if the user typed 1xe for example, asNumber will throw an exception because it is not a valid number
	even though the SHParser recognized it as such. I return nil because it is not really a number.
	Thank you Luciano for reporting the bug - Hernan"
	^[ (self sourceCodeIn: lastRange) asNumber ]
		on: Error
		do: [ :anError | anError return: nil ]
]

{ #category : 'range type - private' }
SHST80RangeType >> receiverAsSymbol [

	^ (self sourceCodeIn: lastRange) asSymbol
]

{ #category : 'range type - private' }
SHST80RangeType >> sourceCodeIn: aRange [

	^ sourceCode copyFrom: aRange start to: aRange end
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendTo: receiver [

	^(self doesReceiverRespondsToMessageIn: receiver)
		ifTrue: [ messageRangeType ]
		ifFalse: [ self messageUndefinedType ]
		
	

]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToClassVar [

	| classVarValue |
	
	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ 
			classVarValue := classOrMetaClass theNonMetaClass classVarValueOf: (self sourceCodeIn: lastRange).
			self typeWhenSendTo: classVarValue ]
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToGlobal [

	^ Smalltalk 
		at: self receiverAsSymbol 
		ifPresent: [ :globalValue | self typeWhenSendTo: globalValue ]
		ifAbsent: [ messageRangeType ]
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToInstanceOf: aClass [

	^ (aClass canUnderstand: self messageName)
		ifTrue: [ messageRangeType ]
		ifFalse: [ self messageUndefinedType ]
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToNumber [

	^self typeWhenSendTo: self receiverAsNumber 
		
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToSelf [

	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ self typeWhenSendToInstanceOf: classOrMetaClass ]
]

{ #category : 'range type - private' }
SHST80RangeType >> typeWhenSendToSuper [

	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ 
			classOrMetaClass theNonMetaClass superclass isNil
				ifTrue: [ self messageUndefinedType ]
				ifFalse: [ self typeWhenSendToInstanceOf: classOrMetaClass superclass]]
]
