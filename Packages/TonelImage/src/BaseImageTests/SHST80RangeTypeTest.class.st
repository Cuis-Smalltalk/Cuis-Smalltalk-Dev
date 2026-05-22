Class {
	#name : 'SHST80RangeTypeTest',
	#superclass : 'TestCase',
	#classVars : [
		'ClassVar1'
	],
	#category : 'BaseImageTests-Tools-Syntax Highlighting',
	#package : 'BaseImageTests',
	#tag : 'Tools-Syntax Highlighting'
}

{ #category : 'messages for testing' }
SHST80RangeTypeTest >> ! something [

	"Do not remove this method because it exists for its name to have implementors - Hernan"
]

{ #category : 'messages for testing' }
SHST80RangeTypeTest >> _zz [

	"Do not remove this method because it exists for its name to have implementors - Hernan"
]

{ #category : 'assertions' }
SHST80RangeTypeTest >> assertTypeOfBinaryMessageSend: aSourceCode is: aExpectedType [

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]

]

{ #category : 'assertions' }
SHST80RangeTypeTest >> assertTypeOfMessageSend: aSourceCode  [
	is: aExpectedType
	in: aClassOrMetaClass 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	| parser ranges st80RangeType type |
	
	parser := SHParserST80 new.
	ranges := parser
		workspace: nil;
		classOrMetaClass: aClassOrMetaClass ;
		source: aSourceCode;
		parse;
		ranges.
		
	st80RangeType := SHST80RangeType for: aSourceCode in: aClassOrMetaClass.
	st80RangeType lastRange: (aReceiverRangeBlock value: ranges).
	type := st80RangeType ofCurrentRangeOrMessageSendIn: (aMessageRangeBlock value: ranges).
	
	self assert: type equals: aExpectedType
	
]

{ #category : 'assertions' }
SHST80RangeTypeTest >> assertTypeOfMessageSend: aSourceCode  [
	is: aExpectedType 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType
		in: self class
		receiverRange: aReceiverRangeBlock 
		messageRange: aMessageRangeBlock
]

{ #category : 'assertions' }
SHST80RangeTypeTest >> assertTypeOfUnaryMessageSend: aSourceCode is: aExpectedType [

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ]

]

{ #category : 'assertions' }
SHST80RangeTypeTest >> assertTypesAreValidWhenMessageSendTo: aReceiverAsString [

	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' _zz' is: #undefinedUnary.
	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' size' is: #unary.

	" binary messages has beed disabled by now - Hernan
	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' ! 2' is: #undefinedBinary.
	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' = 2' is: #binary
	"
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test00MessagesForTestingAreImplemented [

	self assert: (self respondsTo: #_zz).
	self assert: (self respondsTo: #!).
	
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test01TypeOfMessageSentToLiteralNumberAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '1'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test02TypeOfMessageSentToLiteralStringAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '''a'''
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test03TypeOfMessageSentToLiteralSymbolAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '#a'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test04TypeOfMessageSentToLiteralArrayAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '#(1)'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test05TypeOfMessageSentToLiteralBraceArrayAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '{1}'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test06TypeOfMessageSentToLiteralBlockAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '[1]'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test07TypeOfMessageSentToLiteralCharacterAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: '$a'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test08TypeOfMessageSentToLiteralNilAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: 'nil'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test09TypeOfMessageSentToLiteralTrueAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: 'true'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test10TypeOfMessageSentToLiteralFalseAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: 'false'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test11TypeOfMessageSentToSelfAreObtainedCorrectly [

	self 
		assertTypeOfMessageSend: 'm1 self _zz'
		is: #undefinedUnary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
	self 
		assertTypeOfMessageSend: 'm1 self size'
		is: #unary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	"Binary messages have been disabled - Hernan
	self 
		assertTypeOfMessageSend: 'm1 self ! 2'
		is: #undefinedBinary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]."
		
	self 
		assertTypeOfMessageSend: 'm1 self = 2'
		is: #binary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ].
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test12TypeOfMessageSentToSuperAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: 'super'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test13TypesIsUndefinedForAnyMessageSendToSuperFromProtoObject [

	self 
		assertTypeOfMessageSend: 'm1 super size'
		is: #undefinedUnary
		in: ProtoObject
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	"binaryMessages have been disabled - Hernan
	self 
		assertTypeOfMessageSend: 'm1 super = 2'
		is: #undefinedBinary
		in: ProtoObject
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]."
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test14TypeOfMessageSentToGlobalVarAreObtainedCorrectly [

	self assertTypesAreValidWhenMessageSendTo: 'Smalltalk'
]

{ #category : 'tests' }
SHST80RangeTypeTest >> test15TypeOfMessageSentToClassVarAreObtainedCorrectly [

	ClassVar1 := 1.
	self assertTypesAreValidWhenMessageSendTo: 'ClassVar1'
]
