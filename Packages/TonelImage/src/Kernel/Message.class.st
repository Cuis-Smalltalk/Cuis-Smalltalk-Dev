"
I represent a selector and its argument values.
	
Generally, the system does not use instances of Message for efficiency reasons. However, when a message is not understood by its receiver, the interpreter will make up an instance of me in order to capture the information involved in an actual message transmission. This instance is sent it as an argument with the message doesNotUnderstand: to the receiver.
"
Class {
	#name : 'Message',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'args',
		'lookupClass'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
Message class >> selector: aSymbol [
	"Answer an instance of me with unary selector, aSymbol."

	^self new setSelector: aSymbol arguments: (Array new: 0)
]

{ #category : 'instance creation' }
Message class >> selector: aSymbol argument: anObject  [
	"Answer an instance of me whose selector is aSymbol and single argument
	is anObject."

	^self new setSelector: aSymbol arguments: (Array with: anObject)
]

{ #category : 'instance creation' }
Message class >> selector: aSymbol arguments: anArray  [
	"Answer an instance of me with selector, aSymbol, and arguments,
	anArray."

	^self new setSelector: aSymbol arguments: anArray
]

{ #category : 'comparing' }
Message >> = aMessage [

	"Any object is equal to itself"
	self == aMessage ifTrue: [ ^ true ].

	self class == aMessage class ifFalse: [ ^false ].
	selector = aMessage selector ifFalse: [ ^false ].
	lookupClass = aMessage lookupClass ifFalse: [ ^false ].
	^args literalEqual: aMessage arguments
]

{ #category : 'stub creation' }
Message >> addGetterCodeOn: stream [

	stream
		newLine; tab;
		nextPut: $^;
		nextPutAll: selector 
]

{ #category : 'stub creation' }
Message >> addSetterCodeOn: stream with: anArgumentName   [
		
	"This message is sent from CuisUniversity also, not only from base.
	Do not remove it - Hernan"				
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: anArgumentName 
]

{ #category : 'comparing' }
Message >> analogousCodeTo: anObject [
	"For MethodPropertires comparison."
	^self class == anObject class
	  and: [selector == anObject selector
	  and: [args = anObject arguments
	  and: [lookupClass == anObject lookupClass]]]
]

{ #category : 'accessing' }
Message >> argument [
	"Answer the first (presumably sole) argument"

	^args at: 1
]

{ #category : 'stub creation' }
Message >> argumentNameAt: anIndex havingNamed: alreadyNamedArguments [

	| argumentName |
	
	argumentName := (self arguments at: anIndex) argumentName.
	[alreadyNamedArguments includes: argumentName] whileTrue: [argumentName := argumentName, anIndex printString].
	alreadyNamedArguments add: argumentName.

	^argumentName
]

{ #category : 'accessing' }
Message >> arguments [
	"Answer the arguments of the receiver."
	^args
]

{ #category : 'stub creation' }
Message >> createStubMethodFor: aClass [

	^ String streamContents: [ :stream |
		self writeMessageNameOn: stream.
		stream newLine; tab.
		self writeShouldBeImplementedOn: stream.
		(self isGetterFor: aClass) ifTrue: [ self addGetterCodeOn: stream ].
		(self isSetterFor: aClass) ifTrue: [ self addSetterCodeOn: stream with: self arguments first argumentName ].
	]
]

{ #category : 'accessing' }
Message >> fullName [

	^ String streamContents: [ :messageStream |
		args
			ifEmpty: [ messageStream nextPutAll: selector ]
			ifNotEmpty: [ 
				selector keywords with: args 
					do: [ :keyword :argumentName |
						messageStream
							nextPutAll: keyword;
							nextPut:  Character space;
							nextPutAll: argumentName  ]
					separatedBy: [ messageStream nextPut: Character space ]]]
]

{ #category : 'testing' }
Message >> hasArguments [
	^args size > 0
]

{ #category : 'comparing' }
Message >> hasIdenticalContentsAs: aMessage [
	"Answer if the argument's selector and arguments are identically equal to those of the receiver.
	 It is assumed that the argument aMessage is, in fact, a message."
       selector ~~ aMessage selector ifTrue:
		[^false].
	1 to: args size do:
		[:i| (args at: i) ~~ (aMessage arguments at: i) ifTrue: [^false]].
	^true
]

{ #category : 'testing' }
Message >> hasRightNumberOfArguments [
	
	selector isUnary ifTrue: [ ^args isEmpty ].
	^args size = selector keywords size
]

{ #category : 'comparing' }
Message >> hash [
	"Hash is reimplemented because = is implemented."
	^selector hash bitXor: args hash
]

{ #category : 'testing' }
Message >> isGetterFor: aClass [

	^selector isUnary and: [ aClass instVarNames includes: selector  ]
]

{ #category : 'testing' }
Message >> isSetterFor: aClass [

	^selector isKeyword and: [ self numArgs = 1 and: [ aClass instVarNames includes: selector allButLast ]]
]

{ #category : 'accessing' }
Message >> keywords [
	
	^selector keywords 
]

{ #category : 'accessing' }
Message >> lookupClass [

	^ lookupClass
]

{ #category : 'private' }
Message >> lookupClass: aClass [

	lookupClass := aClass
]

{ #category : 'accessing' }
Message >> numArgs [
	"Answer the number of arguments in this message"

	^args size
]

{ #category : 'printing' }
Message >> printOn: stream [

	args isEmpty ifTrue: [^ stream nextPutAll: selector].
	args with: selector keywords do: [:arg :word |
		stream nextPutAll: word.
		stream nextPutAll: ' ('.
		arg printOn: stream.
		stream nextPutAll: ') '.
	].
	stream skipBack.

]

{ #category : 'accessing' }
Message >> selector [
	"Answer the selector of the receiver."
	^selector
]

{ #category : 'sending' }
Message >> sendTo: receiver [
	"answer the result of sending this message to receiver"

	^ receiver perform: selector withArguments: args
]

{ #category : 'accessing' }
Message >> sends: aSelector [
	"answer whether this message's selector is aSelector"

	^selector == aSelector
]

{ #category : 'sending' }
Message >> sentTo: receiver [
	"answer the result of sending this message to receiver"

	^lookupClass
		ifNil: [receiver perform: selector withArguments: args]
		ifNotNil: [receiver perform: selector withArguments: args inSuperclass: lookupClass]
]

{ #category : 'private' }
Message >> setSelector: aSymbol [

	selector := aSymbol.

]

{ #category : 'private' }
Message >> setSelector: aSymbol arguments: anArray  [
	selector := aSymbol.
	args := anArray
]

{ #category : 'printing' }
Message >> storeOn: aStream  [
	"Refer to the comment in Object|storeOn:."

	aStream nextPut: $(;
	 nextPutAll: self class name;
	 nextPutAll: ' selector: ';
	 store: selector;
	 nextPutAll: ' arguments: ';
	 store: args;
	 nextPut: $)
]

{ #category : 'stub creation' }
Message >> writeMessageNameOn: aStream [

	| alreadyNamedArguments |

	alreadyNamedArguments := Set new.
	self selector keywords withIndexDo: [ :keyword :index |
		aStream nextPutAll: keyword.
		self hasArguments ifTrue: [ self writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments ]].
	
	
]

{ #category : 'stub creation' }
Message >> writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments [

	| argumentName |
	
	argumentName := self argumentNameAt: index havingNamed: alreadyNamedArguments.
	
	aStream 
		nextPutAll: ' '; 
		nextPutAll: argumentName; 
		space
	
	
]

{ #category : 'stub creation' }
Message >> writeShouldBeImplementedOn: stream [

	stream				
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.
]
