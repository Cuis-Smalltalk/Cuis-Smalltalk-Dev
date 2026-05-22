"
I represent an occurrence of a pragma in a compiled method.  A pragma is a literal message pattern that occurs between angle brackets at the start of a method after any temporaries.  A common example is the primitive pragma:
	<primitive: 123 errorCode: 'errorCode'>
but one can add one's own and use them as metadata attached to a method.  Because pragmas are messages one can browse senders and implementors and perform them.  One can query a method for its pragmas by sending it the pragmas message, which answers an Array of instances of me, one for each pragma in the method.

I can provide information about the defining class, method, its selector, as well as the information about the pragma keyword and its arguments. See the two 'accessing' protocols for details. 'accessing-method' provides information about the method the pragma is found in, while 'accessing-pragma' is about the pragma itself.

Instances are retrieved using one of the pragma search methods of the 'finding' protocol on the class side.

To browse all methods with pragmas in the system evaluate
	Smalltalk browseAllSelect: [:m| m pragmas notEmpty]
and to browse all nonprimitive methods with pragmas evaluate
	Smalltalk browseAllSelect: [:m| m primitive isZero and: [m pragmas notEmpty]]
"
Class {
	#name : 'Pragma',
	#superclass : 'Object',
	#instVars : [
		'method',
		'keyword',
		'arguments'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol from: aSubClass to: aSuperClass [
	"Answer a collection of all pragmas found in methods of all classes between aSubClass and aSuperClass (inclusive) whose keyword is aSymbol."
	
	^ Array streamContents: [ :stream |
		aSubClass withAllSuperclassesDo: [ :class |
			self withPragmasIn: class do:  [ :pragma |
				pragma keyword = aSymbol
					ifTrue: [ stream nextPut: pragma ] ].
			aSuperClass == class
				ifTrue: [ ^ stream contents ] ] ].
]

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol from: aSubClass to: aSuperClass sortedByArgument: anInteger [
	"Answer a collection of all pragmas found in methods of all classes between aSubClass and aSuperClass (inclusive) whose keyword is aSymbol, sorted according to argument anInteger."

	^ self allNamed: aSymbol from: aSubClass to: aSuperClass sortedUsing: [ :a :b | (a argumentAt: anInteger) < (b argumentAt: anInteger) ].
]

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol from: aSubClass to: aSuperClass sortedUsing: aSortBlock [
	"Answer a collection of all pragmas found in methods of all classes between aSubClass and aSuperClass (inclusive) whose keyword is aSymbol, sorted according to aSortBlock."
	
	^ (self allNamed: aSymbol from: aSubClass to: aSuperClass) sort: aSortBlock.
]

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol in: aClass [
	"Answer a collection of all pragmas found in methods of aClass whose keyword is aSymbol."
	
	^ Array streamContents: [ :stream |
		self withPragmasIn: aClass do: [ :pragma |
			pragma keyword = aSymbol
				ifTrue: [ stream nextPut: pragma ] ] ].
]

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol in: aClass sortedByArgument: anInteger [
	"Answer a collection of all pragmas found in methods of aClass whose keyword is aSymbol, sorted according to argument anInteger."

	^ self allNamed: aSymbol in: aClass sortedUsing: [ :a :b | (a argumentAt: anInteger) < (b argumentAt: anInteger) ].
]

{ #category : 'finding' }
Pragma class >> allNamed: aSymbol in: aClass sortedUsing: aSortBlock [
	"Answer a collection of all pragmas found in methods of aClass whose keyword is aSymbol, sorted according to aSortBlock."
	
	^ (self allNamed: aSymbol in: aClass) sort: aSortBlock.
]

{ #category : 'instance creation' }
Pragma class >> for: aMethod selector: aSelector arguments: anArray [
	^self new
		setMethod: aMethod;
		setKeyword: aSelector;
		setArguments: anArray;
		yourself
]

{ #category : 'private' }
Pragma class >> keyword: aSymbol arguments: anArray [
	^ self new
		setKeyword: aSymbol;
		setArguments: anArray;
		yourself.
]

{ #category : 'private' }
Pragma class >> withPragmasIn: aClass do: aBlock [
	aClass selectorsAndMethodsDo: [ :selector :method | method pragmas do: aBlock ].
]

{ #category : 'comparing' }
Pragma >> = anObject  [
	^self class == anObject class
	  and: [keyword == anObject keyword
	  and: [arguments = anObject arguments]]
]

{ #category : 'literals' }
Pragma >> allLiteralsDo: aBlock [

	keyword allLiteralsDo: aBlock.
	arguments allLiteralsDo: aBlock.
]

{ #category : 'comparing' }
Pragma >> analogousCodeTo: anObject  [
	^self class == anObject class
	  and: [keyword == anObject keyword
	  and: [arguments = anObject arguments]]
]

{ #category : 'accessing-pragma' }
Pragma >> argumentAt: anInteger [
	"Answer one of the arguments of the pragma."
	
	^ self arguments at: anInteger.
]

{ #category : 'accessing-pragma' }
Pragma >> arguments [
	"Answer the arguments of the receiving pragma. For a pragma defined as <key1: val1 key2: val2> this will answer #(val1 val2)."
	
	^ arguments
]

{ #category : 'literals' }
Pragma >> hasLiteral: aLiteral [
	"Extra support for #hasLiteralTest:."
	
	^ (super hasLiteral: aLiteral)
		or: [keyword == #hasLiteralTest: and: [
			self methodClass theNonMetaClass perform: arguments first with: aLiteral]]
]

{ #category : 'testing' }
Pragma >> hasLiteralSuchThat: aBlock [
	"Answer true if litBlock returns true for any literal in the receiver, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	^(aBlock value: keyword)
	   or: [arguments hasLiteralSuchThat: aBlock]
]

{ #category : 'comparing' }
Pragma >> hash [
	^keyword hash + arguments hash
]

{ #category : 'testing' }
Pragma >> is: aSymbol [
	^ aSymbol == #Pragma or: [ super is: aSymbol ]
]

{ #category : 'accessing-pragma' }
Pragma >> key [
	"Answer the keyword of the pragma (the selector of its message pattern).
	 This accessor provides polymorphism with Associations used for properties."
	^keyword
]

{ #category : 'accessing-pragma' }
Pragma >> keyword [
	"Answer the keyword of the pragma (the selector of its message pattern).
	 For a pragma defined as <key1: val1 key2: val2> this will answer #key1:key2:."
	
	^ keyword
]

{ #category : 'accessing-pragma' }
Pragma >> message [
	"Answer the message of the receiving pragma."
	
	^ Message selector: self keyword arguments: self arguments. 
]

{ #category : 'accessing-method' }
Pragma >> method [
	"Answer the compiled-method containing the pragma."
	
	^ method
]

{ #category : 'accessing-method' }
Pragma >> methodClass [
	"Answer the class of the method containing the pragma."
	
	^ method methodClass
]

{ #category : 'accessing-pragma' }
Pragma >> numArgs [
	"Answer the number of arguments in the pragma."

	^ self arguments size.
]

{ #category : 'printing' }
Pragma >> printOn: aStream [
	aStream nextPut: $<.
	self keyword precedence = 1
		ifTrue: [ aStream nextPutAll: self keyword ]
		ifFalse: [
			self keyword keywords with: self arguments do: [ :key :arg |
				aStream nextPutAll: key; space; print: arg; space ].
			aStream skipBack ].
	aStream nextPut: $>.
]

{ #category : 'accessing-method' }
Pragma >> selector [
	"Answer the selector of the method containing the pragma.
	 Do not confuse this with the selector of the pragma's message pattern."
	
	^method selector
]

{ #category : 'initialization' }
Pragma >> setArguments: anArray [
	arguments := anArray
]

{ #category : 'initialization' }
Pragma >> setKeyword: aSymbol [
	keyword := aSymbol
]

{ #category : 'initialization' }
Pragma >> setMethod: aCompiledMethod [
	method := aCompiledMethod
]
