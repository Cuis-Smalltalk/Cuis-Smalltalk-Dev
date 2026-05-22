Class {
	#name : 'MethodReference',
	#superclass : 'Object',
	#instVars : [
		'classSymbol',
		'classIsMeta',
		'methodSymbol',
		'stringVersion'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'instance creation' }
MethodReference class >> class: aClass selector: aSelector [
	^ self new setStandardClass: aClass methodSymbol: aSelector
]

{ #category : 'error description' }
MethodReference class >> classDoesNotExistErrorMessage [

	^'Class does not exist'
]

{ #category : 'instance creation' }
MethodReference class >> classSymbol: aClassName classIsMeta: isMeta methodSymbol: aSelector  [
	
	^self new setClassSymbol: aClassName classIsMeta: isMeta methodSymbol: aSelector stringVersion: ''.
]

{ #category : 'instance creation' }
MethodReference class >> method: aCompiledMethod [
	^self class: aCompiledMethod methodClass selector: aCompiledMethod selector
]

{ #category : 'comparisons' }
MethodReference >> <= anotherMethodReference [
	"By default, sort by class"

	^self classSymbol = anotherMethodReference classSymbol
		ifTrue: [
			self methodSymbol == #Comment
				ifTrue: [ true ]
				ifFalse: [
					anotherMethodReference methodSymbol == #Comment
						ifTrue: [ false ]
						ifFalse: [
							self classIsMeta = anotherMethodReference classIsMeta
								ifTrue: [ self methodSymbol < anotherMethodReference methodSymbol ]
								ifFalse: [ self classIsMeta ]	]]]
		ifFalse: [ self classSymbol < anotherMethodReference classSymbol ]
]

{ #category : 'comparisons' }
MethodReference >> = anotherMethodReference  [
	"Answer whether the receiver and the argument represent the 
	same object."
	self == anotherMethodReference ifTrue: [ ^ true ].
	^ self species == anotherMethodReference species
		and: [self classSymbol == anotherMethodReference classSymbol]
		and: [self classIsMeta == anotherMethodReference classIsMeta]
		and: [self methodSymbol == anotherMethodReference methodSymbol]
]

{ #category : 'queries' }
MethodReference >> actualClass  [

	^self actualClassIfAbsent: nil
]

{ #category : 'queries' }
MethodReference >> actualClassIfAbsent: anAbsentBlock  [

	^Smalltalk 
		at: classSymbol 
		ifPresent: [ :actualClass |
			classIsMeta
				ifTrue: [ actualClass class ]
				ifFalse: [ actualClass ] ]
		ifAbsent: anAbsentBlock 

]

{ #category : 'queries' }
MethodReference >> category [
	^ self actualClass organization categoryOfElement: methodSymbol
]

{ #category : 'queries' }
MethodReference >> classIsMeta [

	^classIsMeta
]

{ #category : 'queries' }
MethodReference >> classSymbol [

	^classSymbol
]

{ #category : 'queries' }
MethodReference >> compiledMethod [
	^ self actualClass compiledMethodAt: methodSymbol
]

{ #category : 'queries' }
MethodReference >> compiledMethodIfAbsent: ifAbsentBlock [

	^ self actualClass
		ifNil: ifAbsentBlock
		ifNotNil: [ :aClass | aClass compiledMethodAt: methodSymbol ifAbsent: ifAbsentBlock ] 
]

{ #category : 'printing' }
MethodReference >> displayStringOrText [
	"To be used in the UI"

	^stringVersion
]

{ #category : 'auto complete' }
MethodReference >> dynamicTypingAutoCompleterDocumentation [

	^self compiledMethod dynamicTypingAutoCompleterDocumentation 
]

{ #category : 'testing' }
MethodReference >> hasVariableBindingTo: aClass [

	^(self compiledMethodIfAbsent: [ ^false ]) hasVariableBindingTo: aClass

]

{ #category : 'comparisons' }
MethodReference >> hash [
	"Answer a SmallInteger whose value is related to the receiver's  
	identity."
	^ (self species hash bitXor: self classSymbol hash)
		bitXor: self methodSymbol hash
]

{ #category : 'setting' }
MethodReference >> indentLevel: anInteger [
	self removeStringVersionPrefix.
	stringVersion := (String streamContents: [:aStream| anInteger timesRepeat: [aStream nextPutAll: '    ']]), stringVersion
]

{ #category : 'testing' }
MethodReference >> isMessageListTitle [

	^(self actualClass isKindOf: Object) and: [methodSymbol first == $_]

]

{ #category : 'queries' }
MethodReference >> isValid [
	"Answer whether the receiver represents a current selector or Comment"

	| aClass |
	(aClass := self actualClass) ifNil: [^ false].
	^ (aClass includesSelector: methodSymbol) or:
		[methodSymbol == #Comment]
]

{ #category : 'source code ranges' }
MethodReference >> messageSendsRangesOf: aSentSelector [

	| compiledMethod |
	
	compiledMethod := self compiledMethodIfAbsent: [ ^#() ].
	^compiledMethod messageSendsRangesOf: aSentSelector 
]

{ #category : 'auto complete' }
MethodReference >> methodClass [

	^self actualClass 
]

{ #category : 'method node' }
MethodReference >> methodNode [
	
	^self compiledMethod methodNode
]

{ #category : 'queries' }
MethodReference >> methodSymbol [

	^methodSymbol
]

{ #category : 'method node' }
MethodReference >> notOptimizedMethodNode [

	^self compiledMethod notOptimizedMethodNode
]

{ #category : 'setting' }
MethodReference >> prefixStringVersionWith: aString [

	self removeStringVersionPrefix.
	stringVersion := '[', aString, '] - ', stringVersion.
]

{ #category : 'printing' }
MethodReference >> printClassAndSelectorOn: aStream [
		
	aStream nextPutAll: classSymbol.
	classIsMeta ifTrue: [ aStream nextPutAll: ' class' ].
	aStream
		nextPutAll: '>>#';
		nextPutAll: methodSymbol
]

{ #category : 'printing' }
MethodReference >> printOn: aStream [
	"Print the receiver on a stream"

	super printOn: aStream.
	aStream space.
	self printClassAndSelectorOn: aStream
]

{ #category : 'testing' }
MethodReference >> referencesParameterAt: parameterIndex [

	^(self compiledMethodIfAbsent: [ ^false ]) referencesParameterAt: parameterIndex 
]

{ #category : 'setting' }
MethodReference >> removeStringVersionPrefix [
	| i prefixCoda |
	prefixCoda := '] - '.
	i := stringVersion findString: prefixCoda.
	i = 0 ifFalse: [stringVersion := stringVersion copyFrom: i + prefixCoda size to: stringVersion size].
	stringVersion := stringVersion withoutLeadingBlanks
]

{ #category : 'queries' }
MethodReference >> selector [

	^methodSymbol
]

{ #category : 'setting' }
MethodReference >> setClass: aClass methodSymbol: methodSym stringVersion: aString [

	self
		setClassSymbol: aClass theNonMetaClass name
		classIsMeta: aClass isMeta
		methodSymbol: methodSym
		stringVersion: aString
]

{ #category : 'setting' }
MethodReference >> setClassSymbol: classSym classIsMeta: isMeta methodSymbol: methodSym stringVersion: aString  [

	classSymbol := classSym asSymbol.
	classIsMeta := isMeta.
	methodSymbol := methodSym asSymbol.
	stringVersion := aString
]

{ #category : 'setting' }
MethodReference >> setStandardClass: aClass methodSymbol: methodSym [

	self
		setClass: aClass
		methodSymbol: methodSym
		stringVersion: aClass name , ' ' , methodSym
]

{ #category : 'queries' }
MethodReference >> sourceCode [

	^ (self actualClassIfAbsent: [ self error: self class classDoesNotExistErrorMessage ]) 
		sourceCodeAt: methodSymbol
]

{ #category : 'queries' }
MethodReference >> sourceCodeIfAbsent: aBlock [

	| actualClass |

	actualClass := self actualClassIfAbsent: [ ^aBlock value ].
	^actualClass sourceCodeAt: methodSymbol ifAbsent: aBlock
]

{ #category : 'string version' }
MethodReference >> stringVersion [

	^stringVersion
]
