"
I represent a key for looking up entries in a data structure. Subclasses of me, such as Association, typically represent dictionary entries.
"
Class {
	#name : 'LookupKey',
	#superclass : 'Magnitude',
	#instVars : [
		'key'
	],
	#category : 'Kernel-Support',
	#package : 'Kernel',
	#tag : 'Support'
}

{ #category : 'instance creation' }
LookupKey class >> key: aKey  [
	"Answer a new instance of me with the argument as the lookup up."
	^self new key: aKey
]

{ #category : 'comparing' }
LookupKey >> < aLookupKey  [
	^key < aLookupKey key
]

{ #category : 'comparing' }
LookupKey >> = aLookupKey [

	self == aLookupKey ifTrue: [ ^ true ].
	self species == aLookupKey species
		ifFalse: [ ^false ].

	^ key = aLookupKey key
]

{ #category : 'literals' }
LookupKey >> allLiteralsDo: aBlock [
	"Treat myself or my key as literal. Do not treat any value (i.e., in Association or Binding) as literal to avoid decending too thoroughly. As a consequence, for example, classes have to be resolved to their class bindings or class names before using them as input in this interface."
	
	aBlock value: self.	
	self key allLiteralsDo: aBlock.
]

{ #category : 'bindings' }
LookupKey >> beBindingOfType: aClass announcing: aBool [
	"Make the receiver a global binding of the given type"
	| old new |
	(Smalltalk associationAt: self key) == self
		ifFalse:[^self error:'Not a global variable binding'].
	self class == aClass ifTrue:[^self].
	old := self.
	new := aClass key: self key value: self value.
	old become: new.
	"NOTE: Now self == read-only (e.g., the new binding)"
	^self recompileBindingsAnnouncing: aBool
]

{ #category : 'bindings' }
LookupKey >> beReadOnlyBinding [
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beReadOnlyBindingAnnouncing: true
]

{ #category : 'bindings' }
LookupKey >> beReadOnlyBindingAnnouncing: aBool [
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beBindingOfType: ReadOnlyVariableBinding announcing: aBool
]

{ #category : 'bindings' }
LookupKey >> beReadWriteBinding [
	"Make the receiver (a global read-only binding) be a read-write binding"
	^self beReadWriteBindingAnnouncing: true
]

{ #category : 'bindings' }
LookupKey >> beReadWriteBindingAnnouncing: aBool [
	"Make the receiver (a global read-write binding) be a read-write binding"
	^self beBindingOfType: Association announcing: aBool
]

{ #category : 'accessing' }
LookupKey >> canAssign [

	^ true
]

{ #category : 'comparing' }
LookupKey >> hash [
	"Hash is reimplemented because = is implemented."

	^key hash
]

{ #category : 'testing' }
LookupKey >> isSpecialReadBinding [
	"Return true if this variable binding is read protected, e.g., should not be accessed primitively but rather by sending #value messages"
	^false
]

{ #category : 'testing' }
LookupKey >> isVariableBinding [
	"Return true if I represent a literal variable binding"
	^true
]

{ #category : 'accessing' }
LookupKey >> key [
	"Answer the lookup key of the receiver."
	^key
]

{ #category : 'accessing' }
LookupKey >> key: anObject  [
	"Store the argument, anObject, as the lookup key of the receiver."
	key := anObject
]

{ #category : 'accessing' }
LookupKey >> name [

	^ self key isString
		ifTrue: [self key]
		ifFalse: [self key printString]
]

{ #category : 'printing' }
LookupKey >> printOn: aStream [

	super printOn: aStream.
	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'bindings' }
LookupKey >> recompileBindingsAnnouncing: aBool [
	"Make the receiver (a global read-write binding) be a read-only binding"
	aBool ifTrue:[
		Utilities informUserDuring: [ :barBlock |
			(Smalltalk allCallsOn: self) do: [ :mref | 
				barBlock value: 'Recompiling ', mref stringVersion.
				mref actualClass recompile: mref methodSymbol ].
		].
	] ifFalse:[
		(Smalltalk allCallsOn: self) do: [ :mref |
			mref actualClass recompile: mref methodSymbol ]
	]
]
