"
I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary.
"
Class {
	#name : 'Association',
	#superclass : 'LookupKey',
	#instVars : [
		'value'
	],
	#category : 'Kernel-Support',
	#package : 'Kernel',
	#tag : 'Support'
}

{ #category : 'instance creation' }
Association class >> key: newKey value: newValue [
	"Answer a new instance of me with the arguments as the key and
	value of the association."
	^(super key: newKey) value: newValue
]

{ #category : 'comparing' }
Association >> = anAssociation [

	self == anAssociation ifTrue: [ ^ true ].
	^ super = anAssociation and: [value = anAssociation value]
]

{ #category : 'testing' }
Association >> analogousCodeTo: anObject [
"
From:  	Oscar Nierstrasz <oscar.nierstrasz@gmail.com>Add oscar.nierstrasz@gmail.com to my Address Book
Subject:  	[Pharo-dev] CompiledMethod>>#= is broken

The following code breaks:

(SortedCollectionTest>>#testDo) = (SortedCollectionTest>>#testStoreOn).

because it makes use of AdditionalMethodState>>#analogousCodeTo: which tries to send #analogousCodeTo: to a property which is an Association.

It seems to me the solution is to implement Association>>#analogousCodeTo: with an equality test:

analogousCodeTo: anObject
   ^self class == anObject class
     and: [ self = anObject ]

Does this make sense?

See:

https://pharo.fogbugz.com/f/cases/12077/MNU-in-AdditionalMethodState-analogousCodeTo

Oscar Nierstrasz

"
   ^self class == anObject class
     and: [ self = anObject ]
]

{ #category : 'comparing' }
Association >> hash [
	"Hash is reimplemented because = is implemented."

	^ key hash
]

{ #category : 'testing' }
Association >> isSpecialWriteBinding [
	"Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages"
	^false
]

{ #category : 'accessing' }
Association >> key: aKey value: anObject  [
	"Store the arguments as the variables of the receiver."

	key := aKey.
	value := anObject
]

{ #category : 'testing' }
Association >> literalEqual: otherLiteral [
	"Answer true if the receiver and otherLiteral represent the same literal.
	Variable bindings are literally equals only if identical.
	This is how variable sharing works, by preserving identity of the Association, and changing only the value."
	^self == otherLiteral
]

{ #category : 'objects from disk' }
Association >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  If I am a known global, write a proxy that will hook up with the same resource in the destination system."

	^ (Smalltalk associationAt: key ifAbsent: nil) == self 
		ifTrue: [dp := DiskProxy global: #Smalltalk selector: #associationOrUndeclaredAt: 
							args: (Array with: key).
			refStrm replace: self with: dp.
			dp]
		ifFalse: [self]
]

{ #category : 'printing' }
Association >> printOn: aStream [

	aStream
		print: self key;
		nextPutAll: ' -> ';
		print: self value
]

{ #category : 'printing' }
Association >> storeOn: aStream [
	"Store in the format (key->value)"
	aStream nextPut: $(.
	key storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
Association >> value [
	"Answer the value of the receiver."
	^value
]

{ #category : 'accessing' }
Association >> value: anObject  [
	"Store the argument, anObject, as the value of the receiver."
	value := anObject
]
