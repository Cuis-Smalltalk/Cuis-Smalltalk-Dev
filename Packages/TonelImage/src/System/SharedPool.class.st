"
A shared pool represents a set of bindings which are accessible to all classes which import the pool in its 'pool dictionaries'. SharedPool is NOT a dictionary but rather a name space. Bindings are represented by 'class variables' - as long as we have no better way to represent them at least.
"
Class {
	#name : 'SharedPool',
	#superclass : 'Object',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'name lookup' }
SharedPool class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol binding |
	aSymbol := varName asSymbol.

	"First look in classVar dictionary."
	binding := self classPool bindingOf: aSymbol.
	binding ifNotNil: [ ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		| poolBinding |
		poolBinding := pool bindingOf: aSymbol.
		poolBinding ifNotNil: [ ^poolBinding ].
	].

	"subclassing is not preserved"
	^nil
]

{ #category : 'name lookup' }
SharedPool class >> bindingsDo: aBlock [
	^self classPool bindingsDo: aBlock
]

{ #category : 'name lookup' }
SharedPool class >> classBindingOf: varName [
	"For initialization messages grant the regular scope"
	^super bindingOf: varName
]

{ #category : 'name lookup' }
SharedPool class >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a binding that begins with aString, false otherwise"

	"First look in classVar dictionary."
	(self classPool hasBindingThatBeginsWith: aString) ifTrue:[^true].
	"Next look in shared pools."
	self sharedPools do:[:pool | 
		(pool hasBindingThatBeginsWith: aString) ifTrue: [^true]].
	^false
]

{ #category : 'name lookup' }
SharedPool class >> includesKey: aName [
	"does this pool include aName"
	^(self bindingOf: aName) notNil
]

{ #category : 'enumerating' }
SharedPool class >> keysDo: aBlock [
"A hopefully temporary fix for an issue arising from miss-spelled variable names in code being compiled. The correction code (see Class>possibleVariablesFor:continuedFrom: assumes that sharedPools are Dictionaries. The proper fix would involve making sure all pools are actually subclasses of SharedPool, which they are not currently."
	self bindingsDo:[:b|
		aBlock value: b key]
]
