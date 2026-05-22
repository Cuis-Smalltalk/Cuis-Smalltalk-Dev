"
I am class holding state for compiled methods. All my instance variables should be actually part of the CompiledMethod itself, but the current implementation of the VM doesn't allow this.  Currently I hold the selector and any pragmas or properties the compiled method has.  Pragmas and properties are stored in indexable fields; pragmas as instances of Pragma, properties as instances of Association.

I am a reimplementation of much of MethodProperties, but eliminating the explicit properties and pragmas dictionaries.  Hence I answer true to isMethodProperties.
"
Class {
	#name : 'AdditionalMethodState',
	#superclass : 'Object',
	#instVars : [
		'method',
		'selector'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
AdditionalMethodState class >> forMethod: aMethod selector: aSelector [
	^(self basicNew: 0)
		selector: aSelector;
		setMethod: aMethod;
		yourself
]

{ #category : 'instance creation' }
AdditionalMethodState class >> selector: aSelector with: aPropertyOrPragma [
	^(self basicNew: 1)
		selector: aSelector;
		basicAt: 1 put: aPropertyOrPragma;
		yourself
]

{ #category : 'literals' }
AdditionalMethodState >> allLiteralsDo: aBlock [

	1 to: self basicSize do: [:index |
		(self basicAt: index) allLiteralsDo: aBlock].
]

{ #category : 'testing' }
AdditionalMethodState >> analogousCodeTo: aMethodProperties [
	| bs |
	self class == aMethodProperties class ifFalse:
		[^false].
	(bs := self basicSize) = aMethodProperties basicSize ifFalse:
		[^false].
	1 to: bs do:
		[:i|
		((self basicAt: i) analogousCodeTo: (aMethodProperties basicAt: i)) ifFalse:
			[^false]].
	^true
]

{ #category : 'accessing' }
AdditionalMethodState >> at: aKey [
	"Answer the property value or pragma associated with aKey."
	
	^self at: aKey ifAbsent: [self error: 'not found']
]

{ #category : 'accessing' }
AdditionalMethodState >> at: aKey ifAbsent: aBlock [
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^aBlock value
]

{ #category : 'accessing' }
AdditionalMethodState >> at: aKey ifAbsentPut: aBlock [
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^method propertyValueAt: aKey put: aBlock value
]

{ #category : 'accessing' }
AdditionalMethodState >> at: aKey put: aValue [
	"Replace the property value or pragma associated with aKey."

	| keyAlreadyExists propertyOrPragma "<Association|Pragma>" |
	keyAlreadyExists := false.
	
	1 to: self basicSize do: [ :i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue: [
			keyAlreadyExists := true.
			propertyOrPragma isVariableBinding
				ifTrue: [ propertyOrPragma value: aValue ]
				ifFalse: [ self basicAt: i put: aValue ]]].
	
	keyAlreadyExists ifFalse: [
		method propertyValueAt: aKey put: aValue ].
	
	^ aValue
]

{ #category : 'copying' }
AdditionalMethodState >> copyWith: aPropertyOrPragma "<Association|Pragma>" [
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(Association == aPropertyOrPragma class
	 or: [Pragma == aPropertyOrPragma class]) ifFalse:
		[self error: self class name, ' instances should hold only Associations or Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do:
		[:i|
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself

]

{ #category : 'copying' }
AdditionalMethodState >> copyWithout: aPropertyOrPragma "<Association|Pragma>" [
	"Answer a copy of the receiver which no longer includes aPropertyOrPragma"
	| bs copy offset |
	copy := self class new: (bs := self basicSize) - ((self includes: aPropertyOrPragma)
													ifTrue: [1]
													ifFalse: [0]).
	offset := 0.
	1 to: bs do:
		[:i|
		(self basicAt: i) = aPropertyOrPragma
			ifTrue: [offset := 1]
			ifFalse: [copy basicAt: i - offset put: (self basicAt: i)]].
	^copy
		selector: selector;
		setMethod: method;
		yourself

]

{ #category : 'testing' }
AdditionalMethodState >> hasAtLeastTheSamePropertiesAs: aMethodProperties [
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."
	aMethodProperties keysAndValuesDo:
		[:k :v|
		(v is: #Pragma)
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includes: k->v) ifFalse: [^false]]].
	^true
]

{ #category : 'testing' }
AdditionalMethodState >> hasLiteralSuchThat: aBlock [
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [(aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]]]
			ifFalse: [propertyOrPragma hasLiteralSuchThat: aBlock]) ifTrue: [^true]].
	^false
]

{ #category : 'testing' }
AdditionalMethodState >> includes: aPropertyOrPragma "<Association|Pragma>" [
	"Test if the property or pragma is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) = aPropertyOrPragma ifTrue:
			[^true]].
	^false
]

{ #category : 'testing' }
AdditionalMethodState >> includesKey: aKey [
	"Test if the property aKey or pragma with selector aKey is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) key == aKey ifTrue:
			[^true]].
	^false
]

{ #category : 'properties' }
AdditionalMethodState >> includesProperty: aKey [
	"Test if the property aKey is present."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^true]].
	^false
]

{ #category : 'testing' }
AdditionalMethodState >> isEmpty [
	^self basicSize = 0
]

{ #category : 'testing' }
AdditionalMethodState >> isMethodProperties [
	"AdditionalMethodState holds
		Pragmas
		Method Properties (in the form of Associations)
	if any CompiledMethod needs them."

	^true
]

{ #category : 'accessing' }
AdditionalMethodState >> keysAndValuesDo: aBlock [
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding
			ifTrue: [aBlock value: propertyOrPragma key value: propertyOrPragma value]
			ifFalse: [aBlock value: propertyOrPragma keyword value: propertyOrPragma]]
]

{ #category : 'accessing' }
AdditionalMethodState >> method: aMethodNodeOrNil [
	"For decompilation"
	method := aMethodNodeOrNil
]

{ #category : 'testing' }
AdditionalMethodState >> notEmpty [
	^self basicSize > 0
]

{ #category : 'accessing' }
AdditionalMethodState >> pragmas [
	"Answer the raw messages comprising my pragmas."
	| pragmaStream propertyOrPragma "<Association|Message>" |
	pragmaStream := WriteStream on: (Array new: self basicSize).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[pragmaStream nextPut: propertyOrPragma]].
	^pragmaStream contents
]

{ #category : 'accessing' }
AdditionalMethodState >> properties [

	| propertyStream propertyOrPragma "<Association|Pragma>" |
	propertyStream := WriteStream on: (Array new: self basicSize * 2).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[propertyStream nextPut: propertyOrPragma key; nextPut: propertyOrPragma value]].
	^IdentityDictionary newFromPairs: propertyStream contents
]

{ #category : 'properties' }
AdditionalMethodState >> propertyKeysAndValuesDo: aBlock [
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[aBlock value: propertyOrPragma key value: propertyOrPragma value]]
]

{ #category : 'properties' }
AdditionalMethodState >> propertyValueAt: aKey [
	"Answer the property value associated with aKey."
	
	^ self propertyValueAt: aKey ifAbsent: [ self error: 'Property not found' ].
]

{ #category : 'properties' }
AdditionalMethodState >> propertyValueAt: aKey ifAbsent: aBlock [
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^propertyOrPragma value]].
	^aBlock value
]

{ #category : 'properties' }
AdditionalMethodState >> removeKey: aKey [
	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."
	
	^ self removeKey: aKey ifAbsent: [ self error: 'Property not found' ].
]

{ #category : 'accessing' }
AdditionalMethodState >> removeKey: aKey ifAbsent: aBlock [
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma key]
				ifFalse: [propertyOrPragma keyword])
			== aKey ifTrue:
			[^method removeProperty: aKey]].
	^aBlock value
]

{ #category : 'accessing' }
AdditionalMethodState >> selector [
	^selector
]

{ #category : 'accessing' }
AdditionalMethodState >> selector: aSymbol [
	selector := aSymbol
]

{ #category : 'accessing' }
AdditionalMethodState >> setMethod: aMethod [
	| propertyOrPragma "<Association|Pragma>" |
	method := aMethod.
	1 to: self basicSize do:
		[ :i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[propertyOrPragma setMethod: aMethod]]
]
