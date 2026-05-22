"
I am a special dictionary holding methods. I am just like a normal Dictionary, except that I am implemented differently.  Each Class has an instance of MethodDictionary to hold the correspondence between selectors (names of methods) and methods themselves.

In a normal Dictionary, the instance variable 'array' holds an array of Associations.  Since there are thousands of methods in the system, these Associations waste space.  

Each MethodDictionary is a variable object, with the list of keys (selector Symbols) in the variable part of the instance.  The variable 'array' holds the values, which are CompiledMethods.

I also maintain the following invariant: (self basicAt: index) isNil = (array at: index) isNil.
"
Class {
	#name : 'MethodDictionary',
	#superclass : 'Dictionary',
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
MethodDictionary class >> new [
	"change the default size to be a bit bigger to help reduce the number of #grows while filing in"
	^self newWithRoomForMoreThan: 16
]

{ #category : 'instance creation' }
MethodDictionary class >> newWithRoomForMoreThan: nElements [
	"Create a Dictionary large enough to hold nElements without growing.
	Note that the basic size must be a power of 2.
	It is VITAL (see grow) that size gets doubled if nElements is a power of 2"
	| size |
	size := 1 bitShift: nElements highBit.
	^ (self basicNew: size) init: size
]

{ #category : 'initialization' }
MethodDictionary class >> rehashAllInstances [

	| instances newInstances |
	instances := self allInstances asArray.
	newInstances := self allInstances collect: [ :each | each rehashWithoutBecome ].
	instances elementsExchangeIdentityWith: newInstances
]

{ #category : 'accessing' }
MethodDictionary >> add: anAssociation [
	^ self at: anAssociation key put: anAssociation value
]

{ #category : 'accessing' }
MethodDictionary >> associationAt: key ifAbsent: aBlock  [
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^(array at: (self scanFor: key)) 
		ifNil: [ aBlock value ]
		ifNotNil: [ :value | key -> value ]
]

{ #category : 'enumeration' }
MethodDictionary >> associationsDo: aBlock  [

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do: [ :i | 
		(self basicAt: i) ifNotNil: [ :key |
			aBlock value: (
				Association key: key value: (array at: i))]]
]

{ #category : 'accessing' }
MethodDictionary >> at: key ifAbsent: aBlock [

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	^ array at: index
]

{ #category : 'accessing' }
MethodDictionary >> at: key put: value [
	"Set the value at key to be value."
	| index |
	index := self findElementOrNil: key.
	(self basicAt: index)
		ifNil: [ 
			tally := tally + 1.
			self basicAt: index put: key]
		ifNotNil: [
			(array at: index) flushCache].
	array at: index put: value.
	self fullCheck.
	^ value
]

{ #category : 'private' }
MethodDictionary >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| key index |
	index := start.
	[ (key := self basicAt: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self scanFor: key) = index ifFalse: [
			self privateSwapArrayElement: index with: newIndex ] ]
]

{ #category : 'private' }
MethodDictionary >> grow  [
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self basicSize.  "This will double the size"
	1 to: self basicSize do: [ :i |
		(self basicAt: i)
			ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self become: newSelf
]

{ #category : 'private' }
MethodDictionary >> keyAt: index [

	^ self basicAt: index
]

{ #category : 'accessing' }
MethodDictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."

	1 to: self basicSize do:
		[:index |
		value == (array at: index)
			ifTrue: [
				(self basicAt: index)
					ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
MethodDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."

	1 to: self basicSize do:
		[:index |
		value = (array at: index)
			ifTrue: [
				(self basicAt: index)
					ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
]

{ #category : 'enumeration' }
MethodDictionary >> keysAndValuesDo: aBlock  [
	"Enumerate the receiver with all the keys and values passed to the block"

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key |
			aBlock value: key value: (array at: i)]
		]
]

{ #category : 'enumeration' }
MethodDictionary >> keysDo: aBlock [

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key |
			aBlock value: key]]
]

{ #category : 'private' }
MethodDictionary >> methodArray [
	^ array
]

{ #category : 'private' }
MethodDictionary >> postCopy [

	array := array copy
]

{ #category : 'private' }
MethodDictionary >> privateSwapArrayElement: oneIndex with: otherIndex [
	| element |
	element := self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	super privateSwapArrayElement: oneIndex with: otherIndex.
]

{ #category : 'private' }
MethodDictionary >> rehash  [
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	self become: newSelf
]

{ #category : 'private' }
MethodDictionary >> rehashWithoutBecome [
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	^newSelf
]

{ #category : 'removing' }
MethodDictionary >> removeAll [
	"This provides a faster way than repeated become.
	a single become is still in use to prevent system crash."
	
	| newSelf |
	tally = 0 ifTrue: [^self].
	newSelf := self species newWithRoomForMoreThan: self size.  "This will preserve the capacity"
	self become: newSelf
]

{ #category : 'private' }
MethodDictionary >> removeDangerouslyKey: key ifAbsent: aBlock [
	"This is not really dangerous.  But if normal removal
	were done WHILE a MethodDict were being used, the
	system might crash.  So instead we make a copy, then do
	this operation (which is NOT dangerous in a copy that is
	not being used), and then use the copy after the removal."

	| index element |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	element := array at: index.
	array at: index put: nil.
	self basicAt: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ element
]

{ #category : 'removing' }
MethodDictionary >> removeKey: key ifAbsent: errorBlock  [
	"The interpreter might be using this MethodDict while
	this method is running!  Therefore we perform the removal
	in a copy, and then atomically become that copy"
	| copy |
	copy := self copy.
	copy removeDangerouslyKey: key ifAbsent: [^ errorBlock value].
	self become: copy
]

{ #category : 'private' }
MethodDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject identityHash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : 'enumeration' }
MethodDictionary >> valuesDo: aBlock [

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do: [ :i |
		(array at: i) ifNotNil: [ :value |
			aBlock value: value]]
]

{ #category : 'enumeration' }
MethodDictionary >> valuesSelect: aCondition [

	| selected |

	selected := OrderedCollection new.
	self valuesDo: [ :aValue | (aCondition value: aValue) ifTrue: [ selected add: aValue ]].

	^selected
]
