"
I represent a set of elements that can be viewed from one of two perspectives: a set of associations, or a container of values that are externally named where the name can be any object that responds to =. The external name is referred to as the key.  I inherit many operations from Set.
"
Class {
	#name : 'Dictionary',
	#superclass : 'Set',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'error descriptions' }
Dictionary class >> keyNotFoundErrorDescriptionFor: key [

	^ String streamContents: [ :strm |
			strm nextPutAll: 	'key: '.
			key printOn: strm.
			strm nextPutAll: ' not found' ]
]

{ #category : 'instance creation' }
Dictionary class >> newFrom: aDict  [
	"Answer an instance of me containing the same associations as aDict.
	 Error if any key appears twice."
	| newDictionary |
	newDictionary := self newWithRoomForMoreThan: aDict size.
	aDict associationsDo:
		[:x |
		(newDictionary includesKey: x key)
			ifTrue: [self error: 'Duplicate key: ', x key printString]
			ifFalse: [newDictionary add: x copy]].
	^ newDictionary

"	NewDictionary newFrom: {1->#a. 2->#b. 3->#c}
	{1->#a. 2->#b. 3->#c} as: NewDictionary
	NewDictionary newFrom: {1->#a. 2->#b. 1->#c}
	{1->#a. 2->#b. 1->#c} as: NewDictionary
"
]

{ #category : 'instance creation' }
Dictionary class >> newFromPairs: anArray  [

	"Answer an instance of me associating (anArray at:i) to (anArray at: i+1)
	 for each odd i.  anArray must have an even number of entries."

	| newDictionary |

	newDictionary := self newWithRoomForMoreThan: (anArray size/2).
	1 to: (anArray size-1) by: 2 do: [ :i|
		newDictionary at: (anArray at: i) put: (anArray at: i+1).
	].
	^ newDictionary

	"  Dictionary newFromPairs: {'Red' . Color red . 'Blue' . Color blue . 'Green' . Color green}. "
]

{ #category : 'comparing' }
Dictionary >> = aDictionary [
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value"

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true


]

{ #category : 'adding' }
Dictionary >> add: anAssociation [
	"Quite like doing
		self at: anAssociation key put: anAssociation value
	but making sure the argument is stored.
	This method should be used when the argument is stored elsewhere,
	and its value should be kept in sync with the value stored in me.

	If the key already exists, and it is desired to keep the existing association, then call #at:put:"

	| index |
	index := self findElementOrNil: anAssociation key.
	(array at: index)
		ifNotNil: [ array at: index put: anAssociation ]
		ifNil: [ self atNewIndex: index put: anAssociation ].
	^ anAssociation
]

{ #category : 'adding' }
Dictionary >> addAll: aCollection [

	"It should hold Associations, then"
	(aCollection is: #Dictionary) ifFalse: [
		^super addAll: aCollection ].
	
	aCollection == self ifFalse: [
		aCollection keysAndValuesDo: [:key :value |
			self at: key put: value]].
	
	^aCollection
]

{ #category : 'accessing' }
Dictionary >> associationAt: key  [
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [ self errorKeyNotFound: key ]
]

{ #category : 'accessing' }
Dictionary >> associationAt: key ifAbsent: aBlock  [
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc
]

{ #category : 'accessing' }
Dictionary >> associationClass [

	^Association
]

{ #category : 'accessing' }
Dictionary >> associations [
	"Answer a Collection containing the receiver's associations."
	| out |
	out := WriteStream on: (Array new: self size).
	self associationsDo: [:value | out nextPut: value].
	^ out contents
]

{ #category : 'enumerating' }
Dictionary >> associationsDo: aBlock  [
	"Evaluate aBlock for each of the receiver's key/value associations."

	super do: aBlock
]

{ #category : 'accessing' }
Dictionary >> at: key  [
	"Answer the value at key.  If key is not found, create an error message."

	^self at: key ifAbsent: [ self errorKeyNotFound: key ]
]

{ #category : 'accessing' }
Dictionary >> at: key ifAbsent: absentBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ assoc value
]

{ #category : 'accessing' }
Dictionary >> at: key ifAbsentOrNilPut: aBlock [
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	| index assoc newObject |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc ifNotNil: [
		assoc value ifNil: [ assoc value: aBlock value ].
		^assoc value ].
	newObject := aBlock value.
	self atNewIndex: index put: (self associationClass key: key value: newObject).
	^ newObject
]

{ #category : 'accessing' }
Dictionary >> at: key ifAbsentPut: aBlock [
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	| index assoc newObject |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc ifNotNil: [
		^assoc value ].
	newObject := aBlock value.
	self atNewIndex: index put: (self associationClass key: key value: newObject).
	^ newObject
]

{ #category : 'accessing' }
Dictionary >> at: key ifPresent: presentBlock [
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [ ^ nil ].
	^ presentBlock value: v

]

{ #category : 'accessing' }
Dictionary >> at: key ifPresent: presentBlock ifAbsent: absentBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."
	"
		Smalltalk at: #zork ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'zork absent' print ]
		Smalltalk at: #Number ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'Number absent' print ]
	"

	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ presentBlock value: assoc value
]

{ #category : 'accessing' }
Dictionary >> at: key put: anObject [
	"Set the value at key to be anObject. 
	If key is not found, create a new entry for key and set is value to anObject.
	If key is found, update the existing association.
	Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [ self atNewIndex: index put: (self associationClass key: key value: anObject) ]
		ifNotNil: [ assoc value: anObject ].
	^ anObject
]

{ #category : 'accessing' }
Dictionary >> bindingOf: varName [

	^self associationAt: varName ifAbsent: nil
]

{ #category : 'compiling' }
Dictionary >> bindingsDo: aBlock [
	^self associationsDo: aBlock
]

{ #category : 'enumerating' }
Dictionary >> collect: aBlock  [
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting values into a collection that is like me. Answer with the new collection."
	
	| newCollection |
	newCollection := self species newWithRoomForMoreThan: self size.
	self associationsDo: [ :each |
		newCollection at: each key put: (aBlock value: each value) ].
	^newCollection
]

{ #category : 'inspecting' }
Dictionary >> customizeExplorerContents [

	^ true.

]

{ #category : 'variable bindings' }
Dictionary >> declare: key from: aDictionary [
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver. Answer it.
	If the key wasn't in the receiver or aDictionary, use nil as the value.
	Note: We allow aDictionary to be nil, although it normally is not."

	| association |
	(self includesKey: key) ifTrue: [^ nil].
	(aDictionary notNil and: [aDictionary includesKey: key])
		ifTrue: [
			association := aDictionary associationAt: key.
			self add: association.
			aDictionary removeKey: key.
			^ association ]
		ifFalse: [
			self at: key put: nil.
			^ nil ]
]

{ #category : 'enumerating' }
Dictionary >> do: aBlock  [
	super do: [:assoc | aBlock value: assoc value]
]

{ #category : 'comparing' }
Dictionary >> emptyCollectionHash [
	^ Dictionary hash
]

{ #category : 'private' }
Dictionary >> errorKeyNotFound: key [

	self error: (self class keyNotFoundErrorDescriptionFor: key)
]

{ #category : 'private' }
Dictionary >> errorValueNotFound [
	self error: 'value not found'
]

{ #category : 'inspecting' }
Dictionary >> explorerContents [

	^self explorerContentsWithIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]
]

{ #category : 'inspecting' }
Dictionary >> explorerContentsWithIndexCollect: twoArgBlock [

	| sortedKeys |
	sortedKeys := self keys sort: [:x :y |
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys collect: [:k | twoArgBlock value: (self at: k) value: k].

]

{ #category : 'testing' }
Dictionary >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	
	self keysDo:[:each | 
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false
]

{ #category : 'comparing' }
Dictionary >> hash [
	"Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash := self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self associationsDo: [ :association | hash := hash bitXor: association hash ]].
	^ hash bitXor: self size hash
]

{ #category : 'testing' }
Dictionary >> includesKey: key  [
	"Answer whether the receiver has a key equal to the argument, key."
	
	self at: key ifAbsent: [^false].
	^true
]

{ #category : 'inspecting' }
Dictionary >> inspectorClass [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ DictionaryInspector
]

{ #category : 'testing' }
Dictionary >> is: aSymbol [
	^aSymbol == #Dictionary or: [ super is: aSymbol ]
]

{ #category : 'private' }
Dictionary >> keyAt: index [
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	| assn |
	assn := array at: index.
	assn ifNil: [^ nil].
	^ assn key
]

{ #category : 'accessing' }
Dictionary >> keyAtIdentityValue: value  [
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound]
]

{ #category : 'accessing' }
Dictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value
]

{ #category : 'accessing' }
Dictionary >> keyAtValue: value  [
	"Answer the key whose value equals the argument, value.  If there is none,
	cause an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]
]

{ #category : 'accessing' }
Dictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."
 
	self associationsDo: 
		[:association | value = association value ifTrue: [^association key]].
	^exceptionBlock value
]

{ #category : 'testing' }
Dictionary >> keyForIdentity: anObject [
	"If anObject is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self associationsDo: [:assoc | assoc value == anObject ifTrue: [^ assoc key]].
	^ nil
]

{ #category : 'accessing' }
Dictionary >> keys [
	"Answer an Array containing the receiver's keys."
	
	^Array streamContents: [:s| self keysDo: [:key| s nextPut: key]] estimatedSize: self size
]

{ #category : 'enumerating' }
Dictionary >> keysAndValuesDo: aBlock [
	^self associationsDo:[:assoc|
		aBlock value: assoc key value: assoc value].
]

{ #category : 'removing' }
Dictionary >> keysAndValuesRemove: keyValueBlock [
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals := OrderedCollection new.
	self associationsDo:
		[:assoc | (keyValueBlock value: assoc key value: assoc value)
			ifTrue: [removals add: assoc key]].
 	removals do:
		[:aKey | self removeKey: aKey]
]

{ #category : 'enumerating' }
Dictionary >> keysDo: aBlock  [
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]
]

{ #category : 'accessing' }
Dictionary >> keysSortedSafely [
	"Answer a sorted Collection containing the receiver's keys."
	| sortedKeys |
	sortedKeys := OrderedCollection new.
	self keysDo: [:each | sortedKeys addLast: each].
	sortedKeys sort:
		[ :x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys
]

{ #category : 'lookup' }
Dictionary >> lookup: anObject [
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	self do: [:each | anObject = each ifTrue: [^each]].
	^nil
]

{ #category : 'private' }
Dictionary >> noCheckAdd: anObject [
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1
]

{ #category : 'testing' }
Dictionary >> occurrencesOf: anObject  [
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count := 0.
	self do: [:each | anObject = each ifTrue: [count := count + 1]].
	^count
]

{ #category : 'private' }
Dictionary >> postCopy [
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [:each | each copy]
]

{ #category : 'printing' }
Dictionary >> printElementsOn: aStream [
	aStream nextPut: $(.
	self keysDo: [ :key |
		aStream print: key; nextPutAll: '->'; print: (self at: key); space ].
	aStream nextPut: $)
]

{ #category : 'private' }
Dictionary >> rehash [
	"Smalltalk rehash."
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array := newSelf array
]

{ #category : 'removing' }
Dictionary >> remove: anObject  [
	self shouldNotImplement
]

{ #category : 'removing' }
Dictionary >> remove: anObject ifAbsent: exceptionBlock  [
	self shouldNotImplement
]

{ #category : 'removing' }
Dictionary >> removeKey: key  [
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [ self errorKeyNotFound: key ]
]

{ #category : 'removing' }
Dictionary >> removeKey: key ifAbsent: aBlock  [
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [^ aBlock value].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value
]

{ #category : 'removing' }
Dictionary >> removeUnreferencedKeys   "Undeclared removeUnreferencedKeys" [

	^ self unreferencedKeys do: [:key | self removeKey: key].
]

{ #category : 'private' }
Dictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : 'enumerating' }
Dictionary >> select: aBlock  [
	"Evaluate aBlock with each of my values as the argument.  Collect into a new 
	dictionary, only those associations for which aBlock evaluates to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection
]

{ #category : 'printing' }
Dictionary >> storeOn: aStream [
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self keysSortedSafely do: [ :key | 
			noneYet
				ifTrue: [noneYet := false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: (self associationAt: key)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : 'removing' }
Dictionary >> unreferencedKeys [
	| currentClass associations referencedAssociations |
	currentClass := nil.
	associations := self associations asIdentitySet.
	referencedAssociations := IdentitySet newWithRoomForMoreThan: associations size.
	Smalltalk allSelect: [ :m |
		m methodClass ~~ currentClass ifTrue: [
			currentClass := m methodClass ].
		m literalsDo: [ :l |
			(l isVariableBinding and: [associations includes: l]) ifTrue: [
				referencedAssociations add: l]].
		false ].
	^((associations reject: [:assoc | referencedAssociations includes: assoc]) collect: [:assoc| assoc key]) asSet
]

{ #category : 'accessing' }
Dictionary >> values [
	"Answer a Collection containing the receiver's values."
	| out |
	out := WriteStream on: (Array new: self size).
	self valuesDo: [:value | out nextPut: value].
	^ out contents
]

{ #category : 'enumerating' }
Dictionary >> valuesDo: aBlock  [
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association value]
]
