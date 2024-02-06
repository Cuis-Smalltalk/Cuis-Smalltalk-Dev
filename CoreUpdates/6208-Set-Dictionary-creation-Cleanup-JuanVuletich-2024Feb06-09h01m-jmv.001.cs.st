'From Cuis6.3 [latest update: #6207] on 6 February 2024 at 9:11:37 am'!

!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
newWithRoomForMoreThan: nElements
	"Create a Set large enough to hold nElements without growing"
	^ self basicNew init: (self sizeFor: nElements)! !


!MethodDictionary class methodsFor: 'instance creation' stamp: 'di 11/4/97 20:11'!
newWithRoomForMoreThan: nElements
	"Create a Dictionary large enough to hold nElements without growing.
	Note that the basic size must be a power of 2.
	It is VITAL (see grow) that size gets doubled if nElements is a power of 2"
	| size |
	size := 1 bitShift: nElements highBit.
	^ (self basicNew: size) init: size! !


!Bag class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:07:13'!
new: nElements
	^ super new setContents: (self contentsClass newWithRoomForMoreThan: nElements)! !


!Set methodsFor: 'enumerating' stamp: 'jmv 2/6/2024 09:05:49'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := self species newWithRoomForMoreThan: self size.
	array do: [ :each | each ifNotNil: [ newSet add: (aBlock value: each)]].
	^ newSet! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
rehash
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array! !


!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new
	^ self newWithRoomForMoreThan: 4! !

!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new: nElements
	"Create a Set large enough to hold nElements without growing"
	^ super new init: (self sizeFor: nElements)! !


!Dictionary methodsFor: 'removing' stamp: 'jmv 2/6/2024 09:08:10'!
unreferencedKeys
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
	^((associations reject: [:assoc | referencedAssociations includes: assoc]) collect: [:assoc| assoc key]) asSet! !

!Dictionary methodsFor: 'enumerating' stamp: 'jmv 2/6/2024 09:07:34'!
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting values into a collection that is like me. Answer with the new collection."
	
	| newCollection |
	newCollection := self species newWithRoomForMoreThan: self size.
	self associationsDo: [ :each |
		newCollection at: each key put: (aBlock value: each value) ].
	^newCollection! !

!Dictionary methodsFor: 'private' stamp: 'jmv 2/6/2024 09:08:03'!
rehash
	"Smalltalk rehash."
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array := newSelf array! !


!Dictionary class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:07:54'!
newFrom: aDict 
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
"! !

!Dictionary class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 09:07:58'!
newFromPairs: anArray 

	"Answer an instance of me associating (anArray at:i) to (anArray at: i+1)
	 for each odd i.  anArray must have an even number of entries."

	| newDictionary |

	newDictionary := self newWithRoomForMoreThan: (anArray size/2).
	1 to: (anArray size-1) by: 2 do: [ :i|
		newDictionary at: (anArray at: i) put: (anArray at: i+1).
	].
	^ newDictionary

	"  Dictionary newFromPairs: {'Red' . Color red . 'Blue' . Color blue . 'Green' . Color green}. "! !


!MethodDictionary methodsFor: 'removing' stamp: 'jmv 2/5/2024 18:26:53'!
removeAll
	"This provides a faster way than repeated become.
	a single become is still in use to prevent system crash."
	
	| newSelf |
	tally = 0 ifTrue: [^self].
	newSelf := self species newWithRoomForMoreThan: self size.  "This will preserve the capacity"
	self become: newSelf! !

!MethodDictionary methodsFor: 'private' stamp: 'jmv 2/5/2024 18:26:41'!
grow 
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self basicSize.  "This will double the size"
	1 to: self basicSize do: [ :i |
		(self basicAt: i)
			ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self become: newSelf! !

!MethodDictionary methodsFor: 'private' stamp: 'jmv 2/5/2024 18:26:47'!
rehash 
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	self become: newSelf! !

!MethodDictionary methodsFor: 'private' stamp: 'jmv 2/5/2024 18:26:50'!
rehashWithoutBecome
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	^newSelf! !


!MethodDictionary class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:26:44'!
new
	"change the default size to be a bit bigger to help reduce the number of #grows while filing in"
	^self newWithRoomForMoreThan: 16! !


!WeakSet methodsFor: 'public' stamp: 'jmv 2/6/2024 09:09:02'!
collect: aBlock
	| each newSet |
	newSet := self species newWithRoomForMoreThan: self size.
	tally = 0 ifTrue: [^newSet ].
	1 to: array size do:
		[:index |
			((each := array at: index) == nil or: [each == flag])
				ifFalse: [newSet add: (aBlock value: each)]
		].
	^newSet! !

