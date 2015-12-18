'From Cuis 4.2 of 25 July 2013 [latest update: #2637] on 18 December 2015 at 3:16:38.823214 pm'!
'Description Please enter a description for this package'!
!provides: 'Collections-SkipLists' 1 2!
!classDefinition: #SkipList category: #'Collections-SkipLists'!
Collection subclass: #SkipList
	instanceVariableNames: 'sortBlock pointers numElements level splice'
	classVariableNames: 'Rand'
	poolDictionaries: ''
	category: 'Collections-SkipLists'!
!classDefinition: 'SkipList class' category: #'Collections-SkipLists'!
SkipList class
	instanceVariableNames: ''!

!classDefinition: #IdentitySkipList category: #'Collections-SkipLists'!
SkipList subclass: #IdentitySkipList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-SkipLists'!
!classDefinition: 'IdentitySkipList class' category: #'Collections-SkipLists'!
IdentitySkipList class
	instanceVariableNames: ''!

!classDefinition: #SkipListNode category: #'Collections-SkipLists'!
Object subclass: #SkipListNode
	instanceVariableNames: 'pointers object'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-SkipLists'!
!classDefinition: 'SkipListNode class' category: #'Collections-SkipLists'!
SkipListNode class
	instanceVariableNames: ''!


!SkipList commentStamp: 'KLC 2/26/2004 12:04' prior: 0!
From "Skip Lists: A Probabilistic Alternative to Balanced Trees" by William Pugh ( http://epaperpress.com/sortsearch/download/skiplist.pdf ):

"Skip lists are a data structure that can be used in place of balanced trees.  Skip lists use probabilistic balancing rather than strictly enforcing balancing and as a result the algorithms for insertion and deletion in skip lists are much simpler and significantly faster than equivalent algorithms for balanced trees."

Notes:

The elements of the skip list must implement #< or you must provide a sort block.

!

!IdentitySkipList commentStamp: '<historical>' prior: 0!
Like a SkipList, except that elements are compared with #== instead of #= .

See the comment of IdentitySet for more information.
!

!Collection methodsFor: '*Collections-SkipLists' stamp: 'LC 6/18/2001 20:30'!
asIdentitySkipList
	"Answer a IdentitySkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: IdentitySkipList! !

!Collection methodsFor: '*Collections-SkipLists' stamp: 'LC 6/18/2001 18:47'!
asSkipList
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SkipList! !

!Collection methodsFor: '*Collections-SkipLists' stamp: 'jmv 12/18/2015 14:58'!
asSkipList: aSortBlock 
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| skipList |
	skipList _ SkipList new: self size sortBlock: aSortBlock.
	skipList addAll: self.
	^ skipList! !

!SkipList methodsFor: 'adding' stamp: 'LC 6/18/2001 18:30'!
add: element 
	self add: element ifPresent: nil.
	^ element! !

!SkipList methodsFor: 'adding' stamp: 'LC 6/18/2001 20:42'!
add: element ifPresent: aBlock
	| node lvl s |
	node _ self search: element updating: splice.
	node ifNotNil: [aBlock ifNotNil: [^ aBlock value: node]].
	lvl _ self randomLevel.
	node _ SkipListNode on: element level: lvl.
	level + 1 to: lvl do: [:i | splice at: i put: self].
	1 to: lvl do: [:i |
				s _ splice at: i.
				node atForward: i put: (s forward: i).
				s atForward: i put: node].
	numElements _ numElements + 1.
	splice atAllPut: nil.
	^ element
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 19:26'!
atForward: i put: node
	level _ node
		ifNil: [pointers findLast: [:n | n notNil]]
		ifNotNil: [level max: i].
	^ pointers at: i put: node! !

!SkipList methodsFor: 'enumerating' stamp: 'LC 6/18/2001 15:39'!
do: aBlock
	self nodesDo: [:node | aBlock value: node object]! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 11:21'!
forward: i 
	^ pointers at: i! !

!SkipList methodsFor: 'testing' stamp: 'LC 6/18/2001 16:59'!
includes: element
	^ (self search: element updating: nil) notNil! !

!SkipList methodsFor: 'initialization' stamp: 'LC 6/18/2001 20:08'!
initialize: maxLevel
	pointers _ Array new: maxLevel.
	splice _ Array new: maxLevel.
	numElements _ 0.
	level _ 0.
	Rand ifNil: [Rand _ Random new]! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 20:14'!
is: node before: element 
	| object |
	node ifNil: [^ false].
	object _ node object.
	^ sortBlock
		ifNil: [object < element]
		ifNotNil: [(self is: object equalTo: element) ifTrue: [^ false].
			sortBlock value: object value: element]! !

!SkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 10:14'!
is: element1 equalTo: element2
	^ element1 = element2! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 13:19'!
is: node theNodeFor: element 
	node ifNil: [^ false].
	node == self ifTrue: [^ false].
	^ self is: node object equalTo: element! !

!SkipList methodsFor: 'testing' stamp: 'LC 6/18/2001 12:53'!
isEmpty
	^ numElements = 0! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:22'!
level
	^ level! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:05'!
maxLevel
	^ pointers size! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:18'!
maxLevel: n
	| newLevel oldPointers |
	newLevel _ n max: level.
	oldPointers _ pointers.
	pointers _ Array new: newLevel.
	splice _ Array new: newLevel.
	1 to: level do: [:i | pointers at: i put: (oldPointers at: i)]
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 16:15'!
next
	^ pointers first! !

!SkipList methodsFor: 'node enumeration' stamp: 'LC 6/18/2001 19:30'!
nodesDo: aBlock
	| node |
	node _ pointers first.
	[node notNil]
		whileTrue:
			[aBlock value: node.
			node _ node next]! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 15:37'!
randomLevel
	| p answer max |
	p _ 0.5.
	answer _ 1.
	max _ self maxLevel.
	[Rand next < p and: [answer < max]]
		whileTrue: [answer _ answer + 1].
	^ answer! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 20:42'!
remove: element ifAbsent: aBlock
	| node i s |
	node _ self search: element updating: splice.
	node ifNil: [^ aBlock value].
	i _ 1.
	[s _ splice at: i.
	i <= level and: [(s forward: i) == node]]
				whileTrue:
					[s atForward: i put: (node forward: i).
					i _ i + 1].
	numElements _ numElements - 1.
	splice atAllPut: nil.
	^ node object
! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 20:25'!
removeAll
	pointers atAllPut: nil.
	splice atAllPut: nil.
	numElements _ 0.
	level _ 0.! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 19:28'!
search: element updating: array
	| node forward |
	node _ self.
	level to: 1 by: -1 do: [:i |
			[forward _ node forward: i.
			self is: forward before: element] whileTrue: [node _ forward].
			"At this point: node < element <= forward"
			array ifNotNil: [array at: i put: node]].
	node _ node next.
	^ (self is: node theNodeFor: element) ifTrue: [node]! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 15:40'!
size
	^ numElements! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:19'!
sortBlock
	^ sortBlock! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 17:30'!
sortBlock: aBlock
	sortBlock _ aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:33'!
maxLevel: maxLevel
	"
	SkipList maxLevel: 5
	"
	^ super new initialize: maxLevel! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:34'!
maxLevel: anInteger sortBlock: aBlock
	^ (self maxLevel: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 11:52'!
new
	"
	SkipList new
	"
	^ super new initialize: 10! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:39'!
new: anInteger
	^ self maxLevel: (anInteger log: 2) ceiling! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 18:40'!
new: anInteger sortBlock: aBlock
	^ (self new: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:32'!
sortBlock: aBlock
	^ self new sortBlock: aBlock! !

!IdentitySkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 20:28'!
is: element1 equalTo: element2
	^ element1 == element2! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:55'!
atForward: i put: node
	^ pointers at: i put: node! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 13:34'!
forward: i 
	^ pointers at: i! !

!SkipListNode methodsFor: 'initialization' stamp: 'LC 6/17/2001 11:54'!
initialize: maxLevel
	pointers _ Array new: maxLevel! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 12:21'!
level
	^ pointers size! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:20'!
next
	^ pointers first! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 10:40'!
object
	^ object! !

!SkipListNode methodsFor: 'private' stamp: 'LC 6/18/2001 10:18'!
object: anObject
	object _ anObject! !

!SkipListNode methodsFor: 'printing' stamp: 'LC 6/18/2001 15:26'!
printOn: aStream
	| first |
	aStream
		nextPut: $[;
		nextPutAll: object printString;
		nextPutAll: ']-->('.
	first _ true.
	pointers do: [:node |
		first ifTrue: [first _ false] ifFalse: [aStream space].
		aStream nextPutAll: (node ifNil: ['*'] ifNotNil: [node object printString])].
	aStream nextPut: $)
! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 09:16'!
new: maxLevel
	^ super new initialize: maxLevel! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 10:20'!
on: element level: maxLevel 
	^ (self new: maxLevel)
		object: element! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 12:44'!
tailOfLevel: n
	^ self on: nil level: n! !
