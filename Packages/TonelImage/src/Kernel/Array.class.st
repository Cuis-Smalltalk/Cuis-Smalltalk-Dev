"
I present an ArrayedCollection whose elements are objects.
"
Class {
	#name : 'Array',
	#superclass : 'ArrayedCollection',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'brace support' }
Array class >> braceStream: nElements [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ WriteStream basicNew braceArray: (self new: nElements)

]

{ #category : 'brace support' }
Array class >> braceWith: a [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 1.
	array at: 1 put: a.
	^ array
]

{ #category : 'brace support' }
Array class >> braceWith: a with: b  [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 2.
	array at: 1 put: a.
	array at: 2 put: b.
	^ array
]

{ #category : 'brace support' }
Array class >> braceWith: a with: b with: c  [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 3.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	^ array
]

{ #category : 'brace support' }
Array class >> braceWith: a with: b with: c with: d [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 4.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	array at: 4 put: d.
	^ array
]

{ #category : 'brace support' }
Array class >> braceWithNone [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ self new: 0
]

{ #category : 'literals' }
Array >> allLiteralsDo: aBlock [

	"I am a literal."
	aBlock value: self.
	
	"All my elements are literals."
	self do: [:each | each allLiteralsDo: aBlock].
]

{ #category : 'converting' }
Array >> asArray [
	"Answer with the receiver itself.
	But for subclasses, answer an actual array!"

	^self class == Array
		ifTrue: [ self ]
		ifFalse: [ self asNewArray ]
]

{ #category : 'accessing' }
Array >> atWrap: index  [
	"Optimized to go through the primitive if possible"
	<primitive: 60>
	^ self at: index - 1 \\ self size + 1
]

{ #category : 'accessing' }
Array >> atWrap: index put: anObject [
	"Optimized to go through the primitive if possible"
	<primitive: 61>
	^ self at: index - 1 \\ self size + 1 put: anObject
]

{ #category : 'enumerating' }
Array >> collect: collectBlock thenSelect: selectBlock [
	"Evaluate collectBlock with each my elements as the argument. Collect the 
	resulting values into a collection that is like me, but only those elements for which
	selectBlock evaluates to true. Answer the new collection.
	Overriden for performance."

	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			| newElement |
			newElement := collectBlock value: (self at: index).
			(selectBlock value: newElement)
				ifTrue: [ strm nextPut: newElement ]]]
]

{ #category : 'system primitives' }
Array >> elementsExchangeIdentityWith: otherArray [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

	<primitive: 128 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[| maxRequired |
		 "In Spur, two-way become may involve making each pair of objects into a forwarder into a copy of the other.
		 So if become fails with #'insufficient object memory', garbage collect, and if necessary, grow memory."
		 maxRequired := (self sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0])
						+ (otherArray sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0]).
		 (Smalltalk garbageCollectMost < maxRequired
		  and: [Smalltalk garbageCollect < maxRequired]) ifTrue:
			[Smalltalk growMemoryByAtLeast: maxRequired].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed
]

{ #category : 'converting' }
Array >> elementsForwardIdentityTo: otherArray [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.

	 The identityHashes remain with the pointers in us rather than with the objects so that
	 the objects in the receiver should still be properly indexed in any existing hashed
	 structures after the mutation.

	Warning: This means that the identityHash of objects in otherArray will change. This
	is most likely a problem if those objects are already in any identityHashed collection!
	See #elementsForwardIdentityTo:copyHash:.
	They should be #rehash 'ed.

	Additionally, any hashed (not identityHashed) collection with elements in the receiver
	will most likely break if the #hash message gives now a different answer!
	They should be #rehash 'ed.

	Warning. This is a dangerous operation and it could lead to a crash if some object in
	receiver or argument is receiver of a method currently in execution.
	See #anyReceiverInStackIn: See senders."

	<primitive: 72 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[self error: 'The virtual machine is out-of-date.  Please upgrade.'].
	self primitiveFailed
]

{ #category : 'converting' }
Array >> elementsForwardIdentityTo: otherArray copyHash: copyHash [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.

	 If copyHash is true, the identityHashes remain with the pointers in rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.	
	Warning: This means that the identityHash of objects in otherArray will change. This
	is most likely a problem if those objects are already in any identityHashed collection!

	If copyHash is false, then the hashes of the objects in otherArray remain unchanged.
	If you know what you're doing this may indeed be what you want.
	Warning: This means that any identityHashed collection with elements in otherArray
	will not be broken, but any identityHashed collection with elements in self will be!
	They should be #rehash 'ed.

	Additionally, any hashed (not identityHashed) collection with elements in the receiver
	will most likely break if the #hash message gives now a different answer!
	They should be #rehash 'ed.

	Warning. This is a dangerous operation and it could lead to a crash if some object in
	receiver or argument is receiver of a method currently in execution.
	See #anyReceiverInStackIn: See senders."

	<primitive: 249 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	self primitiveFailed
]

{ #category : 'converting' }
Array >> evalStrings [
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"
   
  
    ^ self collect: [:each |  
	each isSymbol 
		ifTrue: [each == #true 
			ifTrue: [true]
			ifFalse: [each == #false 
				ifTrue: [false]
				ifFalse:[each == #nil 
					ifTrue: [nil]
					ifFalse: [each]]]] 
		ifFalse: [each isString 
				ifTrue: [Compiler evaluate: each]
				ifFalse: [each isArray 
					ifTrue: [each evalStrings]
					ifFalse:[each]]]
        ]
]

{ #category : 'private' }
Array >> hasLiteralSuchThat: testBlock [
	"Answer true if testBlock returns true for any literal in this array, even if imbedded in 	further Arrays or CompiledMethods.  This method is only intended for private use by 	CompiledMethod 	hasLiteralSuchThat:"
	| lit |
	1 to: self size do: [:index |
		(testBlock value: (lit := self at: index)) ifTrue: [^ true].
		(lit isArray and: [lit hasLiteralSuchThat: testBlock]) ifTrue: [^ true]].
	^ false
]

{ #category : 'testing' }
Array >> identityIncludes: anObject  [
	"Answer whether anObject is one of the receiver's elements."

	^ self statePointsTo: anObject
]

{ #category : 'testing' }
Array >> is: aSymbol [
	^ aSymbol == #Array or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Array >> isArray [
	^true
]

{ #category : 'printing' }
Array >> isLiteral [
	"Definition from Squeak"
	^ self class == Array and: [
		self allSatisfy: [ :each |
			each isLiteral ]].
]

{ #category : 'comparing' }
Array >> literalEqual: other [

	self class == other class ifFalse: [^ false].
	self size = other size ifFalse: [^ false].
	self with: other do: [:e1 :e2 |
		(e1 literalEqual: e2) ifFalse: [^ false]].
	^ true
]

{ #category : 'printing' }
Array >> printAsBraceFormOn: aStream [
	aStream nextPut: ${.
	self do: [:el | el printOn: aStream] separatedBy: [ aStream nextPutAll: ' . '].
	aStream nextPut: $}
]

{ #category : 'printing' }
Array >> printAsLiteralOn: aStream [
	aStream nextPut: $#; nextPut: $(.
	self do: [:each| each printAsLiteralOn: aStream] separatedBy: [aStream space].
	aStream nextPut: $)

]

{ #category : 'printing' }
Array >> printOn: aStream [
	self class == Array ifFalse:
		[^super printOn: aStream].
	self == Smalltalk specialObjectsArray ifTrue:
		[^aStream nextPutAll: 'Smalltalk specialObjectsArray' ].
	self shouldBePrintedAsLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [self printAsBraceFormOn: aStream]
]

{ #category : 'system primitives' }
Array >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'enumerating' }
Array >> select: selectBlock thenCollect: collectBlock [
	"Evaluate selectBlock with each my elements as the argument. For those who evaluate to true,
	collect the result of evaluating collectBlock on them into a collection that is like me.
	Answer the new collection.
	Overriden for performance."

	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			| each |
			each := self at: index.
			(selectBlock value: each) ifTrue: [
				strm nextPut: (collectBlock value: each) ]]]
]

{ #category : 'testing' }
Array >> shouldBePrintedAsLiteral [
	"Detect circular references."
	^self class == Array
	  and: [self shouldBePrintedAsLiteralVisiting: (IdentitySet newWithRoomForMoreThan: 8)]
]

{ #category : 'testing' }
Array >> shouldBePrintedAsLiteralVisiting: aSet [
	self class == Array ifFalse:
		[^false].
	(aSet includes: self) ifTrue:
		[^false].
	aSet add: self.
	^self allSatisfy: [:each | each shouldBePrintedAsLiteralVisiting: aSet]
]

{ #category : 'printing' }
Array >> storeAsBraceFormOn: aStream [
	"
	{(1/3)} storeString
	"
	aStream nextPut: ${.
	self do: [:el | el storeOn: aStream] separatedBy: [ aStream nextPutAll: ' . '].
	aStream nextPut: $}
]

{ #category : 'printing' }
Array >> storeOn: aStream [
	"Use the literal form if possible."
	self shouldBePrintedAsLiteral
		ifTrue:  [self printAsLiteralOn: aStream]
		ifFalse: [self storeAsBraceFormOn: aStream]
]
