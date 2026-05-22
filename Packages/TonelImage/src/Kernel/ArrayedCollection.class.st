"
I am an abstract superclass for collections of elements with the same storage size. My subclasses typically allocate storage as fixed size blocks. 

Element access is by an integer index, from 1 to collection size. Usual accessors are #at: and #at:put:

They are declared as having indexable instances. See:
	#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:

This means that they don't have instance variables. They are what is usually called an Array: a contiguous area of memory, that stores ObjectPointers (i.e. references to any objects), Bytes or 16, 32 or 64 bit Words. Each class is free to use that storarge to encode other kinds of data. See, for example, Float32Array Float64Array or DoubleByteArray. Therefore the size in bytes of each element is not constrained, although it is fixed for each class.

Instance size is fixed at creation. They can not grow or shrink.

For these reasons, there are many classes in the system that have Array like acessors (#at: and #at:put: with integer indexes) but are not in the ArrayedCollection hierarchy. These include OrderedCollection, Interval, Text, etc.
"
Class {
	#name : 'ArrayedCollection',
	#superclass : 'SequenceableCollection',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'aux' }
ArrayedCollection class >> bytesPerBasicElement [
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self class bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	self isBytes ifTrue: [ ^1 ].
	self isDoubleBytes ifTrue: [ ^2 ].
	self isWords ifTrue: [ ^4 ].
	self isDoubleWords ifTrue: [ ^8 ].
	^Smalltalk wordSize.

]

{ #category : 'instance creation' }
ArrayedCollection class >> instancesMustBeOfSpecificSize [
	"Some subclasses create instances of a specific size, and answer true"
	^self numElements ~= 0
]

{ #category : 'instance creation' }
ArrayedCollection class >> new [
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements
]

{ #category : 'instance creation' }
ArrayedCollection class >> new: sizeRequested [
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  
]

{ #category : 'instance creation' }
ArrayedCollection class >> new: size from: aSequenceableCollection [
	"Create a new collection containing all the elements from aSequenceableCollection.
	FloatArray new: 4 from: { 1. 2 }
	FloatArray new: 2 from: { 1. 2. 3. 4 }
	"

	^ (self new: size)
		replaceFrom: 1
		to: (aSequenceableCollection size min: size)
		with: aSequenceableCollection
		startingAt: 1
]

{ #category : 'instance creation' }
ArrayedCollection class >> new: size withAll: value  [
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value
]

{ #category : 'instance creation' }
ArrayedCollection class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray := self new: aCollection size.
	index := 0.
	aCollection do: [ :each | newArray at: (index := index + 1) put: each].
	^ newArray

"
	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
"
]

{ #category : 'instance creation' }
ArrayedCollection class >> newFromStream: s [
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	self isPointers | self isWords not ifTrue: [^ super newFromStream: s].
		"super may cause an error, but will not be called."

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skipBack.
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)
]

{ #category : 'instance creation' }
ArrayedCollection class >> numElements [
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: anObject  [
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject  [
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject  [
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject  [
	"Answer a new instance of me, containing the four arguments as the elements."

	| newCollection |
	newCollection := self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject [
	"Answer a new instance of me, containing only the five arguments as
	elements."

	| newCollection |
	newCollection := self new: 5.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	^newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject [
	"Answer a new instance of me, containing only the 6 arguments as elements."

	| newCollection |
	newCollection := self new: 6.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	newCollection at: 6 put: sixthObject.
	^ newCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> with: firstElement withAll: aSequenceableCollection [
	"Create a new collection containing firstElement, then all the elements from aSequenceableCollection.
	Array with: 1 withAll: (10 to: 20)
	"

	| s |
	s := aSequenceableCollection size+1.
	^ (self new: s)
		at: 1 put: firstElement;
		replaceFrom: 2 to: s with: aSequenceableCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> withAll: aSequenceableCollection [
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection
]

{ #category : 'instance creation' }
ArrayedCollection class >> withAll: aSequenceableCollection with: finalElement [
	"Create a new collection containing all the elements from aSequenceableCollection."

	| answer |
	answer := (self new: aSequenceableCollection size + 1)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection.
	answer at: answer size put: finalElement.
	^answer
]

{ #category : 'stream creation' }
ArrayedCollection class >> writeStream [
	^ WriteStream on: (self new: 100)
]

{ #category : 'adding' }
ArrayedCollection >> add: newObject [
	self shouldNotImplement
]

{ #category : 'objects from disk' }
ArrayedCollection >> byteSize [
	^self basicSize * self class bytesPerBasicElement

]

{ #category : 'private' }
ArrayedCollection >> defaultElement [
	^nil
]

{ #category : 'math functions' }
ArrayedCollection >> derivative [
	| displaced answer |
	displaced := self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first - self first.	"Some reasonable zero"
	answer := self copy.
	answer -= displaced.
	^answer
]

{ #category : 'inspecting' }
ArrayedCollection >> explorerContents [
	"See #indexHead and #indexTail"

	| answer indexHead indexTail total shown elementIndex |
	indexHead := 500.
	indexTail := 30.
	shown := indexHead + indexTail.
	total := self size.

	total <= shown ifTrue: [
		^super explorerContents ].

	answer := Array new: shown.
	1 to: indexHead do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: index)
				name: index printString
				model: self) ].

	elementIndex := total-indexTail+1.
	indexHead+1 to: shown do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: elementIndex)
				name: elementIndex printString
				model: self).
		elementIndex := elementIndex + 1 ].

	^answer
]

{ #category : 'math functions' }
ArrayedCollection >> incrementFraction [
	"
	#(10 12.5 15 20) incrementFraction
	"
	| displaced answer |
	displaced := self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first.
	answer := self copy.
	answer -= displaced.
	^answer / displaced
]

{ #category : 'inserting' }
ArrayedCollection >> insert: anObject shiftingRightAt: anInsertionIndex [

	"Inserts anObject at anInsertionIndex, moving right object between anInsertionIndex and self size, 
	loosing last object. Example: 
	#(0 1 3 4 5) insert: 2 shiftingRightAt: 3
	returns:  #(0 1 2 3 4) 
	" 
	| currentIndex |
	
	self assertIsInBounds: anInsertionIndex.
	currentIndex := self size .
	
	[currentIndex > anInsertionIndex] whileTrue: [
		self at: currentIndex put: (self at: currentIndex-1).
		currentIndex := currentIndex - 1].
	
	self at: anInsertionIndex put: anObject

]

{ #category : 'math functions' }
ArrayedCollection >> integral [
	| answer |
	answer := self copy.
	2 to: answer size do: [ :i |
		answer at: i put: (answer at: i) + (answer at: i-1) ].
	^answer
]

{ #category : 'testing' }
ArrayedCollection >> isEmpty [
	^self size = 0
]

{ #category : 'sorting' }
ArrayedCollection >> isSorted [
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm := elm].
	^ true
]

{ #category : 'sorting' }
ArrayedCollection >> isSortedBy: aBlock [
	"Return true if the receiver is sorted by the given criterion."
	| lastEl el |
	self isEmpty ifTrue:[^true].
	lastEl := self first.
	2 to: self size do: [:i |
		el := self at: i.
		(aBlock value: lastEl value: el) ifFalse:[^false].
		lastEl := el].
	^ true

]

{ #category : 'sorting' }
ArrayedCollection >> mergeFirst: first middle: middle last: last into: dst by: aBlock [
	"Private. Merge the sorted ranges [first..middle] and [middle+1..last] 
	of the receiver into the range [first..last] of dst."

	| i1 i2 val1 val2 out |
	i1 := first.
	i2 := middle + 1.
	val1 := self at: i1.
	val2 := self at: i2.
	out := first - 1.  "will be pre-incremented"

	"select 'lower' half of the elements based on comparator"
	[ (i1 <= middle) and: [ i2 <= last ] ] whileTrue: 	[
		(aBlock 
			ifNil: [ val1 <= val2 ]
			ifNotNil: [ aBlock value: val1 value: val2 ])
				ifTrue: [
					dst at: (out := out + 1) put: val1.
					val1 := self at: (i1 := i1 + 1)]
				ifFalse: [
					dst at: (out := out + 1) put: val2.
					(i2 := i2 + 1) <= last ifTrue: [
						val2 := self at: i2 ] ] ].

	"copy the remaining elements"
	i1 <= middle
		ifTrue: [dst replaceFrom: out + 1 to: last with: self startingAt: i1]
		ifFalse: [dst replaceFrom: out + 1 to: last with: self startingAt: i2]
]

{ #category : 'sorting' }
ArrayedCollection >> mergeSortFrom: startIndex to: stopIndex by: aBlock [
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra 
	copy of the data is used as temporary storage and successive merge 
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	self assert: [startIndex >= 1 and: [startIndex < stopIndex]]. "bad start index"
	self assert: [stopIndex <= self size]. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex 
		src: self copy 
		dst: self 
		by: aBlock
]

{ #category : 'sorting' }
ArrayedCollection >> mergeSortFrom: first to: last src: src dst: dst by: aBlock [
	"Private! Split the range to be sorted in half, sort each half, and merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle := (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock.

]

{ #category : 'printing' }
ArrayedCollection >> printNameOn: aStream [

	aStream
		nextPutAll: self class name withArticle;
		nextPut: $:;
		print: self size
]

{ #category : 'sorting' }
ArrayedCollection >> quickSortFrom: from to: to by: sortBlock [
	"Sort elements i through j of self to be nondescending according to sortBlock using an in-place quicksort with simple median-of-three partitioning with guaranteed O(log(n)) space usage."

	| i j |
	i := from.
	j := to.
	[
		| k l dij temp ij di dj n |
		"The prefix d means the data at that index."
		"Sort di,dj."
		di := self at: i.
		dj := self at: j.
		(sortBlock ifNil: [ di <= dj ] ifNotNil: [ sortBlock value: di value: dj ]) ifFalse: [
			temp := self at: i. self at: i put: (self at: j); at: j put: temp.
			temp := di. di := dj. dj := temp ].
		(n := j + 1  - i) <= 2 ifTrue: [ ^self ].
		"More than two elements."
		ij := (i + j) // 2.  "ij is the midpoint of i and j."
		dij := self at: ij.  "Sort di,dij,dj.  Make dij be their median."
		(sortBlock ifNil: [ di <= dij ] ifNotNil: [ sortBlock value: di value: dij ])
			ifTrue: [
				(sortBlock ifNil: [ dij <= dj ] ifNotNil: [ sortBlock value: dij value: dj ]) ifFalse: [
					temp := self at: j. self at: j put: (self at: ij); at: ij put: temp.
					dij := dj ] ]
			ifFalse: [
				temp := self at: i. self at: i put: (self at: ij); at: ij put: temp.
				dij := di ].
		n = 3 ifTrue: [ ^self ].
		"More than three elements."
		"Find k>i and l<j such that dk,dij,dl are in reverse order.
		Swap k and l.  Repeat this procedure until k and l pass each other."
		k := i.
		l := j.
		[
			[ k <= (l := l - 1) and: [ 
				sortBlock ifNil: [ dij <= (self at: l) ] ifNotNil: [ sortBlock value: dij value: (self at: l) ] ] ] whileTrue.  "i.e. while dl succeeds dij"
			[ (k := k + 1) <= l and: [
				sortBlock ifNil: [ (self at: k) <= dij ] ifNotNil: [ sortBlock value: (self at: k) value: dij ] ] ] whileTrue.  "i.e. while dij succeeds dk"
			k <= l ] whileTrue: [ temp := self at: k. self at: k put: (self at: l); at: l put: temp. ].
		"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk through dj. Sort the larger segment in this method and call another quicksort for the smaller segment. This ensures O(log(n)) space usage."
		i < l 
			ifFalse: [
				k < j
					ifFalse: [ ^self ]
					ifTrue: [ i := k ] ]
			ifTrue: [
				k < j
					ifFalse: [ j := l ]
					ifTrue: [
						l - i <  (j - k)
							ifTrue: [ 
								self quickSortFrom: i to: l by: sortBlock.
								i := k ]
							ifFalse: [
								self quickSortFrom: k to: j by: sortBlock.
								j := l ] ] ] ] repeat
]

{ #category : 'objects from disk' }
ArrayedCollection >> restoreEndianness [
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^ self].

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]
]

{ #category : 'accessing' }
ArrayedCollection >> size [
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize
]

{ #category : 'sorting' }
ArrayedCollection >> sort [
	"Sort this array into ascending order using the '<=' operator."

	self sort: nil
]

{ #category : 'sorting' }
ArrayedCollection >> sort: aBlock [
	"Sort this array using the given comparision block. The block should take two arguments and return true if the first element should precede the second in the sorted result."

	self mergeSortFrom: 1 to: self size by: aBlock.

]

{ #category : 'private' }
ArrayedCollection >> storeElementsFrom: firstIndex to: lastIndex on: aStream  [
	| noneYet defaultElement arrayElement |
	noneYet := true.
	defaultElement := self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement := self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet := false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
]

{ #category : 'printing' }
ArrayedCollection >> storeOn: aStream  [
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : 'objects from disk' }
ArrayedCollection >> writeOn: aStream  [
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	self class isPointers | self class isWords not ifTrue: [^ super writeOn: aStream].
				"super may cause an error, but will not be called."
	aStream nextInt32Put: self basicSize bigEndian: true.
	aStream nextUint32WordsPutAll: self.
]
