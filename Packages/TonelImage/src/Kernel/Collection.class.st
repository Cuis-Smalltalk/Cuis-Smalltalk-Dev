"
I am the abstract superclass of all classes that represent a group of elements.
"
Class {
	#name : 'Collection',
	#superclass : 'Object',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'error descriptions' }
Collection class >> collectionTooSmallDescription [

	^ 'this collection is too small'
]

{ #category : 'error descriptions' }
Collection class >> emptyCollectionDescription [

	^ 'this collection is empty'
]

{ #category : 'instance creation' }
Collection class >> new: size [
	"Redefined in subclasses as appropriate."

	self error: 'Instances of ', self name, ' can not be created with the #new: message. Better call #new instead.'
]

{ #category : 'instance creation' }
Collection class >> newFrom: aCollection [
	"Create a new collection containing all the elements from aCollection"

	^(self new: aCollection size)
		addAll: aCollection;
		yourself
]

{ #category : 'error descriptions' }
Collection class >> notKeyedDescription [

	^ 'Instances of {1} do not respond to keyed accessing messages.' format: { self className }
]

{ #category : 'error descriptions' }
Collection class >> objectNotFoundDescription [

	^ 'Object is not in the collection'
]

{ #category : 'error descriptions' }
Collection class >> sizesDoNotMatchDescription [

	^ 'collection sizes do not match'
]

{ #category : 'instance creation' }
Collection class >> with: anObject  [
	"Answer an instance of me containing anObject."

	^ self new
		add: anObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> with: firstObject with: secondObject  [
	"Answer an instance of me containing the two arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject  [
	"Answer an instance of me containing the three arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject  [
	"Answer an instance of me, containing the four arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject [
	"Answer an instance of me, containing the five arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject [
	"Answer an instance of me, containing the six arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		add: sixthObject;
		yourself
]

{ #category : 'instance creation' }
Collection class >> withAll: aCollection [
	"Create a new collection containing all the elements from aCollection.
	Warning: In ArrayedCollection this method is redefined, and only accepts SequenceableCollections as the argument"

	^self newFrom: aCollection
]

{ #category : 'arithmetic' }
Collection >> * arg [

	^ arg adaptToCollection: self andSend: #*
]

{ #category : 'arithmetic' }
Collection >> + arg [

	^ arg adaptToCollection: self andSend: #+
]

{ #category : 'copying' }
Collection >> , aCollection [
	^self copy addAll: aCollection; yourself
]

{ #category : 'arithmetic' }
Collection >> - arg [

	^ arg adaptToCollection: self andSend: #-
]

{ #category : 'arithmetic' }
Collection >> / arg [

	^ arg adaptToCollection: self andSend: #/
]

{ #category : 'arithmetic' }
Collection >> // arg [

	^ arg adaptToCollection: self andSend: #//
]

{ #category : 'comparing' }
Collection >> = other [
	"Default implementation. Usually redefined in subclasses."

	^self == other
]

{ #category : 'arithmetic' }
Collection >> \\ arg [

	^ arg adaptToCollection: self andSend: #\\
]

{ #category : 'math functions' }
Collection >> abs [
	"Absolute value of all elements in the collection"
	^ self collect: [:a | a abs]
]

{ #category : 'adapting' }
Collection >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with another Collection, return a Collection of
	the results of each element combined with the scalar in that expression."

	rcvr isSequenceable & self isSequenceable ifFalse:
		[self error: 'Only sequenceable collections may be combined arithmetically'].
	^ rcvr with: self collect:
		[:rcvrElement :myElement | rcvrElement perform: selector with: myElement]
]

{ #category : 'adapting' }
Collection >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
]

{ #category : 'adapting' }
Collection >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
]

{ #category : 'adding' }
Collection >> add: newObject  [
	"Include newObject as one of the receiver's elements.  Answer newObject.
	This message should not be sent to instances of subclasses of ArrayedCollection."

	self subclassResponsibility
]

{ #category : 'adding' }
Collection >> add: newObject withOccurrences: anInteger [
	"Add newObject anInteger times to the receiver. Answer newObject."

	anInteger timesRepeat: [self add: newObject].
	^ newObject
]

{ #category : 'adding' }
Collection >> addAll: aCollection  [
	"Include all the elements of aCollection as the receiver's elements.  Answer
	aCollection."

	aCollection do: [:each | self add: each].
	^aCollection
]

{ #category : 'enumerating' }
Collection >> allSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	self do: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true
]

{ #category : 'accessing' }
Collection >> anyOne [
	"Answer any element in the receiver."

	self do: [:each | ^ each].
	self errorEmptyCollection
]

{ #category : 'enumerating' }
Collection >> anySatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false"

	self do:[:item | (aBlock value: item) ifTrue: [^ true]].
	^ false
]

{ #category : 'math functions' }
Collection >> arcCos [
	^self collect: [:each | each arcCos]
]

{ #category : 'math functions' }
Collection >> arcSin [
	^self collect: [:each | each arcSin]
]

{ #category : 'math functions' }
Collection >> arcTan [
	^self collect: [:each | each arcTan]
]

{ #category : 'converting' }
Collection >> as: aPrintingBlock on: aStream delimiter: delimiter last: lastDelimiter [

	| position selfSize |

	position := 1.
	selfSize := self size.

	self
		do: [:elem |
			position := position + 1.
			aPrintingBlock value: elem ]
		separatedBy: [
			aStream nextPutAll: (position = selfSize ifTrue: [lastDelimiter] ifFalse: [delimiter])]
]

{ #category : 'converting' }
Collection >> asArray [
	"Answer an Array whose elements are the elements of the receiver"

	^self asNewArray
]

{ #category : 'converting' }
Collection >> asBag [
	"Answer a Bag whose elements are the elements of the receiver."

	^ self as: Bag
]

{ #category : 'converting' }
Collection >> asByteArray [

	^self as: ByteArray
]

{ #category : 'converting' }
Collection >> asCommaSeparated: aPrintingBlock on: aStream [

	^self as: aPrintingBlock on: aStream delimiter: ', ' last: ' and '
]

{ #category : 'converting' }
Collection >> asCommaStringAnd [

	^String streamContents: [:stream | self asStringOn: stream delimiter: ', ' last: ' and ']
]

{ #category : 'converting' }
Collection >> asDictionary [
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 		-> 	10.
			#itemOrder 		-> 	30.
			#label 			-> 	'Browser'.
			#object 			-> 	Browser.
			#selector 		-> 	#open.
			#icon 			-> 	#editFindReplaceIcon.
			#balloonText 	-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary
]

{ #category : 'converting' }
Collection >> asFloat32Array [
	"Answer a FloatArray whose elements are the elements of the receiver"

	^self as: Float32Array
]

{ #category : 'converting' }
Collection >> asFloat64Array [
	"Answer a Float64Array whose elements are the elements of the receiver"

	^self as: Float64Array
]

{ #category : 'converting' }
Collection >> asIdentitySet [
	"Answer an IdentitySet whose elements are the elements of the receiver"

	^self as: IdentitySet
]

{ #category : 'converting' }
Collection >> asIntegerArray [
	"Answer an IntegerArray whose elements are the elements of the receiver"

	^self as: IntegerArray
]

{ #category : 'converting' }
Collection >> asNewArray [
	"Answer a new Array whose elements are the elements of the receiver"

	^self as: Array
]

{ #category : 'converting' }
Collection >> asOrderedCollection [
	"Answer an OrderedCollection whose elements are the elements of the
	receiver. The order in which elements are added depends on the order
	in which the receiver enumerates its elements. In the case of unordered
	collections, the ordering is not necessarily the same for multiple 
	requests for the conversion."

	^ self as: OrderedCollection
]

{ #category : 'converting' }
Collection >> asPointArray [
	"Answer a PointArray whose elements are the elements of the receiver"

	^self as: PointArray
]

{ #category : 'converting' }
Collection >> asSet [
	"Answer a Set whose elements are the unique elements of the receiver."

	^ self as: Set
]

{ #category : 'converting' }
Collection >> asSortedCollection [
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SortedCollection
]

{ #category : 'converting' }
Collection >> asSortedCollection: aSortBlock  [
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection := SortedCollection sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection
]

{ #category : 'converting' }
Collection >> asStringOn: aStream delimiter: delimString last: lastDelimString [

	self as: [ :elem | aStream nextPutAll: elem asString ] on: aStream delimiter: delimString last: lastDelimString
]

{ #category : 'converting' }
Collection >> asWordArray [
	"Answer a WordArray whose elements are the elements of the receiver"

	^self as: WordArray
]

{ #category : 'enumerating' }
Collection >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it."

	self do: aBlock
]

{ #category : 'enumerating' }
Collection >> associationsDo: elementBlock separatedBy: separatorBlock [
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst := true.
	self associationsDo: [ :element |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]
]

{ #category : 'accessing' }
Collection >> atRandom [
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ Random withDefaultDo: [ :random |
		self atRandom: random ]

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"
]

{ #category : 'accessing' }
Collection >> atRandom: aGenerator  [
	"Answer a random element of the receiver.  Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator.  Causes an error if self has no  
	elements.
	WARNING: This implementation cannot guarantee reproducibility.
	Calling twice with generators setup with the same seed can produce
	different results (for example if the collection is rehashed between calls).
	If you need reproducibility, you should convert the collection to a sequenceable
	collection first."
	| n |
	self emptyCheck.
	n := self size atRandom: aGenerator.
	self do: [:each| (n := n - 1) = 0 ifTrue: [^ each]].
	self error: 'collection invariants broken'
]

{ #category : 'statistics' }
Collection >> average [
	^self mean
]

{ #category : 'statistics' }
Collection >> average: aBlock [

	^ self average: aBlock ifEmpty: [ self errorEmptyCollection ]
]

{ #category : 'statistics' }
Collection >> average: aBlock ifEmpty: emptyBlock  [
	
	^ (self sum: aBlock ifEmpty: [ ^emptyBlock value ]) / self size
]

{ #category : 'testing' }
Collection >> canHoldAnyCodePoint [
	"Why not?"
	^true
]

{ #category : 'math functions' }
Collection >> ceiling [
	^ self collect: [:a | a ceiling]
]

{ #category : 'enumerating' }
Collection >> collect: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument.  Collect the 
	resulting values into a collection that is like the receiver.  Answer the new 
	collection. "

	| newCollection |
	newCollection := self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection
]

{ #category : 'enumerating' }
Collection >> collect: aBlock andFold: aTwoArgBlock [
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b]
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b]
	"

	^ self
		collect: aBlock
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]
]

{ #category : 'enumerating' }
Collection >> collect: aBlock andFold: aTwoArgBlock ifEmpty: emptyBlockOrValue [
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b] ifEmpty: nil
	#() collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	"

	| first answer |
	first := true.
	self do: [ :each |
		| eachValue |
		eachValue :=  aBlock value: each.
		first
			ifTrue: [
				first := false.
				answer := eachValue ]
			ifFalse: [
				answer := aTwoArgBlock
					value: answer
					value: eachValue ]].
	first ifTrue: [ answer := emptyBlockOrValue valueWithPossibleArgument: self ].
	^ answer
]

{ #category : 'enumerating' }
Collection >> collect: collectBlock thenSelect: selectBlock [
	^ (self collect: collectBlock) select: selectBlock
]

{ #category : 'filter streaming' }
Collection >> contents [
	^ self
]

{ #category : 'copying' }
Collection >> copyWith: newElement [
	"Answer a new collection with newElement added (as last
	element if sequenceable)."

	^ self copy
		add: newElement;
		yourself
]

{ #category : 'copying' }
Collection >> copyWithout: oldElement  [
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self reject: [:each | each = oldElement]

"Examples:
	'fred the bear' copyWithout: $e
	#(2 3 4 5 5 6) copyWithout: 5
"
]

{ #category : 'copying' }
Collection >> copyWithoutAll: aCollection [
	"Answer a copy of the receiver that does not contain any elements 
	equal to those in aCollection."

	^ self reject: [:each | aCollection includes: each]
]

{ #category : 'math functions' }
Collection >> cos [
	^self collect: [:each | each cos]
]

{ #category : 'enumerating' }
Collection >> count: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count := 0.
	self do: [:each| (aBlock value: each) ifTrue: [count := count + 1]].
	^ count
]

{ #category : 'math functions' }
Collection >> degreeCos [
	^self collect: [:each | each degreeCos]
]

{ #category : 'math functions' }
Collection >> degreeSin [
	^self collect: [:each | each degreeSin]
]

{ #category : 'enumerating' }
Collection >> detect: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true."

	^ self detect: aBlock ifNone: [self errorNotFound: aBlock]
]

{ #category : 'enumerating' }
Collection >> detect: aBlock ifFound: foundBlock ifNone: exceptionBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the evaluation of foundBlock at the first element for which aBlock evaluates to true."
	^ foundBlock value: (self detect: aBlock ifNone: [^ exceptionBlock value])
]

{ #category : 'enumerating' }
Collection >> detect: aBlock ifNone: exceptionBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value
]

{ #category : 'enumerating' }
Collection >> detectMax: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	| maxElement maxValue |
	self do: [ :each | 
		maxValue
			ifNil: ["first element"
				maxElement := each.
				maxValue := aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				| val |
				(val := aBlock value: each) > maxValue ifTrue: [
					maxElement := each.
					maxValue := val]]].
	^ maxElement
]

{ #category : 'enumerating' }
Collection >> detectMin: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the lowest number.
	If collection empty, return nil."

	| minElement minValue |
	self do: [:each | 
		minValue
			ifNil: ["first element"
				minElement := each.
				minValue := aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				| val |
				(val := aBlock value: each) < minValue ifTrue: [
					minElement := each.
					minValue := val]]].
	^ minElement
]

{ #category : 'enumerating' }
Collection >> difference: aCollection [
	"Answer the set theoretic difference of two collections."

	^ self reject: [:each | aCollection includes: each]
]

{ #category : 'enumerating' }
Collection >> do: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility
]

{ #category : 'enumerating' }
Collection >> do: elementBlock separatedBy: separatorBlock [
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst := true.
	self do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]
]

{ #category : 'enumerating' }
Collection >> do: aBlock without: anItem [
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	^self do:[:el| anItem = el ifFalse:[aBlock value: el]].
]

{ #category : 'private' }
Collection >> emptyCheck [
	self isEmpty ifTrue: [self errorEmptyCollection]
]

{ #category : 'comparing' }
Collection >> emptyCollectionHash [
	^self species hash
]

{ #category : 'private' }
Collection >> errorCollectionTooSmall [

	self error: self class collectionTooSmallDescription
]

{ #category : 'private' }
Collection >> errorEmptyCollection [

	self error: self class emptyCollectionDescription
]

{ #category : 'private' }
Collection >> errorNoMatch [

	self error: self class sizesDoNotMatchDescription
]

{ #category : 'private' }
Collection >> errorNotFound: anObject [
	"Actually, this should raise a special Exception not just an error."

	self error: self class objectNotFoundDescription
]

{ #category : 'private' }
Collection >> errorNotKeyed [

	self error: self class notKeyedDescription

]

{ #category : 'math functions' }
Collection >> exp [
	^self collect: [:each | each exp]
]

{ #category : 'copying' }
Collection >> flatten [

	^ self species streamContents: [ :flattenedStream | self flattenTo: flattenedStream ]
]

{ #category : 'copying' }
Collection >> flattenTo: flattenedStream [

	self do: [ :each | 
		each isCollection 
			ifTrue: [ each flattenTo: flattenedStream ] 
			ifFalse: [ flattenedStream nextPut: each ]].
		
	^ flattenedStream 

]

{ #category : 'math functions' }
Collection >> floor [
	^ self collect: [:a | a floor]
]

{ #category : 'enumerating' }
Collection >> fold: aTwoArgBlock [
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b]
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]
]

{ #category : 'enumerating' }
Collection >> fold: aTwoArgBlock ifEmpty: emptyBlockOrValue [
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: 7
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: emptyBlockOrValue
]

{ #category : 'enumerating' }
Collection >> groupBy: keyBlock  [

	^ self 
		inject: Dictionary new
		into: [ :groupedCollection :elementToGroup | | group |
			group := groupedCollection at: (keyBlock value: elementToGroup) ifAbsentPut: [ OrderedCollection new ].
			group add: elementToGroup.
			groupedCollection ]
		
]

{ #category : 'enumerating' }
Collection >> groupBy: keyBlock having: selectBlock  [
	"Like in SQL operation - Split the receivers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock. "
	
	^ (self groupBy: keyBlock) select: selectBlock
]

{ #category : 'inspecting' }
Collection >> hasContentsInExplorer [

	^ true
]

{ #category : 'comparing' }
Collection >> hash [
	"A default hash function for any collection.  Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash := self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self do: [ :elem | hash := hash bitXor: elem hash]].
	^ hash bitXor: self size hash

]

{ #category : 'testing' }
Collection >> identityIncludes: anObject  [
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:any| any == anObject]
]

{ #category : 'testing' }
Collection >> ifEmpty: aBlock [
	"Evaluate the block if I'm empty
	'' ifEmpty: [ :c | c, 'coda' ] 					'coda'
	'' ifEmpty: [ 'replacement' ]					 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] 				'stuff'
	'stuff' ifEmpty: [ 'replacement' ]				'stuff'
	|c|c := OrderedCollection new. c ifEmpty: [c add: 'stuff']. c			 an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff']. c	 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff']. c		 an OrderedCollection('a')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff']. c  an OrderedCollection('a')
	"

	 self isEmpty ifTrue: [ ^aBlock valueWithPossibleArgument: self ]
]

{ #category : 'testing' }
Collection >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	'' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ] 								 'coda'
	'' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]								 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ]							 'stuffcoda2'
	'stuff' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]						 'replacement2'
	|c|c := OrderedCollection new. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c						  an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c	 		 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c		 			 an OrderedCollection('a' 'stuff2')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c  		 an OrderedCollection('a' 'stuff2')
	"
	^ self isEmpty
		ifTrue: [ emptyBlock valueWithPossibleArgument: self ]
		ifFalse: [ notEmptyBlock valueWithPossibleArgument: self ]
]

{ #category : 'testing' }
Collection >> ifNotEmpty: aBlock [
	"Evaluate the block unless I'm empty
	'' ifNotEmpty: [ :c | c, 'coda' ] 						 ''
	'' ifNotEmpty: [ 'replacement' ]					 	 ''
	'stuff' ifNotEmpty: [ :c | c, 'coda' ] 					 'stuffcoda'
	'stuff' ifNotEmpty: [ 'replacement' ]				 	 'replacement'
	|c|c := OrderedCollection new. c ifNotEmpty: [c add: 'stuff']. c			  an OrderedCollection()
	|c|c := OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff']. c	  an OrderedCollection()
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [c add: 'stuff']. c		  an OrderedCollection('a' 'stuff')
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff']. c   an OrderedCollection('a' 'stuff')
	"

	self isEmpty ifFalse: [^aBlock valueWithPossibleArgument: self]
]

{ #category : 'testing' }
Collection >> ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	''  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ] 									 'coda'
	'' ifNotEmpty: [ 'replacement2' ]ifEmpty: [ 'replacement' 	]								 'replacement'
	'stuff'  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ]								 'stuffcoda2'
	'stuff' ifNotEmpty: [ 'replacement2' ] ifEmpty: [ 'replacement' ]							 'replacement2'
	|c|c := OrderedCollection new. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c			 		 an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c	 	 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c		 		 an OrderedCollection('a' 'stuff2')
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c 	an OrderedCollection('a' 'stuff2')
	"

	^self ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
]

{ #category : 'testing' }
Collection >> includes: anObject  [
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:each | each = anObject]
]

{ #category : 'testing' }
Collection >> includesAllOf: aCollection  [
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true
]

{ #category : 'testing' }
Collection >> includesAnyOf: aCollection  [
	"Answer whether any element of aCollection is one of the receiver's elements."
	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false
]

{ #category : 'testing' }
Collection >> includesSubstringAnywhere: testString [
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
	self do: [ :element |
		element isString
			ifTrue: [
				(element includesSubString: testString) ifTrue: [^ true]].
		element isCollection
			ifTrue: [
				(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false

"#(first (second third) ((allSentMessages ('Elvis' includes:)))) includesSubstringAnywhere:  'lvi'"
]

{ #category : 'enumerating' }
Collection >> inject: thisValue into: binaryBlock  [
	"Accumulate a running value associated with evaluating the argument,
	binaryBlock, with the current value and the receiver as block arguments. 
	The initial value is the value of the argument, thisValue.
		For instance, to sum a collection, use: 
			collection inject: 0 into: [:subTotal :next | subTotal + next]."

	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue
]

{ #category : 'enumerating' }
Collection >> intersection: aCollection [
	"Answer the set theoretic intersection of two collections."

	^ self select: [:each | aCollection includes: each]
]

{ #category : 'testing' }
Collection >> is: aSymbol [
	"Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Collection = aSymbol or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Collection >> isCollection [
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^true
]

{ #category : 'testing' }
Collection >> isEmpty [
	"Answer whether the receiver contains any elements."

	self do: [ :any | ^ false ].
	^true
]

{ #category : 'testing' }
Collection >> isEmptyOrNil [
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

	^ self isEmpty
]

{ #category : 'testing' }
Collection >> isSequenceable [
	^ false
]

{ #category : 'math functions' }
Collection >> ln [
	^self collect: [:each | each ln]
]

{ #category : 'math functions' }
Collection >> log [
	^ self collect: [:each | each log]
]

{ #category : 'math functions' }
Collection >> log2 [
	^ self collect: [:each | each log2]
]

{ #category : 'statistics' }
Collection >> max [
	^ self max: [:each| each]
]

{ #category : 'statistics' }
Collection >> max: aBlock [
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer := answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer
]

{ #category : 'statistics' }
Collection >> mean [
	^ self sum / self size
]

{ #category : 'statistics' }
Collection >> min [
	^ self min: [:each| each]
]

{ #category : 'statistics' }
Collection >> min: aBlock [
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer := answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer
]

{ #category : 'math functions' }
Collection >> negated [
	"Negated value of all elements in the collection"
	^ self collect: [:a | a negated]
]

{ #category : 'enumerating' }
Collection >> noneSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for all elements return true.
	Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
]

{ #category : 'testing' }
Collection >> notEmpty [
	"Answer whether the receiver contains any elements."

	^ self isEmpty not
]

{ #category : 'testing' }
Collection >> occurrencesOf: anObject  [
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally
]

{ #category : 'printing' }
Collection >> printElementsOn: aStream [
	"Print my elements on aStream."

	aStream nextPut: $(.
	self do: [ :element | aStream print: element ] separatedBy: [ aStream space ].
	aStream nextPut: $)
]

{ #category : 'printing' }
Collection >> printNameOn: aStream [

	aStream
		nextPutAll: self class name withArticle
]

{ #category : 'printing' }
Collection >> printOn: aStream  [
	"Append a sequence of characters that identify the receiver to aStream."

	self printNameOn: aStream.
	self printElementsOn: aStream
]

{ #category : 'statistics' }
Collection >> product [
	"Answer the product of all elements in the receiver."
	^ self product: [:each| each]
]

{ #category : 'statistics' }
Collection >> product: aBlock [
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 1 is a good initial value for the product."
	^self collect: aBlock andFold: [ :a :b | a * b ]
]

{ #category : 'arithmetic' }
Collection >> raisedTo: arg [

	^ arg adaptToCollection: self andSend: #raisedTo:
]

{ #category : 'statistics' }
Collection >> range [
	^ self max - self min
]

{ #category : 'math functions' }
Collection >> reciprocal [
	"Return the receiver full of reciprocated elements"
	^ self collect: [:a | a reciprocal]
]

{ #category : 'enumerating' }
Collection >> reduce: aTwoArgBlock [
	"Apply the argument, binaryBlock cumulatively to the elements of the receiver.
	For sequenceable collections the elements will be used in order, for unordered
	collections the order is unspecified."

	^self fold: aTwoArgBlock
]

{ #category : 'enumerating' }
Collection >> reject: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to false.  Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]
]

{ #category : 'enumerating' }
Collection >> reject: rejectBlock thenCollect: collectBlock [

	^self select: [ :each | (rejectBlock value: each) not ] thenCollect: collectBlock
]

{ #category : 'enumerating' }
Collection >> reject: rejectBlock thenDo: doBlock [

	self do: [ :each | (rejectBlock value: each) ifFalse: [ doBlock value: each ]]
]

{ #category : 'removing' }
Collection >> remove: oldObject  [
	"Remove oldObject from the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, raise an error.
	ArrayedCollections cannot respond to this message."

	^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]
]

{ #category : 'removing' }
Collection >> remove: oldObject ifAbsent: anExceptionBlock  [
	"Remove oldObject as one of the receiver's elements.  If several of the
	elements are equal to oldObject, only one is removed. If no element is equal to
	oldObject, answer the result of evaluating anExceptionBlock.  Otherwise,
	answer the argument, oldObject.

	SequenceableCollections can not respond to this message."

	self subclassResponsibility
]

{ #category : 'removing' }
Collection >> removeAll: aCollection  [
	"Remove each element of aCollection from the receiver.  If successful for each,
	answer aCollection."

	aCollection do: [:each | self remove: each].
	^aCollection
]

{ #category : 'removing' }
Collection >> removeAllFoundIn: aCollection  [
	"Remove each element of aCollection which is present in the receiver 
	from the receiver. Answer aCollection. No error is raised if an element
	isn't found. ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each ifAbsent: nil].
	^ aCollection
]

{ #category : 'removing' }
Collection >> removeAllSuchThat: aBlock  [
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  Use a copy to enumerate 
	collections whose order changes when an element is removed (i.e. Sets)."

	self copy do: [:each | (aBlock value: each) ifTrue: [self remove: each]]
]

{ #category : 'math functions' }
Collection >> roundTo: quantum [
	^self collect: [ :each | each roundTo: quantum ]
]

{ #category : 'math functions' }
Collection >> rounded [
	^ self collect: [:a | a rounded]
]

{ #category : 'enumerating' }
Collection >> select: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to true.  Answer the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection
]

{ #category : 'enumerating' }
Collection >> select: selectBlock thenCollect: collectBlock [
	^ (self select: selectBlock) collect: collectBlock
]

{ #category : 'enumerating' }
Collection >> select: selectBlock thenDo: doBlock [
	"Equivalent to 
		(self select: selectBlock) do: doBlock
	but avoid creating an extra collection."

	self do: [ :each | (selectBlock value: each) ifTrue: [ doBlock value: each ]]
]

{ #category : 'math functions' }
Collection >> sign [
	^self collect: [:each | each sign]
]

{ #category : 'math functions' }
Collection >> sin [
	^self collect: [:each | each sin]
]

{ #category : 'accessing' }
Collection >> size [
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^tally
]

{ #category : 'sorting' }
Collection >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted "

	^self sorted: nil
]

{ #category : 'sorting' }
Collection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison."

	^self asNewArray sort: aSortBlockOrNil
]

{ #category : 'private' }
Collection >> species [
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."
	"Redefined here just for reference. See inheritance.
	#collect: avoids using #species in String, when there are non-Character objects
	#select: and #copy avoid using it in SortedCollection"

	^ self class
]

{ #category : 'math functions' }
Collection >> sqrt [
	^ self collect: [:each | each sqrt]
]

{ #category : 'math functions' }
Collection >> squared [
	^ self collect: [:each | each * each]
]

{ #category : 'printing' }
Collection >> storeOn: aStream  [
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : 'statistics' }
Collection >> sum [
	"Answer the sum of all elements in the receiver."
	^ self sum: [:each| each]
]

{ #category : 'statistics' }
Collection >> sum: aBlock [
	^ self sum: aBlock ifEmpty: [self errorEmptyCollection]
]

{ #category : 'statistics' }
Collection >> sum: aBlock ifEmpty: emptyBlockOrValue [
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ] ifEmpty: emptyBlockOrValue
]

{ #category : 'math functions' }
Collection >> tan [
	^self collect: [:each | each tan]
]

{ #category : 'math functions' }
Collection >> truncateTo: quantum [
	^self collect: [ :each | each truncateTo: quantum ]
]

{ #category : 'math functions' }
Collection >> truncated [
	^ self collect: [:a | a truncated]
]

{ #category : 'enumerating' }
Collection >> union: aCollection [
	"Answer the set theoretic union of two collections."

	^ self asSet addAll: aCollection; yourself
]
