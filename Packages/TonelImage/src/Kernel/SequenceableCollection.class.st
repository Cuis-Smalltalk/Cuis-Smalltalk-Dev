"
I am an abstract superclass for collections that have a well-defined order associated with their elements. Thus each element is externally-named by integers referred to as indices.
"
Class {
	#name : 'SequenceableCollection',
	#superclass : 'Collection',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg [
	^self streamContents: blockWithArg estimatedSize: 100
]

{ #category : 'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg estimatedSize: estimatedSize [

	^self streamContents: blockWithArg on: (self new: estimatedSize)
]

{ #category : 'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg limitedTo: sizeLimit [
	| stream |
	stream := LimitedWriteStream on: (self new: (100 min: sizeLimit)).
	stream setLimit: sizeLimit limitBlock: [^ stream contents].
	blockWithArg value: stream.
	^ stream contents
"
String streamContents: [:s | 1000 timesRepeat: [s nextPutAll: 'Junk']] limitedTo: 25
 'JunkJunkJunkJunkJunkJunkJ'
"
]

{ #category : 'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg on: aCollection [

	| stream originalContents |
	
	stream := aCollection writeStream.
	blockWithArg value: stream.
	originalContents := stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents asStreamResult ]
		ifFalse: [ stream contents ]
]

{ #category : 'stream creation' }
SequenceableCollection class >> writeStream [
	
	^WriteStream on: self new
]

{ #category : 'math functions' }
SequenceableCollection >> += anObject [
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v + anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) + (anObject at: i)) ]]
]

{ #category : 'copying' }
SequenceableCollection >> , otherCollection  [
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 printStringBase: 16) copyFrom: 4 to: 6) , ' boy!'
"
]

{ #category : 'math functions' }
SequenceableCollection >> -= anObject [
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v - anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) - (anObject at: i)) ]]
]

{ #category : 'comparing' }
SequenceableCollection >> = otherCollection  [
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	"#( 1 2 3) = (1 to: 3) -> false"
	otherCollection isInterval ifTrue: [ ^false ].
	^ self hasEqualElements: otherCollection
]

{ #category : 'converting' }
SequenceableCollection >> @ aCollection  [
	^ self with: aCollection collect: [:a :b | a @ b]
]

{ #category : 'accessing' }
SequenceableCollection >> after: target [
	"Answer the element after target.  Raise an error if target is not
	in the receiver, but answer nil if there are no elements after it."

	^ self after: target ifNone: nil
]

{ #category : 'accessing' }
SequenceableCollection >> after: target ifNone: exceptionBlock [
	"Answer the element after target.  Answer the result of evaluating
	the exceptionBlock if there are no elements after it."

	| index |
	index := self indexOf: target.
	^ index = 0
		ifTrue: [self errorNotFound: target]
		ifFalse: [index = self size 
			ifTrue: [exceptionBlock value]
			ifFalse: [self at: index + 1]]
]

{ #category : 'accessing' }
SequenceableCollection >> allButFirst [
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

	^ self allButFirst: 1
]

{ #category : 'accessing' }
SequenceableCollection >> allButFirst: n [
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
]

{ #category : 'enumerating' }
SequenceableCollection >> allButFirstDo: block [

	2 to: self size do:
		[:index | block value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> allButLast [
	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
]

{ #category : 'accessing' }
SequenceableCollection >> allButLast: n [
	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
]

{ #category : 'enumerating' }
SequenceableCollection >> allButLastDo: block [

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> antepenultimate [
	
	^self antepenultimateIfAbsent: [ self errorCollectionTooSmall ]
]

{ #category : 'accessing' }
SequenceableCollection >> antepenultimateIfAbsent: aBlock [
	
	| size |
	
	size := self size.
	size >= 3 ifTrue: [ ^self at: size - 2 ].
	^aBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> anyOne [
	^ self first
]

{ #category : 'converting' }
SequenceableCollection >> asByteArray [
	"Answer a ByteArray whose elements are the elements of the receiver.
	Optimized implementation."

	^ ByteArray withAll: self
]

{ #category : 'converting' }
SequenceableCollection >> asColorArray [

	^self as: ColorArray
]

{ #category : 'private' }
SequenceableCollection >> asDigitsAt: anInteger in: aCollection do: aBlock [
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	self do: 
		[:each | 
		aCollection at: anInteger put: each.
		anInteger = aCollection size 
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self asDigitsAt: anInteger + 1 in: aCollection do: aBlock]].
]

{ #category : 'enumerating' }
SequenceableCollection >> asDigitsToPower: anInteger do: aBlock [
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number.

	(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	aCollection := Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock
]

{ #category : 'converting' }
SequenceableCollection >> asNewArray [
	"Answer a new Array whose elements are the elements of the receiver.
	Optimized implementation."

	^ Array withAll: self
]

{ #category : 'stream creation' }
SequenceableCollection >> asStreamResult [
	"See senders.
	Also see #thatCanHoldCodePoints"
	^self
]

{ #category : 'assertions' }
SequenceableCollection >> assertIsInBounds: anIndex [

	(self isInBounds: anIndex) ifFalse: [ self errorSubscriptBounds: anIndex ]
	
]

{ #category : 'accessing' }
SequenceableCollection >> at: index ifAbsent: exceptionBlock [
	"Answer the element at my position index. If I do not contain an element
	at index, answer the result of evaluating the argument, exceptionBlock."

	(self isInBounds: index) ifTrue: [^self at: index].
	^exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: indexArray [
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	^ self species streamContents: [ :stream |
		1 to: indexArray size do: [ :index |
			stream nextPut: (self at: (indexArray at: index)) ]]
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: aCollection put: anObject  [
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: indexArray putAll: valueArray [
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray
]

{ #category : 'accessing' }
SequenceableCollection >> atAllPut: anObject  [
	"Put anObject at every one of the receiver's indices."

	| size |
	(size := self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd [
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd ifAbsent: block [
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd put: obj [
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"

	^ self at: self size + 1 - indexFromEnd put: obj
]

{ #category : 'accessing' }
SequenceableCollection >> atPin: index  [
	"Return the index'th element of me if possible.
	Return the first or last element if index is out of bounds."

	index < 1 ifTrue: [^ self first].
	index > self size ifTrue: [^ self last].
	^ self at: index
]

{ #category : 'accessing' }
SequenceableCollection >> atRandom: aGenerator [
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (self size atRandom: aGenerator)
]

{ #category : 'accessing' }
SequenceableCollection >> atWrap: index  [
	"Answer the index'th element of the receiver.  If index is out of bounds,
	let it wrap around from the end to the beginning until it is in bounds."

	^ self at: index - 1 \\ self size + 1
]

{ #category : 'accessing' }
SequenceableCollection >> atWrap: index put: value [
	"Store value into the index'th element of the receiver.  If index is out
	of bounds, let it wrap around from the end to the beginning until it 
	is in bounds. Answer value."

	^ self at: index  - 1 \\ self size + 1 put: value
]

{ #category : 'accessing' }
SequenceableCollection >> before: target [
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, 
	but answer nil if there are no elements before it (i.e. it is the first element)."

	^ self before: target ifNone: nil
]

{ #category : 'accessing' }
SequenceableCollection >> before: target ifNone: exceptionBlock [
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if there are no elements before it."

	| index |
	index := self indexOf: target.
	^ index = 0
		ifTrue: [self errorNotFound: target]
		ifFalse: [index = 1 
			ifTrue: [exceptionBlock value]
			ifFalse: [self at: index - 1]]
]

{ #category : 'testing' }
SequenceableCollection >> beginsWith: sequence [
	"Answer true if the receiver starts with the argument collection."
	
	| sequenceSize |
	self size < (sequenceSize := sequence size) ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index) ifFalse: [ ^false ] ].
	^true
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: aBlock [
	"Refer to the comment in Collection|collect:."
	| result |
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put: (aBlock value: (self at: index))].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: aBlock from: firstIndex to: lastIndex [
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self copyBlank: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> combinations: k atATimeDo: aBlock [
	"Take the items in the receiver, k at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size choose: k) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	aCollection := Array new: k.
	self combinationsAt: 1 upTo: k in: aCollection after: 0 upTo: self size do: aBlock
]

{ #category : 'private' }
SequenceableCollection >> combinationsAt: j upTo: k in: aCollection after: m upTo: n do: aBlock [
	"Choose k of N items and put in aCollection.  j-1 already chosen.  Indexes of items are in numerical order to avoid duplication.  In this slot, we are allowed to use items in self indexed by m+1 up to n.  m is the index used for position j-1."
	"(1 to: 6) combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	m+1 to: n do: [:index | 
		aCollection at: j put: (self at: index).
		j = k
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self combinationsAt: j + 1 upTo: k in: aCollection after: index upTo: n do: aBlock]]
]

{ #category : 'converting' }
SequenceableCollection >> concatenation [
	|result index|

	result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
	index := 0.
	self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
	^result
]

{ #category : 'copying' }
SequenceableCollection >> copyAfter: anElement [
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self species new ])
]

{ #category : 'copying' }
SequenceableCollection >> copyAfterLast: anElement [
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self species new ])
]

{ #category : 'copying' }
SequenceableCollection >> copyBlank: requestedSize [
	"Answer an instance like me, of size n, but without content.
	All elements may be nil, zero or some other default value if needed."

	^self species new: requestedSize
]

{ #category : 'copying' }
SequenceableCollection >> copyFrom: start count: n [
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self copyBlank: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start
]

{ #category : 'copying' }
SequenceableCollection >> copyFrom: start to: stop  [
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1 max: 0.
	^(self copyBlank: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start
]

{ #category : 'copying' }
SequenceableCollection >> copyReplaceAll: oldSubstring with: newSubstring  [
	"Default is not to do token matching.
	See also String copyReplaceTokens:with:"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: false
	"'How now brown cow?' copyReplaceAll: 'ow' with: 'ello'"
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Pile'"
]

{ #category : 'private' }
SequenceableCollection >> copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens [
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |
	(ifTokens and: [ self isString not ])
		ifTrue: [(self is: #Text) ifFalse: [
			self error: 'Token replacement only valid for Strings']].
	aString := self.
	startSearch := 1.
	[(currentIndex := aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex := currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isValidInIdentifiers not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isValidInIdentifiers not]]])
			ifTrue: [aString := aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch := currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch := currentIndex + 1]
					ifFalse: [startSearch := currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "

]

{ #category : 'copying' }
SequenceableCollection >> copyReplaceFrom: start to: stop with: replacementCollection  [
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self copyBlank: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection
]

{ #category : 'copying' }
SequenceableCollection >> copyUpThrough: anElement  [
	"Answer all elements up to and including anObject. If there
	is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
]

{ #category : 'copying' }
SequenceableCollection >> copyUpTo: anElement  [
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : 'copying' }
SequenceableCollection >> copyUpToLast: anElement [
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : 'copying' }
SequenceableCollection >> copyWith: newElement [
	"Answer a copy of the receiver that is 1 bigger than the receiver and has 
	newElement at the last element."

	| answer |
	answer := self copyBlank: self size + 1.
	answer replaceFrom: 1 to: self size with: self startingAt: 1.
	answer at: answer size put: newElement.
	^answer
]

{ #category : 'inspecting' }
SequenceableCollection >> customizeExplorerContents [

	^ true.

]

{ #category : 'enumerating' }
SequenceableCollection >> do: aBlock  [
	"Refer to the comment in Collection >> #do:
	Note: Subclasses need to redefine either #do: or #size (or both!).
	"
	1 to: self size do: [ :index |
		aBlock value: (self at: index) ]
]

{ #category : 'gui' }
SequenceableCollection >> do: aBlock displayingProgress: aString [
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			self withIndexDo: [ :each :i |
				barBlock value: i.
				aBlock value: each]]
]

{ #category : 'enumerating' }
SequenceableCollection >> do: elementBlock separatedBy: separatorBlock [
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> do: aBlock without: anItem [
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]
]

{ #category : 'accessing' }
SequenceableCollection >> eighth [
	"Answer the eighth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 8
]

{ #category : 'testing' }
SequenceableCollection >> endsWith: sequence [
	"Answer true if the receiver ends with the argument collection."
	
	| sequenceSize offset |
	sequenceSize := sequence size.
	(offset := self size - sequenceSize) < 0 ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index + offset) ifFalse: [ ^false ] ].
	^true
]

{ #category : 'private' }
SequenceableCollection >> errorOutOfBounds [
	self error: 'indices are out of bounds'
]

{ #category : 'inspecting' }
SequenceableCollection >> explorerContents [

	^self asArray withIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]
]

{ #category : 'accessing' }
SequenceableCollection >> fifth [
	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
]

{ #category : 'accessing' }
SequenceableCollection >> fillWith: aCollection [

	self replaceFrom: 1 to: (self size min: aCollection size) with: aCollection startingAt: 1 
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinary: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]
	"
	^self findBinary: aBlock do: [ :found | found ] ifNone: [ self errorNotFound: aBlock ]
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinary: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the found element as argument
	If no matching element is found, evaluate exceptionBlock,
	with the  'bounding' elements (or nil) as arguments.
	Examples:
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 11 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 12 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 0.5 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 25 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ',{a. b} printString) print]
	"
	^ self
		findBinaryIndex: aBlock
		do: [ :foundIndex |
			actionBlock value: (self at: foundIndex) ]
		ifNone: [ :prevIndex :nextIndex |
			exceptionBlock
				value:
					(prevIndex > 0 ifTrue: [ self at: prevIndex ])
				value:
					(nextIndex <= self size ifTrue: [ self at: nextIndex ]) ].
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinaryIndex: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]
	"
	^self findBinaryIndex: aBlock do: [ :found | found ] ifNone: [ self errorNotFound: aBlock]
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the index as argument
	If no matching element is found, evaluate exceptionBlock,
	with the indexes of the  'bounding' elements as arguments.
	Warning: Might give invalid indexes, see examples below
	Examples:
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 11 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 12 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 0.5 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 25 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ',{a. b} printString) print]
	"
	| index low high test |
	low := 1.
	high := self size.
	[
		index := high + low // 2.
		low > high ] whileFalse: [
		test := aBlock value: (self at: index).
		test = 0 
			ifTrue: [ ^actionBlock value: index ]
			ifFalse: [ test > 0
				ifTrue: [ low := index + 1 ]
				ifFalse: [ high := index - 1 ]]].
	^exceptionBlock valueWithPossibleArgs: {high. low}
]

{ #category : 'enumerating' }
SequenceableCollection >> findFirst: aBlock [
	"Return the index of my first element for which aBlock evaluates as true."

	^self findFirst: aBlock startingAt: 1
]

{ #category : 'enumerating' }
SequenceableCollection >> findFirst: aBlock startingAt: firstIndex [
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex"

	| s index |
	s := self size.
	index := firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self at: index)) ifTrue: [^index ].
			index := index + 1.
			].
	^ 0
]

{ #category : 'enumerating' }
SequenceableCollection >> findFirstBasicAt: aBlock startingAt: firstIndex [
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex.
	Use #basicAt: for accessing elements, not #at:
	This means, for String, evaluate aBlock on each byte."

	| s index |
	s := self size.
	index := firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self basicAt: index)) ifTrue: [^index ].
			index := index + 1.
			].
	^ 0
]

{ #category : 'enumerating' }
SequenceableCollection >> findLast: aBlock [
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index := self size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
]

{ #category : 'accessing' }
SequenceableCollection >> first [
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self isEmpty ifTrue: [self errorEmptyCollection].
	^ self at: 1
]

{ #category : 'accessing' }
SequenceableCollection >> first: n [
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: n
]

{ #category : 'accessing' }
SequenceableCollection >> firstAvailable: numberOfObjects [
	"Answer the first numberOfObjects in the receiver, subject to availability"

	^self first: (numberOfObjects min: self size)
]

{ #category : 'accessing' }
SequenceableCollection >> fourth [
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
]

{ #category : 'enumerating' }
SequenceableCollection >> from: start to: stop do: aBlock [
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [:index | aBlock value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> from: startIndex to: endIndex put: anObject [
	"Put anObject in all indexes between startIndex 
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"

	| written toWrite thisWrite |

	startIndex > endIndex ifTrue: [^self].
	self at: startIndex put: anObject.
	written := 1.
	toWrite := endIndex - startIndex + 1.
	[written < toWrite] whileTrue:
		[
			thisWrite := written min: toWrite - written.
			self 
				replaceFrom: startIndex + written
				to: startIndex + written + thisWrite - 1
				with: self startingAt: startIndex.
			written := written + thisWrite
		].
	^anObject
]

{ #category : 'enumerating' }
SequenceableCollection >> groupsOf: n atATimeDo: aBlock  [
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | 
			| args |
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].
]

{ #category : 'copying' }
SequenceableCollection >> grownTo: newSize [

	| grownCollection |
	grownCollection := self copyBlank: newSize.
	grownCollection replaceFrom: 1 to: self size with: self startingAt: 1.
	^grownCollection
]

{ #category : 'comparing' }
SequenceableCollection >> hasEqualElements: otherCollection [
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size := self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true
]

{ #category : 'comparing' }
SequenceableCollection >> hash [
	"Subclasses might use other methods.
	However #hashQuick is suggested for very large collections."
	^ self hashQuick
]

{ #category : 'comparing' }
SequenceableCollection >> hashQuick [
	^self hashQuick: self species hash
]

{ #category : 'comparing' }
SequenceableCollection >> hashQuick: initialHash [
	| hash size step |

	size := self size.
	hash := (initialHash + size hash) hashMultiply.
	step := size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem := self at: i.
		elem == self ifFalse: [
			hash := (hash + elem hash) hashMultiply]].
	^hash
]

{ #category : 'accessing' }
SequenceableCollection >> head: anInteger [
	"Answer the first anInteger objects in the receiver, subject to availability"
	
	^self firstAvailable: anInteger 
]

{ #category : 'accessing' }
SequenceableCollection >> identityIndexOf: anElement  [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> identityIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value
]

{ #category : 'testing' }
SequenceableCollection >> ifInBounds: anIndex ifNot: aValuable [

	^(self isInBounds: anIndex) ifTrue: [ anIndex ] ifFalse: aValuable 
]

{ #category : 'testing' }
SequenceableCollection >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement  [
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	^self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement startingAt: start [
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer 0."

	^self indexOf: anElement startingAt: start ifAbsent: 0
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	start to: self size do:
		[:i | (self at: i) = anElement ifTrue: [^ i]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfMax [
	"Answer the index of the maximum value in me."
	
	| answer max e |
	max := self at: 1.
	answer := 1.
	2 to: self size do: [ :i |
		e := self at: i.
		e > max ifTrue: [
			max := e.
			answer := i ]].
	^answer
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfSubCollection: aSubCollection startingAt: anIndex  [
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal the rest of
	the elements of aSubCollection.  Begin the search at element anIndex of the 
	receiver.  If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first := sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index := 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index := index+1]]].
	^ exceptionBlock value
]

{ #category : 'enumerating' }
SequenceableCollection >> intervalOfSubCollection: aSubCollectionToFind [

	| startingIndex |

	startingIndex := self indexOfSubCollection: aSubCollectionToFind startingAt: 1.

	^startingIndex toSelfPlus: aSubCollectionToFind size
]

{ #category : 'comparing' }
SequenceableCollection >> is: otherCollection subcollectionAt: index [
	"Answer whether the receiver includes otherCollection as a subcollection at position index."

	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := otherCollection size.
	sequenceSize = 0 ifTrue: [ ^true ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :i |
		(self at: index+i-1) = (otherCollection at: i) ifFalse: [ ^false ]].
	^true
]

{ #category : 'testing' }
SequenceableCollection >> isInBounds: anIndex [

	^anIndex between: 1 and: self size
]

{ #category : 'testing' }
SequenceableCollection >> isSequenceable [
	^ true
]

{ #category : 'enumerating' }
SequenceableCollection >> keysAndValuesDo: aBlock [
	"Enumerate the receiver with all the keys and values"
	1 to: self size do:[:i|
		aBlock value: i value: (self at: i).
	].
]

{ #category : 'accessing' }
SequenceableCollection >> last [
	"Answer the last element of the receiver.
	Raise an error if the collection is empty."

	| size |
	(size := self size) = 0 ifTrue: [self errorEmptyCollection].
	^ self at: size
]

{ #category : 'accessing' }
SequenceableCollection >> last: n [
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size
]

{ #category : 'accessing' }
SequenceableCollection >> lastAvailable: numberOfObjects [
	"Answer the last numberOfObjects in the receiver, subject to availability"

	^self last: (numberOfObjects min: self size)
]

{ #category : 'accessing' }
SequenceableCollection >> lastIfEmpty: aBlock [
	
	^self ifEmpty: aBlock ifNotEmpty: [ self at: self size ]
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement [
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: aBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: firstIndex by: -1 do: [ :index |
		(self at: index) = anElement ifTrue: [^ aBlock value: index]]
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	self lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: [ :index | ^index ].
	^exceptionBlock value.
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> middle [
	"Answer the middle element of the receiver."
	self emptyCheck.
	^ self at: self size // 2 + 1
]

{ #category : 'accessing' }
SequenceableCollection >> ninth [
	"Answer the ninth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 9
]

{ #category : 'enumerating' }
SequenceableCollection >> pairsCollect: aBlock  [
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
]

{ #category : 'enumerating' }
SequenceableCollection >> pairsDo: aBlock  [
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript newLine; show: b, ' is number ', a printString]
"
]

{ #category : 'accessing' }
SequenceableCollection >> penultimate [
	"Answer the penultimate element of the receiver.
	Raise an error if the collection is empty or has just one element."

	^self penultimateIfAbsent: [self errorCollectionTooSmall].

]

{ #category : 'accessing' }
SequenceableCollection >> penultimateIfAbsent: aBlock [
	
	| size |
	
	size := self size.
	size >= 2 ifTrue: [ ^self at: size-1 ].
	^aBlock value
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsDo: aBlock [
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self copy permutationsStartingAt: 1 do: aBlock
]

{ #category : 'private' }
SequenceableCollection >> permutationsStartingAt: anInteger do: aBlock [
	"#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	anInteger > self size ifTrue: [^self].
	anInteger = self size ifTrue: [^aBlock value: self].
	anInteger to: self size do:
		[:i | self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i]
]

{ #category : 'enumerating' }
SequenceableCollection >> polynomialEval: thisX [
	"Treat myself as the coefficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coefficient for the highest power.
	https://en.wikipedia.org/wiki/Horner's_method"
	"  #(1 2 3) polynomialEval: 2   "   "is 3*X^2 + 2*X + 1 with X = 2"

	| index sum |
	sum := self at: (index := self size).
	[ (index := index - 1) >= 1 ] whileTrue: [
		sum := sum * thisX + (self at: index) ].
	^sum
]

{ #category : 'converting' }
SequenceableCollection >> printStringWithNewline [
	"Convert to a string with returns between items.  Elements are usually strings.
	 Useful for labels for PopUpMenus.
	#('something' 'there') asStringWithNewline
	"
	
	^String streamContents: [ :labelStream |
		self do: [ :each |
			each isString
				ifTrue: [ labelStream nextPutAll: each; newLine ]
				ifFalse: [
					each printOn: labelStream.
					labelStream newLine ]].
		self size > 0 ifTrue: [ labelStream skipBack ]]
]

{ #category : 'enumerating' }
SequenceableCollection >> quickFindFirst: aBlock [
	"Return the index of my first element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findFirst:
	
	1 to: 1000 :: findFirst: [:x | x squared > 123456]
	1 to: 1000 :: quickFindFirst: [:x | x squared > 123456]
	
	1 to: 1000 :: findFirst: [:x | x squared > 1234560]
	1 to: 1000 :: quickFindFirst: [:x | x squared > 1234560]
	"
	^self
		findBinaryIndex: [ :x | (aBlock value: x) ifTrue: [-1] ifFalse: [1]]
		do: [ :i | ]
		ifNone: [ :i1 :i2 | i2 > self size ifFalse: [i2] ifTrue: [0] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> quickFindLast: aBlock [
	"Return the index of my last element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findLast:
	
	1 to: 1000 :: findLast: [:x | x squared < 123456]
	1 to: 1000 :: quickFindLast: [:x | x squared < 123456]
	
	1 to: 1000 :: findLast: [:x | x squared < -10]
	1 to: 1000 :: quickFindLast: [:x | x squared < -10]

	1 to: 1000 :: findLast: [:x | x squared < 1234560]
	1 to: 1000 :: quickFindLast: [:x | x squared < 1234560]
	"
	^self
		findBinaryIndex: [ :x | (aBlock value: x) ifTrue: [1] ifFalse: [-1]]
		do: [ :i | ]
		ifNone: [ :i1 :i2 | i1 ]
]

{ #category : 'accessing' }
SequenceableCollection >> quickIndexOf: anElement  [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ anElement < some ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]
]

{ #category : 'stream creation' }
SequenceableCollection >> readStream [
	^ ReadStream on: self
]

{ #category : 'converting' }
SequenceableCollection >> readStreamFrom: start to: stop [
	^ ReadStream on: self from: start to: stop
]

{ #category : 'removing' }
SequenceableCollection >> remove: oldObject ifAbsent: anExceptionBlock  [
	"SequencableCollections cannot implement removing."

	self shouldNotImplement
]

{ #category : 'enumerating' }
SequenceableCollection >> replace: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into self."

	1 to: self size do: [ :index |
		self at: index put: (aBlock value: (self at: index)) ]
]

{ #category : 'accessing' }
SequenceableCollection >> replaceAll: oldObject with: newObject  [
	"Replace all occurences of oldObject with newObject"
	| index |
	index := self indexOf: oldObject startingAt: 1.
	[index = 0] whileFalse: [
		self at: index put: newObject.
		index := self indexOf: oldObject startingAt: index + 1 ].
]

{ #category : 'accessing' }
SequenceableCollection >> replaceFrom: start to: stop with: replacement  [
	"This destructively replaces elements from start to stop in the receiver.  
	Answer the receiver itself.
	Use copyReplaceFrom:to:with: for insertion/deletion which may alter the 
	size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1
]

{ #category : 'accessing' }
SequenceableCollection >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]
]

{ #category : 'converting' }
SequenceableCollection >> reverse [
	^ self reversed
]

{ #category : 'enumerating' }
SequenceableCollection >> reverseDo: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument, starting
	with the last element and taking each in sequence up to the first.  For
	SequenceableCollections, this is the reverse of the enumeration in do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> reverseWith: aSequenceableCollection do: aBlock  [
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  
	corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorNoMatch].
	self size
		to: 1
		by: -1
		do: [:index | aBlock value: (self at: index)
				value: (aSequenceableCollection at: index)]
]

{ #category : 'converting' }
SequenceableCollection >> reversed [
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self copyBlank: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result

]

{ #category : 'accessing' }
SequenceableCollection >> second [
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2
]

{ #category : 'enumerating' }
SequenceableCollection >> select: aBlock  [
	"Refer to the comment in Collection>>select: ."
	^self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			(aBlock value: (self at: index))
				ifTrue: [ strm nextPut: (self at: index) ]]]
]

{ #category : 'accessing' }
SequenceableCollection >> seventh [
	"Answer the seventh element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 7
]

{ #category : 'copying' }
SequenceableCollection >> shuffleBy: aRandom [

	self size to: 1 by: -1 do: [ :i |
		self swap: i with: ((1 to: i) atRandom: aRandom) ]
]

{ #category : 'copying' }
SequenceableCollection >> shuffled [
	^ Random withDefaultDo: [ :random |
		"To answer a mutable collection when receiver is, for example, an Interval."
		(self collect: [ :each | each ]) shuffleBy: random ]

"Examples:
	($A to: $Z) shuffled
	(1 to: 20) shuffled
	'This is a String' shuffled
"
]

{ #category : 'copying' }
SequenceableCollection >> shuffledBy: aGenerator [
	"To answer a mutable collection when receiver is, for example, an Interval."
	^ (self collect: [ :each | each ]) shuffleBy: aGenerator
]

{ #category : 'accessing' }
SequenceableCollection >> sixth [
	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
]

{ #category : 'sorting' }
SequenceableCollection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison."

	^(super sorted: aSortBlockOrNil) as: self class
]

{ #category : 'accessing' }
SequenceableCollection >> swap: oneIndex with: anotherIndex  [
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
]

{ #category : 'accessing' }
SequenceableCollection >> tail: anInteger [
	"Answer the last anInteger objects in the receiver, subject to availability"
	
	^self lastAvailable: anInteger 
]

{ #category : 'stream creation' }
SequenceableCollection >> thatCanBeModified [
	"By default, SequenceableCollections can handle #at:put:"

	^self
]

{ #category : 'accessing' }
SequenceableCollection >> third [
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
]

{ #category : 'enumerating' }
SequenceableCollection >> upTo: anObject [
	"Deprecated. Use copyUpTo:"

	^ self copyUpTo: anObject
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection allSatisfy: twoArgConditionBlock   [
	
	self with: otherCollection do: [ :left :right | (twoArgConditionBlock value: left value: right) ifFalse: [ ^false ]].
	^true
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection anySatisfy: twoArgConditionBlock  [
	
	self with: otherCollection do: [ :left :right | (twoArgConditionBlock value: left value: right) ifTrue: [ ^true ]].
	^false
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection collect: twoArgBlock [
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection do: twoArgBlock  [
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection do: twoArgBlock separatedBy: separatorBlock [

	| beforeFirst |

	beforeFirst := true.
	self with: otherCollection do: [ :selfElement :otherCollectionElement |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		twoArgBlock value: selfElement value: otherCollectionElement ].



]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection reverseDo: twoArgBlock  [
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	self size to: 1 by: -1 do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection with: thirdCollection collect: threeArgBlock [
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| n result |
	n := self size.
	otherCollection size = n ifFalse: [ self error: 'otherCollection must be the same size' ].
	thirdCollection size = n ifFalse: [ self error: 'thirdCollection must be the same size' ].
	result := self copyBlank: n.
	1 to: n do: [ :index | 
		result at: index put:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection with: thirdCollection do: threeArgBlock  [
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	| n |
	n := self size.
	otherCollection size = n ifFalse: [self error: 'otherCollection must be the same size'].
	thirdCollection size = n ifFalse: [self error: 'thirdCollection must be the same size'].
	1 to: n do: [ :index |
		threeArgBlock
			value: (self at: index)
			value: (otherCollection at: index)
			value: (thirdCollection at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexCollect: elementAndIndexBlock [
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexDo: elementAndIndexBlock  [
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexDo: elementAndIndexBlock separatedBy: separatorBlock [
	"Evaluate the elementAndIndexBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do: [:index |
		index = 1 ifFalse: [separatorBlock value].
		elementAndIndexBlock value: (self at: index) value: index]
]

{ #category : 'enumerating' }
SequenceableCollection >> withNextDo: twoArgBlock [
	"Evaluate the block with each element and the one following it.
	For the last element, next is nil
	(1 to: 10) asArray withNextDo: [ :each :next | {each. next} print ]
	#() withNextDo: [ :a :b | {a. b} print ]
	"
	| first previous |
	first := true.
	self do: [ :each |
		first ifTrue: [
			first := false ]
		ifFalse: [
			twoArgBlock value: previous value: each ].
		previous := each ].
	first ifFalse: [
		twoArgBlock value: previous value: nil ]
]

{ #category : 'enumerating' }
SequenceableCollection >> withPreviousCyclicDo: twoArgBlock [
	"Evaluate the block with each element and the one before it.
	For the first element, previous is the last one. i.e. each object is at some point the first of the pair, and at some other point the second of the pair
	(1 to: 10) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1 2 3) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1 2) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#() withPreviousCyclicDo: [ :a :b | {a. b} print ]
	"
	| previous |
	self size < 2 ifTrue: [^self ].
	previous := self last.
	self do: [ :each |
		twoArgBlock value: each value: previous.
		previous := each ].
]

{ #category : 'enumerating' }
SequenceableCollection >> withPreviousDo: twoArgBlock [
	"Evaluate the block with each element and the one before it.
	For the first element, previous is nil
	(1 to: 10) asArray withPreviousDo: [ :each :previous | {previous. each} print ]
	#() withPreviousDo: [ :a :b | {a. b} print ]
	"
	| previous |
	previous := nil.
	self do: [ :each |
		twoArgBlock value: each value: previous.
		previous := each ].
]

{ #category : 'stream creation' }
SequenceableCollection >> writeStream [
	^ WriteStream on: self
]
