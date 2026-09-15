'From Cuis7.9 [latest update: #8183] on 15 September 2026 at 11:17:04 am'!
'Description '!
!provides: 'CollectionsTests' 1 1!
SystemOrganization addCategory: #CollectionsTests!
SystemOrganization addCategory: #'CollectionsTests-Arrayed'!
SystemOrganization addCategory: #'CollectionsTests-Streams'!


!classDefinition: #BagTest category: #CollectionsTests!
TestCase subclass: #BagTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'BagTest class' category: #CollectionsTests!
BagTest class
	instanceVariableNames: ''!

!classDefinition: #BinarySearchTest category: #CollectionsTests!
TestCase subclass: #BinarySearchTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'BinarySearchTest class' category: #CollectionsTests!
BinarySearchTest class
	instanceVariableNames: ''!

!classDefinition: #CollectTest category: #CollectionsTests!
TestCase subclass: #CollectTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'CollectTest class' category: #CollectionsTests!
CollectTest class
	instanceVariableNames: ''!

!classDefinition: #CollectionTest category: #CollectionsTests!
TestCase subclass: #CollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'CollectionTest class' category: #CollectionsTests!
CollectionTest class
	instanceVariableNames: ''!

!classDefinition: #DictionaryTest category: #CollectionsTests!
TestCase subclass: #DictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'DictionaryTest class' category: #CollectionsTests!
DictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #IntervalTest category: #CollectionsTests!
TestCase subclass: #IntervalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'IntervalTest class' category: #CollectionsTests!
IntervalTest class
	instanceVariableNames: ''!

!classDefinition: #OrderedCollectionTest category: #CollectionsTests!
TestCase subclass: #OrderedCollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'OrderedCollectionTest class' category: #CollectionsTests!
OrderedCollectionTest class
	instanceVariableNames: ''!

!classDefinition: #OrderedDictionaryTest category: #CollectionsTests!
TestCase subclass: #OrderedDictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'OrderedDictionaryTest class' category: #CollectionsTests!
OrderedDictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #SequenceableCollectionTest category: #CollectionsTests!
TestCase subclass: #SequenceableCollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'SequenceableCollectionTest class' category: #CollectionsTests!
SequenceableCollectionTest class
	instanceVariableNames: ''!

!classDefinition: #WeakIdentitySetTest category: #CollectionsTests!
TestCase subclass: #WeakIdentitySetTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests'!
!classDefinition: 'WeakIdentitySetTest class' category: #CollectionsTests!
WeakIdentitySetTest class
	instanceVariableNames: ''!

!classDefinition: #ArrayTest category: #'CollectionsTests-Arrayed'!
TestCase subclass: #ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Arrayed'!
!classDefinition: 'ArrayTest class' category: #'CollectionsTests-Arrayed'!
ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #DoubleByteArrayTest category: #'CollectionsTests-Arrayed'!
TestCase subclass: #DoubleByteArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Arrayed'!
!classDefinition: 'DoubleByteArrayTest class' category: #'CollectionsTests-Arrayed'!
DoubleByteArrayTest class
	instanceVariableNames: ''!

!classDefinition: #Float32ArrayTest category: #'CollectionsTests-Arrayed'!
TestCase subclass: #Float32ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Arrayed'!
!classDefinition: 'Float32ArrayTest class' category: #'CollectionsTests-Arrayed'!
Float32ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #Float64ArrayTest category: #'CollectionsTests-Arrayed'!
TestCase subclass: #Float64ArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Arrayed'!
!classDefinition: 'Float64ArrayTest class' category: #'CollectionsTests-Arrayed'!
Float64ArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ReadStreamTest category: #'CollectionsTests-Streams'!
TestCase subclass: #ReadStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Streams'!
!classDefinition: 'ReadStreamTest class' category: #'CollectionsTests-Streams'!
ReadStreamTest class
	instanceVariableNames: ''!

!classDefinition: #ReadWriteStreamTest category: #'CollectionsTests-Streams'!
TestCase subclass: #ReadWriteStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Streams'!
!classDefinition: 'ReadWriteStreamTest class' category: #'CollectionsTests-Streams'!
ReadWriteStreamTest class
	instanceVariableNames: ''!

!classDefinition: #WriteStreamTest category: #'CollectionsTests-Streams'!
TestCase subclass: #WriteStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CollectionsTests-Streams'!
!classDefinition: 'WriteStreamTest class' category: #'CollectionsTests-Streams'!
WriteStreamTest class
	instanceVariableNames: ''!


!WriteStreamTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!BagTest methodsFor: 'tests' stamp: 'jmv 2/Nov/2022, 7:11:59 pm (UTC)'!
testProduct

	| bag |	
	bag _ Bag new.
	bag add: 3; add: 4; add: 4; add: 5.
	self assert: (bag product: [ :v | v ]) = 240.
	self assert: (bag product: [ :v | v-1 ]) = 72.! !

!BagTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:00:47 pm (UTC)'!
testSumIfEmptyEvalsIfEmptyBlockWhenEmpty

	self assert: (Bag new sum: [:each | 1 ] ifEmpty: [ 'empty' ]) equals: 'empty'.! !

!BagTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:00:58 pm (UTC)'!
testSumIfEmptyEvalsSumBlockForEachElementSumingTheResult

	| bag |
	
	bag := Bag new.
	bag add: 'hello'; add: 'hello'.
	bag add: 'bye'.
	
	self assert: (bag sum: [ :aString | aString = 'hello' ifTrue: [ 2 ] ifFalse: [ -1 ]] ifEmpty: [ self fail ]) equals: 3.! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 25/May/2016, 6:36:00 pm (UTC)'!
testquickFindFirst

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindFirst: [:one | one >= eachElement]) = eachIndex.
			self assert: (collection quickFindFirst: [:one | one >= (eachElement - 0.5)]) = eachIndex
		].
	self assert: (collection quickFindFirst: [:one | one > 2101]) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 25/May/2016, 6:36:00 pm (UTC)'!
testquickFindLast

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindLast: [:one | one <= eachElement]) = eachIndex.
			self assert: (collection quickFindLast: [:one | one <= (eachElement + 0.5)]) = eachIndex
		].
	self assert: (collection quickFindLast: [:one | one < 0]) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 25/May/2016, 6:46:00 pm (UTC)'!
testquickIndexOf

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickIndexOf: eachElement) = eachIndex
		].
	self assert: (collection quickIndexOf: 999) = 0.
	self assert: (collection quickIndexOf: 2101) = 0! !

!BinarySearchTest methodsFor: 'testing' stamp: 'sqr 25/May/2016, 7:01:00 pm (UTC)'!
testquickIndexOfWithSemistableOrder

	| collection |
	collection := SortedCollection
		sortBlock: [:x :y | x key <= y key].
	1 to: 1000 do: [:each | collection add: each -> each].
	1 to: 50 do: [:each | collection add: 42 -> each].
	self assert: (collection quickIndexOf: 42 -> 6) = 48! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 30/Nov/2014, 3:34:00 pm (UTC)'!
testIdentitySet
	"
	CollectTest  new testIdentitySet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = IdentitySet.
	self assert: result = #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	self assert: result = col! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 30/Nov/2014, 2:51:00 pm (UTC)'!
testOrderedCollection
	"
	CollectTest  new testOrderedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asOrderedCollection.
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(2 4 6 8 10) asOrderedCollection! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 30/Nov/2014, 3:34:00 pm (UTC)'!
testSet
	"
	CollectTest  new testSet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asSet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = Set.
	self assert: result = #(1 2 3) asSet.
	self assert: result = col! !

!CollectTest methodsFor: 'testing' stamp: 'jmv 30/Nov/2014, 3:01:00 pm (UTC)'!
testSortedCollection
	"
	CollectTest  new testSortedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asSortedCollection: [ :a :b | a > b ].
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(10 8 6 4 2) asOrderedCollection! !

!CollectionTest methodsFor: 'groupBy tests' stamp: 'jmv 31/Jul/2023, 4:01:40 pm (UTC)'!
testGroupByHavingSelectsTheResultOfGroupBy

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ] having: [ :group | group size > 2 ].
	
	self assert: (groupedByEven size) equals: 1.	
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false).! !

!CollectionTest methodsFor: 'groupBy tests' stamp: 'jmv 31/Jul/2023, 4:01:59 pm (UTC)'!
testGroupByReturnsADictionaryThatGroupsACollectionByThePluggableKey

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ].
	
	self assert: groupedByEven size equals: 2.
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger even ]) equals: (groupedByEven at: true).
	self assert:(collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false).! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 17/Nov/2018, 3:45:15 pm (UTC)'!
testAverageFailsWhenTheCollectionIsEmpty

	self 
		should: [ #() average: [ :each | each ] ]
		raise: Error 
		description: Collection emptyCollectionDescription! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 17/Nov/2018, 3:45:19 pm (UTC)'!
testAverageIfEmptyCalculatesItAsUsualWhenTheCollectionIsNotEmpty

	| someNumbers |
	
	someNumbers _ #(1 5).

	self 
		assert: (someNumbers average: [ :each | each ] ifEmpty: [ self fail ])
		equals: someNumbers average! !

!CollectionTest methodsFor: 'average tests' stamp: 'HAW 17/Nov/2018, 3:45:22 pm (UTC)'!
testAverageIfEmptyEvaluatesEmptyBlockWhenTheCollectionIsEmpty

	| emptyBlock |
	
	emptyBlock _ [ 0 ].

	self 
		assert: (#() average: [ :each | each asInteger ] ifEmpty: emptyBlock)
		equals: emptyBlock value! !

!CollectionTest methodsFor: 'average tests' stamp: 'jmv 6/Jan/2021, 3:57:27 pm (UTC)'!
testsAverageWorksWithABlock

	self assert: ({ '1' . '2' . '3' } average: [ :each | each asNumber ]) equals: 2! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:11:08 pm (UTC)'!
assertAsCommaSeparated: aCollection equals: expectedString 
	
	self 
		assert: (String streamContents: [ :stream | aCollection asCommaSeparated: [ :elem | stream print: elem + 1] on: stream ])
		equals: expectedString! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:12:31 pm (UTC)'!
testAsCommaSeparatedOnWorksAsCommaStringAnd

	self assertAsCommaSeparated: #() equals: ''. 
	self assertAsCommaSeparated: #(1) equals: '2'.
	self assertAsCommaSeparated: #(1 2) equals: '2 and 3'. 
	self assertAsCommaSeparated: #(1 2 3) equals: '2, 3 and 4'.! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:11:51 pm (UTC)'!
testAsCommaStringAndDoesNotAddAnythingWhenEmpty

	self assert: #() asCommaStringAnd isEmpty ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:11:59 pm (UTC)'!
testAsCommaStringAndDoesNotAddAnythingWhenHasOneElement

	self assert: #(1) asCommaStringAnd equals: '1' ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:12:06 pm (UTC)'!
testAsCommaStringAndSeparatesWithAndWhenTwoOneElements

	self assert: #(1 2) asCommaStringAnd equals: '1 and 2' ! !

!CollectionTest methodsFor: 'as comma separated tests' stamp: 'HAW 6/Mar/2019, 7:12:21 pm (UTC)'!
testAsCommaStringAndSeparatesWithCommaButLastOneWithAndWhenMoreThanTwoElements

	self assert: #(1 2 3) asCommaStringAnd equals: '1, 2 and 3' ! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:02:25 pm (UTC)'!
testItMaintainsTheSameCollectionSpecies

	self assert: (OrderedCollection with: 1 with: #(2)) flatten equals: (OrderedCollection with: 1 with: 2).! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:02:53 pm (UTC)'!
testWhenCollectionContainsASingleLevelOfElementsItReturnsTheSameCollection

	self assert: #(1 2 3) flatten equals: #(1 2 3).! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:03:14 pm (UTC)'!
testWhenCollectionContainsMoreThanTwoLevelsltReturnsTheElementsOfAllNestedCollections

	self assert: #(1 #(2 3) #(4 #(5))) flatten equals: #(1 2 3 4 5).! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:03:38 pm (UTC)'!
testWhenCollectionContainsStringCollectionsItDoesNotFlattenTheStrings

	self assert: #('string1' #('string2' 'string3')) flatten equals: #('string1' 'string2' 'string3').! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:03:59 pm (UTC)'!
testWhenCollectionContainsStringsItReturnsTheSameCollection

	self assert: #('string1' 'string2' 'string3') flatten equals: #('string1' 'string2' 'string3').! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:04:18 pm (UTC)'!
testWhenCollectionContainsTwoLevelsOfElementsItReturnsTheElementsOfAllNestedCollections
	
	self assert: #(#(1) #(2 3)) flatten equals: #(1 2 3).! !

!CollectionTest methodsFor: 'flatten tests' stamp: 'jmv 31/Jul/2023, 4:04:27 pm (UTC)'!
testWhenCollectionIsEmptyItReturnsTheSameCollection

	self assert: #() flatten equals: #().! !

!CollectionTest methodsFor: 'misc tests' stamp: 'jpb 3/Aug/2019, 3:18:50 am (UTC)'!
testIsCollection
	self assert: (Bag new is: #Collection).! !

!CollectionTest methodsFor: 'misc tests' stamp: 'jmv 12/Jun/2019, 10:22:57 pm (UTC)'!
testSetEquality
	self assert: Set new = IdentitySet new.
	self assert: Set new hash = IdentitySet new hash.
	self assert: Dictionary new = IdentityDictionary new.
	self assert: Dictionary new hash = IdentityDictionary new hash.
	self assert: Dictionary new = OrderedDictionary new.
	self assert: Dictionary new hash = OrderedDictionary new hash.
	self assert: IdentityDictionary new = OrderedDictionary new.
	self assert: IdentityDictionary new hash = OrderedDictionary new hash.

	self deny: Set new = Dictionary new! !

!DictionaryTest methodsFor: 'tests' stamp: 'jpb 3/Aug/2019, 3:17:49 am (UTC)'!
testIsCollection
	self assert: (Dictionary new is: #Collection)! !

!DictionaryTest methodsFor: 'tests' stamp: 'jmv 20/Sep/2023, 7:43:32 pm (UTC)'!
testKeyNotFoundSignalTheRightMessage

	self 
		should: [ Dictionary new errorKeyNotFound: 'theKey' ]
		raise: Error
		withMessageText: 'key: ''theKey'' not found'! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 18/Dec/2018, 2:56:31 pm (UTC)'!
testAt
	"
	IntervalTest new testAt
	"
	self assert: (2 to: 5 by: 2) first = 2.
	self assert: ((2 to: 5 by: 2) at: 1) = 2.
	self assert: ((2 to: 5 by: 2) at: 2) = 4.
	self assert: (2 to: 5 by: 2) last = 4.

	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: ((0 to: 2.4 by: 0.1) at: 1) = 0.
	self assert: (((0 to: 2.4 by: 0.1) at: 2) isWithin: 1 floatsFrom: 0.1).
	self assert: (((0 to: 2.4 by: 0.1) at: 3) isWithin: 1 floatsFrom: 0.2).
	self assert: (((0 to: 2.4 by: 0.1) at: 23) isWithin: 1 floatsFrom: 2.2).
	self assert: ((0 to: 2.4 by: 0.1) at: 24) = 2.3.
	self assert: ((0 to: 2.4 by: 0.1) at: 25) = 2.4.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 1:53:25 pm (UTC)'!
testFloatInterval
	"
	IntervalTest new testFloatInterval
	"
	
	self assert: (0 to: 2.4 by: 0.1) size = 25.
	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.
	self assert: (2.4 to: 0 by: -0.1) size = 25.
	self assert: (2.4 to: 0 by: -0.1) first = 2.4.
	self assert: (2.4 to: 0 by: -0.1) last = 0.

	2 to: 200 do: [ :n |
		self assert: (0.0 to: 1.0 count: n) size = n.
		self assert: (0.0 to: 1.0 count: n) first = 0.0.
		self assert: ((0.0 to: 1.0 count: n) last isWithin: 1 floatsFrom: 1.0)].
	2 to: 200 do: [ :n |
		self assert: (1.0 to: 0.0 count: n) size = n.
		self assert: (1.0 to: 0.0 count: n) first = 1.0.
		self assert: ((1.0 to: 0.0 count: n) last isWithin: 1 floatsFrom: 0.0)].! !

!IntervalTest methodsFor: 'testing' stamp: 'HAW 17/Mar/2019, 11:20:11 am (UTC)'!
testFloatToByDo
	"
	IntervalTest new testFloatToByDo
	"
	| interval toByDo |

	"See comment at #to:by:do:"
	self shouldFail: [
		interval _ 0 to: 2.4 by: 0.1.
		toByDo _ Array streamContents: [ :strm |
			0 to: 2.4 by: 0.1 do: [ :each |
				strm nextPut: each ]].
		self assert: toByDo size = interval size.
		1 to: toByDo size do: [ :i |
			self assert: (toByDo at: i) = (interval at: i) ]]! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 1:51:16 pm (UTC)'!
testFloatToByDo_negInc
	"
	IntervalTest new testFloatToByDo_negInc
	"
	| interval toByDo |

	"See comment at #to:by:do:"
	self shouldFail: [
		interval := 2.4 to: 0 by: -0.1.
		toByDo := Array streamContents: [ :strm |
			2.4 to: 0 by: -0.1 do: [ :each |
				strm nextPut: each ]].
		self assert: toByDo size = interval size.
		1 to: toByDo size do: [ :i |
			self assert: (toByDo at: i) = (interval at: i) ]]! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 27/Mar/2016, 9:18:00 pm (UTC)'!
testIntegerInterval
	"
	IntervalTest new testIntegerInterval
	"
	| z |
	self assert: (2 to: 5 by: 2) size = 2.
	self assert: (2 to: 5 by: 2) last = 4.
	z _ 0.
	(2 to: 5 by: 2) do: [ :i | z _ i ].
	self assert: z = 4! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 1:48:31 pm (UTC)'!
testIntegerInterval_negInc
	"
	IntervalTest new testIntegerInterval_negInc
	"
	| z |
	self assert: (5 to: 2 by: -2) size = 2.
	self assert: (5 to: 2 by: -2) last = 3.
	z := 0.
	(5 to: 2 by: -2) do: [ :i | z := i ].
	self assert: z = 3.! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 27/Apr/2016, 6:29:00 pm (UTC)'!
testIntegerToByDo
	"
	IntervalTest new testIntegerToByDo
	"
	| interval toByDo |
	interval _ 2 to: 5 by: 2.
	toByDo _ Array streamContents: [ :strm |
		2 to: 5 by: 2 do: [ :each |
			strm nextPut: each ]].
	self assert: toByDo size = interval size.
	1 to: toByDo size do: [ :i |
		self assert: (toByDo at: i) = (interval at: i) ]! !

!IntervalTest methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 1:49:03 pm (UTC)'!
testIntegerToByDo_negInc
	"
	IntervalTest new testIntegerToByDo_negInc
	"
	| interval toByDo |
	interval := 5 to: 2 by: -2.
	toByDo := Array streamContents: [ :strm |
		5 to: 2 by: -2 do: [ :each |
			strm nextPut: each ]].
	self assert: toByDo size = interval size.
	1 to: toByDo size do: [ :i |
		self assert: (toByDo at: i) = (interval at: i) ].! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:43:33 pm (UTC)'!
testDoesNotIncludeNumbersOutsideTheInInterval

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self deny: (from1To10 includes: 0).
	self deny: (from1To10 includes: 11)! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:35:03 pm (UTC)'!
testDoesNotIncludeObjectsThatAreNotNumbers

	self deny: ((1 to: 10) includes: $a)! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:53:40 pm (UTC)'!
testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStep

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self deny: (from1To10By2 includes: 0).
	self deny: (from1To10By2 includes: 2).
	self deny: (from1To10By2 includes: 6).
	self deny: (from1To10By2 includes: 10).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:54:48 pm (UTC)'!
testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStepWithReversedIntervals

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self deny: (from10To1ByMinus2 includes: 0).
	self deny: (from10To1ByMinus2 includes: 1).
	self deny: (from10To1ByMinus2 includes: 5).
	self deny: (from10To1ByMinus2 includes: 9).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:43:03 pm (UTC)'!
testIncludedNumbersRespectAStepDifferentToOne

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self assert: (from1To10By2 includes: 1).
	self assert: (from1To10By2 includes: 5).
	self assert: (from1To10By2 includes: 9).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:55:55 pm (UTC)'!
testIncludedNumbersRespectNegativeSteps

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self assert: (from10To1ByMinus2 includes: 10).
	self assert: (from10To1ByMinus2 includes: 6).
	self assert: (from10To1ByMinus2 includes: 2).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:43:26 pm (UTC)'!
testIncludesNumbersInsideTheInterval

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self assert: (from1To10 includes: 1).
	self assert: (from1To10 includes: 5).
	self assert: (from1To10 includes: 10).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:43:21 pm (UTC)'!
testIncludesNumbersInsideTheIntervalEvenForReversedIntervals

	| from10To1 |
	
	from10To1 := 10 to: 1 by: -1.
	
	self assert: (from10To1 includes: 1).
	self assert: (from10To1 includes: 5).
	self assert: (from10To1 includes: 10).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 4:01:44 pm (UTC)'!
testIntervalWithOneNumberAndNegativeStepBiggerThanOneIncludeOnlyThatNumber

	| from1To1ByMinus2 |
	
	from1To1ByMinus2 := 1 to: 1 by: -2.
	
	self assert: (from1To1ByMinus2 includes: 1).
	self deny: (from1To1ByMinus2 includes: -1).
	self deny: (from1To1ByMinus2 includes: 3).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 4:01:54 pm (UTC)'!
testIntervalWithOneNumberAndNegativeStepIncludeOnlyThatNumber

	| from1To1ByMinus1 |
	
	from1To1ByMinus1 := 1 to: 1 by: -1.
	
	self assert: (from1To1ByMinus1 includes: 1).
	self deny: (from1To1ByMinus1 includes: 0).
	self deny: (from1To1ByMinus1 includes: 2).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 4:00:46 pm (UTC)'!
testIntervalWithOneNumberAndStepBiggerThanOneIncludesOnlyTheNumber

	| from1To1By2 |
	
	from1To1By2 := 1 to: 1 by: 2.
	
	self assert: (from1To1By2 includes: 1).
	self deny: (from1To1By2 includes: -1).
	self deny: (from1To1By2 includes: 3).! !

!IntervalTest methodsFor: 'tests - includes' stamp: 'HAW 2/May/2020, 3:59:06 pm (UTC)'!
testIntervalWithOneNumberIncludesOnlyTheNumber

	| from1To1 |
	
	from1To1 := 1 to: 1.
	
	self assert: (from1To1 includes: 1).
	self deny: (from1To1 includes: 0).
	self deny: (from1To1 includes: 2).! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:15 am (UTC)'!
testIncludesAllOfFailsWhenFirstNotIncluded

	self deny: ((3 to: 9 by: 2) includesAllOf: (1 to: 9 by: 2))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:18 am (UTC)'!
testIncludesAllOfFailsWhenLastNotIncluded

	self deny: ((1 to: 7 by: 2) includesAllOf: (1 to: 9 by: 2))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:21 am (UTC)'!
testIncludesAllOfFailsWithOtherTypeOfCollectionWhenElementNotIncluded

	self deny: ((1 to: 9 by: 2) includesAllOf: {1. 3. 5. 6. 7. 9})! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:24 am (UTC)'!
testIncludesAllOfIntervalWithIncludedFirstLastAndDifferentIncrement

	self assert: ((1 to: 9 by: 2) includesAllOf: (3 to: 7 by: 4))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:27 am (UTC)'!
testIncludesAllOfIntervalWithIncludedFirstSameLastAndIncrement

	self assert: ((1 to: 10 by: 2) includesAllOf: (3 to: 10 by: 2))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:29 am (UTC)'!
testIncludesAllOfIntervalWithSameFirstIncrementAndIncludedLast

	self assert: ((1 to: 10 by: 2) includesAllOf: (1 to: 7 by: 2))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:32 am (UTC)'!
testIncludesAllOfIntervalWithSameFirstLastAndIncrement

	self assert: ((1 to: 10 by: 2) includesAllOf: (1 to: 10 by: 2))! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'HAW 12/Jun/2022, 12:08:35 am (UTC)'!
testIncludesAllOfOtherTypeOfCollection

	self assert: ((1 to: 9 by: 2) includesAllOf: {1. 3. 5. 7. 9})! !

!IntervalTest methodsFor: 'tests - includesAll' stamp: 'jmv 15/Sep/2026, 1:58:26 pm (UTC)'!
testIncludesAllWithNegativeIncrement
	"
	self new testIncludesAllWithNegativeIncrement
	"
	self assert: ((1 to: 9 by: 2) includesAllOf: (9 to: 3 by: -2)).! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:51:17 pm (UTC)'!
testDoesNotIntersectRangeWhen_FirstAfter_LastAfter

	self deny: ((5 to: 10) intersectsRange: (11 to: 12))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:51:25 pm (UTC)'!
testDoesNotIntersectRangeWhen_FirstBefore_LastBefore

	self deny: ((5 to: 10) intersectsRange: (1 to: 4))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:51:55 pm (UTC)'!
testIntersectsRangeWhen_FirstBefore_LastAfter

	self assert: ((5 to: 10) intersectsRange: (4 to: 11))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:51:40 pm (UTC)'!
testIntersectsRangeWhen_FirstBefore_LastIncluded

	self assert: ((5 to: 10) intersectsRange: (1 to: 5))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:51:47 pm (UTC)'!
testIntersectsRangeWhen_FirstIncluded_LastAfter

	self assert: ((5 to: 10) intersectsRange: (5 to: 11))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'HAW 2/Nov/2024, 2:52:03 pm (UTC)'!
testIntersectsRangeWhen_FirstIncluded_LastIncluded

	self assert: ((5 to: 10) intersectsRange: (5 to: 10))! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'jmv 15/Sep/2026, 2:01:17 pm (UTC)'!
testIntersectsRangeWhenEmptyIntersection

	self assert: ((5 to: 11 by: 2) intersectsRange: (6 to: 10 by: 2)).! !

!IntervalTest methodsFor: 'tests - intersectsRange' stamp: 'jmv 15/Sep/2026, 2:03:05 pm (UTC)'!
testIntersectsRangeWithNegativeIncrement

	self assert: ((5 to: 11 by: 2) intersectsRange: (10 to: 4 by: -2)).
	self assert: ((5 to: 11 by: 2) intersectsRange: (12 to: 6 by: -2)).! !

!IntervalTest methodsFor: 'tests - strict inclusion' stamp: 'jmv 15/Sep/2026, 1:38:07 pm (UTC)'!
testStrictlyIncludesRange
	"
	self new testStrictlyIncludesRange
	"
	self assert: ((5 to: 10) strictlyIncludesRange: (6 to: 10)).
	self assert: ((5 to: 10) strictlyIncludesRange: (5 to: 9)).
	self assert: ((5 to: 10) strictlyIncludesRange: (6 to: 9)).
	
	self deny: ((5 to: 10) strictlyIncludesRange: (5 to: 10)).
	self deny: ((5 to: 10) strictlyIncludesRange: (4 to: 10)).
	self deny: ((5 to: 10) strictlyIncludesRange: (5 to: 11)).
	self deny: ((5 to: 10) strictlyIncludesRange: (4 to: 11)).
	
	self assert: ((5 to: 10) strictlyIncludesRange: (6 to: 10 by: 2)).
	self assert: ((5 to: 10) strictlyIncludesRange: (5 to: 9 by: 2)).
	self assert: ((5 to: 10) strictlyIncludesRange: (6 to: 9 by: 2)).
	
	self deny: ((5 to: 9 by: 2) strictlyIncludesRange: (5 to: 9)).
	self deny: ((5 to: 9) strictlyIncludesRange: (5 to: 9 by: 2)).
	
	self deny: ((5 to: 9 by: 2) strictlyIncludesRange: (6 to: 10)).
	self assert: ((5 to: 9 by: 2) strictlyIncludesRange: (6 to: 9)).
	
	self deny: ((5 to: 9 by: 2) strictlyIncludesRange: (6 to: 10 by: 2)).
	self assert: ((5 to: 9 by: 2) strictlyIncludesRange: (7 to: 9 by: 2)).
	self assert: ((5 to: 9 by: 2) strictlyIncludesRange: (6 to: 8 by: 2)).! !

!IntervalTest methodsFor: 'tests - strict inclusion' stamp: 'jmv 15/Sep/2026, 2:07:10 pm (UTC)'!
testStrictlyIncludesRangeWithNegativeIncrement
	"
	self new testStrictlyIncludesRangeWithNegativeIncrement
	"
	self assert: ((5 to: 10) strictlyIncludesRange: (10 to: 6 by: -1)).
	self assert: ((5 to: 10) strictlyIncludesRange: (9 to: 5 by: -1)).
	self assert: ((5 to: 10) strictlyIncludesRange: (9 to: 6 by: -1)).
	
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (6 to: 10)).
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (5 to: 9)).
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (6 to: 9)).
	
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (10 to: 6 by: -1)).
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (9 to: 5 by: -1)).
	self assert: ((10 to: 5 by: -1) strictlyIncludesRange: (9 to: 6 by: -1)).! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'jpb 3/Aug/2019, 3:18:24 am (UTC)'!
testIsCollection
	self assert: (OrderedCollection new is: #Collection).! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:04:53 pm (UTC)'!
testStreamContentsWorksAsExpected

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | stream nextPut: 1 ].
	
	self assert: contents equals: (OrderedCollection with: 1).! !

!OrderedCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:05:11 pm (UTC)'!
testWriteStreamOnOrderedCollectionGrowsAsExpected

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | 
		1 to: 101 do: [ :aNumber | stream nextPut: aNumber ]].
	
	1 to: 101 do: [ :aNumber | self assert: (contents at: aNumber) equals: aNumber ].! !

!OrderedDictionaryTest methodsFor: 'tests' stamp: 'mir 21/Nov/2025, 9:39:07 pm (UTC)'!
testCopy
	| data dict dictCopy dataToRemove  |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict := OrderedDictionary new.
	data := self sampleData.
	dataToRemove := self sampleData2.

	data do: [ :pair |
		dict at: pair first put: pair second ].

	dictCopy := dict copy.

	dataToRemove do: [ :pair |
		dict removeKey: pair first ].
	dataToRemove do: [ :pair |
		dictCopy removeKey: pair first ].
	self assert: dict keys = dictCopy keys.! !

!OrderedDictionaryTest methodsFor: 'tests' stamp: 'jmv 25/May/2018, 2:36:59 pm (UTC)'!
testOrder
	| data dict orderingByCollect orderingByDo orderingBySelect aux desiredOrder |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict at: pair first put: pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.


	"Test that order is correct even if different. Also test alternative way to add stuff to dict."
	dict _ OrderedDictionary new.
	data _ self sampleData reversed.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict add: pair first -> pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.
! !

!OrderedDictionaryTest methodsFor: 'tests' stamp: 'jmv 25/May/2018, 2:41:04 pm (UTC)'!
testRemove
	| data dict dataToRemove desiredOrder orderingByDo |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	dataToRemove _ self sampleData2.

	data do: [ :pair |
		dict at: pair first put: pair second ].
	dataToRemove do: [ :pair |
		dict removeKey: pair first ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].

	desiredOrder _ OrderedCollection new.
	data do: [ :pair | desiredOrder add: pair second ].
	dataToRemove do: [ :pair | desiredOrder remove: pair second ].
	
	self assert: orderingByDo = desiredOrder asArray.! !

!OrderedDictionaryTest methodsFor: 'aux' stamp: 'jmv 25/May/2018, 2:16:25 pm (UTC)'!
sampleData
	^ { {1. 'uno'}. {6. 'seis'}. {8. 'ocho'}. {7. 'siete'}. {2. 'dos'}. {3. 'tres'}. {4. 'cuatro'}. {5. 'cinco'}}! !

!OrderedDictionaryTest methodsFor: 'aux' stamp: 'jmv 25/May/2018, 2:16:35 pm (UTC)'!
sampleData2
	^ { {1. 'uno'}. {8. 'ocho'}. {3. 'tres'}}! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 25/May/2019, 1:49:41 pm (UTC)'!
testCombinationsAtATimeDoShouldEvaluateBlockWithEmptyCollection
	
	| combinationWasEmpty |
	
	combinationWasEmpty := false.
	'ab' combinations: 0 atATimeDo: [ :combination | combinationWasEmpty := combination isEmpty ].
	
	self assert: combinationWasEmpty! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:05:29 pm (UTC)'!
testCombinationsAtATimeDoWorksAsExpected
	
	| combinations |
	 
	combinations := OrderedCollection new.
	'abc' combinations: 2 atATimeDo: [ :combination | combinations add: combination copy].
	
	self assert: combinations size equals: 3.
	self assert: (combinations includes: #($a $b)).
	self assert: (combinations includes: #($a $c)).
	self assert: (combinations includes: #($b $c)).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:05:44 pm (UTC)'!
testDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1.
	traversedElements := OrderedCollection new.
	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ self fail ] ].

	self assert: traversedElements equals: collectionToTraverse.! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:06:00 pm (UTC)'!
testDoSeparatedByDoesNotValueSeparatedBlockOnLastElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1 with: 2.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ traversedElements add: $-] ].

	self assert: traversedElements equals: (OrderedCollection with: 1 with: $- with: 2).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 10:29:27 am (UTC)'!
testDoSeparatedByDoesNothingForEmptyCollection

	self shouldntFail: [ #() do: [ :anElement | self fail ] separatedBy: [ self fail ] ]! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:06:15 pm (UTC)'!
testFirstAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize

	self assert: (#(10 20 30) firstAvailable: 4) equals:  #(10 20 30).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:06:30 pm (UTC)'!
testFirstAvailableReturnsAllObjectsWhenRequestedTheReceiversSize

	self assert: (#(10 20 30) firstAvailable: 3) equals: #(10 20 30).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:06:45 pm (UTC)'!
testFirstAvailableReturnsTheFirstRequestedObjectsWhenTheyAreAvailable

	self assert: (#(10 20 30) firstAvailable: 2) equals: #(10 20).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:06:59 pm (UTC)'!
testLastAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize

	self assert: (#(10 20 30) lastAvailable: 4) equals: #(10 20 30).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:07:16 pm (UTC)'!
testLastAvailableReturnsAllObjectsWhenRequestedTheReceiversSize

	self assert: (#(10 20 30) lastAvailable: 3) equals: #(10 20 30).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:07:34 pm (UTC)'!
testLastAvailableReturnsTheLastRequestedObjectsWhenTheyAreAvailable

	self assert: (#(10 20 30) lastAvailable: 2) equals: #(20 30).! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 29/Oct/2024, 8:33:04 pm (UTC)'!
testWithAllSatisfyReturnsFalseWhenOneIsNotTrue

	self deny: (#(1 2 3) with: #(1 2 4) allSatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 29/Oct/2024, 8:31:27 pm (UTC)'!
testWithAllSatisfyReturnsTrueWhenAllReturnTrue

	self assert: (#(1 2 3) with: #(1 2 3) allSatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 30/Oct/2024, 12:10:33 pm (UTC)'!
testWithAnySatisfyReturnsFalseIfNoneSatisfies

	self deny: (#(1 2 3) with: #(4 5 6) anySatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 30/Oct/2024, 12:10:43 pm (UTC)'!
testWithAnySatisfyReturnsTrueIfOneSatisfies

	self assert: (#(1 2 3) with: #(4 5 3) anySatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:07:58 pm (UTC)'!
testWithIndexDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ] separatedBy: [ self fail ] ].

	self assert: traversedElements size equals: 1.
	self assert: traversedElements first equals: $a->1.! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'jmv 31/Jul/2023, 4:08:54 pm (UTC)'!
testWithIndexDoSeparatedByDoesNotValueSeparatedBlockOnLastElement

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a with: $b.
	traversedElements := OrderedCollection new.

	collectionToTraverse
		withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ]
		separatedBy: [ traversedElements add: $-].

	self assert: traversedElements size equals: 3.
	self assert: traversedElements first equals: $a->1.
	self assert: traversedElements second equals: $-.
	self assert: traversedElements third equals: $b->2.! !

!SequenceableCollectionTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 10:30:18 am (UTC)'!
testWithIndexDoSeparatedByDoesNothingForEmptyCollection

	self shouldntFail: [ #() withIndexDo: [ :anElement :index | self fail ] separatedBy: [ self fail ] ]! !

!WeakIdentitySetTest methodsFor: 'testing' stamp: 'jmv 15/Jan/2025, 1:54:47 pm (UTC)'!
test
	"
	(also tests WeakSet a bit)
	WeakIdentitySetTest new test
	"
	| ary1 ary2 count w wi |
	ary1 := { 3@4 . 4@5 }.
	ary2 := { 13@4 . 14@5 }.
	w := WeakSet new.
	w addAll: ary1.
	wi := WeakIdentitySet new.
	wi addAll: ary2.
	
	self assert: w size = 2.
	count := 0.
	w do: [ :each |
		count := count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (w includes: ary1 first).
	self assert: (w includes: ary1 second).
	self assert: (w includes: 3@4).
	self assert: (w includes: 4@5).

	self assert: wi size = 2.
	count := 0.
	wi do: [ :each |
		count := count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).
	self deny: (wi includes: 13@4).
	self deny: (wi includes: 14@5).

	"Now make one element in each disappear"
	ary1 at: 1 put: 9.
	ary2 at: 1 put: 99.
	Smalltalk garbageCollect.

	self assert: w size = 1.
	self assert: w estimatedSize = 2.
	count := 0.
	w do: [ :each |
		count := count + 1 ].
	self assert: count = 1.
	self deny: (w includes: ary1 first).
	self assert: (w includes: ary1 second).

	self assert: wi size = 1.
	self assert: wi estimatedSize = 2.
	count := 0.
	wi do: [ :each |
		count := count + 1 ].
	self assert: count = 1.
	self deny: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 20/Dec/2018, 6:41:00 pm (UTC)'!
testCanNotInsertShiftingRightWhenInsertionPointIsBiggerThanSize

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := arrayToInsertTo size + 1.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 20/Dec/2018, 6:39:47 pm (UTC)'!
testCanNotInsertShiftingRightWhenInsertionPointIsLessThanOne

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := 0.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 20/Dec/2018, 6:27:08 pm (UTC)'!
testInsertShiftingRightLoosesLastElementAndKeepsPreviousOnes

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 2) equals: #(1 2 3 4)! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 20/Dec/2018, 6:27:21 pm (UTC)'!
testInsertShiftingRightReplacesElementWhenSizeIsOne

	self assert: (#(1) copy insert: 2 shiftingRightAt: 1) equals: #(2)! !

!ArrayTest methodsFor: 'test - insert:ShiftingRight:' stamp: 'HAW 20/Dec/2018, 6:28:20 pm (UTC)'!
testInsertShiftingRightReplacesLastElementWhenInsertingAtLastIndex

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 4) equals: #(1 3 4 2)! !

!ArrayTest methodsFor: 'test - testing' stamp: 'jpb 3/Aug/2019, 2:59:46 am (UTC)'!
testIsArray
	self assert: (#(1 2 3) is: #Array).
	self assert: (#() is: #Array).! !

!ArrayTest methodsFor: 'test - testing' stamp: 'jpb 3/Aug/2019, 3:09:00 am (UTC)'!
testIsCollection
	self assert: (#(1 2 3) is: #Collection).
	self assert: (#() is: #Collection).! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:09:15 pm (UTC)'!
testAntepenultimateIfAbsentReturnsAntepenultimateIfExists

	self assert: (#(1 2 3) antepenultimateIfAbsent: [ self fail ]) equals: 1.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:09:26 pm (UTC)'!
testAntepenultimateIfAbsentValuesIfAbsentBlockWhenNoAntepenultimate

	self assert: (#(1 2) antepenultimateIfAbsent: [ 0 ]) equals: 0.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:09:35 pm (UTC)'!
testAntepenultimateReturnsAntepenultimateIfExists

	self assert: #(1 2 3) antepenultimate equals: 1.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 3/May/2020, 4:49:10 am (UTC)'!
testAntepenultimateSignalsErrorWhenCollectionIsNotBigEnough

	self
		should: [ #(1 2) antepenultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:10:37 pm (UTC)'!
testLastIfEmptyReturnsLastWhenExist

	self assert: (#(1) lastIfEmpty: [ self fail ]) equals: 1.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:10:53 pm (UTC)'!
testLastIfEmptyValuesEmptyBlockWhenEmpty

	self assert: (#() lastIfEmpty: [ 1 ]) equals: 1.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 4/May/2020, 4:43:47 am (UTC)'!
testLastSignalsErrorWhenEmpty

	self
		should: [ #() last ]
		raise: Error
		withMessageText: Collection emptyCollectionDescription! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:11:01 pm (UTC)'!
testPenultimateIfAbsentReturnsPenultimateIfExists

	self assert: (#(2 3) penultimateIfAbsent: [ self fail ]) equals: 2.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:11:17 pm (UTC)'!
testPenultimateIfAbsentValuesIfAbsentBlockWhenNoPenultimate

	self assert: (#(1) penultimateIfAbsent: [ 2 ]) equals: 2.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'jmv 31/Jul/2023, 4:11:24 pm (UTC)'!
testPenultimateReturnsPenultimateIfExists

	self assert: #(2 3) penultimate equals: 2.! !

!ArrayTest methodsFor: 'test - accessing' stamp: 'HAW 3/May/2020, 4:43:44 am (UTC)'!
testPenultimateSignalsErrorWhenCollectionIsNotBigEnough

	self
		should: [ #(1) penultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription! !

!DoubleByteArrayTest methodsFor: 'testing' stamp: 'jmv 5/Jul/2024, 6:53:22 pm (UTC)'!
testSigned
	"
	
	DoubleByteArray holds 16 bit unsigned or signed numbers.
	DoubleByteArrayTest new testSigned
	"
        | u16 |
        u16 := DoubleByteArray new: 7.

	self assert: u16 size = 7.
        u16 signedAt: 1 put:  16r7123.
        self assert: ((u16 signedAt: 1) = 16r7123).
        u16 signedAt: 1 put: 0.
        self assert: ((u16 signedAt: 1) = 0).
        u16 signedAt: 1 put:  -1.
        self assert: ((u16 signedAt: 1) = -1).
        u16 signedAt: 1 put:  -32768.
        self assert: ((u16 signedAt: 1) = -32768).
        self should: [u16 signedAt: 1 put:  -32769] raise: Error. "to big"
        self should: [u16 signedAt: 1 put:  (16r7FFF + 1)] raise: Error. "too big"! !

!DoubleByteArrayTest methodsFor: 'testing' stamp: 'jmv 5/Jul/2024, 6:53:31 pm (UTC)'!
testUnsigned
	"
	DoubleByteArray holds 16 bit unsigned or signed numbers.
	DoubleByteArrayTest new testUnsigned
	"
        | u16 |
        u16 := DoubleByteArray new: 5.

	self assert: u16 size = 5.
        u16 at: 1 put:  16r8123.
        self assert: ((u16 at: 1) = 16r8123).
        u16 at: 1 put: 0.
        self assert: ((u16 at: 1) = 0).
        u16 at: 1 put:  32768.
        self assert: ((u16 at: 1) = 32768).
        u16 at: 1 put:  16rFFFF.
        self assert: ((u16 at: 1) = 16rFFFF).
        self should: [u16 at: 1 put:  -1] raise: Error. "wrong sign"
        self should: [u16 at: 1 put:  (16rFFFF + 1)] raise: Error. "too big"! !

!Float32ArrayTest methodsFor: 'expected failures' stamp: 'jmv 20/Jul/2026, 1:21:34 pm (UTC)'!
expectedFailures

	^#(testSum2).! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:42:23 am (UTC)'!
testIsType
	self assert: (#[1.2 2.4 3.6 ] is: #Collection).
	self assert: (#[1.2 2.4 3.6] is: #Float32Array).! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:32:45 pm (UTC)'!
testSum1
	| a |
	a := {0.0. 1.0. Float pi. Float e} asFloat32Array.
	self assert: (a + a) = (a * 2 ).! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:48:10 pm (UTC)'!
testSum2
	"
	This test is failing. There seems to be a rounding error in FloatArray primitiveSum, at least on
		MacARM OpenSmalltalk VM as of July, 2026
	Desired value is
		(Float32Array with: Float pi+1) first
			4.141592 502593994 .
		((Float32Array with: Float pi+1) basicAt: 1) hex
			'16r408487ED' .
		((Float32Array with: Float pi+1) first) - (Float pi+1)
			-1.5099579897537296e-7 .
	Computed value is
		((Float32Array with: Float pi)+1) first
			 4.141592 979431152 .
		(((Float32Array with: Float pi)+1) basicAt: 1) hex
			'16r408487EE' .
		((Float32Array with: Float pi)+1) first- (Float pi+1)
			 3.2584135922775204e-7 .
	"
	self assert:
		({0.0. 1.0. Float pi. Float e} asFloat32Array + 1)
			=
		({0.0+1. 1.0+1. Float pi+1. Float e+1} asFloat32Array).! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:50:29 am (UTC)'!
testZeroArrayDividend

	self assert: #[1.2 2.4 0.0 ] copy / #[1.2 1.2 1.2 ] = #[1.0 2.0 0.0 ]. ! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:43:45 pm (UTC)'!
testZeroArrayDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] copy / #[1.0 1.0 0.0 ]] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: #[1.0 1.0 0.0 0.0 ] ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ].

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: #[1.0 1.0 0.0]
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.

	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: #[0.0 1.0 1.0]
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592].
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:48:06 am (UTC)'!
testZeroDividend

	self assert: #[1.2 2.4 3.6 0.0 ] copy / 1.2 = #[1.0 2.0 3.0 0.0 ]. ! !

!Float32ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:49:36 am (UTC)'!
testZeroDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] copy / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ] copy.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592] copy.
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:41:31 am (UTC)'!
divideByArrayIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:41:44 am (UTC)'!
divideByArrayOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[1.0 1.0 1.0]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:41:49 am (UTC)'!
divideByArrayZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:41:54 am (UTC)'!
divideByScalarIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:41:57 am (UTC)'!
divideByScalarOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float32ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 11:42:14 am (UTC)'!
divideByScalarZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] copy
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:47:22 pm (UTC)'!
testIsType
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat64Array is: #Collection).
	self assert: (#[-100.0 -100.0 -100.0 -200.0 ] asFloat64Array is: #Float64Array).! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:47:49 pm (UTC)'!
testSum
	| a |
	self assert:
		({0.0. 1.0. Float pi. Float e} asFloat64Array + 1)
			=
		({0.0+1. 1.0+1. Float pi+1. Float e+1} asFloat64Array).
	a := {0.0. 1.0. Float pi. Float e} asFloat64Array.
	self assert: (a + a) = (a * 2 ).! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:46:23 am (UTC)'!
testZeroArrayDividend

	self assert: #[1.2 2.4 0.0 ] asFloat64Array / #[1.2 1.2 1.2 ] asFloat64Array = #[1.0 2.0 0.0 ] asFloat64Array.! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:49:17 pm (UTC)'!
testZeroArrayDivisor

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] asFloat64Array / #[1.0 1.0 0.0 ]] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat64Array divideBy: #[1.0 1.0 0.0 0.0 ] asFloat64Array ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ] asFloat64Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat64Array
			divideBy: #[1.0 1.0 0.0] asFloat64Array
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.

	self should: [
		#[0.0 2.0 3.141592] asFloat64Array
			divideBy: #[0.0 1.0 1.0] asFloat64Array
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592] asFloat64Array.
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 12:50:05 pm (UTC)'!
testZeroDividend

	self assert: {1.2. 2.4. 3.6. 0.0 } asFloat64Array / 1.2 = {1.0. 2.0. 3.0. 0.0 } asFloat64Array.! !

!Float64ArrayTest methodsFor: 'Tests' stamp: 'jmv 20/Jul/2026, 11:49:46 am (UTC)'!
testZeroDivisor

	"Test implicit exception"
	| |
	self should: [ #[1.2 2.4 3.6 ] asFloat64Array / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat64Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ] asFloat64Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat64Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] asFloat64Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592] asFloat64Array.
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:47:13 pm (UTC)'!
divideByArrayIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat64Array
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:47:09 pm (UTC)'!
divideByArrayOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat64Array
		divideBy: #[1.0 1.0 1.0]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:47:04 pm (UTC)'!
divideByArrayZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat64Array
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:47:00 pm (UTC)'!
divideByScalarIndeterminate
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat64Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:46:42 pm (UTC)'!
divideByScalarOne
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat64Array
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!Float64ArrayTest methodsFor: 'Aux' stamp: 'jmv 20/Jul/2026, 12:46:54 pm (UTC)'!
divideByScalarZero
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat64Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]! !

!ReadStreamTest methodsFor: 'tests' stamp: 'jmv 29/Oct/2024, 12:25:49 am (UTC)'!
testMatchStart
	| string matchStart |
	string := UnicodeString fromUtf8Bytes: #[57 194 160 226 130 172 64 226 130 172 226 130 172].

	1 to: string size do: [ :i |
		i to: string size do: [ :j |
			matchStart := string readStream matchStart: (string copyFrom: i to: j).
			self assert: (string at: matchStart) = (string at: i)]].! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 2:21:47 pm (UTC)'!
testUpTo1ShortRecords
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| stream |
	stream _ ReadStream on: 'record-1Xrecord-2Xrecord-incomplete'. 
	self assert:  ((stream upTo: $X) =  'record-1').
	self assert:  ((stream upTo: $X) =  'record-2').
	self assert:  ((stream upTo: $X) =  'record-incomplete').
	self assert:  ((stream upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (stream position = 35). ! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 2:55:33 pm (UTC)'!
testUpTo3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	stream _ ReadStream on: longString.
	read _ stream upTo: $X.
	self assert: read = longString.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 2:54:11 pm (UTC)'!
testUpTo4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return all the chunk  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	read _ stream upTo: $X.
	self assert: read size = 5000.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 2:58:48 pm (UTC)'!
testUpTo5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time the second block up to EOF.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	" first scan, the delimiter is found but not printed.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan. the delimiter is not found, all second chunk is returned  "
	read _ stream upTo: $X.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 2:59:51 pm (UTC)'!
testUpTo6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end. expected to return 
      two big chunks in two read, delimiters excluded.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter excluded "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan, return chunk, delimiter excluded.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $b.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'jmv 29/Oct/2024, 11:23:45 am (UTC)'!
testUpToAll
	| string prefix |

	prefix := 'https://webmail.gandi.net/SOGo/so/juan@cuis.st/Mail/view#!!/Mail/0/INBOX/7020'
		readStream upToAll: '://'.
	self assert: prefix = 'https'.

	string := UnicodeString fromUtf8Bytes: #[57 194 160 226 130 172 64 226 130 172 226 130 172].
	1 to: string size do: [ :i |
		i to: string size do: [ :j |
			prefix := string readStream upToAll: (string copyFrom: i to: j).
			self assert: (string at: prefix size + 1) = (string at: i)]].! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:00:52 pm (UTC)'!
testUpToTerminator1ShortRecords
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| stream |
	stream _ ReadStream on: 'record-1Xrecord-2Xrecord-incomplete'. 
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (stream position = 18).! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:01:38 pm (UTC)'!
testUpToTerminator3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return nil. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	stream _ ReadStream on: longString.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:02:04 pm (UTC)'!
testUpToTerminator4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return nil  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:02:36 pm (UTC)'!
testUpToTerminator5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time nil.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter not found, returns nil  "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:03:06 pm (UTC)'!
testUpToTerminator6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end expected to return 
      two big chunks in two read, delimiters included.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter found, return chunk, delimiter included   "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:06:39 pm (UTC)'!
testUpTo1ShortRecords
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| stream |
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: 'record-1Xrecord-2Xrecord-incomplete'.
	stream position: 0.
	self assert:  ((stream upTo: $X) =  'record-1').
	self assert:  ((stream upTo: $X) =  'record-2').
	self assert:  ((stream upTo: $X) =  'record-incomplete').
	self assert:  ((stream upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (stream position = 35). ! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:07:32 pm (UTC)'!
testUpTo3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: longString.
	stream position: 0.
	read _ stream upTo: $X.
	self assert: read = longString.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:08:12 pm (UTC)'!
testUpTo4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return all the chunk  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	read _ stream upTo: $X.
	self assert: read size = 5000.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:08:41 pm (UTC)'!
testUpTo5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time the second block up to EOF.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	" first scan, the delimiter is found but not printed.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan. the delimiter is not found, all second chunk is returned  "
	read _ stream upTo: $X.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:08:54 pm (UTC)'!
testUpTo6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end. expected to return 
      two big chunks in two read, delimiters excluded.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	" first scan, delimiter is found, return all the block delimiter excluded "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan, return chunk, delimiter excluded.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $b.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:12:06 pm (UTC)'!
testUpToTerminator1ShortRecords
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| stream |
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: 'record-1Xrecord-2Xrecord-incomplete'.
	stream position: 0.
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (stream position = 18).! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:12:49 pm (UTC)'!
testUpToTerminator3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return nil. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: longString.
	stream position: 0.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:13:03 pm (UTC)'!
testUpToTerminator4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return nil  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:13:18 pm (UTC)'!
testUpToTerminator5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time nil.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter not found, returns nil  "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.! !

!ReadWriteStreamTest methodsFor: 'tests' stamp: 'NM 25/Oct/2021, 3:13:33 pm (UTC)'!
testUpToTerminator6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end expected to return 
      two big chunks in two read, delimiters included.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter found, return chunk, delimiter included   "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.! !

!WriteStreamTest methodsFor: 'tests' stamp: 'jmv 6/Jul/2022, 1:09:16 am (UTC)'!
testIsStream
	self assert: (String writeStream is: #Stream).! !
