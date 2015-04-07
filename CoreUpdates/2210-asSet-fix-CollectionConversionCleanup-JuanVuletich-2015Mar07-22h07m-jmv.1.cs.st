'From Cuis 4.2 of 25 July 2013 [latest update: #2208] on 7 March 2015 at 11:11:08.253667 pm'!

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:34'!
asFloatArray
	"Answer a FloatArray whose elements are the elements of the receiver"

	^self as: FloatArray! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:36'!
asIntegerArray
	"Answer an IntegerArray whose elements are the elements of the receiver"

	^self as: IntegerArray! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:43'!
asPointArray
	"Answer a PointArray whose elements are the elements of the receiver"

	^self as: PointArray! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:53'!
asWordArray
	"Answer a WordArray whose elements are the elements of the receiver"

	^self as: WordArray! !


!Collection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:57'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^(self new: aCollection size)
		addAll: aCollection;
		yourself! !


!Heap class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 23:04'!
newFrom: aCollection
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		reSort;
		yourself! !


!SortedCollection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:48'!
new: anInteger sortBlock: aBlock 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^(self new: anInteger) sortBlock: aBlock! !


!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:43'!
asArray
	"Answer an Array whose elements are the elements of the receiver"

	^self as: Array! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:08'!
asBag
	"Answer a Bag whose elements are the elements of the receiver."

	^ self as: Bag! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:28'!
asByteArray

	^self as: ByteArray! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:29'!
asCharacterSet
	"Answer a CharacterSet whose elements are the unique elements of the receiver.  The receiver should only contain characters"

	^self as: CharacterSet! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:38'!
asIdentitySet
	"Answer an IdentitySet whose elements are the elements of the receiver"

	^self as: IdentitySet! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:41'!
asSet
	"Answer a Set whose elements are the unique elements of the receiver."

	^ self as: Set! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:46'!
asSkipList: aSortBlock 
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| skipList |
	skipList _ SkipList new: self size sortBlock: aSortBlock.
	skipList addAll: self.
	^ skipList! !

!Collection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:48'!
asSortedCollection: aSortBlock 
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection _ SortedCollection new: self size sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection! !


!Collection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 23:10'!
withAll: aCollection
	"Create a new collection containing all the elements from aCollection.
	Warning: In ArrayedCollection this method is redefined, and only accepts SequenceableCollections as the argument"

	^self newFrom: aCollection! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:26'!
asArray
	"Answer an Array whose elements are the elements of the receiver.
	Optimized implementation."

	^ Array withAll: self! !

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:28'!
asByteArray
	"Answer a ByteArray whose elements are the elements of the receiver.
	Optimized implementation."

	^ ByteArray withAll: self! !

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:32'!
asColorArray

	^self as: ColorArray! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:23'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 23:10'!
withAll: aSequenceableCollection
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection! !


!Array methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:43'!
asArray
	"Answer with the receiver itself.
	But for subclasses, answer an actual array!!"

	^self class == Array
		ifTrue: [ self ]
		ifFalse: [ super asArray ]! !


!SortedCollection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:48'!
sortBlock: aBlock 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^(self new: 10) sortBlock: aBlock! !


!Set methodsFor: 'converting' stamp: 'jmv 3/7/2015 22:44'!
asSet
	"Answer with the receiver itself.
	But for subclasses, answer an actual array!!"

	^self class == Set
		ifTrue: [ self ]
		ifFalse: [ super asSet ]! !

!methodRemoval: Number #asB3DVector3!
Number removeSelector: #asB3DVector3!
!methodRemoval: SkipList class #newFrom:!
SkipList class removeSelector: #newFrom:!
!methodRemoval: Set class #newFrom:!
Set class removeSelector: #newFrom:!
!methodRemoval: SortedCollection class #new:!
SortedCollection class removeSelector: #new:!
!methodRemoval: OrderedCollection class #newFrom:!
OrderedCollection class removeSelector: #newFrom:!
!methodRemoval: Heap class #withAll:!
Heap class removeSelector: #withAll:!
!methodRemoval: String #asFourCode!
String removeSelector: #asFourCode!
!methodRemoval: ByteArray #asByteArrayPointer!
ByteArray removeSelector: #asByteArrayPointer!
!methodRemoval: SequenceableCollection #asFloatArray!
SequenceableCollection removeSelector: #asFloatArray!
!methodRemoval: SequenceableCollection #asIntegerArray!
SequenceableCollection removeSelector: #asIntegerArray!
!methodRemoval: SequenceableCollection #asPointArray!
SequenceableCollection removeSelector: #asPointArray!
!methodRemoval: SequenceableCollection #asWordArray!
SequenceableCollection removeSelector: #asWordArray!
!methodRemoval: Bag class #newFrom:!
Bag class removeSelector: #newFrom:!
