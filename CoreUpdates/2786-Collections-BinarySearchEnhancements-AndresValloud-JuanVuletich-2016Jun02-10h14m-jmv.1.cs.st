'From Cuis 4.2 of 25 July 2013 [latest update: #2785] on 2 June 2016 at 10:15:53.452311 am'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 6/2/2016 10:06'!
quickIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ anElement < some ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 6/2/2016 09:40'!
quickFindFirst: aBlock
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
		ifNone: [ :i1 :i2 | i2 > self size ifFalse: [i2] ifTrue: [0] ]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 6/2/2016 10:02'!
quickFindLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findFirst:
	
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
		ifNone: [ :i1 :i2 | i1 ]! !


!SortedCollection methodsFor: 'accessing' stamp: 'jmv 6/2/2016 10:04'!
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	| sortBlockToUse |
	sortBlockToUse _ sortBlock ifNil: [[:a :b | a <= b ]].
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | exceptionBlock value ]! !

!SortedCollection methodsFor: 'accessing' stamp: 'jmv 6/2/2016 10:10'!
quickIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	| sortBlockToUse |
	sortBlockToUse _ sortBlock ifNil: [[:a :b | a <= b ]].
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]! !

