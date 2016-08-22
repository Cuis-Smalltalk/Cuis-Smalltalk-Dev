'From Cuis 4.2 of 25 July 2013 [latest update: #2850] on 12 August 2016 at 9:59:24 am'!

!Collection methodsFor: 'converting' stamp: 'jmv 8/12/2016 09:57:35'!
asNewArray
	"Answer a new Array whose elements are the elements of the receiver"

	^self as: Array! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 8/12/2016 09:57:40'!
asNewArray
	"Answer a new Array whose elements are the elements of the receiver.
	Optimized implementation."

	^ Array withAll: self! !


!Collection methodsFor: 'converting' stamp: 'jmv 8/12/2016 09:57:48'!
asArray
	"Answer an Array whose elements are the elements of the receiver"

	^self asNewArray! !

!Collection methodsFor: 'sorting' stamp: 'jmv 8/12/2016 09:58:52'!
sorted: aSortBlockOrNil
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison."

	^self asNewArray sort: aSortBlockOrNil! !


!Array methodsFor: 'converting' stamp: 'jmv 8/12/2016 09:58:21'!
asArray
	"Answer with the receiver itself.
	But for subclasses, answer an actual array!!"

	^self class == Array
		ifTrue: [ self ]
		ifFalse: [ self asNewArray ]! !

!methodRemoval: OrderedCollection #sorted:!
OrderedCollection removeSelector: #sorted:!
!methodRemoval: Array #sorted:!
Array removeSelector: #sorted:!
!methodRemoval: SequenceableCollection #asArray!
SequenceableCollection removeSelector: #asArray!
