'From Cuis 5.0 of 7 November 2016 [latest update: #3490] on 17 November 2018 at 1:28:17 pm'!

!Collection methodsFor: 'private' stamp: 'GC 11/17/2018 13:12:25'!
errorCollectionTooSmall

	self error: self class collectionTooSmallDescription! !


!Collection class methodsFor: 'error descriptions' stamp: 'GC 11/17/2018 13:12:24'!
collectionTooSmallDescription

	^ 'this collection is too small'! !

!Collection class methodsFor: 'error descriptions' stamp: 'GC 11/17/2018 13:23:46'!
notKeyedDescription

	^ 'Instances of {1} do not respond to keyed accessing messages.' format: { self className }! !

!Collection class methodsFor: 'error descriptions' stamp: 'GC 11/17/2018 13:18:00'!
objectNotFoundDescription

	^ 'Object is not in the collection'! !

!Collection class methodsFor: 'error descriptions' stamp: 'GC 11/17/2018 13:17:06'!
sizesDoNotMatchDescription

	^ 'collection sizes do not match'! !


!Collection methodsFor: 'private' stamp: 'GC 11/17/2018 13:15:10'!
errorEmptyCollection

	self error: self class emptyCollectionDescription! !

!Collection methodsFor: 'private' stamp: 'GC 11/17/2018 13:17:21'!
errorNoMatch

	self error: self class sizesDoNotMatchDescription! !

!Collection methodsFor: 'private' stamp: 'GC 11/17/2018 13:18:15'!
errorNotFound: anObject
	"Actually, this should raise a special Exception not just an error."

	self error: self class objectNotFoundDescription! !

!Collection methodsFor: 'private' stamp: 'GC 11/17/2018 13:23:53'!
errorNotKeyed

	self error: self class notKeyedDescription
! !

!Collection methodsFor: 'statistics' stamp: 'HAW 11/17/2018 11:34:23'!
average: aBlock

	^ self average: aBlock ifEmpty: [ self errorEmptyCollection ]! !

!Collection methodsFor: 'statistics' stamp: 'HAW 11/17/2018 11:38:13'!
average: aBlock ifEmpty: emptyBlock 
	
	^ (self sum: aBlock ifEmpty: [ ^emptyBlock value ]) / self size! !


!Collection class methodsFor: 'error descriptions' stamp: 'GC 11/17/2018 13:13:11'!
emptyCollectionDescription

	^ 'this collection is empty'! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'GC 11/17/2018 13:12:01'!
penultimate
	"Answer the penultimate element of the receiver.
	Raise an error if the collection is empty or has just one element."

	| size |
	(size _ self size) < 2 ifTrue: [self errorCollectionTooSmall].
	^ self at: size-1! !

!methodRemoval: Collection class #collectionToSmallDescription!
Collection class removeSelector: #collectionToSmallDescription!
!methodRemoval: Collection class #notKeyedDescription:!
Collection class removeSelector: #notKeyedDescription:!
!methodRemoval: Collection #emptyCollectionDescription!
Collection removeSelector: #emptyCollectionDescription!
!methodRemoval: Collection #errorCollectionToSmall!
Collection removeSelector: #errorCollectionToSmall!
