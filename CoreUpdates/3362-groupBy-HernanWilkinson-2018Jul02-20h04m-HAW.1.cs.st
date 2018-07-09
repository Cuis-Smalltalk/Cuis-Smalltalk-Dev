'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 5 July 2018 at 3:17:35 pm'!

!Collection methodsFor: 'enumerating' stamp: 'HAW 7/5/2018 14:49:22'!
groupBy: keyBlock 

	^ self 
		inject: Dictionary new
		into: [ :groupedCollection :elementToGroup | | group |
			group := groupedCollection at: (keyBlock value: elementToGroup) ifAbsentPut: [ OrderedCollection new ].
			group add: elementToGroup.
			groupedCollection ]
		! !


!Collection methodsFor: 'enumerating' stamp: 'HAW 7/5/2018 15:14:50'!
groupBy: keyBlock having: selectBlock 
	"Like in SQL operation - Split the receivers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock. "
	
	^ (self groupBy: keyBlock) select: selectBlock! !

