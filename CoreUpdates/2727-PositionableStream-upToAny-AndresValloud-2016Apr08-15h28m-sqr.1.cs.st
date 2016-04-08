'From Cuis 4.2 of 25 July 2013 [latest update: #2726] on 8 April 2016 at 3:30:28.223649 pm'!

!PositionableStream methodsFor: 'accessing' stamp: 'sqr 4/7/2016 17:15'!
upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (self collectionSpecies new: 100).
	[self atEnd or: [aCollection includes: (element := self next)]]
		whileFalse: [newStream nextPut: element].
	(aCollection includes: element)
		ifTrue: [self skip: -1].
	^newStream contents! !

!methodRemoval: StandardFileStream #upToAny:!
StandardFileStream removeSelector: #upToAny:!
