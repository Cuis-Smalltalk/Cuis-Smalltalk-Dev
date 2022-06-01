'From Cuis 6.0 [latest update: #5211] on 1 June 2022 at 9:18:03 am'!

!WriteStream methodsFor: 'accessing' stamp: 'jmv 3/12/2017 18:55:36'!
nextPutAllString: aString withAttributes: attributesArray

	| newEnd |
	(self isCompatibleWithContents: aString)
		ifFalse: [ ^ self nextPutAll: aString ].

	newEnd _ position + aString size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: aString
		attributes: attributesArray
		startingAt: 1.
	position _ newEnd! !

