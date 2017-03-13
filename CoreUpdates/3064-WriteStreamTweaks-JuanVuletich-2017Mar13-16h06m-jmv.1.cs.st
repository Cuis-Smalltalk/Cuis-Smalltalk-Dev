'From Cuis 5.0 of 7 November 2016 [latest update: #3063] on 13 March 2017 at 4:16:45 pm'!

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

!WriteStream methodsFor: 'private' stamp: 'jmv 3/12/2017 19:08:07'!
growTo: anInteger
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize grownCollection newSize |
	oldSize _ collection size.
     newSize _ anInteger + (oldSize max: 20).
	grownCollection _ collection class new: newSize.
	collection _ grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit _ collection size! !

!WriteStream methodsFor: 'private' stamp: 'jmv 3/12/2017 19:07:28'!
pastEndPut: anObject
	"Grow the collection.
	Then we put <anObject> at the current write position."

	self growTo: collection size + 1.
	collection at: (position _ position + 1) put: anObject! !

