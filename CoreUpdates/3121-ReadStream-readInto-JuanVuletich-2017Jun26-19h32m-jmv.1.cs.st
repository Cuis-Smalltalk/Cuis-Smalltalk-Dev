'From Cuis 5.0 of 7 November 2016 [latest update: #3120] on 26 June 2017 at 8:03:37 pm'!

!ReadStream methodsFor: 'accessing' stamp: 'jmv 6/26/2017 19:34:17'!
readInto: byteArray startingAt: startIndex count: count
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max _ (readLimit - position) min: count.
	byteArray 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position _ position + max.
	^max! !

