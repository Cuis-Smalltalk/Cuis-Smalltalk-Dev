'From Cuis 6.0 [latest update: #5368] on 7 July 2022 at 3:24:51 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 15:24:39'!
readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"
	
	^(self nextBytes: count into: byteArray startingAt: startIndex) size - startIndex + 1
! !

