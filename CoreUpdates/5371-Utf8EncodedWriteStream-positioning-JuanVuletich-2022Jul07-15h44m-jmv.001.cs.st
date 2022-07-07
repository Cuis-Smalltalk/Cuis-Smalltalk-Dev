'From Cuis 6.0 [latest update: #5370] on 7 July 2022 at 3:48:40 pm'!

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 15:44:56'!
position: pos
	"Argument is a byte position"

	super position: pos.
	lastWrittenElement _ nil.
	numberOfCodePointsWritten _ nil.! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 15:45:55'!
skip
	"Leave an empty byte position."

	self skip: 1! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 15:46:23'!
skip: n
	"Skips the next amount bytes in the byte store."

	self position: (self position + n min: self size).! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 15:47:02'!
skipBack
	"Overwrite last written byte."

	self skip: -1.! !

