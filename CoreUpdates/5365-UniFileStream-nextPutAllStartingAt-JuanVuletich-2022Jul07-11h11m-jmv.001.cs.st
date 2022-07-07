'From Cuis 6.0 [latest update: #5364] on 7 July 2022 at 11:37:02 am'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:36:25'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this file. All 3 kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aStringOrBytes startingAt: startIndex count: anInteger.
	lastWrittenElement _ aStringOrBytes at: anInteger.! !

