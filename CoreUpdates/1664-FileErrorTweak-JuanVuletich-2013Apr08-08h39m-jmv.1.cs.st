'From Cuis 4.1 of 12 December 2012 [latest update: #1663] on 8 April 2013 at 8:39:07 am'!

!StandardFileStream methodsFor: 'primitives' stamp: 'jmv 4/7/2013 17:34'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])! !

