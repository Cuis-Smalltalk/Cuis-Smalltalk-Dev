'From Cuis 4.2 of 25 July 2013 [latest update: #2396] on 1 July 2015 at 10:56:29.943325 am'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 6/30/2015 11:52'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| file form |
	file _ fileName asDirectoryEntry readStream binary.
	form _ self fromBinaryStream: file.
	file close.
	^ form
! !

