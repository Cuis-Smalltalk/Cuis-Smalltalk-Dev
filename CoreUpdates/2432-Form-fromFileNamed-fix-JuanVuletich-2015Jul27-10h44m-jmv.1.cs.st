'From Cuis 4.2 of 25 July 2013 [latest update: #2431] on 27 July 2015 at 10:45:51.202526 am'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 7/25/2015 17:48'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| file form |
	file _ fileName asFileEntry readStream binary.
	form _ self fromBinaryStream: file.
	file close.
	^ form
! !

