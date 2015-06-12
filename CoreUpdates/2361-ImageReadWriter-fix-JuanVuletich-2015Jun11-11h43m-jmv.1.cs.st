'From Cuis 4.2 of 25 July 2013 [latest update: #2360] on 11 June 2015 at 12:22:17.742377 pm'!

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 6/11/2015 12:15'!
putForm: aForm onFileNamed: fileName
	"Store the given form on a file of the given name."

	| writer |
	writer _ self onBinaryStream: (FileStream newFileNamed: fileName) binary.
	[ writer nextPutImage: aForm ] ensure: [
		writer close ]! !

