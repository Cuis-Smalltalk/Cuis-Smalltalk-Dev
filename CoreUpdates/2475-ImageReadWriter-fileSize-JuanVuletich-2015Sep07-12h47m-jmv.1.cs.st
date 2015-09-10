'From Cuis 4.2 of 25 July 2013 [latest update: #2474] on 7 September 2015 at 12:53:30.634897 pm'!
!classDefinition: #ImageReadWriter category: #'Graphics-Files'!
Object subclass: #ImageReadWriter
	instanceVariableNames: 'stream fileSize '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files'!

!ImageReadWriter methodsFor: 'private' stamp: 'jmv 9/7/2015 12:47'!
binaryStream: aStream
	stream _ aStream.
	fileSize _ stream size! !

!classDefinition: #ImageReadWriter category: #'Graphics-Files'!
Object subclass: #ImageReadWriter
	instanceVariableNames: 'stream fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files'!
