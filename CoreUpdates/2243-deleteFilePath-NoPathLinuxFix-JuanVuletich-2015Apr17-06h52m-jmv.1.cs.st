'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 17 April 2015 at 6:53:50.776076 am'!

!FileDirectory class methodsFor: 'create/delete file' stamp: 'jmv 4/16/2015 20:57'!
deleteFilePath: fileName
	"Delete the file after finding its directory"
	"este quiere un fullPath. Y el unico sender le manda un nombre simple...
	Modificarlo para que ande en ambos casos!! (incluso en freakin linux)
	directoryEntryFor: filenameOrPath
	"

	| dir fullName |
	
	fullName _ FileStream fullName: fileName.
	dir _ self on: (self dirPathFor: fullName).
	dir deleteFileNamed: (self localNameFor: fileName).
! !

