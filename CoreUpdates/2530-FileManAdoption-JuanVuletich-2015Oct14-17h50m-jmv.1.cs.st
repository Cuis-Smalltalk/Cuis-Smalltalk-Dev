'From Cuis 4.2 of 25 July 2013 [latest update: #2528] on 14 October 2015 at 6:22:57.735202 pm'!

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 10/14/2015 17:57'!
privateFileNamed: fullPathName
	"Open a file with the given name for reading and writing.
	If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new open: fullPathName forWrite: true! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 10/14/2015 18:21'!
privateForceNewFileNamed: fullPathName 
	"Create a new file with the given name, and answer a stream opened 
	for writing on that file. If the file already exists, delete it without 
	asking before creating the new file."

	fullPathName asFileEntry exists
		ifFalse: [
			^ (self new open: fullPathName forWrite: true)
				ifNil: ["Failed to open the file"
					(FileDoesNotExistException fileName: fullPathName) signal]].

	FmFileIOAccessor default deleteFile: fullPathName
		ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , fullPathName) signal].

	^ (self new open: fullPathName forWrite: true)
		ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: fullPathName) signal ]! !


!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/14/2015 17:57'!
fileNamed: fullPathName
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass privateFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/14/2015 18:02'!
forceNewFileNamed: fullPathName
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	^ self concreteStreamClass privateForceNewFileNamed: fullPathName
! !

!methodRemoval: StandardFileStream class #fileNamed:!
StandardFileStream class removeSelector: #fileNamed:!
!methodRemoval: StandardFileStream class #forceNewFileNamed:!
StandardFileStream class removeSelector: #forceNewFileNamed:!
!methodRemoval: FileStream class #forceNewFileNamed:!
FileStream class removeSelector: #forceNewFileNamed:!
