'From Cuis 4.2 of 25 July 2013 [latest update: #1777] on 30 July 2013 at 12:04:10.768109 am'!

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:00'!
isAFileFullNamed: fullFileName
	"Answer true if a file of the given name exists."

	| f |
	f _ self new open: fullFileName forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true
! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:02'!
readOnlyFileFullNamed: fullFileName 
	"Open an existing file with the given name for reading."

	| f |
	f _ self new open: fullFileName forWrite: false.
	^ f
		ifNil: ["File does not exist..."
			((FileDoesNotExistException fileName: fullFileName) readOnly: true) signal].

	"StandardFileStream readOnlyFileNamed: 'kjsd.txt' "! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:01'!
forceNewFileNamed: fileName 
	"Create a new file with the given name, and answer a stream opened 
	for writing on that file. If the file already exists, delete it without 
	asking before creating the new file."
	| dir localName fullName f |
	fullName _ self fullName: fileName.
	(self isAFileFullNamed: fullName)
		ifFalse: [
			f _ self new open: fullName forWrite: true.
			^ f
				ifNil: ["Failed to open the file"
					(FileDoesNotExistException fileName: fullName) signal]].
	dir _ FileDirectory forFileName: fullName.
	localName _ FileDirectory localNameFor: fullName.
	dir
		deleteFileNamed: localName
		ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , fullName) signal].
	f _ self new open: fullName forWrite: true.
	^ f
		ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: fullName) signal]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:01'!
isAFileNamed: fileName
	"Answer true if a file of the given name exists."
	^self isAFileFullNamed: (self fullName: fileName)! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:01'!
newFileNamed: fileName
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, ask the user what to do."

	| fullName |
	fullName _ self fullName: fileName.

	^(self isAFileFullNamed: fullName)
		ifTrue: ["file already exists:"
			(FileExistsException fileName: fullName fileClass: self) signal]
		ifFalse: [self new open: fullName forWrite: true]

! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:01'!
oldFileNamed: fileName
	"Open an existing file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	| fullName |
	fullName _ self fullName: fileName.

	^(self isAFileFullNamed: fullName)
		ifTrue: [self new open: fullName forWrite: true]
		ifFalse: ["File does not exist..."
			(FileDoesNotExistException fileName: fullName) signal]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:02'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |

	"If full path name is not specified, get it assuming default directory."
	fullName _ self fullName: fileName.

	^ (self isAFileFullNamed: fullName)
		ifTrue: [ self readOnlyFileFullNamed: fullName ]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/30/2013 00:02'!
readOnlyFileNamed: fileName 
	"Open an existing file with the given name for reading."

	| fullName |
	fullName _ self fullName: fileName.
	^ self readOnlyFileFullNamed: fullName

	"StandardFileStream readOnlyFileNamed: 'kjsd.txt' "! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 7/29/2013 23:54'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: (FileDirectory localNameFor: fullFileName) , '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ FillInTheBlankMorph request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ self oldFileNamed: newName ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 7/29/2013 23:54'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName |
	dir _ FileDirectory forFileName: fullFileName.
	localName _ FileDirectory localNameFor: fullFileName.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir deleteFileNamed: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		^ self newFileNamed: newName].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 7/29/2013 23:57'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ FileDirectory forFileName: fullFileName.
	files _ dir fileNames.
	fileName _ FileDirectory localNameFor: fullFileName.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: (FileDirectory localNameFor: fullFileName), '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , FileDirectory slash , (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ self readOnlyFileNamed: newName].
	^ self error: 'Could not open a file'! !

