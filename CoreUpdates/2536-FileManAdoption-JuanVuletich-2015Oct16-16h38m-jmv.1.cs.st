'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 5:16:17.619429 pm'!

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/16/2015 17:11'!
privateFileNamed: aFileEntry
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass new open: aFileEntry pathName forWrite: true! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/16/2015 17:11'!
privateForceNewFile: fileEntry
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| pathName |
	pathName _ fileEntry pathName.
	fileEntry exists
		ifFalse: [
			^ (self concreteStreamClass new open: pathName forWrite: true)
				ifNil: ["Failed to open the file"
					(FileDoesNotExistException fileName: pathName) signal]].

	self deleteFile: pathName
		ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , pathName) signal].

	^ (self concreteStreamClass new open: pathName forWrite: true)
		ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: pathName) signal ]! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/16/2015 17:11'!
privateNewFile: fileEntry
	"Create a new file with the given full pathName."

	^fileEntry exists
		ifTrue: [
			(FileExistsException fileName: fileEntry pathName fileClass: self concreteStreamClass) signal]
		ifFalse: [
			self concreteStreamClass new open: fileEntry pathName forWrite: true ]! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/16/2015 17:11'!
privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	^(self concreteStreamClass new open: pathName forWrite: false)
		ifNil: ["File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal]! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 16:33'!
defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, FileDirectory slash, SourceFileVersionString, '.sources'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/16/2015 16:55'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FmFileIOAccessor default copy: oldChangesName asFileEntry to: newChangesName asFileEntry ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !


!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/16/2015 16:55'!
copyTo: aFileEntryOrString 
	| targetEntry |
	(self fileAccessor directoryExists: self pathName) ifTrue: [^self asDirectoryEntry copyTo: aFileEntryOrString].
	
	targetEntry := aFileEntryOrString asFileEntry.
	targetEntry isDirectory
		ifTrue: [targetEntry := (targetEntry / self name) ensureAsFile].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/16/2015 17:00'!
privateAppendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStream: that closes the file for you."

	self exists ifFalse: [
		^ self privateWriteStream ].
	^ (self fileAccessor privateFileNamed: self) setToEnd! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/16/2015 16:57'!
privateForceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStream: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateForceNewFile: self! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/16/2015 16:53'!
privateReadStream
	"Raise FileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStream: that closes the file for you."

	^ self fileAccessor privateReadOnlyFile: self! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/16/2015 16:51'!
privateWriteStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStream: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 10/16/2015 16:56'!
copy: fromFileEntry to: toFileEntry 
	| readStr writeStr |
	[readStr := (self privateReadOnlyFile: fromFileEntry) binary.
	writeStr := (self privateForceNewFile: toFileEntry) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [
			readStr
				ifNotNil: [ :r | r close ].
			writeStr
				ifNotNil: [ :w | w close ]]! !

!methodRemoval: StandardFileStream class #privateFileNamed:!
StandardFileStream class removeSelector: #privateFileNamed:!
!methodRemoval: StandardFileStream class #privateForceNewFileNamed:!
StandardFileStream class removeSelector: #privateForceNewFileNamed:!
!methodRemoval: FmFileIOAccessor #fileNamed:!
FmFileIOAccessor removeSelector: #fileNamed:!
!methodRemoval: FmFileIOAccessor #forceNewFileNamed:!
FmFileIOAccessor removeSelector: #forceNewFileNamed:!
!methodRemoval: FmFileIOAccessor #newFileNamed:!
FmFileIOAccessor removeSelector: #newFileNamed:!
!methodRemoval: FmFileIOAccessor #privateNewFileNamed:!
FmFileIOAccessor removeSelector: #privateNewFileNamed:!
!methodRemoval: FmFileIOAccessor #readOnlyFileNamed:!
FmFileIOAccessor removeSelector: #readOnlyFileNamed:!
