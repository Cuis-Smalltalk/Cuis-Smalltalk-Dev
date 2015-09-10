'From Cuis 4.2 of 25 July 2013 [latest update: #2477] on 9 September 2015 at 8:44:40.02546 pm'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 9/3/2015 23:07'!
readStreamIfError: errorHandlingBlock
	
	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FmFileIOAccessor fileDoesNotExistException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 9/3/2015 23:07'!
tryReadStream
	
	^ self fileAccessor readOnlyFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/9/2015 20:35'!
tryWriteStream
	"If the file already exists, give the chance to pick another name or overwrite it."
	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 9/3/2015 23:21'!
writeStreamIfError: errorHandlingBlock
	self refresh.
		
	^[self fileAccessor newFileNamed: self pathName]
		on: FmFileIOAccessor fileExistsException
		do: [:ex | errorHandlingBlock value: ex]! !


!FmFileIOAccessor class methodsFor: 'accessing exception' stamp: 'mu 9/3/2015 23:25'!
fileExistsException
	^self dialectUtilClass fileExistsException! !


!FmPortableUtil class methodsFor: 'accessing' stamp: 'mu 9/3/2015 23:26'!
fileExistsException
	^nil! !


!FmSqPortableUtil class methodsFor: 'accessing' stamp: 'mu 9/3/2015 23:17'!
fileExistsException
	^FileExistsException! !


!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 6/6/2015 21:13'!
concatPathComponents: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.
	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := self fileEntryClass pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry asDirectoryEntry ].
		parentEntry := entry ].
	^entry! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/9/2015 20:33'!
readStream
	"Usually you might prefer #tryReadStream, that doesn't handle the possible
		FileDoesNotExistException
	This method, instead, creates a new, empty file, that has nothing to be read."
	^self readStreamIfError: [:ex |
		self assureExistence.
		ex retry]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/9/2015 20:35'!
writeStream
	"If the file already exists, delete it first without asking."
	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 6/6/2015 21:09'!
setParent: anFmFileEntry
	parent := anFmFileEntry! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'jmv 6/6/2015 21:42'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self slash.
	tokens ifEmpty: [^ nil].
	self isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName isDriveName
				ifTrue: [
					self drive: guessedDriveName.
					tokens := tokens copyFrom: 2 to: tokens size.]].
	self pathComponents: tokens! !


!FmSqPortableUtil class methodsFor: 'startUp' stamp: 'mu 9/3/2015 22:27'!
startUp
	FmFileIOAccessor onSystemStartUp! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 9/9/2015 20:40'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| file form |
	file _ fileName asFileEntry tryReadStream binary.
	form _ self fromBinaryStream: file.
	file close.
	^ form
! !

