'From Cuis6.3 [latest update: #6235] on 21 February 2024 at 4:54:14 pm'!

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/21/2024 16:48:48'!
absolutePathString: aString
	"Argument must be an absolute path"

	^self pathTokens: (FileIOAccessor default expandTildeFor: aString) asPathTokens.! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/21/2024 16:46:43'!
pathTokens: tokens
	"Argument can be an absolute or relative path"

	(tokens notEmpty and: [self fileAccessor isDriveSupported])
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil.! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/21/2024 16:53:20'!
relativePathString: aString to: aDirectoryEntry
	"Argument must be a relative path"

	^self pathTokens: (FileIOAccessor default
		absolutePathComponentsFor: aString
		relativeTo: aDirectoryEntry).! !


!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/21/2024 16:53:25'!
pathString: aString
	"Argument can be an absolute or relative path"

	^aString isAbsolutePathName
		ifTrue: [ self absolutePathString: aString ]
		ifFalse: [ self relativePathString: aString to: DirectoryEntry currentDirectory ].! !

