'From Cuis7.5 [latest update: #7648] on 9 October 2025 at 9:34:30 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/9/2025 21:32:08'!
userChangesAppendDo: aBlock
	"Convenience. On write errors, write a message to Transcript and continue."

	[
		Utilities logsUserChanges ifTrue: [
			DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: aBlock ]]
		on: Error do: [ :ex |
			Transcript show: 'Can not log to User changes. '; show: ex  printString; newLine ].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/9/2025 21:33:04'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr := self organization commentRemoteStr.
	(aString isEmpty) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr := oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file := SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			header := String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].
	SystemChangeNotifier uniqueInstance classCommented: self.! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/9/2025 21:33:08'!
fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileComment := RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment := fileComment].
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 10/9/2025 21:33:12'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString := RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/9/2025 21:33:20'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	"Note: This renames the existing user changes file, and creates a new one."
	| changesFile directory oldUserChanges |
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	[
		(Preferences at: #autoNumberUserChanges) ifTrue: [
			oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
			oldUserChanges exists ifTrue: [
				directory := oldUserChanges parent.
				PreviousUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
				oldUserChanges rename: PreviousUserChangesName ]]]
		on: Error do: [].
	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/9/2025 21:33:26'!
logToUserChanges: aString preamble: preambleOrNil

	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		preambleOrNil ifNotNil: [
			stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
		stream nextChunkPut: aString.
		stream flush ].! !


!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 10/9/2025 21:28:43'!
userBaseDirectory
	"The top directory for files generated by the system on behalf of the user
		userBaseDirectory/SavedChangeSets 						Folder for saving new ChangeSets
		userBaseDirectory/Packages: 								Folder for new code packages
		userBaseDirectory/UserChanges: 							Folder to store user changes files
		userBaseDirectory/FileOuts: 								Folder for filed out code
		userBaseDirectory/Logs: 									Folder for error logs and other logs
	DirectoryEntry userBaseDirectory
	See #cuisBaseDirectory. See #currentDirectory."

	UserBaseDirectory notNil ifTrue: [
		^ UserBaseDirectory ].

	self workingDirectory ifNotNil: [ :wd |
		"Don't use / as the UserBaseDirectory, as most likely we don't have rights to write to it.
		This could happen if a proper #workingDirectory is not set by the host os, for instance because
		Cuis was started from drag and drop in an os file explorer tool."
		wd isRoot ifFalse: [
			UserBaseDirectory := wd ]].

	"If no OS current directory, use a sibling of #cuisBaseDirectory
	If OS current directory is same as #cuisBaseDirectory, take a sibling too"
	(UserBaseDirectory isNil or: [ UserBaseDirectory = self cuisBaseDirectory ]) ifTrue: [
		UserBaseDirectory := self cuisBaseDirectory parent / (self cuisBaseDirectory name, '-UserFiles').
		UserBaseDirectory assureExistence ].

	^ UserBaseDirectory! !

