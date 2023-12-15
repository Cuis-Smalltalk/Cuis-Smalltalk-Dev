'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:44:34 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 12/15/2023 15:44:54'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: Smalltalk imageName pathAndLocalName second), 
		(Preferences at: #userChangesFileNameExtension)! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:46:30'!
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
	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
				stream newLine; nextPut: $!!.	"directly"
				header := String streamContents: [:strm | strm nextPutAll: self name;
					nextPutAll: ' commentStamp: '.
					aStamp storeOn: strm.
					strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
				stream nextChunkPut: header.
				stream newLine; nextChunkPut: aString.
			].
	].
	SystemChangeNotifier uniqueInstance classCommented: self! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:46:37'!
fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileComment := RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment := fileComment].
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 12/15/2023 15:46:44'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	Smalltalk assureStartupStampLogged.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString := RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 12/15/2023 15:47:15'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges := DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName.
		oldUserChanges exists ifTrue: [
			directory := oldUserChanges parent.
			oldUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].
	StartupStamp := nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 12/15/2023 15:46:57'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString.
			stream flush ]]! !

