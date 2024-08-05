'From Cuis7.1 [latest update: #6563] on 5 August 2024 at 3:31:10 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/5/2024 15:27:23'!
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
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/5/2024 15:27:32'!
logSnapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	msg := self snapshotMessageFor: save andQuit: quit.
	(SourceFiles at: 2) ifNotNil: [
		save ifTrue: [
			LastQuitLogPosition := (SourceFiles at: 2)
				 setToEnd;
				 position ]].
	self logChange: msg.
	Transcript
		 newLine;
		 show: msg;
		 newLine! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 8/5/2024 15:28:13'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
		oldUserChanges exists ifTrue: [
			directory := oldUserChanges parent.
			oldUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 8/5/2024 15:27:28'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	self logToUserChanges: aString preamble: preambleOrNil! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 8/5/2024 15:25:17'!
setStartupStamp

	| dateAndTime |
	dateAndTime := DateAndTime now.
	StartupStamp := String streamContents: [ :stream |
		stream nextPutAll: '----STARTUP---- ('.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream
			nextPutAll: ') as ';
			nextPutAll: self imageName ].
	self assureStartupStampLogged.
! !

