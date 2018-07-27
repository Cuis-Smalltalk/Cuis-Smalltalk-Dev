'From Cuis 5.0 of 7 November 2016 [latest update: #3377] on 26 July 2018 at 6:28:18 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 7/26/2018 18:22:27'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file _ SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			header _ String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
				stream newLine; nextPut: $!!.	"directly"
				header _ String streamContents: [:strm | strm nextPutAll: self name;
					nextPutAll: ' commentStamp: '.
					aStamp storeOn: strm.
					strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
				stream nextChunkPut: header.
				stream newLine; nextChunkPut: aString.
			].
	].
	SystemChangeNotifier uniqueInstance classCommented: self! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 7/26/2018 18:22:23'!
fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	classComment ifNotNil: [
		aFileStream newLine.
		fileComment _ RemoteString newString: classComment text
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment _ fileComment].
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; nextChunkPut: classComment text ]]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 7/26/2018 18:22:32'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles notNil and: [(file _ SourceFiles at: fileIndex) notNil]) ifTrue: [

		Smalltalk assureStartupStampLogged.
		file setToEnd.

		preambleBlock value: file.  "Write the preamble"
		remoteString _ RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.

		file nextChunkPut: ' '.
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]
	]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 7/26/2018 17:58:26'!
logSnapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	self assureStartupStampLogged.
	msg _ self snapshotMessageFor: save andQuit: quit.
	(SourceFiles at: 2) ifNotNil: [
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ]].
	self logChange: msg.
	Transcript
		 newLine;
		 show: msg;
		 newLine! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/26/2018 18:23:44'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp asString; newLine.
			self forceChangesToDisk ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp asString; newLine ].
	StartupStamp _ nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/26/2018 17:58:32'!
logChange: aStringOrText 
	"Write the argument, aString, onto the changes file."
	^ self logChange: aStringOrText preamble: nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/26/2018 18:27:21'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString _ aStringOrText asString.
	(aString findFirst: [:char | char isSeparator not]) = 0
		ifTrue: [^ self].  "null doits confuse replay"
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString ]]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 7/26/2018 18:15:37'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	Utilities logsUserChanges: false.
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | 
			[ self atEnd ] whileFalse: [
					barBlock value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

