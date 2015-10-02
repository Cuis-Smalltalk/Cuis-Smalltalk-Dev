'From Cuis 4.2 of 25 July 2013 [latest update: #2508] on 27 September 2015 at 11:35:54.295113 pm'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/27/2015 20:52'!
appendStream: blockWithArg 
	| stream |
	stream _ self privateAppendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/27/2015 20:28'!
forceWriteStream: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self privateForceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/27/2015 20:51'!
readStream: blockWithArg 
	"Raise FileDoesNotExistException if not found."
	| stream result |
	stream _ self privateReadStream.
	[ result _ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]].
	^ result! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/27/2015 20:46'!
writeStream: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self privateWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/27/2015 20:45'!
privateAppendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStream: that closes the file for you."

	self exists ifFalse: [
		^ self privateWriteStream ].
	^ (self fileAccessor fileNamed: self pathName) setToEnd! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/27/2015 20:30'!
privateForceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStream: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/27/2015 20:30'!
privateReadStream
	"Raise FileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStream: that closes the file for you."

	^ self fileAccessor readOnlyFileNamed: self pathName! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/27/2015 20:30'!
privateWriteStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStream: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor newFileNamed: self pathName! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 9/27/2015 23:28'!
fileOutSelections 
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStream: [ :stream |
				stream timeStamp.
				listSelections with: changeList do: [ :selected :item |
					selected ifTrue: [
						item fileOutOn: stream ]]]! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 9/27/2015 23:28'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	(self name, '.st') asFileEntry writeStream: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 9/27/2015 23:28'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [ FmDirectoryEntry smalltalkImageDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStream: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/27/2015 20:21'!
fileIn: anFmFileEntry
	"File in the entire contents of the file specified by the name provided"

	anFmFileEntry ifNil: [^ Smalltalk beep ].
	"Argument could also be a String"
	anFmFileEntry asFileEntry readStream: [ :stream |
		"horrendo. Esto hace self close ahi adentro!!!!!!!!!!"
		stream fileIn ]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 9/27/2015 23:28'!
fileOut

	(FillInTheBlankMorph request: 'Enter the file name' initialAnswer:'') asFileEntry writeStream: [ :stream | 
		sourceSystem isEmpty ifFalse:[
			stream nextChunkPut: sourceSystem printString; newLine ].
		self fileOutOn: stream.
		stream newLine; newLine.
		classes do: [ :cls |
			cls needsInitialize ifTrue: [
				stream newLine; nextChunkPut: cls name,' initialize']].
		stream newLine ]! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 9/27/2015 20:23'!
requirements
	"Answer my requirements"
	
	| answer |
	pathName asFileEntry readStream: [ :stream |
		answer _ (CodePackageFile buildFileStream: stream) requires ].
	^ answer! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/27/2015 20:22'!
install
	"Preconditions have been satisfied.  Install the required package."
	pathName asFileEntry readStream: [ :stream |
		"seguro que esta toronja cierra el stream ahi adentro!!!!!!"
		CodePackageFile basicInstallPackageStream: stream ].

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !


!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 9/27/2015 20:41'!
assureExistence
	self exists ifTrue: [^self].
	self parent assureExistence.
	self forceWriteStream: []! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/27/2015 23:29'!
appendContents: aStringOrBytes 
	self
		appendStream: [:str | 
			aStringOrBytes isString
				ifFalse: [str binary].
			str nextPutAll: aStringOrBytes]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/27/2015 20:25'!
binaryContents
	| answer |
	self readStream: [ :stream |
		answer _ self contentsOf: stream binary ].
	^ answer! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/27/2015 20:45'!
binaryContents: aByteArray
	self writeStream: [ :stream |
		"ojo que hace el close ahi adentro..."
		self setContentsOf: stream binary to: aByteArray ]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/27/2015 20:34'!
textContents
	^self contentsOf: self privateReadStream! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/27/2015 20:45'!
textContents: aString
	self writeStream: [ :stream |
		"ojo que hace el close ahi adentro..."
		self setContentsOf: stream to: aString ]! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 9/27/2015 20:43'!
pathName

	^ String streamContents: [ :stream |
		self printPathOn: stream ]! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'jmv 9/27/2015 20:44'!
pipe: filterBlock to: aFileEntryOrString 
	| nextEntry inStream outStream |
	nextEntry := aFileEntryOrString asFileEntry.
	[inStream := self privateReadStream.
	outStream := nextEntry privateWriteStream.
	filterBlock value: inStream value: outStream]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry
! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'jmv 9/27/2015 20:44'!
pipeRepeat: filterBlock while: terminateBlock to: aFileEntryOrString 
	| nextEntry inStream outStream |
	nextEntry := aFileEntryOrString asFileEntry.
	[inStream := self privateReadStream.
	outStream := nextEntry privateWriteStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry! !

!FmFileEntry methodsFor: 'cuis extensions' stamp: 'jmv 9/27/2015 23:29'!
form
	"Read a Form or ColorForm from the given file.
	'../../4.2.04.tiff' asFileEntry form display
	"

	| form |
	self readStream: [ :stream |
		form _ Form fromBinaryStream: stream binary ].
	^ form! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 9/27/2015 23:29'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| form |
	fileName asFileEntry readStream: [ :stream |
		form _ self fromBinaryStream: stream binary ].
	^ form! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 9/27/2015 23:28'!
fileOut

	(self name, '.st') asFileEntry writeStream: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!methodRemoval: FmFileEntry #appendStream!
FmFileEntry removeSelector: #appendStream!
!methodRemoval: FmFileEntry #appendStreamContents:!
FmFileEntry removeSelector: #appendStreamContents:!
!methodRemoval: FmFileEntry #forceWriteStream!
FmFileEntry removeSelector: #forceWriteStream!
!methodRemoval: FmFileEntry #forceWriteStreamContents:!
FmFileEntry removeSelector: #forceWriteStreamContents:!
!methodRemoval: FmFileEntry #readStream!
FmFileEntry removeSelector: #readStream!
!methodRemoval: FmFileEntry #readStreamContents:!
FmFileEntry removeSelector: #readStreamContents:!
!methodRemoval: FmFileEntry #readStreamIfError:!
FmFileEntry removeSelector: #readStreamIfError:!
!methodRemoval: FmFileEntry #textContents:by:!
FmFileEntry removeSelector: #textContents:by:!
!methodRemoval: FmFileEntry #textContentsBy:!
FmFileEntry removeSelector: #textContentsBy:!
!methodRemoval: FmFileEntry #tryReadStream!
FmFileEntry removeSelector: #tryReadStream!
!methodRemoval: FmFileEntry #tryWriteStream!
FmFileEntry removeSelector: #tryWriteStream!
!methodRemoval: FmFileEntry #writeStream!
FmFileEntry removeSelector: #writeStream!
!methodRemoval: FmFileEntry #writeStreamConfirming!
FmFileEntry removeSelector: #writeStreamConfirming!
!methodRemoval: FmFileEntry #writeStreamContents:!
FmFileEntry removeSelector: #writeStreamContents:!
!methodRemoval: FmFileEntry #writeStreamIfError:!
FmFileEntry removeSelector: #writeStreamIfError:!
!methodRemoval: String #fileContents:by:!
String removeSelector: #fileContents:by:!
!methodRemoval: String #fileContentsBy:!
String removeSelector: #fileContentsBy:!
