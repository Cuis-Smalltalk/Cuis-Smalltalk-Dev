'From Cuis 4.2 of 25 July 2013 [latest update: #2496] on 16 September 2015 at 12:01:36.476398 pm'!

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'jmv 9/16/2015 11:29'!
fileNamesMatching: pat
	"
	FmDirectoryEntry currentDirectory fileNamesMatching: '*'
	"

	^ self fileNames select: [ :each | pat match: each ]! !

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'jmv 9/16/2015 10:15'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version |

	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits _ splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version _ 1 ]
			ifFalse: [ version _ (splits last at: 2) + 1 ].
	^ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName! !

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'jmv 9/16/2015 11:05'!
nextNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 9/16/2015 11:02'!
fileOutSelections 
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStreamContents: [ :stream |
				stream timeStamp.
				listSelections with: changeList do: [ :selected :item |
					selected ifTrue: [
						item fileOutOn: stream ]]]! !


!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2015 08:30'!
test: byteCount fileName: fileName
	"
	AsyncFile new test: 10000 fileName: 'testData'
	"

	| buf1 buf2 bytesWritten bytesRead |
	buf1 _ String new: byteCount withAll: $x.
	buf2 _ String new: byteCount.
	self open: fileName asAbsolutePathName forWrite: true.
	self primWriteStart: fileHandle
		fPosition: 0
		fromBuffer: buf1
		at: 1
		count: byteCount.
	semaphore wait.
	bytesWritten _ self primWriteResult: fileHandle.
	self close.
	
	self open: fileName asAbsolutePathName forWrite: false.
	self primReadStart: fileHandle fPosition: 0 count: byteCount.
	semaphore wait.
	bytesRead _
		self primReadResult: fileHandle
			intoBuffer: buf2
			at: 1
			count: byteCount.
	self close.

	buf1 = buf2 ifFalse: [self error: 'buffers do not match'].
	^ 'wrote ', bytesWritten printString, ' bytes; ',
	   'read ', bytesRead printString, ' bytes'
! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 9/16/2015 11:02'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	(self name, '.st') asFileEntry writeStreamContents: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 9/16/2015 11:02'!
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
		
	nameToUse asFileEntry writeStreamContents: [ :stream |
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


!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/16/2015 11:58'!
fileIn: anFmFileEntry
	"File in the entire contents of the file specified by the name provided"

	|  stream |
	anFmFileEntry ifNil: [^ Smalltalk beep ].
	"Argument could also be a String"
	stream _ anFmFileEntry asFileEntry readStream.
	stream fileIn! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/16/2015 10:30'!
install: anFmFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	| fileEntry |
	"Argument could also be a String"
	fileEntry _ anFmFileEntry asFileEntry.
	ChangeSet installing: fileEntry  name do: [ self fileIn: fileEntry  ].
	('Installed ChangeSet: ', fileEntry  name) print! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/16/2015 10:23'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ 'CoreUpdates' asDirectoryEntry.
	updatesFileDirectory exists ifFalse: [
		updatesFileDirectory _ 'Cuis-Smalltalk-Dev/CoreUpdates' asDirectoryEntry ].

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ].! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/16/2015 10:30'!
installNewUpdates: updatesFileDirectory

	(self newUpdates: updatesFileDirectory) do: [ :each |
		self install: each ].
	Smalltalk aboutThisSystem! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/16/2015 10:28'!
newUpdates: updatesFileDirectory
	^(updatesFileDirectory files select: [ :each |
		each name asNumber > SystemVersion current highestUpdate])
			asSortedCollection: [ :a :b | a name < b name ]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 9/16/2015 11:01'!
fileOut

	(FillInTheBlankMorph request: 'Enter the file name' initialAnswer:'') asFileEntry writeStreamContents: [ :stream | 
		sourceSystem isEmpty ifFalse:[
			stream nextChunkPut: sourceSystem printString; newLine ].
		self fileOutOn: stream.
		stream newLine; newLine.
		classes do: [ :cls |
			cls needsInitialize ifTrue: [
				stream newLine; nextChunkPut: cls name,' initialize']].
		stream newLine ]! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 9/16/2015 11:03'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| form |
	fileName asFileEntry readStreamContents: [ :stream |
		form _ self fromBinaryStream: stream binary ].
	^ form! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 9/16/2015 11:04'!
fileOut

	(self name, '.st') asFileEntry writeStreamContents: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!methodRemoval: SystemOrganizer #fileOut!
SystemOrganizer removeSelector: #fileOut!
