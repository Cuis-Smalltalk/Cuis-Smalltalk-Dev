'From Cuis 4.2 of 25 July 2013 [latest update: #1776] on 29 July 2013 at 11:48:07.65888 pm'!

!FileStream commentStamp: '<historical>' prior: 0!
I represent a Stream that accesses a FilePage from a File. One use for my instance is to access larger "virtual Strings" than can be stored contiguously in main memory. I restrict the objects stored and retrieved to be Integers or Characters. An end of file pointer terminates reading; it can be extended by writing past it, or the file can be explicitly truncated.
	
To use the file system for most applications, you typically create a FileStream. This is done by sending a message to a FileDirectory (file:, oldFile:, newFile:, rename:newName:) which creates a (sub)instance of me. Accesses to the file are then done via this instance.

*** On DOS, files cannot be shortened!!  ***  To overwrite a file with a shorter one, first delete the old file (FileDirectory deleteFilePath: 'Hard Disk:aFolder:dataFolder:foo') or (aFileDirectory deleteFileNamed: 'foo').  Then write your new shorter version.

Usually, the active class is StandardFileStream. But regardless of this, it is conventional to reference FileStream in code. This allows replacing StandardFileStream with something else (and not a real file in the host OS file system) if needed.!


!FileStream commentStamp: '<historical>' prior: 0!
I represent a Stream that accesses a FilePage from a File. One use for my instance is to access larger "virtual Strings" than can be stored contiguously in main memory. I restrict the objects stored and retrieved to be Integers or Characters. An end of file pointer terminates reading; it can be extended by writing past it, or the file can be explicitly truncated.
	
To use the file system for most applications, you typically create a FileStream. This is done by sending a message to a FileDirectory (file:, oldFile:, newFile:, rename:newName:) which creates a (sub)instance of me. Accesses to the file are then done via this instance.

*** On DOS, files cannot be shortened!!  ***  To overwrite a file with a shorter one, first delete the old file (FileDirectory deleteFilePath: 'Hard Disk:aFolder:dataFolder:foo') or (aFileDirectory deleteFileNamed: 'foo').  Then write your new shorter version.

Usually, the active class is StandardFileStream. But regardless of this, it is conventional to reference FileStream in code. This allows replacing StandardFileStream with something else (and not a real file in the host OS file system) if needed.!


!FileDoesNotExistException methodsFor: 'initialize-release' stamp: 'jmv 7/29/2013 23:46'!
initialize
	super initialize.
	readOnly _ false! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/29/2013 23:16'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |

	"If full path name is not specified, get it assuming default directory."
	fullName _ self fullName: fileName.

	^ (self isAFileNamed: fullName)
		ifTrue: [ self readOnlyFileNamed: fullName ]! !

!StandardFileStream class methodsFor: 'dnd requests' stamp: 'jmv 7/29/2013 23:18'!
requestDropStream: dropIndex
	"Request a read-only stream for some file that was dropped onto Squeak"
	^self new requestDropStream: dropIndex! !


!FileList methodsFor: 'file list' stamp: 'jmv 7/29/2013 23:42'!
readOnlyStream
	"Answer a read-only stream on the selected file. For the various stream-reading services."

	^self directory ifNotNil: [ :dir | dir readOnlyFileNamed: fileName ]! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/29/2013 23:36'!
condenseChanges	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseChanges"

	| f oldChanges classCount |
	f _ FileStream fileNamed: 'ST80.temp'.
	f timeStamp.
'Condensing Changes File...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: Smalltalk classNames size
	during:
		[:bar | classCount _ 0.
		Smalltalk allClassesDo:
			[:class | bar value: (classCount _ classCount + 1).
			class moveChangesTo: f.
			class putClassCommentToCondensedChangesFile: f.
			class class moveChangesTo: f]].
	LastQuitLogPosition _ f position.
	f close.

	CompiledMethod allInstancesDo:
		[ : e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChanges close.
	FileDirectory default 
		deleteFileNamed: oldChanges name , '.old';
		rename: oldChanges name toBe: oldChanges name , '.old';
		rename: f name toBe: oldChanges name.
	self setMacFileInfoOn: oldChanges name.
	SourceFiles at: 2
			put: (FileStream oldFileNamed: oldChanges name).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/29/2013 23:37'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Cuis cannot locate 
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ FileStream oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ FileStream oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ FileStream oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !


!FileDoesNotExistException methodsFor: 'exceptionDescription' stamp: 'jmv 7/29/2013 23:46'!
defaultAction
	"The default action taken if the exception is signaled."


	^ readOnly
		ifTrue: [ StandardFileStream readOnlyFileDoesNotExistUserHandling: fileName ]
		ifFalse: [ StandardFileStream fileDoesNotExistUserHandling: fileName ]! !


!FileDoesNotExistException class methodsFor: 'examples' stamp: 'jmv 7/29/2013 23:31'!
example
	"
	FileDoesNotExistException example
	"

	| result |
	result _ [(FileStream readOnlyFileNamed: 'error42.log') contentsOfEntireFile]
		on: FileDoesNotExistException
		do: [:ex | 'No error log'].
	Transcript show: result; newLine! !


!FileExistsException methodsFor: 'exceptionDescription' stamp: 'jmv 7/29/2013 23:39'!
defaultAction
	"The default action taken if the exception is signaled."

	^ fileClass fileExistsUserHandling: fileName! !


!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:34'!
fileNamed: localFileName
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist."

	^ FileStream fileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
forceNewFileNamed: localFileName
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking."

	^ FileStream forceNewFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
newFileNamed: localFileName
	"Create a new file with the given name in this directory.
	If the file already exists, give the chance to pick another name or overwrite it."

	^ FileStream newFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
oldFileNamed: localFileName
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort."

	^ FileStream oldFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
readOnlyFileNamed: localFileName
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort."

	^ FileStream readOnlyFileNamed: (self fullNameFor: localFileName)
! !


!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:13'!
fileNamed: fileName 
	^ self concreteStream fileNamed: fileName! !

!FileStream class methodsFor: 'instance creation'!
fullName: fileName
	^ FileDirectory default fullNameFor: fileName! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:15'!
isAFileNamed: fileName
	"return whether a file exists with the given name"
	^self concreteStream isAFileNamed: fileName! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:15'!
newFileNamed: fileName 
	^ self concreteStream newFileNamed: fileName! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:16'!
oldFileNamed: fileName 
	^ self concreteStream oldFileNamed: fileName! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:17'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."
	^ self concreteStream oldFileOrNoneNamed: fileName! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 7/29/2013 23:17'!
readOnlyFileNamed: fileName 
	^ self concreteStream readOnlyFileNamed: fileName! !

!FileStream class methodsFor: 'dnd requests' stamp: 'jmv 7/29/2013 23:18'!
requestDropStream: dropIndex
	"Request a read-only stream for some file that was dropped onto Squeak"
	^ self concreteStream requestDropStream: dropIndex! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 7/29/2013 23:14'!
isAFileNamed: fileName
	"Answer true if a file of the given name exists."

	| f |
	f _ self new open: (self fullName: fileName) forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true
! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/29/2013 23:36'!
clear
	| stream |
	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		
		logToFile ifTrue: [
			stream _ FileStream forceNewFileNamed: self filename.
			[
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine
			] ensure: [ stream close ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/29/2013 23:36'!
clearFile
	| stream |
	accessSemaphore critical: [
		stream _ FileStream forceNewFileNamed: self filename.
		[
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine
		] ensure: [ stream close ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 7/29/2013 23:35'!
addEntry: aString logToFile: otherString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| stream |
	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [
			firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		"external file"
		otherString ifNotNil: [
			[
				stream _ FileStream fileNamed: self filename.
				stream
					setToEnd;
					nextPutAll: otherString;
					flush]
			ensure: [ stream close ]
		]
	]! !

!methodRemoval: StandardFileStream class #fullName:!
StandardFileStream class removeSelector: #fullName:!

!StandardFileStream class reorganize!
('file creation' crc16OfFileNamed: fileNamed: forceNewFileNamed: isAFileNamed: newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileNamed:)
('registry' register: registry retryWithGC:until:forFileNamed: unregister:)
('error handling' fileDoesNotExistUserHandling: fileExistsUserHandling: readOnlyFileDoesNotExistUserHandling:)
('dnd requests' requestDropStream:)
('services')
!

!methodRemoval: FileStream class #creationAndModificationTimeOf:!
FileStream class removeSelector: #creationAndModificationTimeOf:!
!methodRemoval: FileStream class #unload!
FileStream class removeSelector: #unload!

!FileStream class reorganize!
('instance creation' fileNamed: forceNewFileNamed: fullName: isAFileNamed: new newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileNamed:)
('concrete classes' concreteStream)
('dnd requests' requestDropStream:)
!

!methodRemoval: FileExistsException #fileClass!
FileExistsException removeSelector: #fileClass!
!methodRemoval: FileDoesNotExistException #readOnly!
FileDoesNotExistException removeSelector: #readOnly!

!FileDoesNotExistException reorganize!
('accessing' readOnly:)
('exceptionDescription' defaultAction)
('initialize-release' initialize)
!

!methodRemoval: FileStreamException #fileName!
FileStreamException removeSelector: #fileName!
