'From Cuis 4.2 of 25 July 2013 [latest update: #2509] on 28 September 2015 at 12:34:22.94527 pm'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 9/28/2015 12:29'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [
					FmDirectoryEntry currentDirectory
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


!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'jmv 9/28/2015 12:29'!
fileNamesMatching: pat
	"
	FmDirectoryEntry currentDirectory fileNamesMatching: '*'
	"

	^ self fileNames select: [ :each | pat match: each ]! !

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'jmv 9/28/2015 12:34'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version candidate |

	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits _ splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version _ 1 ]
			ifFalse: [ version _ (splits last at: 2) + 1 ].
	candidate _ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName.
	"all the above seems to fail on Cog on Linux. It looks like the OS file list is not updated immediately.
	To see this, just do 'file out and keep' twice on your current change set."
	[ candidate asFileEntry exists ] whileTrue: [
		version _ version + 1.
		candidate _ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName ].
	^ candidate! !


!FmFileIOAccessor reorganize!
('actions' asAbsolutePathName: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: directoryExists: directoryExists:in: fileExists: fileExists:in: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn:from: imageDirectoryPath lookupEntryIn:index: try:forFileNamed:)
('utilities' baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: extensionFor: fileNamesIn: splitNameVersionExtensionFor:)
('accessing' concreteStreamClass currentDriveName drives extensionDelimiter pathNameDelimiter slash)
('file stream creation' fileNamed: forceNewFileNamed: newFileNamed: readOnlyFileNamed:)
('testing' isCaseSensitive isDriveSupported onMac onUnix onWindows)
('primitives' imagePath primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primImagePath primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
!

