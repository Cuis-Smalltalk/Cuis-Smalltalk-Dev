'From Cuis7.9 [latest update: #8035] on 30 June 2026 at 2:19:48 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 30/Jun/2026, 6:02:43 pm (UTC)'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared,
	then ask where to save, suggesting the NewPackages folder."
	fullFileName ifNil: [
		fullFileName :=
			self request: 'Save on path: '
				initialAnswer: (DirectoryEntry newPackagesDirectory // self packageFileName) pathName
				orCancel: [ ^self ]].

	"If autoNumberPackages is set, back up the current package file."
	(Preferences at: #autoNumberPackages) ifTrue: [ | backUpName oldFile directory |
		oldFile := fullFileName asFullFileEntry.
		oldFile exists ifTrue: [
			directory := oldFile parent.
			backUpName := directory
				nextNameFor: oldFile nameWithoutExtension
				coda: (Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai]), '.pck'
				extension: 'st'.
			oldFile rename: backUpName ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self).! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 30/Jun/2026, 6:02:13 pm (UTC)'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and a unique numeric tag."

	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := folder
		nextNameFor: nameToUse
		coda: (Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai])
		extension: 'cs.st'.

	folder // nameToUse writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream writeSystemTimeStamp.
			self fileOutPreambleOn: fileStream.
			self fileOutOn: fileStream.
			self fileOutPostscriptOn: fileStream ]].

	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ].! !


!DirectoryEntry methodsFor: 'services' stamp: 'jmv 30/Jun/2026, 6:01:12 pm (UTC)'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	It can be nil.
	"

	| files splits version candidate |
	files := self fileNamesMatching: (baseFileName,'*.', extension).
	splits := files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits := splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version := 1 ]
			ifFalse: [ version := (splits last at: 2) + 1 ].
	candidate := (baseFileName, (fileNameCoda ifNil: ['']), '.', (String streamContents: [ :strm | version printOn: strm length: 3 zeroPadded: true ]), '.', extension) asFileName.
	^ candidate! !

