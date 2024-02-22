'From Cuis6.3 [latest update: #6236] on 22 February 2024 at 2:13:49 pm'!

!FileSystemEntry commentStamp: 'jmv 2/22/2024 14:00:01' prior: 0!
I represent a single file entry (including directory).

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See DirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFullFileEntry fileSize
	Smalltalk imageName asFullFileEntry parent directories do: [ :a | a print ]!


!FileEntry commentStamp: 'jmv 2/22/2024 14:00:07' prior: 0!
I represent a single file entry.

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See DirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFullFileEntry fileSize
	Smalltalk imageName asFullFileEntry parent directories do: [ :a | a print ]!


!ChangeList class methodsFor: 'public access' stamp: 'jmv 2/22/2024 10:19:25'!
browseRecentLog
	"ChangeList browseRecentLog"
	"Prompt with a menu of how far back to go to browse the current image's changes log file"
	^ self
		browseRecentLogOn: Smalltalk currentChangesName asFullFileEntry
		startingFrom: Smalltalk lastQuitLogPosition! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 2/22/2024 13:50:41'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp prevPos prevFileIndex preamble tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod stampFrom: tokens.
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorMethodRef |
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 2/22/2024 13:50:48'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod := class.
	oldCommentRemoteStr := class  organization commentRemoteStr.
	currentCompiledMethod := oldCommentRemoteStr.
	selectorOfMethod := #Comment.
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy := SourceFiles collect: [ :x | x ifNotNil: [x name asFullFileEntry readStream]].
	position := oldCommentRemoteStr position.
	file := sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp := '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos := nil ].
		position := prevPos.
		prevPos notNil ifTrue: [ file := sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 2/22/2024 13:50:54'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file := SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFullFileEntry readStreamDo: [ :stream | | p |
			preamble := ''.
			p := 0 max: self filePosition.
			p < stream size ifTrue: [
				"It appears that on Linux systems, immediately after adding a new method, the OS file cache may not be updated, so contents may still not be accessible. Protect against that."
				stream position: p.
				"Skip back blank space."
				stream backChunk.
				"Find and answer preamble chunk."
				preamble := stream backChunk ]]
	] on: FileDoesNotExistException do: [ :ex | preamble := '' ].
	^ preamble! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 2/22/2024 13:46:22'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class | 
					barBlock value: (classCount := classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition := f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFullFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 2/22/2024 13:47:32'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFullFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFullFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:51:17'!
filenameForUpdatedImage: aBoolean
	"If aBoolean, include highest update number	"
	| fileName newName changesName systemVersion |
	systemVersion := SystemVersion current.
	fileName := String streamContents: [ :strm |
		strm nextPutAll: systemVersion versionString.
		aBoolean ifTrue: [
			strm
				nextPut: $-;
				print: systemVersion highestUpdate ].
		self wordSize = 4 ifTrue: [
			strm nextPutAll: '-32' ]].
	newName := fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName := DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName := self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFullFileEntry exists ifTrue: [
		self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.'.
		^nil  ].
	^newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:56:14'!
saveAndQuitAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: true embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:56:20'!
saveAndStayAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:59:49'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self request: 'New file name?' initialAnswer: self imageName asFullFileEntry name do: [:newName|
		((((self fullNameForImageNamed: newName) asFullFileEntry exists not
			and: [(self fullNameForChangesNamed: newName) asFullFileEntry exists not])
			or: [self confirm: ('{1} already exists. Overwrite?' format: {newName})]))
					ifTrue: [
						self saveAs: newName clearAllClassState: false]]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:56:25'!
saveAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/22/2024 13:59:53'!
snapshotMessageFor: save andQuit: quit

	| dateAndTime |
	dateAndTime := DateAndTime now.
	^String streamContents: [ :stream |
		stream 
			nextPutAll: self tagHeader;
			nextPutAll: (self snapshotTagFor: save andQuit: quit);
			nextPutAll: self tagTail.
		stream nextPut: $(.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream nextPut: $).
		stream
			space;
			nextPutAll: self imageName asFullFileEntry name;
			nextPutAll: ' priorSource: ';
			print: LastQuitLogPosition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/22/2024 13:49:06'!
externalizeSources   
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName := self defaultSourcesName.
	sourcesName asFullFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: sourcesName asFullFileEntry readStream.

	changesName := self defaultChangesName.
	changesName asFullFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: changesName asFullFileEntry appendStream.

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/22/2024 13:49:25'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg lastQuitLogPosition snapshotChunk entryPathName |

	"Do not open source files if internalized (i.e. notNil)"
	sources := SourceFiles at: 1.
	sources ifNil: [
		entry := self defaultSourcesName asFullFileEntry.
		entry exists ifFalse: [
			entry := self alternativeSourcesName asFullFileEntry ].
		entry exists ifTrue: [
			sources := [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg := 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
			self logStartupError: msg.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes := SourceFiles at: 2.
	changes ifNil: [
		entry := self defaultChangesName asFullFileEntry.
		entryPathName := entry pathName.
		"Read the last snapshot chunk. Validate that the .changes file is appropriate for this image."
		entry exists
			ifTrue: [
				lastQuitLogPosition := self lastQuitLogPosition.
				lastQuitLogPosition > 0 ifTrue: [
					entry readStreamDo: [ :changesFile |
						changesFile position: lastQuitLogPosition.
						snapshotChunk := changesFile nextChunk ].
					((snapshotChunk beginsWith: self tagHeader) and: [
						snapshotChunk includesSubString: 'priorSource: ']) ifFalse: [
							(Preferences at: #warnIfNoChangesFile) ifTrue: [
								self logStartupError:
									'Incorrect changes file: ', entryPathName, String newLineString,
									'Missing code will be decompiled', String newLineString,
									'New source code will not be saved' ].
							entry := nil ]]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [
					self logStartupError:
						'Could not find changes file: ', entryPathName, String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved' ].
				entry := nil ].
		entry ifNotNil: [
			changes := [ entry appendStream ]
				on: FileWriteError
				do: [
					self logStartupError:
						'Could not write to changes file: ', entryPathName, String newLineString,
						'Changes file will not be used.', String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved'.
					nil ]]].
	ChangesInitialFileSize := changes ifNotNil: [ changes position ].

	SourceFiles := Array with: sources with: changes.! !

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 2/22/2024 13:59:44'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ self imageName asFullFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			uint32At: 1
			bigEndian: self isBigEndian ]! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 2/22/2024 13:52:21'!
restoreLostChangesManually

	ChangeList browseFrom: LastQuitLogPosition on: self currentChangesName asFullFileEntry labeled: 'Lost changes'
! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 2/22/2024 13:52:26'!
withChangesFileDo: aBlock

	^self currentChangesName asFullFileEntry readStreamDo: aBlock! !


!UniFileStream methodsFor: 'access' stamp: 'jmv 2/22/2024 14:09:10'!
fileEntry
	^ self fullName asFullFileEntry! !


!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 2/22/2024 14:06:34'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection := (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 1 ifTrue: [
		^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue: [
		newName := self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFullFileEntry ].
	^ self error: 'Could not open a file'! !

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 2/22/2024 14:06:39'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFullFileEntry ].
	^ self error: 'Could not open a file'! !


!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 2/22/2024 10:14:19'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFullFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFullFileEntry exists or: [ newFileFullName asFullDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 2/22/2024 10:15:39'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFullDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asFullDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asFullDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !


!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/22/2024 10:16:22'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	DirectoryEntry roots
	"
	^ FileIOAccessor default drives
		ifEmpty: [
			"On Linux and MacOsX"
			{ '/' asFullDirectoryEntry } ]! !


!CodePackage methodsFor: 'naming' stamp: 'jmv 2/22/2024 10:18:57'!
packageDirectory

	^self packageDirectoryName asFullDirectoryEntry! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 2/22/2024 10:18:31'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName :=
			(DirectoryEntry newPackagesDirectory // self packageFileName) pathName ].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 2/22/2024 14:12:36'!
codePackageFile
	codePackageFile isNil ifTrue: [
		codePackageFile := CodePackageFile onFileEntry: pathName asFullFileEntry.
		"?"
		pathName := nil ].
	^codePackageFile! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 2/22/2024 14:13:04'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFullFileEntry exists ifTrue: [ ^ true ]].
	packageFileName := self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry := directory // packageFileName.
		entry exists ifTrue: [
			"Try this one. If success, keep it."
			self pathName: entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			"Nope. Don't keep it."
			self pathName: nil ]].
	^ false! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 2/22/2024 14:12:47'!
placesToLookForPackagesDo: aBlock

	| myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir := pathName asFullFileEntry parent.
		aBlock value: myDir ].

	"Look in codePackageFile folder"
	codePackageFile ifNotNil: [
		myDir := codePackageFile fullName asFullFileEntry parent.
		aBlock value: myDir ].

	"Packages that come included with Cuis"
	aBlock value: DirectoryEntry packagesDirectory.
	DirectoryEntry packagesDirectory allRegularDirectoriesDo: aBlock.

	"Packages created by user"
	aBlock value: DirectoryEntry newPackagesDirectory.

	"Packages in other folders or repos in the project directory"
	aBlock value: DirectoryEntry projectBaseDirectory.
	DirectoryEntry projectBaseDirectory allRegularDirectoriesDo: aBlock.! !


!DropFilesAction methodsFor: 'evaluating - private' stamp: 'jmv 2/22/2024 14:07:20'!
fileNamedDropped: aFileName 

	| options selectionIndex menu |
	
	selectedFileEntry := aFileName asFullFileEntry.
	options := FileList itemsForFileEntry: selectedFileEntry.
	options isEmpty ifTrue: [ ^self inform: 'No action found for ', selectedFileEntry name ].
	menu := self createMenuFor: options.
		
	selectionIndex := menu startUpWithCaption: 'Select action for ', selectedFileEntry name.
	
	selectionIndex = 0 ifTrue: [ ^self ].
	(options isInBounds: selectionIndex) ifTrue: [ ^self performService: (options at: selectionIndex) ].
	"The only available option is 'stop here'. This could change if #createMenuFor: changes - Hernan"
	stopHereBlock value
	
! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 2/22/2024 14:08:42'!
installNewUpdates: updatesFileDirectory

	| warnUser |
	self newUpdates: updatesFileDirectory do: [ :each |
		self install: each ].	

	warnUser := false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | codePackageFile |
			codePackageFile := CodePackageFile onFileEntry: fullFileName asFullFileEntry.
			(codePackageFile provides isLaterThan: package provides) ifTrue: [
				warnUser := true.
				package hasUnsavedChanges
					ifTrue: [ ('Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.') print ]
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than the currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: Smalltalk versionString, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ]! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 2/22/2024 14:09:22'!
name

	^ self fullName asFullFileEntry name! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 2/22/2024 10:18:08'!
install
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	localName := fullName asFullFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS := ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS := nil ].
		pckClass := CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				((self packageName asIdentifier: true), 'Package') = ee name ifTrue: [
					ee fileInDefinitionAndMetaclass.
					pckClass := Smalltalk at: ee name ]]].
		newCodePackage := pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.

		fullName asFullFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 2/22/2024 14:09:40'!
packageNameFrom: fullName

	^fullName asFullFileEntry nameWithoutExtension! !

