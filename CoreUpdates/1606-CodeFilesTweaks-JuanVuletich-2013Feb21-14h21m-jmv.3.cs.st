'From Cuis 4.1 of 12 December 2012 [latest update: #1605] on 21 February 2013 at 3:19 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:56'!
currentChangesName
	"Answer the full path to the version-stable source code currently in use"
	^(SourceFiles at: 2) name! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 15:18'!
saveAs: newName
	"Save the image  under a new name."

	| oldChangesName newChangesName |
	(SourceFiles at: 2) ifNotNil: [
		oldChangesName _ self currentChangesName.
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (FileDirectory default fullNameFor: (self fullNameForImageNamed: newName));
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 2/21/2013 14:44'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| f classCount dir newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please designate the version
for the new source code file...' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	f _ FileStream newFileNamed: newSourcesName.
	f timeStamp.
'Condensing Sources File...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: Smalltalk classNames size
	during: [ :bar |
		classCount _ 0.
		Smalltalk allClassesDo: [ :class |
			bar value: (classCount _ classCount + 1).
			class fileOutOn: f moveSource: true toFile: 1]].
	f close.

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	dir _ FileDirectory default.
	dir rename: oldChangesName toBe: oldChangesName, '.old'.
	newChangesName _ self defaultChangesName.
	(FileStream newFileNamed: newChangesName)
		timeStamp; close.
	LastQuitLogPosition _ 0.

	self setMacFileInfoOn: newChangesName.
	self setMacFileInfoOn: newSourcesName.
	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 14:30'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	| newName |
	newName _ self getFileNameFromUser.
	newName ifNil: [^ self].
 	self okayToSave ifFalse: [^self].
	self saveAs: newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 14:30'!
saveAsNewVersion
	"Save the image/changes using the next available version number."

	"Smalltalk saveAsNewVersion"
	| newName changesName aName anIndex |
 	self okayToSave ifFalse: [^self].

	aName _ FileDirectory baseNameFor: (FileDirectory default localNameFor: Smalltalk imageName).
	anIndex _ aName lastIndexOf: FileDirectory dot asCharacter ifAbsent: nil.
	(anIndex notNil and: [(aName copyFrom: anIndex + 1 to: aName size) isAllDigits])
		ifTrue:
			[aName _ aName copyFrom: 1 to: anIndex - 1].

	newName _ FileDirectory default nextNameFor: aName extension: FileDirectory imageSuffix.
	changesName _ self fullNameForChangesNamed: newName.

	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory default includesKey: changesName)
		ifTrue:
			[^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.'].

	self saveAs: newName! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/20/2013 22:55'!
externalizeSources   
	"Write the sources and changes streams onto external files."
 	"Smalltalk externalizeSources"

	| sourcesName changesName aFile |
	sourcesName _ self defaultSourcesName.
	(FileDirectory default fileExists: sourcesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', sourcesName].
	changesName _ self defaultChangesName.
	(FileDirectory default fileExists: changesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', changesName].

	aFile _ FileStream newFileNamed: sourcesName.
	aFile nextPutAll: SourceFiles first originalContents.
	aFile close.
	self setMacFileInfoOn: sourcesName.
	SourceFiles at: 1 put: (FileStream readOnlyFileNamed: sourcesName).

	aFile _ FileStream newFileNamed: changesName.
	aFile nextPutAll: SourceFiles last contents.
	aFile close.
	"On Mac, set the file type and creator (noop on other platforms)"
	FileDirectory default
		setMacFileNamed: changesName
		type: 'STch'
		creator: 'FAST'.
	SourceFiles at: 2 put: (FileStream oldFileNamed: changesName).

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 15:00'!
internalizeChangeLog    
		"Smalltalk internalizeChangeLog"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aName aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	aName _ self currentChangesName.
	(aFile _ SourceFiles last) ifNil: [
		(FileDirectory default fileExists: aName)
			ifFalse: [^ self halt: 'Cannot locate ', aName, ' so cannot proceed.'].
		aFile _ FileStream readOnlyFileNamed: aName].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 15:00'!
internalizeSources
		"Smalltalk internalizeSources"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aName aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse:
		[^ self inform: 'Okay - abandoned'].

	aName _ self currentSourcesName.
	(aFile _ SourceFiles first) ifNil: [
		(FileDirectory default fileExists: aName)
			ifFalse: [^ self halt: 'Cannot locate ', aName, ' so cannot proceed.'].
		aFile _ FileStream readOnlyFileNamed: aName].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	aName _ self currentChangesName.
	(aFile _ SourceFiles last) ifNil: [
		(FileDirectory default fileExists: aName)
			ifFalse: [^ self halt: 'Cannot locate ', aName, ' so cannot proceed.'].
		aFile _ FileStream readOnlyFileNamed: aName].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'! !

!methodRemoval: SystemDictionary #localChangesName!
SystemDictionary removeSelector: #localChangesName!
!methodRemoval: SystemDictionary #newSourcesName!
SystemDictionary removeSelector: #newSourcesName!
!methodRemoval: SystemDictionary #saveChanges:inFileNamed:!
SystemDictionary removeSelector: #saveChanges:inFileNamed:!
!methodRemoval: SystemDictionary #saveChangesInFileNamed:!
SystemDictionary removeSelector: #saveChangesInFileNamed:!
!methodRemoval: SystemDictionary #saveImageInFileNamed:!
SystemDictionary removeSelector: #saveImageInFileNamed:!
