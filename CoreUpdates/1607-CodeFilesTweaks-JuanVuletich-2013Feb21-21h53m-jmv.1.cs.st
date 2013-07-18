'From Cuis 4.1 of 12 December 2012 [latest update: #1606] on 21 February 2013 at 10:17:45 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 2/21/2013 22:04'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| f classCount dir newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
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
	oldChangesName ifNotNil: [
		dir _ FileDirectory default.
		dir rename: oldChangesName toBe: oldChangesName, '.old' ].
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

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/21/2013 22:14'!
currentChangesName
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"
	| changes |
	changes _ SourceFiles second.
	^(changes isKindOf: FileStream)
		ifTrue: [ changes name ]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/21/2013 22:13'!
currentSourcesName
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"
	| sources |
	sources _ SourceFiles first.
	^(sources isKindOf: FileStream)
		ifTrue: [ sources name ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 22:11'!
saveAs: newName
	"Save the image  under a new name."

	self currentChangesName ifNotNil: [ :oldChangesName | | newChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 21:58'!
closeSourceFiles
	"Shut down the source files if appropriate.  1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file"

	1 to: 2 do: [:i |
		((SourceFiles at: i) isKindOf: FileStream)
			ifTrue: [
				(SourceFiles at: i) close.
				SourceFiles at: i put: nil]]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 22:17'!
internalizeChangeLog    
		"
		Smalltalk internalizeChangeLog
		"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles second) ifNil: [
		^ self halt: 'Cannot locate Changes file so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 22:16'!
internalizeSources
		"
		Smalltalk internalizeSources
		"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles first) ifNil: [
		^ self halt: 'Cannot locate Sources file so cannot proceed.'].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	(aFile _ SourceFiles last) ifNil: [
		^ self halt: 'Cannot locate Changes so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'! !

