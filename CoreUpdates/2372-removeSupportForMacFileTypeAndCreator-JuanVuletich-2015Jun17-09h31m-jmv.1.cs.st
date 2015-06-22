'From Cuis 4.2 of 25 July 2013 [latest update: #2369] on 17 June 2015 at 9:44:30.687365 am'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/17/2015 09:42'!
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
	SourceFiles at: 2
			put: (FileStream oldFileNamed: oldChanges name).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/17/2015 09:42'!
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

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/17/2015 09:42'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/17/2015 09:42'!
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
	SourceFiles at: 1 put: (FileStream readOnlyFileNamed: sourcesName).

	aFile _ FileStream newFileNamed: changesName.
	aFile nextPutAll: SourceFiles last contents.
	aFile close.
	SourceFiles at: 2 put: (FileStream oldFileNamed: changesName).

	self inform: 'Sources successfully externalized'! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2015 09:39'!
fileNamed: aString
	"Here is the way to use DataStream and ReferenceStream:
rr _ ReferenceStream fileNamed: 'test.obj'.
rr nextPut: <your object>.
rr close.
"

	^ self on: (FileStream fileNamed: aString).		"will be binary"! !

!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2015 09:39'!
forceNewFileNamed: aString
	"Here is the way to use DataStream and ReferenceStream:
rr _ ReferenceStream forceNewFileNamed: 'test.obj'.
rr nextPut: <your object>.
rr close.
"

	^ self on: (FileStream forceNewFileNamed: aString).		"will be binary"! !

!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2015 09:40'!
newFileNamed: aString
	"Here is the way to use DataStream and ReferenceStream:
rr _ ReferenceStream fileNamed: 'test.obj'.
rr nextPut: <your object>.
rr close.
"

	^ self on: (FileStream newFileNamed: aString).		"will be binary"! !


!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 09:40'!
fileOutObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| srefStream |
	self timeStamp.

	"Append the object's raw data"
	srefStream _ SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"
! !

!methodRemoval: StandardFileStream #setFileTypeToObject!
StandardFileStream removeSelector: #setFileTypeToObject!
!methodRemoval: FileDirectory #getMacFileTypeAndCreator:!
FileDirectory removeSelector: #getMacFileTypeAndCreator:!
!methodRemoval: FileDirectory #primGetMacFileNamed:type:creator:!
FileDirectory removeSelector: #primGetMacFileNamed:type:creator:!
!methodRemoval: FileDirectory #primSetMacFileNamed:type:creator:!
FileDirectory removeSelector: #primSetMacFileNamed:type:creator:!
!methodRemoval: FileDirectory #setMacFileNamed:type:creator:!
FileDirectory removeSelector: #setMacFileNamed:type:creator:!
!methodRemoval: SystemDictionary #setMacFileInfoOn:!
SystemDictionary removeSelector: #setMacFileInfoOn:!
