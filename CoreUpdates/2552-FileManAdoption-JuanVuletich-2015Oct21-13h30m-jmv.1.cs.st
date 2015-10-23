'From Cuis 4.2 of 25 July 2013 [latest update: #2550] on 21 October 2015 at 1:32:36.567828 pm'!
!classDefinition: #FmFileEntry category: #'FileMan-Core'!
FmEntry subclass: #FmFileEntry
	instanceVariableNames: 'fileSize options '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileEntry commentStamp: '<historical>' prior: 0!
I represent a single file entry.

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See FmDirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFileEntry fileSize
	Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]!


!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:36'!
/ pathString
	"Answer an instance of FmDirectoryEntry.
	If you want an instance of FmFileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asString asPathComponents! !

!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:36'!
// pathString
	"Answer an instance of FmFileEntry.
	If you want an instance of FmDirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asString asPathComponents! !

!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:37'!
concatPathComponentsAsDirectory: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry! !

!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:37'!
concatPathComponentsAsFile: components

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FmFileEntry pathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:52'!
isFile
	^false! !


!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/21/2015 12:50'!
binaryContents: aByteArray
	self forceWriteStream: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].
	self refresh! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/21/2015 12:50'!
textContents: aString
	self forceWriteStream: [ :stream |
		self setContentsOf: stream to: aString ].
	self refresh! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:48'!
initValuesFrom: otherEntry
	otherEntry ifNil: [^self].
	super initValuesFrom: otherEntry.
	self fileSize: otherEntry fileSize! !

!FmFileEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:52'!
isFile
	^true! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'jmv 10/21/2015 12:55'!
refresh
	super refresh.
	fileSize := nil! !


!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:51'!
exists
	
	| dirNames pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self ].

	self isRoot ifTrue: [^true].
	
	self fileAccessor onUnix ifFalse: [
		pathName _ self pathName.
		pathName = self fileAccessor slash ifTrue: [ ^ true ].
		^self fileAccessor basicDirectoryExists: self pathName]. 

	dirNames := self fileAccessor directoryNamesIn: self parent.

	^self fileAccessor isCaseSensitive 
		ifTrue: [ dirNames includes: self name ]
		ifFalse: [ dirNames anySatisfy: [ :n | n sameAs: self name ]]! !

!methodRemoval: FmDirectoryEntry class #pathName:!
FmDirectoryEntry class removeSelector: #pathName:!
!methodRemoval: FmDirectoryEntry #childrenDo:filesMatches:directoryMatches:!
FmDirectoryEntry removeSelector: #childrenDo:filesMatches:directoryMatches:!
!methodRemoval: FmFileEntry class #pathComponents:!
FmFileEntry class removeSelector: #pathComponents:!
!methodRemoval: FmFileEntry class #pathComponents:drive:!
FmFileEntry class removeSelector: #pathComponents:drive:!
!methodRemoval: FmFileEntry class #pathName:!
FmFileEntry class removeSelector: #pathName:!
!methodRemoval: FmFileEntry class #releaseClassCachedState!
FmFileEntry class removeSelector: #releaseClassCachedState!

!FmFileEntry class reorganize!
('examples' example1 example2 example3 example5 example6 example7)
!

!methodRemoval: FmFileEntry #/!
FmFileEntry removeSelector: #/!
!methodRemoval: FmFileEntry #//!
FmFileEntry removeSelector: #//!
!methodRemoval: FmFileEntry #=!
FmFileEntry removeSelector: #=!
!methodRemoval: FmFileEntry #allChildrenDo:!
FmFileEntry removeSelector: #allChildrenDo:!
!methodRemoval: FmFileEntry #allDirectoriesDo:!
FmFileEntry removeSelector: #allDirectoriesDo:!
!methodRemoval: FmFileEntry #allFilesDo:!
FmFileEntry removeSelector: #allFilesDo:!
!methodRemoval: FmFileEntry #allFilesDo:matches:!
FmFileEntry removeSelector: #allFilesDo:matches:!
!methodRemoval: FmFileEntry #at:!
FmFileEntry removeSelector: #at:!
!methodRemoval: FmFileEntry #at:ifAbsent:!
FmFileEntry removeSelector: #at:ifAbsent:!
!methodRemoval: FmFileEntry #at:put:!
FmFileEntry removeSelector: #at:put:!
!methodRemoval: FmFileEntry #baseName!
FmFileEntry removeSelector: #baseName!
!methodRemoval: FmFileEntry #basicPathComponents:!
FmFileEntry removeSelector: #basicPathComponents:!
!methodRemoval: FmFileEntry #binaryAt:!
FmFileEntry removeSelector: #binaryAt:!
!methodRemoval: FmFileEntry #binaryAt:ifAbsent:!
FmFileEntry removeSelector: #binaryAt:ifAbsent:!
!methodRemoval: FmFileEntry #children!
FmFileEntry removeSelector: #children!
!methodRemoval: FmFileEntry #childrenDo:!
FmFileEntry removeSelector: #childrenDo:!
!methodRemoval: FmFileEntry #childrenDo:filesMatches:directoryMatches:!
FmFileEntry removeSelector: #childrenDo:filesMatches:directoryMatches:!
!methodRemoval: FmFileEntry #concatPathComponentsAsDirectory:!
FmFileEntry removeSelector: #concatPathComponentsAsDirectory:!
!methodRemoval: FmFileEntry #concatPathComponentsAsFile:!
FmFileEntry removeSelector: #concatPathComponentsAsFile:!
!methodRemoval: FmFileEntry #creationTime!
FmFileEntry removeSelector: #creationTime!
!methodRemoval: FmFileEntry #creationTime:!
FmFileEntry removeSelector: #creationTime:!
!methodRemoval: FmFileEntry #directories!
FmFileEntry removeSelector: #directories!
!methodRemoval: FmFileEntry #directoriesDo:!
FmFileEntry removeSelector: #directoriesDo:!
!methodRemoval: FmFileEntry #directoriesMatches:!
FmFileEntry removeSelector: #directoriesMatches:!
!methodRemoval: FmFileEntry #drive!
FmFileEntry removeSelector: #drive!
!methodRemoval: FmFileEntry #drive:!
FmFileEntry removeSelector: #drive:!
!methodRemoval: FmFileEntry #ensureParent!
FmFileEntry removeSelector: #ensureParent!
!methodRemoval: FmFileEntry #extension!
FmFileEntry removeSelector: #extension!
!methodRemoval: FmFileEntry #fileAccessor!
FmFileEntry removeSelector: #fileAccessor!
!methodRemoval: FmFileEntry #files!
FmFileEntry removeSelector: #files!
!methodRemoval: FmFileEntry #filesDo:!
FmFileEntry removeSelector: #filesDo:!
!methodRemoval: FmFileEntry #filesMatches:!
FmFileEntry removeSelector: #filesMatches:!
!methodRemoval: FmFileEntry #hash!
FmFileEntry removeSelector: #hash!
!methodRemoval: FmFileEntry #includesKey:!
FmFileEntry removeSelector: #includesKey:!
!methodRemoval: FmFileEntry #initValuesFromParent!
FmFileEntry removeSelector: #initValuesFromParent!
!methodRemoval: FmFileEntry #isDirectory!
FmFileEntry removeSelector: #isDirectory!
!methodRemoval: FmFileEntry #isDriveSupported!
FmFileEntry removeSelector: #isDriveSupported!
!methodRemoval: FmFileEntry #isRoot!
FmFileEntry removeSelector: #isRoot!
!methodRemoval: FmFileEntry #keys!
FmFileEntry removeSelector: #keys!
!methodRemoval: FmFileEntry #latestFileMatches:!
FmFileEntry removeSelector: #latestFileMatches:!
!methodRemoval: FmFileEntry #modificationTime!
FmFileEntry removeSelector: #modificationTime!
!methodRemoval: FmFileEntry #modificationTime:!
FmFileEntry removeSelector: #modificationTime:!
!methodRemoval: FmFileEntry #name!
FmFileEntry removeSelector: #name!
!methodRemoval: FmFileEntry #name:!
FmFileEntry removeSelector: #name:!
!methodRemoval: FmFileEntry #nameVersionExtension!
FmFileEntry removeSelector: #nameVersionExtension!
!methodRemoval: FmFileEntry #nameWithoutExtension!
FmFileEntry removeSelector: #nameWithoutExtension!
!methodRemoval: FmFileEntry #oldestFileMatches:!
FmFileEntry removeSelector: #oldestFileMatches:!
!methodRemoval: FmFileEntry #onMac!
FmFileEntry removeSelector: #onMac!
!methodRemoval: FmFileEntry #onUnix!
FmFileEntry removeSelector: #onUnix!
!methodRemoval: FmFileEntry #onWindows!
FmFileEntry removeSelector: #onWindows!
!methodRemoval: FmFileEntry #options!
FmFileEntry removeSelector: #options!
!methodRemoval: FmFileEntry #options:!
FmFileEntry removeSelector: #options:!
!methodRemoval: FmFileEntry #parent!
FmFileEntry removeSelector: #parent!
!methodRemoval: FmFileEntry #parent:!
FmFileEntry removeSelector: #parent:!
!methodRemoval: FmFileEntry #parents!
FmFileEntry removeSelector: #parents!
!methodRemoval: FmFileEntry #pathComponents!
FmFileEntry removeSelector: #pathComponents!
!methodRemoval: FmFileEntry #pathComponents:!
FmFileEntry removeSelector: #pathComponents:!
!methodRemoval: FmFileEntry #pathComponents:detectDrive:!
FmFileEntry removeSelector: #pathComponents:detectDrive:!
!methodRemoval: FmFileEntry #pathName!
FmFileEntry removeSelector: #pathName!
!methodRemoval: FmFileEntry #pathName:!
FmFileEntry removeSelector: #pathName:!
!methodRemoval: FmFileEntry #printOn:!
FmFileEntry removeSelector: #printOn:!
!methodRemoval: FmFileEntry #printPathOn:!
FmFileEntry removeSelector: #printPathOn:!
!methodRemoval: FmFileEntry #removeKey:!
FmFileEntry removeSelector: #removeKey:!
!methodRemoval: FmFileEntry #removeKey:ifAbsent:!
FmFileEntry removeSelector: #removeKey:ifAbsent:!
!methodRemoval: FmFileEntry #setContentsOf:to:!
FmFileEntry removeSelector: #setContentsOf:to:!
!methodRemoval: FmFileEntry #setParent:!
FmFileEntry removeSelector: #setParent:!
!methodRemoval: FmFileEntry #version!
FmFileEntry removeSelector: #version!
!classDefinition: #FmFileEntry category: #'FileMan-Core'!
FmEntry subclass: #FmFileEntry
	instanceVariableNames: 'fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileEntry reorganize!
('actions-rio' < <<)
('enumeration' assureExistence)
('actions-file' appendContents: binaryContents binaryContents: copyTo: delete fileContents fileContents: rename: textContents textContents:)
('accessing-stream' appendStream: forceWriteStream: readStream: writeStream:)
('accessing' fileSize fileSize:)
('private' initValuesFrom: privateAppendStream privateForceWriteStream privateReadStream privateWriteStream)
('testing' exists isFile)
('initialize-release' refresh)
('actions-pipe' pipe:to: pipeRepeat:to: pipeRepeat:while:to:)
('cuis extensions' form)
!

