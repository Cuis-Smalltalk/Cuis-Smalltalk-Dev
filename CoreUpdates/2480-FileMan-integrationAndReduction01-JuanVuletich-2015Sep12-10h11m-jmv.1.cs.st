'From Cuis 4.2 of 25 July 2013 [latest update: #2479] on 12 September 2015 at 10:29:28.202319 am'!

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 9/12/2015 10:12'!
concatPathComponents: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.
	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmFileEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry asDirectoryEntry ].
		parentEntry := entry ].
	^entry! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/12/2015 10:17'!
readStreamIfError: errorHandlingBlock
	
	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FileDoesNotExistException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/12/2015 10:18'!
writeStreamIfError: errorHandlingBlock
	self refresh.
		
	^[self fileAccessor newFileNamed: self pathName]
		on: FileExistsException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: 'converting' stamp: 'jmv 9/12/2015 10:13'!
asDirectoryEntry
	^ self as: FmDirectoryEntry! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 9/12/2015 10:13'!
root
	^FmDirectoryEntry root! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/12/2015 10:13'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := FmDirectoryEntry
				pathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !


!FmFileEntry class methodsFor: 'instance creation' stamp: 'jmv 9/12/2015 10:13'!
entry: bareFileEntry
	| inst |
	inst := bareFileEntry isDirectory
		ifTrue: [FmDirectoryEntry new]
		ifFalse: [FmFileEntry new].
	inst isDirectory: bareFileEntry isDirectory.
	inst name: bareFileEntry name.
	inst initValuesFrom: bareFileEntry.
	^inst! !


!FmDirectoryEntry methodsFor: 'converting' stamp: 'jmv 9/12/2015 10:11'!
ensureAsFile
	^self as: FmFileEntry! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 9/12/2015 10:11'!
entriesIn: directoryFullPath from: parentEntry 
	^ (self fileAccessor bareEntriesIn: directoryFullPath)
		collect: [:each | 
			| entry | 
			entry := FmFileEntry entry: each.
			entry parent: parentEntry.
			entry]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/12/2015 10:17'!
at: localFileName ifAbsent: block 
	^ [self at: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/12/2015 10:17'!
binaryAt: localFileName ifAbsent: block 
	^ [self binaryAt: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 9/12/2015 10:13'!
default 
	"FmDirectoryEntry default"
	^FmDirectoryEntry pathName: FmFileIOAccessor defaultDirectoryPath! !


!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 9/12/2015 10:23'!
beDefault
	| oldDefault |
	self = FmFileIOAccessor ifTrue: [^self]. "This method is for a subclass"
	oldDefault := FmFileIOAccessor default.
	oldDefault notNil ifTrue: [Smalltalk removeFromStartUpList: oldDefault dialectUtilClass].
	Default := self new.
	Smalltalk addToStartUpList: Default dialectUtilClass! !

!methodRemoval: FmSqPortableUtil class #addToStartUpList:!
FmSqPortableUtil class removeSelector: #addToStartUpList:!
!methodRemoval: FmSqPortableUtil class #fileDoesNotExistException!
FmSqPortableUtil class removeSelector: #fileDoesNotExistException!
!methodRemoval: FmSqPortableUtil class #fileExistsException!
FmSqPortableUtil class removeSelector: #fileExistsException!
!methodRemoval: FmSqPortableUtil class #removeFromStartUpList:!
FmSqPortableUtil class removeSelector: #removeFromStartUpList:!

!FmSqPortableUtil class reorganize!
('actions')
('startUp' startUp)
!

!methodRemoval: FmPortableUtil class #addToStartUpList:!
FmPortableUtil class removeSelector: #addToStartUpList:!
!methodRemoval: FmPortableUtil class #fileDoesNotExistException!
FmPortableUtil class removeSelector: #fileDoesNotExistException!
!methodRemoval: FmPortableUtil class #fileExistsException!
FmPortableUtil class removeSelector: #fileExistsException!
!methodRemoval: FmPortableUtil class #removeFromStartUpList:!
FmPortableUtil class removeSelector: #removeFromStartUpList:!

!FmPortableUtil class reorganize!
('as yet unclassified')
!

!methodRemoval: FmFileIOAccessor class #fileDoesNotExistException!
FmFileIOAccessor class removeSelector: #fileDoesNotExistException!
!methodRemoval: FmFileIOAccessor class #fileExistsException!
FmFileIOAccessor class removeSelector: #fileExistsException!
!methodRemoval: FmFileIOAccessor class #obsolete!
FmFileIOAccessor class removeSelector: #obsolete!
!methodRemoval: FmDirectoryEntry class #fileEntryClass!
FmDirectoryEntry class removeSelector: #fileEntryClass!

!FmDirectoryEntry class reorganize!
('instance creation' default on: parent pathName: root roots)
!

!methodRemoval: FmFileEntry class #directoryEntryClass!
FmFileEntry class removeSelector: #directoryEntryClass!
!methodRemoval: FmFileEntry class #fileEntryClass!
FmFileEntry class removeSelector: #fileEntryClass!
!methodRemoval: FmFileEntry class #obsolete!
FmFileEntry class removeSelector: #obsolete!

!FmFileEntry class reorganize!
('accessing' drives)
('instance creation' entry: pathComponents: pathComponents:drive: pathName:)
('examples' example1 example2 example3 example4 example5 example6 example7)
('class initialization')
('starting up' onSystemStartUp)
!

!methodRemoval: FmFileEntry #directoryEntryClass!
FmFileEntry removeSelector: #directoryEntryClass!
!methodRemoval: FmFileEntry #fileEntryClass!
FmFileEntry removeSelector: #fileEntryClass!
