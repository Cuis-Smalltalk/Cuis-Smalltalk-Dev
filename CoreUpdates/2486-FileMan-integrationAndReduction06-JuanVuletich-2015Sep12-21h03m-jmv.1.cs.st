'From Cuis 4.2 of 25 July 2013 [latest update: #2483] on 12 September 2015 at 9:04:52.664875 pm'!
!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'defaultDirectoryPath '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/12/2015 20:44'!
bareEntryFromArray: entryArray in: pathString
	^DirectoryEntry fromArray: entryArray directory: (FileDirectory on: pathString)! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/12/2015 20:44'!
defaultDirectoryPath
	"Answer the value of defaultDirectoryPath"
	defaultDirectoryPath ifNil: [self initDefaultDirectoryPath].
	^ defaultDirectoryPath! !

!FmFileIOAccessor methodsFor: 'initialize-release' stamp: 'jmv 9/12/2015 20:44'!
initDefaultDirectoryPath
	| dir |
	dir := self imageDirectoryPath.
	self initDefaultDirectoryPathWith: dir! !

!FmFileIOAccessor methodsFor: 'initialize-release' stamp: 'jmv 9/12/2015 20:44'!
initDefaultDirectoryPathWith: barePath
	| dirName |
	"dirName := (FilePath pathName: barePath isEncoded: true) asSqueakPathName."
	dirName := barePath.
	[dirName endsWith: self slash] whileTrue:[
		dirName := dirName copyFrom: 1 to: dirName size - self slash size.
	].
	defaultDirectoryPath := dirName! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/12/2015 20:44'!
createDirectory: fullPathName
	^self primCreateDirectory: fullPathName! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/12/2015 20:44'!
deleteDirectory: fullPathName
	^self primDeleteDirectory: fullPathName! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/12/2015 20:44'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/12/2015 20:44'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	(self fileExists: oldFileFullName) ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(self fileExists: newFileFullName)
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/12/2015 20:44'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	(self directoryExists: oldFileFullName) ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(self directoryExists: newFileFullName)
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/12/2015 20:56'!
bareEntriesIn: fullPath
	"
	FmFileIOAccessor default bareEntriesIn: ''
	"
	| entries index done entryArray |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: fullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [entries addLast: (self bareEntryFromArray: entryArray in: fullPath)].
		index := index + 1].

	^ entries asArray! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/12/2015 20:44'!
lookupEntryIn: fullPathName index: index
	
	^self primLookupEntryIn: fullPathName index: index! !


!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 9/12/2015 20:55'!
initialize
	"
	FmFileIOAccessor initialize
	"
	self initDefault.
	self beDefault! !

FmFileIOAccessor initialize!

!FmFileIOAccessor class reorganize!
('class initialization' beDefault initDefault initialize)
('instance creation' default)
('accessing' defaultDirectoryPath)
('events' onSystemStartUp)
!

!methodRemoval: FmFileIOAccessor #bareEntryFromArray:!
FmFileIOAccessor removeSelector: #bareEntryFromArray:!
!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'defaultDirectoryPath'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'FileMan-Core'!
