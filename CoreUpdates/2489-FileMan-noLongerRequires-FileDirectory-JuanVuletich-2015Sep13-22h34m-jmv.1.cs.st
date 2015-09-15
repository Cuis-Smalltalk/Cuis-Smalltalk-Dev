'From Cuis 4.2 of 25 July 2013 [latest update: #2488] on 13 September 2015 at 10:35:57.924256 pm'!

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/13/2015 22:33'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	FmFileIOAccessor default entriesIn: '' from: nil
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: directoryFullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory
					ifTrue: [FmDirectoryEntry new]
					ifFalse: [FmFileEntry new].
				entry isDirectory: isDirectory.
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				entry fileSize: (entryArray at: 5).
				entries addLast: entry ].
		index := index + 1].

	entries _ entries asArray.
	parentEntryOrNil ifNotNil: [
		entries do: [ :each |
			each parent: parentEntryOrNil ]].
	^entries! !


!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 9/13/2015 21:31'!
initChildren
	| dir |
	dir := self pathName.
	self exists ifFalse: [^children := #()].
	children := self fileAccessor entriesIn: dir from: self.
	^children! !


!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 4/2/2008 17:14'!
basicDirectoryExists: fullPathName

	| result |
	result := self lookupEntryIn: fullPathName index: 1.
 	^result ~= #badDirectoryPath! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/12/2015 20:44'!
lookupEntryIn: fullPathName index: index
	
	^self primLookupEntryIn: fullPathName index: index! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 9/13/2015 21:51'!
directoryNamesIn: fullPath
	"
	FmFileIOAccessor default directoryNamesIn: 'C:\Windows'
	"
	
	^(self entriesIn: fullPath from: nil)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 9/13/2015 21:51'!
entryNamesIn: fullPath
	"
	FmFileIOAccessor default entryNamesIn: 'C:\Windows\'
	"
	
	^(self entriesIn: fullPath from: nil) collect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 9/13/2015 21:50'!
fileNamesIn: fullPath
	"
	FmFileIOAccessor default fileNamesIn: 'C:\Windows'
	"
	
	^((self entriesIn: fullPath from: nil)
		reject: [ :each | each isDirectory ])
		collect: [ :each | each name ]! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/13/2015 21:51'!
drives
	"
	FmFileIOAccessor default drives
	"
	self onUnix ifTrue: [^#()].
	^(self entriesIn: '' from: nil) collect: [ :each | each name]! !


!FmFileIOAccessor class reorganize!
('class initialization' initialize)
('instance creation' default)
('accessing' defaultDirectoryPath)
!

!methodRemoval: FmFileIOAccessor #bareEntriesIn:!
FmFileIOAccessor removeSelector: #bareEntriesIn:!
!methodRemoval: FmFileIOAccessor #bareEntryFromArray:in:!
FmFileIOAccessor removeSelector: #bareEntryFromArray:in:!
!methodRemoval: FmDirectoryEntry #asFileDirectory!
FmDirectoryEntry removeSelector: #asFileDirectory!
!methodRemoval: FmDirectoryEntry #entriesIn:from:!
FmDirectoryEntry removeSelector: #entriesIn:from:!

!FmDirectoryEntry reorganize!
('actions-path' /)
('enumeration' allChildrenDo: allChildrenDo:filesMatches:directoryMatches: allDirectoriesDo: allFilesDo: allFilesDo:matches: childrenDo: childrenDo:filesMatches:directoryMatches: directoriesDo: directoriesMatches: filesDo: filesMatches: latestFileMatches: oldestFileMatches:)
('converting' asDirectoryEntry asFileEntry ensureAsDirectory ensureAsFile)
('actions-directory' assureExistence copyTo: delete recursiveDelete rename:)
('private' assureExistenceFileNamed: basicRecursiveDelete initChildren)
('dictionary-like' at: at:by: at:ifAbsent: at:put: at:put:by: binaryAt: binaryAt:ifAbsent: binaryAt:put: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing' children directories directory directoryNames file fileNames files)
('testing' exists isDirectory)
('initialize-release' refresh refreshChildren)
!

