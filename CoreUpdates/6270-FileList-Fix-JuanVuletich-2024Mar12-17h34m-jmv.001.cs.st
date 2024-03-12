'From Cuis6.3 [latest update: #6269] on 12 March 2024 at 5:37:31 pm'!

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 3/12/2024 17:20:06'!
allParents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 3/12/2024 17:32:49'!
commonPathWith: aFileSystemEntry
	"
	DirectoryEntry currentDirectory commonPathWith: DirectoryEntry projectBaseDirectory
	"
	| i lastI theirPathComponents |
	self drive = aFileSystemEntry drive
		ifFalse: [ ^nil ].
	i := 1.
	theirPathComponents := aFileSystemEntry pathComponents.
	lastI := pathComponents size min: theirPathComponents size.
	[i <= lastI and: [(pathComponents at: i) = (theirPathComponents at: i)]] whileTrue: [
		i := i + 1 ].
	^DirectoryEntry
				withPathComponents: (pathComponents copyFrom: 1 to: i-1)
				drive: self drive
		! !


!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 3/12/2024 17:30:40'!
isInPathTo: aFileSystemEntry
	| theirPathComponents size |
	size := pathComponents size.
	theirPathComponents := aFileSystemEntry pathComponents.
	theirPathComponents size > size ifFalse: [
		^false ].
	1 to: size do: [ :index |
		(pathComponents at: index) = (theirPathComponents at: index) ifFalse: [^ false]].
	^ true! !


!FileList methodsFor: 'initialization' stamp: 'jmv 3/12/2024 17:32:59'!
topDirectoryWrappers

	topDirectories := (Preferences at: #initialFileListDirectories) 
		caseOf: {
			[ #cuis ]  		-> 	[ | topDirectoryToUse |
				topDirectoryToUse := directory. "must be inside the tree of some of the included"
				(topDirectoryToUse commonPathWith: DirectoryEntry projectBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry userBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry cuisBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry currentDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				{ topDirectoryToUse} ].
			[ #roots ] -> [ DirectoryEntry roots ].
			}.
	^topDirectories collect: [ :dir |
		FileDirectoryWrapper with: dir name: (dir name ifNil: [ '/' ]) model: self ]! !

!FileList methodsFor: 'private' stamp: 'jmv 3/12/2024 17:31:02'!
postOpen
	| pathRoot pathElements |
	directory ifNotNil: [
		pathRoot := topDirectories detect: [ :any | any isInPathTo: directory ].
		pathElements := pathRoot pathComponents size = 0
			ifTrue: [ {directory drive}, directory pathComponents ]
			ifFalse: [ (directory pathComponents
					copyFrom: (pathRoot pathComponents size)
					to: directory pathComponents size) ].
		self changed: #(openPath), pathElements ]! !

!methodRemoval: DirectoryEntry #isAncestorOf: stamp: 'jmv 3/12/2024 17:35:08'!
DirectoryEntry removeSelector: #isAncestorOf:!
!methodRemoval: FileSystemEntry #commonAncestorWith: stamp: 'jmv 3/12/2024 17:35:19'!
FileSystemEntry removeSelector: #commonAncestorWith:!
!methodRemoval: FileSystemEntry #ancestors stamp: 'jmv 3/12/2024 17:35:17'!
FileSystemEntry removeSelector: #ancestors!
