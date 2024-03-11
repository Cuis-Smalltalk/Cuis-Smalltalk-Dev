'From Cuis6.3 [latest update: #6254] on 11 March 2024 at 12:54:19 pm'!

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 3/11/2024 11:41:44'!
ancestors
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 3/11/2024 11:16:38'!
commonAncestorWith: aFileSystemEntry
	"
	DirectoryEntry currentDirectory commonAncestorWith:  DirectoryEntry projectBaseDirectory
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


!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 3/11/2024 11:48:34'!
isAncestorOf: aFileSystemEntry
	| theirPathComponents size |
	size := pathComponents size.
	theirPathComponents := aFileSystemEntry pathComponents.
	theirPathComponents size > size ifFalse: [
		^false ].
	1 to: size do: [ :index |
		(pathComponents at: index) = (theirPathComponents at: index) ifFalse: [^ false]].
	^ true! !

!methodRemoval: FileSystemEntry #parents stamp: 'jmv 3/11/2024 12:52:04'!
FileSystemEntry removeSelector: #parents!
