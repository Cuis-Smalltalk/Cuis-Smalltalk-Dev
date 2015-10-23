'From Cuis 4.2 of 25 July 2013 [latest update: #2539] on 20 October 2015 at 12:06:20.518111 pm'!

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/20/2015 08:24'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)"

	| candidate |
	FmFileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FmFileIOAccessor default onMac) ifTrue: [
		candidate _ self upToFirstPathSeparator.
		"Aparentemente en Mac, 
			xxx/yyy means xxx must be a drive name
			/xxx/yyy means xxx could be any folder in root. Check to make sure!!
		"
		('/' asDirectoryEntry directoryNames includes: candidate) ifTrue: [
			^candidate ]].
	
	^ nil! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 10/20/2015 11:57'!
isAbsolutePathName
	| upperName |
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FmFileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	FmFileIOAccessor default onMac ifTrue: [
		upperName := self asUppercase.
		^'/' asDirectoryEntry directoryNames anySatisfy: [ :each |
			(upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						(self at: nextPos) isPathSeparator ]]].

	^ false! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 10/20/2015 08:13'!
isDriveName
	FmFileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	FmFileIOAccessor default onMac ifTrue: [
		^'/' asDirectoryEntry directoryNames includes: self].

	^false! !


!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/19/2015 17:17'!
drive
	self onUnix ifTrue: [^ drive := nil].
"	drive
		ifNil: [drive := self fileAccessor currentDriveName]."
		"horrible!!!!!!!!"
	^ drive! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/19/2015 17:08'!
name
	^name ifNil: [ drive ]! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 10/20/2015 11:32'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	FmDirectoryEntry roots
	"
	^ FmFileIOAccessor default drives
		ifEmpty: [
			"
			On Linux. Check on Mac.
			En Mac estaria bien. Seria un raiz del sistema donde estan los discos como 1er subfolder"
			{ '/' asDirectoryEntry } ]
		ifNotEmpty: [ :drvs |
			"Chequear en Windows. Deberia contestar el root de cada disco, bien formado.
			El Name seria vacio. El disco bien puesto. #name contesta el disco."
			"aca, mandar asFileEntry o asDirectoryEntry deberia contestar lo mismo.
			Es mas, no tiene sentido que existan esos 2 metodos........
			En todo caso, #exists es falso y no hace nada!!"
			drvs collect: [ :each | each asDirectoryEntry ]]! !


!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/20/2015 08:22'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Warning: If parentEntryOrNil is nil, answered instances are broken, 
	and essentially only useful for asking back the attributes we have just set.
	Not pretty.

		FmFileIOAccessor default entriesIn: '' from: nil
	
		FmFileIOAccessor default entriesIn: '.' from: '.' asFileEntry
		
	YAW (YetAnotherWarning): On Unix, directoryFullPath = '' means 
		FmDirectoryEntry currentDirectory
		FmDirectoryEntry smalltalkImageDirectory
			(NOT SURE!!!!!!!!)
	In Windows, it means 'give me the disk drives'...
	On Mac?
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
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ]
				ifNil: [	"Tratar de arreglar lo que dice (horrible) el comentario de este metodo"
					entry pathName: entry name ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/20/2015 11:17'!
directoryNamesIn: fullPath
	"
	FmFileIOAccessor default directoryNamesIn: 'C:\Windows'
	
FmFileIOAccessor default directoryNamesIn: ''
	Linux: Ojo. Esto en Unix NO VA AL ROOT.
		Equivale a 
		FmFileIOAccessor default directoryNamesIn: '.'
			Confirmado. Va a 	FmDirectoryEntry currentDirectory
	Windows
		 #('C:' 'D:')
	Mac
		?
	"
	
	^(self entriesIn: fullPath from: nil)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 10/20/2015 12:05'!
drives
	"
	Answer a collection of Strings
	FmFileIOAccessor default drives
	"
	drives _ nil. 		"will change if you mount or unmount drives!!"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [ #() ]
			ifFalse: [ (self entriesIn: '' from: nil) collect: [ :each | each name ]]].
	^drives! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'jmv 10/20/2015 11:21'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of FmDirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!methodRemoval: FmDirectoryEntry class #root!
FmDirectoryEntry class removeSelector: #root!
!methodRemoval: FmFileEntry #root!
FmFileEntry removeSelector: #root!
