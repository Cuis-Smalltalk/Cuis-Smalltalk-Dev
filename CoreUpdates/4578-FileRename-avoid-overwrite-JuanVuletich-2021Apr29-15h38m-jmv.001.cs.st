'From Cuis 5.0 [latest update: #4579] on 29 April 2021 at 3:43:12 pm'!

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 4/29/2021 15:42:46'!
rename: newName

	| fileEntry directoryEntry |
	fileEntry _ self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry _ self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor renameDirectory: self pathName to: directoryEntry pathName.
	self name: newName! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 4/29/2021 15:42:52'!
rename: newName

	| fileEntry directoryEntry |
	fileEntry _ self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry _ self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor rename: self pathName to: fileEntry pathName.
	self name: newName.! !

