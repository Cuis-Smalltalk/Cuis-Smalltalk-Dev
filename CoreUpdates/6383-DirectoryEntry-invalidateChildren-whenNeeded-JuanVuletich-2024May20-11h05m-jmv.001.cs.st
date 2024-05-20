'From Cuis7.1 [latest update: #6381] on 20 May 2024 at 11:12:50 am'!

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/19/2024 21:00:26'!
copy: fromFileEntry to: toFileEntry 
	| readStr writeStr |
	[readStr := (self privateReadOnlyFile: fromFileEntry) binary.
	writeStr := (self privateForceNewFile: toFileEntry) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [
			readStr
				ifNotNil: [ :r | r close ].
			writeStr
				ifNotNil: [ :w | w close ]].
	toFileEntry parent invalidateChildren! !


!DirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 5/19/2024 21:02:23'!
delete
	self fileAccessor deleteDirectory: self pathName.
	self invalidateChildren.
	parent ifNotNil: [ parent invalidateChildren ].! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 5/19/2024 21:04:09'!
rename: newName

	| fileEntry directoryEntry |
	fileEntry := self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry := self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor renameDirectory: self pathName to: directoryEntry pathName.
	self name: newName.
	self parent 	invalidateChildren.! !

!DirectoryEntry methodsFor: 'private' stamp: 'jmv 5/19/2024 21:01:01'!
assureExistenceDirNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceDirNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName.
	self invalidateChildren! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 5/20/2024 11:10:02'!
children
	(lastChildrenSync isNil or: [
		"This message may be sent many times. Don't call the primitives too often.
		Any time we invalidate children ourselves (by adding / removing / renaming entries) we send #invalidateChildren.
		Still, if directory contents are modified elsewhere (and especially outside Cuis), there will be a delay until we see those changes."
		(DateAndTime now - lastChildrenSync) totalSeconds > 2]) ifTrue: [
			children := nil ].
	children ifNil: [self initChildren].
	^children! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/19/2024 20:59:36'!
delete
	self fileAccessor deleteFile: self pathName.
	self parent invalidateChildren.! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/19/2024 20:59:40'!
rename: newName

	| fileEntry directoryEntry |
	fileEntry := self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry := self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor rename: self pathName to: fileEntry pathName.
	self name: newName.
	self parent invalidateChildren.! !

