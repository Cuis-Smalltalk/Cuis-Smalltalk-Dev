'From Cuis7.1 [latest update: #6401] on 24 May 2024 at 4:36:22 pm'!

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/24/2024 16:30:36'!
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
	toFileEntry parent invalidateChildren.
	toFileEntry invalidate.! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 5/24/2024 16:33:41'!
fileSize: fileEntry

	| pathName f size |
	pathName := fileEntry pathName.
	"At least on Linux 64 Cog, opening a directory as a stream and asking #size answers some absurd number: 9223372036854775807"
	(self basicDirectoryExists: pathName) ifTrue: [^ nil ].
	f := UniFileStream new open: pathName forWrite: false.
	f ifNil: [^ nil].
	size := f size.
	f close.
	fileEntry invalidate.
	^ size! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/24/2024 16:30:58'!
delete
	self fileAccessor deleteFile: self pathName.
	self parent invalidateChildren.
	self invalidate.! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/24/2024 16:31:43'!
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
	self parent invalidateChildren.
	self invalidate.! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/24/2024 16:32:26'!
appendStreamDo: blockWithArg 
	| stream |
	stream := self appendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].
! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/24/2024 16:32:46'!
forceWriteStreamDo: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream := self forceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].
! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/24/2024 16:33:13'!
writeStreamDo: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream := self writeStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].
! !

!FileEntry methodsFor: 'accessing' stamp: 'jmv 5/24/2024 16:28:26'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamDo: that closes the file for you."

	self invalidate.
	self exists ifFalse: [
		^ self writeStream ].
	^ (self fileAccessor privateWriteableFile: self) setToEnd! !

