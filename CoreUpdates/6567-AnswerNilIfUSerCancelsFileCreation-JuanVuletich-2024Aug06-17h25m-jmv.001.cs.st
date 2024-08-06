'From Cuis7.1 [latest update: #6566] on 6 August 2024 at 5:25:57 pm'!

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 8/6/2024 17:07:23'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry := fullFileName asFullFileEntry.
	dir := entry parent.
	localName := entry name.
	choice := (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	"overwrite that file"
	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	"choose another name"
	choice = 2 ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	"choice = 3: cancel"
	"Answer nil. Let callers decide whether to continue or not"
	^nil.! !


!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 8/6/2024 17:08:46'!
writeStreamDo: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist.
	Note: If file already exists and user selects 'cancel', the block is passed nil as argument!!"
	| stream |
	stream := self writeStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].
! !

!FileEntry methodsFor: 'accessing' stamp: 'jmv 8/6/2024 16:55:45'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist.
	Note: Answer nil if for whatever reason the file can not be created.
	"

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self! !

