'From Cuis 5.0 of 7 November 2016 [latest update: #2977] on 17 November 2016 at 10:31:18 am'!
!classDefinition: #StdIOReadStream category: #'System-Support'!
Stream subclass: #StdIOReadStream
	instanceVariableNames: 'fileID name buffer1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOReadStream commentStamp: '<historical>' prior: 0!
Standard Input Stream.

A basic problem/restriction with this code is that currently the VM runs multiple VM threads within a single OS thread.

This means that waiting on StdIn blocks the VM, suspending all Smalltalk code.!

!classDefinition: #StdIOWriteStream category: #'System-Support'!
Stream subclass: #StdIOWriteStream
	instanceVariableNames: 'fileID name buffer1 collection readLimit position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOWriteStream commentStamp: '<historical>' prior: 0!
Standard Output/Error Streams.!


!StdIOReadStream methodsFor: 'open / close' stamp: 'jmv 11/17/2016 10:12:24'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
	buffer1 _ String new: 1! !

!StdIOReadStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 10:01:57'!
next
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!!"

	| count |
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]! !

!StdIOReadStream methodsFor: 'printing' stamp: 'jmv 11/17/2016 09:47:10'!
printOn: aStream
	"Put a printed version of the receiver onto aStream."

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !

!StdIOReadStream methodsFor: 'primitives' stamp: 'jmv 11/17/2016 09:45:28'!
primRead: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self error: 'File read failed'! !


!StdIOReadStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 09:46:36'!
stdin
	| newSelf |
	newSelf _ self basicNew.
	newSelf 
		openOnHandle: self stdinHandle 
		name: 'stdin'.
	^newSelf! !

!StdIOReadStream class methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:26:47'!
stdinHandle

	^ StdIOWriteStream stdioHandles at: 1! !


!StdIOWriteStream methodsFor: 'open / close' stamp: 'jmv 11/17/2016 10:14:32'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
	buffer1 _ String new: 1! !

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 10:19:51'!
flush
	"Flush pending changes"
	^self primFlush: fileID! !

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 10:18:42'!
nextPut: char
	"Write the given character to this file."

	buffer1 at: 1 put: char.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	^ char
! !

!StdIOWriteStream methodsFor: 'printing' stamp: 'jmv 11/17/2016 09:35:56'!
printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !

!StdIOWriteStream methodsFor: 'primitives' stamp: 'jmv 11/17/2016 10:20:17'!
primFlush: id
	"Flush pending changes to the disk"

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>! !

!StdIOWriteStream methodsFor: 'primitives' stamp: 'jmv 11/17/2016 10:19:14'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])! !


!StdIOWriteStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 10:15:08'!
stderr
	| newSelf |
	newSelf _ self basicNew.
	newSelf 
		openOnHandle: self stderrHandle 
		name: 'stderr'.
	^newSelf! !

!StdIOWriteStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 10:15:11'!
stdout
	| newSelf |
	newSelf _ self basicNew.
	newSelf 
		openOnHandle: self stdoutHandle 
		name: 'stdout'.
	^newSelf! !

!StdIOWriteStream class methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:26:20'!
stderrHandle

	^ self stdioHandles at: 3! !

!StdIOWriteStream class methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:26:09'!
stdioHandles
" answer handles: #(stdin stdout stderr) "
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed! !

!StdIOWriteStream class methodsFor: 'accessing' stamp: 'jmv 11/17/2016 09:26:12'!
stdoutHandle

	^ self stdioHandles at: 2! !

!classRemoval: #StdIOFileStream!
Smalltalk removeClassNamed: #StdIOFileStream!
