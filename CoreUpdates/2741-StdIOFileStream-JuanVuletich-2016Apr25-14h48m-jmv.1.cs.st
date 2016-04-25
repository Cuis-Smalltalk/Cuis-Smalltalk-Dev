'From Cuis 4.2 of 25 July 2013 [latest update: #2740] on 25 April 2016 at 3:00:15.474681 pm'!
!classDefinition: #StdIOFileStream category: #'System-Files'!
StandardFileStream subclass: #StdIOFileStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!StdIOFileStream commentStamp: '<historical>' prior: 0!
Standard Input/Output/Error FileStreams.

A basic problem/restriction with this code is that currently the VM runs multiple VM threads within a single OS thread.

This means that waiting on StdIn blocks the VM, suspending all Smalltalk code.!


!StdIOFileStream methodsFor: 'file open/close' stamp: 'KenD 1/24/2016 16:04'!
openOnHandle: aFileID name: streamName forWrite: writeMode
	"Initialize the file with the given handle. If writeMode is true then
	 allow writing, otherwise put the file in read-only mode.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."
	fileID := aFileID.
	name := streamName.
	rwmode := writeMode.
	buffer1 := String new: 1.
	self enableReadBuffering! !


!StdIOFileStream class methodsFor: 'instance creation' stamp: 'KenD 1/24/2016 16:08'!
stderr
	| newSelf |
	newSelf := self new.
	newSelf 
		openOnHandle: self stderrHandle 
		name: 'stderr'
		forWrite: true.
	^newSelf! !

!StdIOFileStream class methodsFor: 'instance creation' stamp: 'KenD 1/24/2016 16:07'!
stdin
	| newSelf |
	newSelf := self new.
	newSelf 
		openOnHandle: self stdinHandle 
		name: 'stdin'
		forWrite: false.
	^newSelf! !

!StdIOFileStream class methodsFor: 'instance creation' stamp: 'KenD 1/24/2016 16:07'!
stdout
	| newSelf |
	newSelf := self new.
	newSelf 
		openOnHandle: self stdoutHandle 
		name: 'stdout'
		forWrite: true.
	^newSelf! !

!StdIOFileStream class methodsFor: 'accessing' stamp: 'KenD 1/24/2016 16:01'!
stderrHandle

	^ self stdioHandles at: 3! !

!StdIOFileStream class methodsFor: 'accessing' stamp: 'KenD 1/24/2016 16:01'!
stdinHandle

	^ self stdioHandles at: 1! !

!StdIOFileStream class methodsFor: 'accessing' stamp: 'KenD 1/24/2016 15:44'!
stdioHandles
" answer handles: #(stdin stdout stderr) "
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed! !

!StdIOFileStream class methodsFor: 'accessing' stamp: 'KenD 1/24/2016 16:01'!
stdoutHandle

	^ self stdioHandles at: 2! !

