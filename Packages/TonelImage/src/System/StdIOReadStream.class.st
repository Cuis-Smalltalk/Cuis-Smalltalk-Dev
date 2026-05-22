"
Standard Input Stream.

A basic problem/restriction with this code is that currently the VM runs multiple VM threads within a single OS thread.

This means that waiting on StdIn blocks the VM, suspending all Smalltalk code.
"
Class {
	#name : 'StdIOReadStream',
	#superclass : 'Stream',
	#instVars : [
		'fileID',
		'name',
		'buffer1',
		'peeked'
	],
	#classVars : [
		'StdIn'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'cached state access' }
StdIOReadStream class >> releaseClassCachedState [

	StdIn := nil
]

{ #category : 'instance creation' }
StdIOReadStream class >> stdin [
	StdIn ifNil: [
		StdIn := self basicNew.
		StdIn 
			openOnHandle: self stdinHandle 
			name: 'stdin' ].
	^StdIn
]

{ #category : 'accessing' }
StdIOReadStream class >> stdinHandle [

	^ StdIOWriteStream stdioHandles at: 1
]

{ #category : 'streaming' }
StdIOReadStream >> atEnd [
	^ false
]

{ #category : 'streaming' }
StdIOReadStream >> next [
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!"

	"If last call was #peek, not #next, then just answer cached value."
	peeked
		ifFalse: [ self privateRead ]
		ifTrue: [ peeked := false ].

	"peeked is always false on exit"
	^buffer1 at: 1
]

{ #category : 'open / close' }
StdIOReadStream >> openOnHandle: aFileID name: streamName [
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID := aFileID.
	name := streamName.
	buffer1 := String new: 1.
	peeked := false
]

{ #category : 'streaming' }
StdIOReadStream >> peek [
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!
	
	Do not advance the stream!"

	"Multiple calls to #peek don't make new reads"
	peeked ifFalse: [
		self privateRead.
		peeked := true ].

	"peeked is always true on exit"
	^buffer1 at: 1
]

{ #category : 'streaming' }
StdIOReadStream >> peekFor: aCharacter  [
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar := self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false
]

{ #category : 'primitives' }
StdIOReadStream >> primRead: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self error: 'File read failed'
]

{ #category : 'printing' }
StdIOReadStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream."

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
]

{ #category : 'streaming' }
StdIOReadStream >> privateRead [
	"Read one Character.
	Private."
	| count |
	count := self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	count = 1 ifFalse: [ buffer1 at: 1 put: nil ]
]
