'From Cuis 5.0 of 7 November 2016 [latest update: #2980] on 17 November 2016 at 11:51:03 am'!
!classDefinition: #StdIOReadStream category: #'System-Support'!
Stream subclass: #StdIOReadStream
	instanceVariableNames: 'fileID name buffer1 peeked '
	classVariableNames: 'StdIn '
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #StdIOWriteStream category: #'System-Support'!
Stream subclass: #StdIOWriteStream
	instanceVariableNames: 'fileID name buffer1 collection readLimit position '
	classVariableNames: 'StdOut StdErr '
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOReadStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 11:28:43'!
peek
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!!
	
	Do not advance the stream!!"

	"Multiple calls to #peek don't make new reads"
	peeked ifFalse: [
		self privateRead.
		peeked _ true ].

	"peeked is always true on exit"
	^buffer1 at: 1! !

!StdIOReadStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 11:33:22'!
peekFor: aCharacter 
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar _ self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false! !

!StdIOReadStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 11:24:52'!
privateRead
	"Read one Character.
	Private."
	| count |
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	count = 1 ifFalse: [ buffer1 at: 1 put: nil ]! !


!StdIOReadStream class methodsFor: 'cached state access' stamp: 'jmv 11/17/2016 11:45:00'!
releaseClassCachedState

	StdIn _ nil! !


!StdIOWriteStream methodsFor: 'character writing' stamp: 'jmv 11/17/2016 11:08:02'!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !


!StdIOWriteStream class methodsFor: 'cached state access' stamp: 'jmv 11/17/2016 11:44:50'!
releaseClassCachedState

	StdOut _ nil.
	StdErr _ nil! !


!StdIOReadStream methodsFor: 'open / close' stamp: 'jmv 11/17/2016 11:13:30'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
	buffer1 _ String new: 1.
	peeked _ false! !

!StdIOReadStream methodsFor: 'streaming' stamp: 'jmv 11/17/2016 11:28:44'!
next
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!!"

	"If last call was #peek, not #next, then just answer cached value."
	peeked
		ifFalse: [ self privateRead ]
		ifTrue: [ peeked _ false ].

	"peeked is always false on exit"
	^buffer1 at: 1! !


!StdIOReadStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 11:45:39'!
stdin
	StdIn ifNil: [
		StdIn _ self basicNew.
		StdIn 
			openOnHandle: self stdinHandle 
			name: 'stdin' ].
	^StdIn! !


!StdIOWriteStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 11:46:13'!
stderr
	StdErr ifNil: [
		StdErr _ self basicNew.
		StdErr 
			openOnHandle: self stderrHandle 
			name: 'stderr' ].
	^StdErr! !

!StdIOWriteStream class methodsFor: 'instance creation' stamp: 'jmv 11/17/2016 11:46:37'!
stdout
	StdOut ifNil: [
		StdOut _ self basicNew.
		StdOut 
			openOnHandle: self stdoutHandle 
			name: 'stdout' ].
	^StdOut! !

!classDefinition: #StdIOReadStream category: #'System-Support'!
Stream subclass: #StdIOReadStream
	instanceVariableNames: 'fileID name buffer1 peeked'
	classVariableNames: 'StdIn'
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOReadStream class reorganize!
('instance creation' stdin)
('accessing' stdinHandle)
('cached state access' releaseClassCachedState)
!

!classDefinition: #StdIOWriteStream category: #'System-Support'!
Stream subclass: #StdIOWriteStream
	instanceVariableNames: 'fileID name buffer1'
	classVariableNames: 'StdErr StdOut'
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOWriteStream class reorganize!
('instance creation' stderr stdout)
('accessing' stderrHandle stdioHandles stdoutHandle)
('cached state access' releaseClassCachedState)
!

