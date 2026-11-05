'From Cuis7.9 [latest update: #8334] on 5 November 2026 at 6:37:40 pm'!

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/Nov/2026, 9:35:58 pm (UTC)'!
primLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On Windows, the empty string enumerates the mounted volumes/drives.
	
	On macOS, the primitive fails.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories.
	The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.
	
	Warning: The 'name' field is an instance of String, but in reality it contains the bytes for its UTF-8 representation.
	For instance, if the real name is 'puño' we'll get 'puÃ±o', as
	(String withAll: ('puño' asUtf8Bytes asArray collect: [ :n | Character codePoint: n ]))        =        'puño'
	Senders MUST do appropriate conversion.
	
	Consider calling #primFixedNameLookupEntryIn:index: instead.
	"

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath! !

