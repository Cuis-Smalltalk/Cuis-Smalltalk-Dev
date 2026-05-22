Class {
	#name : 'FileIOAccessorTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-FileMan',
	#package : 'BaseImageTests',
	#tag : 'System-FileMan'
}

{ #category : 'testing' }
FileIOAccessorTest >> testDirectoryExists [
	"FileIOAccessorTest debug: #testDirectoryExists"
	| subDirString dirString |
	subDirString := 99999 atRandom asString.
	dirString := DirectoryEntry userBaseDirectory pathName, FileIOAccessor default slash, subDirString.

	FileIOAccessor default createDirectory: dirString.

	self should: [ dirString asDirectoryEntry exists ].

	FileIOAccessor default deleteDirectory: dirString.

	self shouldnt: [ dirString asDirectoryEntry exists ].
]
