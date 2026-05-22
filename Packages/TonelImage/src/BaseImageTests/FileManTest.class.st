"
FileManTest suite run
"
Class {
	#name : 'FileManTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-FileMan',
	#package : 'BaseImageTests',
	#tag : 'System-FileMan'
}

{ #category : 'private' }
FileManTest >> directoryEntryForTest [
	^'./fmTestDir' asDirectoryEntry
]

{ #category : 'private' }
FileManTest >> randomFileName [
	^100000 atRandom asString, '.fmtst'
]

{ #category : 'testing' }
FileManTest >> testAbsolutePath [
	"FileManTest debug: #testAbsolutePath"
	| dirEntry dirEntry1 dirEntry2 dirEntry3 |
	dirEntry := '/' asDirectoryEntry.
	self should: [dirEntry = '\' asDirectoryEntry].
	
	dirEntry1 := '/temp/' asDirectoryEntry.
	self should: [dirEntry1 = '\temp' asDirectoryEntry].

	dirEntry2 := '/temp/a' asDirectoryEntry.
	self should: [dirEntry2 = '\temp\a' asDirectoryEntry].

	dirEntry3 := 'C:/temp/b' asDirectoryEntry.
	self should: [dirEntry3 = 'C:\temp\b' asDirectoryEntry].
]

{ #category : 'testing' }
FileManTest >> testAtPut [
	"FileManTest debug: #testAtPut" 
	| dir bytes |
	dir := self directoryEntryForTest.
	dir at: 'test1' put: 'Hello'.
	self should: [(dir at: 'test1') = 'Hello'].
	self should: [dir includesKey: 'test1'].

	bytes := #(1 2 3 4 5 6) asByteArray.
	dir binaryAt: 'test2' put: bytes.
	self should: [(dir binaryAt: 'test2') = bytes].
	self should: [dir includesKey: 'test2'].

	dir removeKey: 'test1'.

	self shouldnt: [dir includesKey: 'test1'].

	dir recursiveDelete.
	self should: [dir exists not]
]

{ #category : 'testing' }
FileManTest >> testConcatenation [
	"
	FileManTest debug: #testConcatenation
	"
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa/bbb' / 'ccc' / 'ddd\eee' / 'fff/ggg').
	dir at: 'test1' put: 'RecursiveDeleted!'.

	self assert: dir name = 'ggg'.
	self assert: dir parent name = 'fff'.
	self assert: dir parent parent name = 'eee'.
	self assert: dir parent parent parent name = 'ddd'.
	self assert: dir parent parent parent parent name = 'ccc'.
	self assert: dir parent parent parent parent parent name = 'bbb'.
	self assert: dir parent parent parent parent parent parent name = 'aaa'.

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].
]

{ #category : 'testing' }
FileManTest >> testCopy [
	"FileManTest debug: #testCopy" 
	| file1 file2 |
	file1 := self randomFileName asFileEntry.
	file2 := file1 parent // self randomFileName.

	file1 fileContents: 'This is a test'.

"	self should: [file2 fileContents isEmpty]."
	self should: [file2 exists not].

	file1 copyTo: file2 pathName.

	self should: [file2 fileContents = 'This is a test'].

	file1 delete.
	file2 delete.
	self should: [file1 exists not].
	self should: [file2 exists not]
	
	
	
]

{ #category : 'testing' }
FileManTest >> testDefaultDirectory [
	"
	FileManTest debug: #testDefaultDirectory
	"
	
	| pathComponents |

	self assert: '' asDirectoryEntry =  DirectoryEntry userBaseDirectory.
	pathComponents := '' asDirectoryEntry pathComponents.
	self assert: pathComponents = DirectoryEntry userBaseDirectory pathComponents
]

{ #category : 'testing' }
FileManTest >> testFileContents [
	"FileManTest debug: #testFileContents" 
	| file1 file2 bytes |
	file1 := self randomFileName asFileEntry.
	file1 fileContents: 'This is a test'.
	self should: [file1 fileContents = 'This is a test'].
	file1 delete.
	self should: [file1 exists not].

	file2 := self randomFileName asFileEntry.
	bytes := #(1 2 3 4 5 6) asByteArray.
	file2 fileContents: bytes.
	self should: [file2 fileContents = bytes asString].
	self should: [file2 binaryContents = bytes].
	file2 delete.
	self should: [file2 exists not]
]

{ #category : 'testing' }
FileManTest >> testIsAbsolutePathName [
	"
	FileManTest debug: #testIsAbsolutePathName
	"
	self assert: '/' isAbsolutePathName.
	self assert: '/temp/' isAbsolutePathName.
	self assert: '/temp/a' isAbsolutePathName.
	Smalltalk platformName = 'Win32' ifTrue: [
		self assert: 'C:/temp/b' isAbsolutePathName ].
	
	self deny: './' isAbsolutePathName.
	self deny: '../' isAbsolutePathName.
	self deny: 'afile' isAbsolutePathName.
]

{ #category : 'testing' }
FileManTest >> testIsRelativePathName [
	"
	FileManTest debug: #testIsRelativePathName
	"
	self assert: './' isRelativePathName.
	self assert: '../' isRelativePathName.
"	self assert: 'afile' isRelativePathName."
	self deny: '/' isRelativePathName.
	self deny: '/temp/' isRelativePathName.
	self deny: '/temp/a' isRelativePathName.
	self deny: 'C:/temp/b' isRelativePathName.
	self deny: 'Macintosh HD/tmp' isRelativePathName.
]

{ #category : 'testing' }
FileManTest >> testPathComponents [
	"
	FileManTest debug: #testPathComponents
	"
	| pathComponents |

	pathComponents := './aaa/bbb\ccc/ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd').

	pathComponents := '/aaa/bbb\ccc/ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd').

	pathComponents := 'aaa/bbb\ccc/ddd' asDirectoryEntry pathComponents.
	pathComponents := pathComponents last: 4.
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd')
]

{ #category : 'testing' }
FileManTest >> testPipe [
	"FileManTest debug: #testPipe" 
	| reverseFilter file1 file2 file3 |

	reverseFilter := [:in :out | out nextPutAll: (in upToEnd reverse)].

	file1 := self randomFileName asFileEntry.
	file2 := self randomFileName asFileEntry.
	file3 := self randomFileName asFileEntry.

	file1 fileContents: 'This is a pipe test'.

	file1 pipe: reverseFilter to: file2 pathName.

	self should: [('.' asDirectoryEntry at: file1 name) = 'This is a pipe test'].	
	self should: [(file2 fileContents) = 'tset epip a si sihT'].	
"	self should: [(file3 fileContents) isEmpty]."
	self should: [file3 exists not].

	file2 pipe: reverseFilter to: file3 pathName.
	self should: [(file3 fileContents) = 'This is a pipe test'].	

	file1 delete.
	file2 delete.
	file3 delete.
	self should: [file1 exists not].
	self should: [file2 exists not].
	self should: [file3 exists not]
	
	
	
]

{ #category : 'testing' }
FileManTest >> testRecursiveDelete [
	"FileManTest debug: #testRecursiveDelete" 
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee').
	dir at: 'test1' put: 'RecursiveDelete!'.
	self should: [(dir at: 'test1') = 'RecursiveDelete!'].

	dir removeKey: 'test1'.

	self shouldnt: [(dir // 'test1') exists].

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].

	
]

{ #category : 'testing' }
FileManTest >> testRefresh [
	"FileManTest debug: #testRefresh" 
	| file1 |
	file1 := self randomFileName asFileEntry.

	file1 fileContents: '1234567890'.
	self should: [file1 fileSize = 10].

	file1 fileContents: '123'.
	self should: [file1 fileSize = 3].
	

	file1 delete.
	self should: [file1 exists not].
	
]

{ #category : 'testing' }
FileManTest >> testRename [
	"FileManTest debug: #testRename" 
	| file1 |
	file1 := self randomFileName asFileEntry.
	file1 fileContents: 'ToBeRenamed'.

	self shouldnt: [file1 name = 'newName1'].

	file1 rename: 'newName1'.

	self should: [file1 name = 'newName1'].
	self should: [file1 exists].

	self should: [file1 fileContents = 'ToBeRenamed'].

	file1 delete.
	self should: [file1 exists not].
	
]

{ #category : 'testing' }
FileManTest >> testRoot [
	"FileManTest debug: #testRoot"
	| root |
	root := DirectoryEntry roots first.
	self should: [root pathComponents isEmpty].
	FileIOAccessor default onUnix ifTrue: [
		self should: [root = '\' asDirectoryEntry]. 
		self should: [root = '/' asDirectoryEntry]]
]

{ #category : 'testing' }
FileManTest >> testStream [
	"FileManTest debug: #testStream" 
	| file1 contents formerContents allContents |
	file1 := self randomFileName asFileEntry.
	file1 writeStreamDo: [:str | str nextPutAll: 'HELLO!'].
	contents := file1 readStreamDo: [:str | str upToEnd].
	self should: [contents = 'HELLO!'].

	file1 appendStreamDo: [:str | str nextPutAll: 'AGAIN!'].

	formerContents := file1 readStreamDo: [:str | str upTo:$!].
	self should: [formerContents = 'HELLO'].

	allContents := file1 readStreamDo: [:str | str upToEnd].
	self should: [allContents = 'HELLO!AGAIN!'].

	file1 delete.
	self should: [file1 exists not].
	
]
