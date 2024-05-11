'From Cuis6.3 [latest update: #6355] on 10 May 2024 at 5:49:47 pm'!

!FileEntry methodsFor: 'accessing' stamp: 'bp 5/10/2024 17:44:36'!
fileSize

	self invalidateIfOld.

	"Slow version."
	"fileSize ifNil: [self fileAccessor updateEntry: self]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a directory"
	fileSize ifNil: [
		fileSize := self fileAccessor fileSize: self.
		exists := fileSize notNil ].

	^fileSize! !


!FileEntry class methodsFor: 'examples' stamp: 'bp 5/10/2024 17:42:20'!
example3
	"FileEntry example3"

	"Remove 'test1' file created example2"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') removeKey: 'test1'.

	"Recursive delete"
	'./subDir' asDirectoryEntry recursiveDelete! !

