'From Cuis7.9 [latest update: #8235] on 24 September 2026 at 5:13:07 pm'!
'Description '!
!provides: 'SystemTests' 1 1!
!requires: 'DynamicallyCodeCreatingTestSupport' 1 0 nil!
SystemOrganization addCategory: #'SystemTests-Support'!
SystemOrganization addCategory: #'SystemTests-Text'!
SystemOrganization addCategory: #'SystemTests-FileMan'!
SystemOrganization addCategory: #'SystemTests-Files'!
SystemOrganization addCategory: #'SystemTests-Object Storage'!


!classDefinition: #SystemOrganizerTest category: #'SystemTests-Support'!
DynamicallyCodeCreatingTest subclass: #SystemOrganizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Support'!
!classDefinition: 'SystemOrganizerTest class' category: #'SystemTests-Support'!
SystemOrganizerTest class
	instanceVariableNames: ''!

!classDefinition: #FileIOAccessorTest category: #'SystemTests-FileMan'!
TestCase subclass: #FileIOAccessorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-FileMan'!
!classDefinition: 'FileIOAccessorTest class' category: #'SystemTests-FileMan'!
FileIOAccessorTest class
	instanceVariableNames: ''!

!classDefinition: #FileManTest category: #'SystemTests-FileMan'!
TestCase subclass: #FileManTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-FileMan'!
!classDefinition: 'FileManTest class' category: #'SystemTests-FileMan'!
FileManTest class
	instanceVariableNames: ''!

!classDefinition: #CuisSourceFileArrayTest category: #'SystemTests-Files'!
TestCase subclass: #CuisSourceFileArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Files'!
!classDefinition: 'CuisSourceFileArrayTest class' category: #'SystemTests-Files'!
CuisSourceFileArrayTest class
	instanceVariableNames: ''!

!classDefinition: #FileStreamTest category: #'SystemTests-Files'!
TestCase subclass: #FileStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Files'!
!classDefinition: 'FileStreamTest class' category: #'SystemTests-Files'!
FileStreamTest class
	instanceVariableNames: ''!

!classDefinition: #ReferenceStreamTest category: #'SystemTests-Object Storage'!
TestCase subclass: #ReferenceStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Object Storage'!
!classDefinition: 'ReferenceStreamTest class' category: #'SystemTests-Object Storage'!
ReferenceStreamTest class
	instanceVariableNames: ''!

!classDefinition: #SmartRefStreamTest category: #'SystemTests-Object Storage'!
TestCase subclass: #SmartRefStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Object Storage'!
!classDefinition: 'SmartRefStreamTest class' category: #'SystemTests-Object Storage'!
SmartRefStreamTest class
	instanceVariableNames: ''!

!classDefinition: #IniFileTest category: #'SystemTests-Support'!
TestCase subclass: #IniFileTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Support'!
!classDefinition: 'IniFileTest class' category: #'SystemTests-Support'!
IniFileTest class
	instanceVariableNames: ''!

!classDefinition: #SystemConsistencyTest category: #'SystemTests-Support'!
TestCase subclass: #SystemConsistencyTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Support'!
!classDefinition: 'SystemConsistencyTest class' category: #'SystemTests-Support'!
SystemConsistencyTest class
	instanceVariableNames: ''!

!classDefinition: #SystemDictionaryTest category: #'SystemTests-Support'!
TestCase subclass: #SystemDictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Support'!
!classDefinition: 'SystemDictionaryTest class' category: #'SystemTests-Support'!
SystemDictionaryTest class
	instanceVariableNames: ''!

!classDefinition: #TranscriptTest category: #'SystemTests-Support'!
TestCase subclass: #TranscriptTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Support'!
!classDefinition: 'TranscriptTest class' category: #'SystemTests-Support'!
TranscriptTest class
	instanceVariableNames: ''!

!classDefinition: #TextTest category: #'SystemTests-Text'!
TestCase subclass: #TextTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Text'!
!classDefinition: 'TextTest class' category: #'SystemTests-Text'!
TextTest class
	instanceVariableNames: ''!

!classDefinition: #TrieTest category: #'SystemTests-Text'!
TestCase subclass: #TrieTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Text'!
!classDefinition: 'TrieTest class' category: #'SystemTests-Text'!
TrieTest class
	instanceVariableNames: ''!

!classDefinition: #TrieUnicodeTest category: #'SystemTests-Text'!
TestCase subclass: #TrieUnicodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SystemTests-Text'!
!classDefinition: 'TrieUnicodeTest class' category: #'SystemTests-Text'!
TrieUnicodeTest class
	instanceVariableNames: ''!


!FileManTest commentStamp: 'jmv 5/31/2016 10:49' prior: 0!
FileManTest suite run!

!CuisSourceFileArrayTest commentStamp: 'pb 1/24/2016 20:00' prior: 0!
This test documents the source pointer address conversion methods for CuisSourceFileArray.

See class comment there. 
!

!SystemConsistencyTest commentStamp: '<historical>' prior: 0!
General system - wide image health tests.!

!TranscriptTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!SystemOrganizerTest methodsFor: 'testing' stamp: 'HAW 16/May/2024, 2:00:12 pm (UTC)'!
testRenameSystemCategoryAlsoRenameClassExtensions
	
	| oldCategoryName oldExtensionName newCategoryName newExtensionName classWithCategoryToRename classOrganizer metaclassOrganizer |

	oldCategoryName := 'ACategoryToRename'. 
	oldExtensionName := '*', oldCategoryName, ' SubCategory'.
	newCategoryName := self classCategoryOfTestData.
	newExtensionName := '*', newCategoryName, ' SubCategory'.
	classWithCategoryToRename := self createClassNamed: #AClassWithCategoryToRename.
	[ classOrganizer := classWithCategoryToRename organization.
	classOrganizer addCategory: oldExtensionName.
	metaclassOrganizer := classWithCategoryToRename class organization.
	metaclassOrganizer addCategory: oldExtensionName.

	SystemOrganization renameSystemCategory: oldCategoryName to: newCategoryName.

	self assert: classWithCategoryToRename category equals: newCategoryName.
	self deny: (classOrganizer categories includes: oldExtensionName).
	self assert: (classOrganizer categories includes: newExtensionName).
	self deny: (metaclassOrganizer categories includes: oldExtensionName).
	self assert: (metaclassOrganizer categories includes: newExtensionName) ] ensure: [
		SystemOrganization removeSystemCategory: oldCategoryName ].
! !

!FileIOAccessorTest methodsFor: 'testing' stamp: 'jmv 15/Mar/2024, 8:53:35 pm (UTC)'!
testDirectoryExists
	"FileIOAccessorTest debug: #testDirectoryExists"
	| subDirString dirString |
	subDirString := 99999 atRandom asString.
	dirString := DirectoryEntry userBaseDirectory pathName, FileIOAccessor default slash, subDirString.

	FileIOAccessor default createDirectory: dirString.

	self should: [ dirString asDirectoryEntry exists ].

	FileIOAccessor default deleteDirectory: dirString.

	self shouldnt: [ dirString asDirectoryEntry exists ].! !

!FileManTest methodsFor: 'private' stamp: 'jmv 31/May/2016, 2:28:00 pm (UTC)'!
directoryEntryForTest
	^'./fmTestDir' asDirectoryEntry! !

!FileManTest methodsFor: 'private' stamp: 'jmv 31/May/2016, 2:28:00 pm (UTC)'!
randomFileName
	^100000 atRandom asString, '.fmtst'! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 4/Mar/2024, 7:35:39 pm (UTC)'!
testAbsolutePath
	"FileManTest debug: #testAbsolutePath"
	| dirEntry dirEntry1 dirEntry2 dirEntry3 |
	dirEntry := '/' asDirectoryEntry.
	self should: [dirEntry = '\' asDirectoryEntry].
	
	dirEntry1 := '/temp/' asDirectoryEntry.
	self should: [dirEntry1 = '\temp' asDirectoryEntry].

	dirEntry2 := '/temp/a' asDirectoryEntry.
	self should: [dirEntry2 = '\temp\a' asDirectoryEntry].

	dirEntry3 := 'C:/temp/b' asDirectoryEntry.
	self should: [dirEntry3 = 'C:\temp\b' asDirectoryEntry].! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:49:00 pm (UTC)'!
testAtPut
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
	self should: [dir exists not]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 4/Mar/2024, 7:35:54 pm (UTC)'!
testConcatenation
	"
	FileManTest debug: #testConcatenation
	"
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa/bbb' / 'ccc' / 'ddd\eee' / 'fff/ggg').
	dir at: 'test1' put: 'RecursiveDeleted!!'.

	self assert: dir name = 'ggg'.
	self assert: dir parent name = 'fff'.
	self assert: dir parent parent name = 'eee'.
	self assert: dir parent parent parent name = 'ddd'.
	self assert: dir parent parent parent parent name = 'ccc'.
	self assert: dir parent parent parent parent parent name = 'bbb'.
	self assert: dir parent parent parent parent parent parent name = 'aaa'.

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:49:00 pm (UTC)'!
testCopy
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
	
	
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 15/Mar/2024, 8:53:40 pm (UTC)'!
testDefaultDirectory
	"
	FileManTest debug: #testDefaultDirectory
	"
	
	| pathComponents |

	self assert: '' asDirectoryEntry =  DirectoryEntry userBaseDirectory.
	pathComponents := '' asDirectoryEntry pathComponents.
	self assert: pathComponents = DirectoryEntry userBaseDirectory pathComponents! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:49:00 pm (UTC)'!
testFileContents
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
	self should: [file2 exists not]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 15/Jun/2022, 9:16:57 pm (UTC)'!
testIsAbsolutePathName
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
	self deny: 'afile' isAbsolutePathName.! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:50:00 pm (UTC)'!
testIsRelativePathName
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
	self deny: 'Macintosh HD/tmp' isRelativePathName.! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 4/Mar/2024, 7:36:12 pm (UTC)'!
testPathComponents
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
	self assert: pathComponents asArray = #('aaa' 'bbb' 'ccc' 'ddd')! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:50:00 pm (UTC)'!
testPipe
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
	
	
	! !

!FileManTest methodsFor: 'testing' stamp: 'wbsl 15/Mar/2025, 3:27:03 am (UTC)'!
testRecursiveDelete
	"FileManTest debug: #testRecursiveDelete" 
	| dir |
	dir := ('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee').
	dir at: 'test1' put: 'RecursiveDelete!!'.
	self should: [(dir at: 'test1') = 'RecursiveDelete!!'].

	dir removeKey: 'test1'.

	self shouldnt: [(dir // 'test1') exists].

	'./subDir' asDirectoryEntry recursiveDelete.
	self shouldnt: [dir exists].
	self shouldnt: ['./subDir' asDirectoryEntry exists].

	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:50:00 pm (UTC)'!
testRefresh
	"FileManTest debug: #testRefresh" 
	| file1 |
	file1 := self randomFileName asFileEntry.

	file1 fileContents: '1234567890'.
	self should: [file1 fileSize = 10].

	file1 fileContents: '123'.
	self should: [file1 fileSize = 3].
	

	file1 delete.
	self should: [file1 exists not].
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 2:50:00 pm (UTC)'!
testRename
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
	! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 4/Mar/2024, 7:36:21 pm (UTC)'!
testRoot
	"FileManTest debug: #testRoot"
	| root |
	root := DirectoryEntry roots first.
	self should: [root pathComponents isEmpty].
	FileIOAccessor default onUnix ifTrue: [
		self should: [root = '\' asDirectoryEntry]. 
		self should: [root = '/' asDirectoryEntry]]! !

!FileManTest methodsFor: 'testing' stamp: 'jmv 31/May/2016, 3:19:00 pm (UTC)'!
testStream
	"FileManTest debug: #testStream" 
	| file1 contents formerContents allContents |
	file1 := self randomFileName asFileEntry.
	file1 writeStreamDo: [:str | str nextPutAll: 'HELLO!!'].
	contents := file1 readStreamDo: [:str | str upToEnd].
	self should: [contents = 'HELLO!!'].

	file1 appendStreamDo: [:str | str nextPutAll: 'AGAIN!!'].

	formerContents := file1 readStreamDo: [:str | str upTo:$!!].
	self should: [formerContents = 'HELLO'].

	allContents := file1 readStreamDo: [:str | str upToEnd].
	self should: [allContents = 'HELLO!!AGAIN!!'].

	file1 delete.
	self should: [file1 exists not].
	! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:26:50 pm (UTC)'!
testAddressRange
	"Test source pointer to file position address translation across the full address range"
	
	| sf |
	sf := CuisSourceFileArray new.
	(16rFC000000 to: 16rFFFFFFFF by: 811) do: [:e | | i a p |
		i := sf fileIndexFromHistoryPointer: e.
		p := sf filePositionFromHistoryPointer: e.
		a := sf historyPointerFromFileIndex: i andPosition: p.
		self assert: a = e].! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:30:41 pm (UTC)'!
testChangesFileAddressRange
	"Test file position to source pointer address translation for the changes file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 i p |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 2 andPosition: e.
		i := sf fileIndexFromHistoryPointer: a.
		self assert: i = 2.
		p := sf filePositionFromHistoryPointer: a.
		self assert: p = e.
		a2 := sf historyPointerFromFileIndex: 2 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rFE000000 and: 16rFEFFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rFE000000 and: 16rFFFFFFFF)].
! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:28:31 pm (UTC)'!
testFileIndexFromSourcePointer
	"Test derivation of file index for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file mapping"
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFC000000).
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFC000013).
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFCFFFFFF).
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFD000000).
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFD000013).
	self assert: 1 = (sf fileIndexFromHistoryPointer: 16rFDFFFFFF).
	(16rFC000000 to: 16rFCFFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromHistoryPointer: e)].
	(16rFD000000 to: 16rFDFFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromHistoryPointer: e)].
	"changes file mapping"
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFE000000).
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFE000013).
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFEFFFFFF).
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFF000000).
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFF000013).
	self assert: 2 = (sf fileIndexFromHistoryPointer: 16rFFFFFFFF).
	(16rFE000000 to: 16rFEFFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromHistoryPointer: e)].
	(16rFF000000 to: 16rFFFFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromHistoryPointer: e)].! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:29:02 pm (UTC)'!
testFilePositionFromSourcePointer
	"Test derivation of file position for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file"
	self assert: 0 = (sf filePositionFromHistoryPointer: 16rFC000000).
	"changes file"
	self assert: 0 = (sf filePositionFromHistoryPointer: 16rFE000000).! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:29:29 pm (UTC)'!
testSourcePointerFromFileIndexAndPosition
	"Test valid input ranges"

	| sf |
	sf := CuisSourceFileArray new.
	self should: [sf historyPointerFromFileIndex: 0 andPosition: 0] raise: Error.
	self shouldnt: [sf historyPointerFromFileIndex: 1 andPosition: 0] raise: Error.
	self shouldnt: [sf historyPointerFromFileIndex: 2 andPosition: 0] raise: Error.
	self should: [sf historyPointerFromFileIndex: 0 andPosition: 3] raise: Error.
	self should: [sf historyPointerFromFileIndex: 1 andPosition: -1] raise: Error.
	self shouldnt: [sf historyPointerFromFileIndex: 1 andPosition: 16rFCFFFFFF] raise: Error.
	self shouldnt: [sf historyPointerFromFileIndex: 1 andPosition: 16rFD000000] raise: Error.
	self should: [sf historyPointerFromFileIndex: 3 andPosition: 0] raise: Error.
	self should: [sf historyPointerFromFileIndex: 4 andPosition: 0] raise: Error.
	
	self assert: 16rFC000000 = (sf historyPointerFromFileIndex: 1 andPosition: 0).
	self assert: 16rFE000000 = (sf historyPointerFromFileIndex: 2 andPosition: 0).! !

!CuisSourceFileArrayTest methodsFor: 'testing' stamp: 'jmv 23/Sep/2026, 3:31:00 pm (UTC)'!
testSourcesFileAddressRange
	"Test file position to source pointer address translation for the sources file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 p i |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 1 andPosition: e.
		i := sf fileIndexFromHistoryPointer: a.
		self assert: i = 1.
		p := sf filePositionFromHistoryPointer: a.
		self assert: p = e.
		a2 := sf historyPointerFromFileIndex: 1 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rFC000000 and: 16rFCFFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e := ee // 32 * 32.
		a := sf historyPointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rFC000000 and: 16rFDFFFFFF)].! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 27/Oct/2025, 3:01:22 pm (UTC)'!
testAtEnd
	| path fs fragment1 fragment2 |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	
	fragment1 := 'Some stuff.'.
	path asFileEntry fileContents: fragment1.
	fs := path asFileEntry readStream.
	self deny: fs atEnd.
	self assert: fs upToEnd =  'Some stuff.'.
	self assert: fs atEnd.
	self assert: fs size = fragment1 size.

	fragment2 := 'Some stuff'.
	path asFileEntry appendContents: fragment2.
	self assert: fs size = (fragment1 size + fragment2 size).
	self deny: fs atEnd.
	self assert: fs upToEnd =  fragment2.
	self assert: fs atEnd.
	
	fs close .
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:09:27 am (UTC)'!
testUpTo1ShortRecords
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	path asFileEntry fileContents: 'record-1Xrecord-2Xrecord-incomplete'. 
	fs := path asFileEntry readStream . 
	self assert:  ((fs upTo: $X) =  'record-1').
	self assert:  ((fs upTo: $X) =  'record-2').
	self assert:  ((fs upTo: $X) =  'record-incomplete').
	self assert:  ((fs upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (fs position = 35). 
	fs close . 
	path asFileEntry delete.

	! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:08:32 am (UTC)'!
testUpTo2IncompleteRecords
	"
	. This test stresses the motivation for introducing upTo:delimiterIsTerminator:
	. if two separate processes are reading and writing records into the same file 
	upTo can potentially create a bit a of a mess by reading in half records and complete records.
	. please compare to the equivalent testUpToTerminatorX to see what is probably the behaviour your are looking for 
	"
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. } .
	" a process starts to write data into a file but it does not end cleanly the writing "
	path asFileEntry fileContents: 'record-1Xrec'. 
	fs := path asFileEntry readStream . 
	" upTo reads 'record-1' and the second time reads half a token "
	self assert:  ((fs upTo: $X) =  'record-1').
	self assert:  ((fs upTo: $X) =  'rec').
	". the writing process comes back online and terminates its record writing "
	path asFileEntry appendContents: 'ord-2Xrecord-3X'.
	". upTo reads  an half token and then a complete one  "
	self assert:  ((fs upTo: $X) =  'ord-2').
	self assert:  ((fs upTo: $X) =  'record-3').
	". when there is nothing more to read upTo returns the empty string " 
	self assert:  ((fs upTo: $X) =  '').
	fs close .
	path asFileEntry delete.

	! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:09:35 am (UTC)'!
testUpTo3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| path longString fs read |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	longString := (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	path asFileEntry fileContents: longString.
	fs := path asFileEntry readStream.
	read := fs upTo: $X.
	self assert: read = longString.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:10:50 am (UTC)'!
testUpTo4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return all the chunk  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 5000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i >= 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	read := fs upTo: $X.
	self assert: read size = 5000.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:05 am (UTC)'!
testUpTo5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time the second block up to EOF.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			i > 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, the delimiter is found but not printed.  "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan. the delimiter is not found, all second chunk is returned  "
	read := fs upTo: $X.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:11 am (UTC)'!
testUpTo6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end. expected to return 
      two big chunks in two read, delimiters excluded.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			((Interval
				from: 3001
				to: 5999) includes: i) ifTrue: [ fs nextPut: $b ].
			i = 6000 ifTrue: [ fs nextPut: $X ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter excluded "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan, return chunk, delimiter excluded.  "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:14:02 am (UTC)'!
testUpToAll
	| string path fromFile fromString |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. } .
	string := UnicodeString fromUtf8Bytes: #[57 57 194 160 226 130 172].
	DirectoryEntry userBaseDirectory at: path put: string.
	self assert: ((DirectoryEntry userBaseDirectory at: path) = string).
	fromString := string readStream upToAll: (string copyFrom: 3 to: 4).
	fromFile := (DirectoryEntry userBaseDirectory // path) readStream upToAll: (string copyFrom: 3 to: 4).
	self assert: fromFile = fromString.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:10:00 am (UTC)'!
testUpToTerminator1ShortRecords
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	path asFileEntry fileContents: 'record-1Xrecord-2Xrecord-incomplete'. 
	fs := path asFileEntry readStream . 
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (fs position = 18).
	fs close . 
	path asFileEntry delete .
	! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:10:04 am (UTC)'!
testUpToTerminator2IncompleteRecords
	"
	. This test stresses the motivation for introducing upTo:delimiterIsTerminator: 
	. if two separate processes are reading and writing records into the same file 
	upTo:delimiterIsTerminator:true will not loose any token and will never return half written tokens.
	"
	|path fs|
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	" a process starts to write data into a file but it does not end cleanly the writing "
	path asFileEntry fileContents: 'record-1Xrec'. 
	fs := path asFileEntry readStream . 
	" upTo:strict reads upto what it can find and ignores the rest "
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	". the writing process comes back online and terminates its record writing "
	path asFileEntry appendContents: 'ord-2Xrecord-3X'.
	". upTo:strict is ready and reads just completed tokens and brand new ones.  "
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-3X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	fs close .
	path asFileEntry delete.

	! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:29 am (UTC)'!
testUpToTerminator3LongUnterminatedRecord
	"Long input, no delimiter found, expected to return nil. "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	path asFileEntry fileContents:
		((1 to: 100)
			inject: ''
			into: [ :prev :each |
				prev , 'A lot of stuff, needs over 2000 chars!! ' ]).
	fs := path asFileEntry readStream.
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:35 am (UTC)'!
testUpToTerminator4LongUnterminatedRecord
	"Big chunk, not delimiter found, expected return nil  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 5000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i >= 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:43 am (UTC)'!
testUpToTerminator5TerminatedAndUnterminatedLongRecords
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time nil.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			i > 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter included "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter not found, returns nil  "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.! !

!FileStreamTest methodsFor: 'tests' stamp: 'jmv 20/May/2024, 12:11:49 am (UTC)'!
testUpToTerminator6TerminatedLongRecords
	"Two big chunks, one delimiter in the middle, one at the end expected to return 
      two big chunks in two read, delimiters included.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			((Interval
				from: 3001
				to: 5999) includes: i) ifTrue: [ fs nextPut: $b ].
			i = 6000 ifTrue: [ fs nextPut: $X ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter included "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter found, return chunk, delimiter included   "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 26/Sep/2012, 2:42:00 am (UTC)'!
testDiskProxy
	"
	ReferenceStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 17/Sep/2025, 12:36:46 pm (UTC)'!
testSmallIntegers
	"
	ReferenceStreamTest new testSmallIntegers
	"
	| newInstance oldInstance |
	oldInstance := {-1. 0 . 1. 123456. -123456. 1234567890. -1234567890. 123456789012345678. -123456789012345678. (2 raisedTo: 60)-1. (2 raisedTo: 60) negated }.
	newInstance := ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	self assert: newInstance = oldInstance.! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 6/Dec/2011, 12:19:00 pm (UTC)'!
testSortedCollection
	"
	ReferenceStreamTest new testSortedCollection
	"
	| newInstance oldInstance |
	oldInstance _ SortedCollection sortBlock: [ :a :b | a printString < b printString ].
	oldInstance add: 'hi'; add: 'there'; add: 'you'; add: 'all'.
	newInstance _ ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	self assert: newInstance asArray = oldInstance asArray! !

!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 2/Jul/2022, 12:23:32 am (UTC)'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	ReferenceStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ ReferenceStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	UISupervisor whenUIinSafeState: [window delete].! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 26/Sep/2012, 2:42:00 am (UTC)'!
testDiskProxy
	"
	SmartRefStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 6/Dec/2011, 12:34:00 pm (UTC)'!
testSortedCollection
	"
	SmartRefStreamTest new testSortedCollection
	"
	| newInstance oldInstance |
	oldInstance _ SortedCollection sortBlock: [ :a :b | a printString < b printString ].
	oldInstance add: 'hi'; add: 'there'; add: 'you'; add: 'all'.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	self assert: newInstance asArray = oldInstance asArray! !

!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 2/Jul/2022, 12:23:37 am (UTC)'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	SmartRefStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ SmartRefStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	UISupervisor whenUIinSafeState: [window delete].! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:43:19 pm (UTC)'!
testBasic
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	section at: 'bar' put: 'baz1'.
	self assert: (section at: 'bar') equals: 'baz1'.
	self assert: (ini atSection: 'foo1' entry: 'bar') equals: 'baz1'.

	ini atSection: 'foo2' entry: 'bar' put: 'baz2'.
	self assert: (ini atSection: 'foo1' entry: 'bar') equals: 'baz1'.
	self assert: ((ini sectionNamed: 'foo1') at: 'bar') equals: 'baz1'.
	self assert: (ini atSection: 'foo2' entry: 'bar') equals: 'baz2'.
	self assert: ((ini sectionNamed: 'foo2') at: 'bar') equals: 'baz2'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:46:00 pm (UTC)'!
testBasicGlobal
	| ini |
	ini := IniFile new.

	ini globalEntry: 'bar' put: 'baz1'.
	self assert: (ini globalSection at: 'bar') equals: 'baz1'.
	self assert: (ini atGlobalEntry: 'bar') equals: 'baz1'.
! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:35:30 pm (UTC)'!
testMissingEntry
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	self assert: (section at: 'bar') isNil.
	self assert: (section at: 'bar' orDefault: 'whoknows1?') equals: 'whoknows1?'.
	self assert: (ini atSection: 'foo1' entry: 'bar' orDefault: 'whoknows2?') equals: 'whoknows2?'.
	self assert: (ini atSection: 'foo3' entry: 'bar') isNil.
	self assert: (ini atSection: 'foo3' entry: 'bar' orDefault: 'whoknows3?') equals: 'whoknows3?'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:25:23 pm (UTC)'!
testMissingEntryStoringDefault
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	self assert: (section at: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (section at: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:38:30 pm (UTC)'!
testMissingGlobalEntry
	| ini |
	ini := IniFile new.

	self assert: ini globalSection isNil.
	ini ensureGlobalSection.
	self assert: ini globalSection notNil.
	self assert: (ini globalSection at: 'bar') isNil.
	self assert: (ini globalSection at: 'bar' orDefault: 'whoknows1?') equals: 'whoknows1?'.
	self assert: (ini atGlobalEntry: 'bar' orDefault: 'whoknows2?') equals: 'whoknows2?'.
	self assert: (ini atGlobalEntry: 'bar') isNil.
	self assert: (ini atGlobalEntry: 'bar' orDefault: 'whoknows3?') equals: 'whoknows3?'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:40:53 pm (UTC)'!
testMissingGlobalEntryStoringDefault1
	| ini section |
	ini := IniFile new.

	section := ini ensureGlobalSection.
	self assert: section notNil.
	self assert: (section at: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (section at: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:41:53 pm (UTC)'!
testMissingGlobalEntryStoringDefault2
	| ini |
	ini := IniFile new.

	self assert: (ini atGlobalEntry: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atGlobalEntry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (ini atGlobalEntry: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atGlobalEntry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:42:56 pm (UTC)'!
testMissingGlobalSection
	| ini |
	ini := IniFile new.

	self assert: ini globalSection isNil.
	ini ensureGlobalSection.
	self deny: ini globalSection isNil.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 2:26:31 pm (UTC)'!
testMissingSection
	| ini |
	ini := IniFile new.

	self assert: (ini sectionNamed: 'foo1') isNil.
	ini ensureSectionNamed: 'foo1'.
	self deny: (ini sectionNamed: 'foo1') isNil.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:02:50 pm (UTC)'!
testReadEmptyValue
	| content ini |
	content := '[foo]
key1 =      '.

	ini := IniFile readFrom: content readStream.
	self assert: ((ini sectionNamed: 'foo') at: 'key1') isNil! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:04:51 pm (UTC)'!
testReadGlobalAndTwoNamedSections
	| content ini |
	content := '
key1 = val1global

key2 = val2global

[bar]
;comment

key1 = val1bar

key2 = val2
;key2 = oldvalue

;foo heading comment
[foo]
key1=    val1
key2= val with a space in it

'.

	ini := IniFile readFrom: content readStream.
	self assert: (ini atGlobalEntry: 'key1') = 'val1global'.
	self assert: (ini atGlobalEntry: 'key2') = 'val2global'.
	self assert: (ini atSection: 'foo' entry: 'key1') = 'val1'.
	self assert: (ini atSection: 'foo' entry: 'key2') = 'val with a space in it'.
	self assert: (ini atSection: 'bar' entry: 'key1') = 'val1bar'.
	self assert: (ini atSection: 'bar' entry: 'key2') = 'val2'.
! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:03:48 pm (UTC)'!
testReadTwoSections
	| content ini |
	content := '

[bar]
;comment

key1 = val1bar

key2 = val2
;key2 = oldvalue

;foo heading comment
[foo]
key1=    val1
key2= val with a space in it

'.

	ini := IniFile readFrom: content readStream.
	self assert: ini globalSection isNil.
	self assert: (ini atSection: 'foo' entry: 'key1') = 'val1'.
	self assert: (ini atSection: 'foo' entry: 'key2') = 'val with a space in it'.
	self assert: (ini atSection: 'bar' entry: 'key1') = 'val1bar'.
	self assert: (ini atSection: 'bar' entry: 'key2') = 'val2'.
! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:35:38 pm (UTC)'!
testWriteGlobalSection
	| out written globalSection |
	out := IniFile new.
	out globalEntry: 'key1' put: 'val1'.
	out globalEntry: 'key2' put: 'val2'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	globalSection := (IniFile readFrom: written readStream) globalSection.
	self assert: (globalSection at: 'key1') = 'val1'.
	self assert: (globalSection at: 'key2') = 'val2'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:36:00 pm (UTC)'!
testWriteMultipleSections
	| out written in |
	out := IniFile new.
	(out ensureSectionNamed: 'foo') at: 'key1' put: 'val1'.
	(out ensureSectionNamed: 'foo') at: 'key2' put: 'val2'.
	(out ensureSectionNamed: 'bar') at: 'key1' put: 'val1x'.
	(out ensureSectionNamed: 'bar') at: 'key2' put: 'val2y'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	in := IniFile readFrom: written readStream.
	self assert: ((in sectionNamed: 'foo') at: 'key1') = 'val1'.
	self assert: ((in sectionNamed: 'foo') at: 'key2') = 'val2'.
	self assert: ((in sectionNamed: 'bar') at: 'key1') = 'val1x'.
	self assert: ((in sectionNamed: 'bar') at: 'key2') = 'val2y'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:37:12 pm (UTC)'!
testWriteMultipleSections2
	| out written in |
	out := IniFile new.
	out globalEntry: 'key1' put: 'g1'.
	out globalEntry: 'key2' put: 'g2'.
	(out ensureSectionNamed: 'foo') at: 'key1' put: 'val1'.
	(out ensureSectionNamed: 'foo') at: 'key2' put: 'val2'.
	(out ensureSectionNamed: 'bar') at: 'key1' put: 'val1x'.
	(out ensureSectionNamed: 'bar') at: 'key2' put: 'val2y'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	in := IniFile readFrom: written readStream.
	self assert: (in atGlobalEntry: 'key1') = 'g1'.
	self assert: (in atGlobalEntry: 'key2') = 'g2'.
	self assert: ((in sectionNamed: 'foo') at: 'key1') = 'val1'.
	self assert: ((in sectionNamed: 'foo') at: 'key2') = 'val2'.
	self assert: ((in sectionNamed: 'bar') at: 'key1') = 'val1x'.
	self assert: ((in sectionNamed: 'bar') at: 'key2') = 'val2y'.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 18/Oct/2024, 7:44:51 pm (UTC)'!
testWriteNilGlobalValue
	| ini written expected |
	ini := IniFile new.
	ini globalEntry: 'key1' put: nil.
	written := String streamContents: [ :stream | ini writeOn: stream ].
	expected := 'key1=
'.
	self assert: written equals: expected.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 18/Oct/2024, 7:44:45 pm (UTC)'!
testWriteNilValue
	| ini written expected |
	ini := IniFile new.
	ini atSection: 'foo' entry: 'key1' put: nil.
	written := String streamContents: [ :stream | ini writeOn: stream ].
	expected := '[foo]
key1=

'.
	self assert: written equals: expected.! !

!IniFileTest methodsFor: 'tests' stamp: 'jmv 10/Jul/2024, 3:28:51 pm (UTC)'!
testWriteSingleSection
	| out written foo |
	out := IniFile new.
	out atSection: 'foo' entry: 'key1' put: 'val1'.
	out atSection: 'foo' entry: 'key2' put: 'val2'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	foo := (IniFile readFrom: written readStream) sectionNamed: 'foo'.
	self assert: (foo at: 'key1') = 'val1'.
	self assert: (foo at: 'key2') = 'val2'.! !

!SystemConsistencyTest methodsFor: 'testing' stamp: 'jmv 7/May/2012, 1:35:00 pm (UTC)'!
testMethodsWithUnboundGlobals
	"
	SystemConsistencyTest new testMethodsWithUnboundGlobals
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty! !

!TranscriptTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 9:19:12 pm (UTC)'!
assertTranscriptContentsAdded: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 9:19:06 pm (UTC)'!
assertTranscriptContentsAdded2: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	{transcriptContentsBeforeBlockEvaluation. expectedAddedString.Transcript contents} inspect.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptTest methodsFor: 'assertions' stamp: 'HAW 20/Mar/2019, 11:15:48 am (UTC)'!
assertTranscriptContentsDoesNotChangeAfter: aBlock
	
	self assertTranscriptContentsAdded: '' after: aBlock! !

!TextTest methodsFor: 'testing' stamp: 'jmv 17/May/2026, 10:11:31 pm (UTC)'!
testConcatenation
	| t1 t2 |

	t1 := 'sample' bold.
	t2 := 'text' italic.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold optimizedForMutationSpeed.
	t2 := 'text' italic.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold.
	t2 := 'text' italic optimizedForMutationSpeed.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold optimizedForMutationSpeed.
	t2 := 'text' italic optimizedForMutationSpeed.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).! !

!TextTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:40:53 am (UTC)'!
testIsText
	self assert: (Text new is: #Text).! !

!TextTest methodsFor: 'testing' stamp: 'jmv 17/May/2026, 10:40:12 pm (UTC)'!
testRunsBehavior
	"Weird but true since Smalltalk-80"
	| c |
	self assert: ('stuff' bold runs at: 1000) = {TextEmphasis bold}.
	self assert: ('stuff' bold optimizedForMutationSpeed runs at: 1000) = {TextEmphasis bold}.
	
	self assert: ('stuff' bold runs copyFrom: 34 to: 34) first = {TextEmphasis bold}.
	self assert: ('stuff' bold optimizedForMutationSpeed runs copyFrom: 34 to: 34) first = {TextEmphasis bold}.
	
	self assert: ('stuff' bold runs rangeOf: TextEmphasis bold startingAt: 3) = (1 to: 5).
	self assert: ('stuff' bold optimizedForMutationSpeed runs rangeOf: TextEmphasis bold startingAt: 3) = (1 to: 5).
	
	self assert: ('stuff' bold runs runLengthAt: 3) = 3.
	self assert: ('stuff' bold optimizedForMutationSpeed runs runLengthAt: 3) = 3.
	
	c := OrderedCollection new.
	'stuff' bold runs runsFrom: 1 to: 5 do: [ :attr | c add: attr  ].
	self assert: c asSet asArray = {{TextEmphasis bold}}.
	c := OrderedCollection new.
	'stuff' bold optimizedForMutationSpeed runs runsFrom: 1 to: 5 do: [ :attr | c add: attr ].
	self assert: c asSet asArray = {{TextEmphasis bold}}.
	
	self assert: ('stuff' bold runs copyReplaceFrom: 3 to: 20 with: 'extralongstuff' italic runs) size = 16.
	self assert: ('stuff' bold optimizedForMutationSpeed runs copyReplaceFrom: 3 to: 20 with: 'extralongstuff' italic optimizedForMutationSpeed runs) size = 16.! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 22/Jun/2011, 2:05:00 am (UTC)'!
assert: subject isEquivalentToDictionary: controlGroup
	"Test #size, #do:, "
	| count subjectAsDictionary someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup keysDo: [ :each |
		self assert: (subject includesKey: each) ].
	subject keysDo: [ :each |
		self assert: (controlGroup includesKey: each) ].
	controlGroup keysAndValuesDo: [ :k :v |
		self assert: (subject at: k) = v ].
	subject keysAndValuesDo: [ :k :v |
		self assert: (controlGroup at: k) = v ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includesKey: someRandomString)
		= (controlGroup includesKey: someRandomString).
	subjectAsDictionary _ Dictionary new.
	count _ 0.
	subject keysAndValuesDo: [ :k :v |
		subjectAsDictionary at: k put: v.
		count _ count + 1 ].
	self assert: subjectAsDictionary size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsDictionary = controlGroup.! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 15/Jun/2011, 1:58:00 pm (UTC)'!
assert: subject isEquivalentToSet: controlGroup
	"Test #size, #do:, "
	| count subjectAsSet someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup do: [ :each |
		self assert: (subject includes: each) ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includes: someRandomString)
		= (controlGroup includes: someRandomString).
	subjectAsSet _ Set new.
	count _ 0.
	subject do: [ :element |
		subjectAsSet add: element.
		count _ count + 1 ].
	self assert: subjectAsSet size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsSet = controlGroup.! !

!TrieTest methodsFor: 'aux' stamp: 'jmv 22/Jun/2011, 4:51:00 pm (UTC)'!
assert: subject prefixSelection: aString isEquivalentToSet: controlGroup
	| selection1 selection2 |
	selection1 _ OrderedCollection new.
	subject forPrefix: aString keysAndValuesDo: [ :k :v |
		selection1 add: k ].
	selection2 _ (controlGroup select: [ :each |
		aString isEmpty or: [
			each asLowercase asUnaccented beginsWith: aString asLowercase asUnaccented]]) asOrderedCollection 
				sort: [ :a :b | a asLowercase asUnaccented < b asLowercase asUnaccented ].
	self assert: selection1 = selection2.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 2:14:00 am (UTC)'!
testAllMesssagesLikeDicionary
	"
	TrieTest new testAllMesssagesLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].
	self assert: subject isEquivalentToDictionary: controlGroup.

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 2:01:00 am (UTC)'!
testAllMesssagesLikeSet
	"
	TrieTest new testAllMesssagesLikeSet
	"
	| subject  controlGroup allMessages copy|
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].
	self assert: subject isEquivalentToSet: controlGroup.

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 12/Jun/2019, 9:38:28 pm (UTC)'!
testIncludesReturnsFalseForObjectsThatAreNotString
	
	self deny: ((Trie with: 'hello') includes: 1)
	! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 12/Jun/2019, 9:38:51 pm (UTC)'!
testIncludesReturnsFalseForStringsNotIncluded
	
	self deny: ((Trie with: 'hello') includes: 'bye')
	! !

!TrieTest methodsFor: 'testing' stamp: 'HAW 12/Jun/2019, 9:39:09 pm (UTC)'!
testIncludesReturnsTrueForIncludedStrings
	
	self assert: ((Trie with: 'hello') includes: 'hello')
	! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 20/Sep/2023, 7:43:54 pm (UTC)'!
testKeyNotFoundSignalTheRightMessage

	self 
		should: [ Trie new errorKeyNotFound: 'theKey' ]
		raise: Error
		withMessageText: 'key: ''theKey'' not found'! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 4:30:00 pm (UTC)'!
testPrefixIteration
	"
	TrieTest new testPrefixIteration
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'cat'.
	controlGroup add: 'cat'.
	subject add: 'cart'.
	controlGroup add: 'cart'.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	subject add: 'ñandues'.
	controlGroup add: 'ñandues'.
	subject add: 'ÑANDÚSES'.
	controlGroup add: 'ÑANDÚSES'.
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñan' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandú' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'Ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ÑANDÚ' isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 4:50:00 pm (UTC)'!
testPrefixIteration2
	"
	TrieTest new testPrefixIteration2
	"
	| subject  controlGroup allMsg toAvoid |
	subject _ Trie new.
	controlGroup _ Set new.

	allMsg _ Smalltalk allImplementedMessages.
	toAvoid _ (allMsg asArray collect: [ :each | each asLowercase asUnaccented ]) asBag.
	allMsg do: [ :symbol |
		(toAvoid occurrencesOf: symbol) = 1 ifTrue: [
			subject add: symbol.
			controlGroup add: symbol ]].
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'a' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:p' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:pu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:nonExistant:' isEquivalentToSet: controlGroup.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 23/Jun/2011, 3:02:00 pm (UTC)'!
testRemoveLikeDicionary
	"
	TrieTest new testRemoveLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.

	subject at: 'doNotRemove' put: 'this'.
	controlGroup at: 'doNotRemove' put: 'this'.
	
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].

	subject at: 'doNotRemove2' put: 'this2'.
	controlGroup at: 'doNotRemove2' put: 'this2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	
	allMessages do: [ :symbol |
		subject removeKey: symbol.
		controlGroup removeKey: symbol ].

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2.
	self assert: subject isEmpty = controlGroup isEmpty.

	subject removeKey: 'doNotRemove'.
	controlGroup removeKey: 'doNotRemove'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject removeKey: 'doNotRemove2'.
	controlGroup removeKey: 'doNotRemove2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 7:54:00 pm (UTC)'!
testRemoveLikeSet
	"
	TrieTest new testRemoveLikeSet
	"
	| subject  controlGroup allMessages copy |
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.

	subject add: 'doNotRemove'.
	controlGroup add: 'doNotRemove'.
	
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].

	subject add: 'doNotRemove2'.
	controlGroup add: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	
	allMessages do: [ :symbol |
		subject remove: symbol.
		controlGroup remove: symbol ].

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.

	subject remove: 'doNotRemove'.
	controlGroup remove: 'doNotRemove'.

	self assert: subject isEquivalentToSet: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject remove: 'doNotRemove2'.
	controlGroup remove: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.! !

!TrieTest methodsFor: 'testing' stamp: 'jmv 22/Jun/2011, 7:42:00 pm (UTC)'!
testSetProtocol
	"
	TrieTest new testSetProtocol
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.
	self assert: subject isEquivalentToSet: controlGroup.
	
	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cat'.
	controlGroup add: 'cat'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cart'.
	controlGroup add: 'cart'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandu'.
	controlGroup add: 'ñandu'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ÑANDÚ'.
	controlGroup add: 'ÑANDÚ'.
	self assert: subject isEquivalentToSet: controlGroup.! !

!TrieUnicodeTest methodsFor: 'testing' stamp: 'jmv 6/Jul/2022, 12:11:26 pm (UTC)'!
test01
	| t all |
	t _ Trie new.
	t add: #toto.
	t at: #toton put: 47.
	t add: 'carrr' asByteString.
	t at: 'car' asByteString put: Float pi.
	t at: 'cat' asByteString put: Date today.
	self assert: (t includesKey: 'car').
	self deny: (t includes: 'car').
	self assert: (t includesKey: 'cat').
	self deny: (t includes: 'cat').
	self assert: (t includes: #toto).
	self assert: (t includes: 'toto').
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 5.
	self assert: (all includes: {#toto. #toto class. #toto. #toto class}).
	self assert: (all includes: {#toton. #toton class. 47. SmallInteger}).
	self assert: (all includes: {'carrr'. String. 'carrr'. String}).
	self assert: (all includes: {'car'. String. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. String. Date today. Date}).! !

!TrieUnicodeTest methodsFor: 'testing' stamp: 'jmv 7/Dec/2022, 3:46:05 pm (UTC)'!
test02
	| t all |
	t _ Trie new.
	t add: #toto.
	t at: #toton put: 47.
	t add: 'carrrs' asByteString.
	t add: 'carrr' asUnicodeString.
	t at: 'car' asUnicodeString put: Float pi.
	t at: 'cat' asByteString put: Date today.
	self assert: (t includesKey: 'carrrs' asByteString).
	self assert: (t includes: 'carrrs' asByteString).
	self assert: (t includesKey: 'carrrs' asUnicodeString).
	self assert: (t includes: 'carrrs' asUnicodeString).
	self assert: (t includesKey: 'carrr' asByteString).
	self assert: (t includes: 'carrr' asByteString).
	self assert: (t includesKey: 'carrr' asUnicodeString).
	self assert: (t includes: 'carrr' asUnicodeString).
	self assert: (t includesKey: 'car' asByteString).
	self deny: (t includes: 'car' asByteString).
	self assert: (t includesKey: 'cat' asByteString).
	self deny: (t includes: 'cat' asByteString).
	self assert: (t includes: #toto).
	self assert: (t includes: 'toto' asByteString).
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 6.
	self assert: (all includes: {#toto. #toto class. #toto. #toto class}).
	self assert: (all includes: {#toton. #toton class. 47. SmallInteger}).
	self assert: (all includes: {'carrrs'. String. 'carrrs'. String}).
	self assert: (all includes: {'carrr'. UnicodeString. 'carrr'. UnicodeString}).
	self assert: (all includes: {'car'. UnicodeString. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. String. Date today. Date}).! !

!TrieUnicodeTest methodsFor: 'testing' stamp: 'jmv 20/Jan/2023, 11:51:57 pm (UTC)'!
test03
	| t all s1 s2 |
	t _ Trie new.
	s1 _ ('totoU' asByteString, Random next mantissaPart printString) asUnicodeString asSymbol.
	t add: s1.
	s2 _ ('totóUn' asByteString, Random next mantissaPart printString) asUnicodeString asSymbol.
	t at: s2 put: 47.
	t add: 'carrr' asUnicodeString.
	t at: 'car' asByteString put: Float pi.
	t at: 'cat' asUnicodeString put: Date today.
	self assert: (t includesKey: 'car' asByteString).
	self deny: (t includes: 'car' asByteString).
	self assert: (t includesKey: 'cat' asByteString).
	self deny: (t includes: 'cat' asByteString).
	self assert: (t includes: s1).
	self assert: (t includes: s1 asString).
	all _ Array streamContents: [ :strm |
		t keysAndValuesDo: [ :k :v | strm nextPut: {k. k class. v. v class} ]].
	self assert: all size = 5.
	self assert: (all includes: {s1. Symbol. s1. Symbol}).
	self assert: (all includes: {s2. UnicodeSymbol. 47. SmallInteger}).
	self assert: (all includes: {'carrr'. UnicodeString. 'carrr'. UnicodeString}).
	self assert: (all includes: {'car'. String. Float pi. SmallFloat64}).
	self assert: (all includes: {'cat'. UnicodeString. Date today. Date}).! !
