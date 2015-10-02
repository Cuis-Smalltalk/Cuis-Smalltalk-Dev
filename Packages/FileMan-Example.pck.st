'From Cuis 4.2 of 25 July 2013 [latest update: #1969] on 29 April 2014 at 10:43:04.240305 pm'!
'Description Please enter a description for this package'!
!provides: 'FileMan-Example' 1 3!
!classDefinition: #FmBackgroundDirectoryEntry category: #'FileMan-Example'!
FmDirectoryEntry subclass: #FmBackgroundDirectoryEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Example'!
!classDefinition: 'FmBackgroundDirectoryEntry class' category: #'FileMan-Example'!
FmBackgroundDirectoryEntry class
	instanceVariableNames: ''!

!classDefinition: #FmBackupDirectoryEntry category: #'FileMan-Example'!
FmDirectoryEntry subclass: #FmBackupDirectoryEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Example'!
!classDefinition: 'FmBackupDirectoryEntry class' category: #'FileMan-Example'!
FmBackupDirectoryEntry class
	instanceVariableNames: ''!

!classDefinition: #FmGZipDirectoryEntry category: #'FileMan-Example'!
FmDirectoryEntry subclass: #FmGZipDirectoryEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Example'!
!classDefinition: 'FmGZipDirectoryEntry class' category: #'FileMan-Example'!
FmGZipDirectoryEntry class
	instanceVariableNames: ''!


!FmBackgroundDirectoryEntry commentStamp: '<historical>' prior: 0!
This is a simple example for adding special behaviors to FmDirectoryEntry.

I write file contents in background, if its contets is big.

---
mu 3/10/2008 15:33!

!FmBackupDirectoryEntry commentStamp: 'mu 5/4/2007 23:26' prior: 0!
This is a simple example for adding special behaviors to FmDirectoryEntry.

I backup file contents automatically, while users are not conscious about that.

Usage:
dir := './withBackup' asDirectoryEntry: FmBackupDirectoryEntry.
dir at: 'text' put: 'abc'.
dir at: 'text' put: 'def'.
(dir at: 'text') inspect. "def"
(dir backupAt: 'text') inspect. "abc"
((dir / 'sub') at: 'text2' put: '123').
((dir / 'sub') at: 'text2' put: '456').
((dir / 'sub') at: 'text2') inspect. "456"
((dir / 'sub') backupAt: 'text2') inspect. "123"

---
mu 5/4/2007 17:15!

!FmGZipDirectoryEntry commentStamp: 'mu 3/29/2009 23:56' prior: 0!
This is a simple example for adding special behaviors to FmDirectoryEntry.

I put and get file contents as gzipped, while users are not conscious about that.

Usage:
| dir |
dir := './gzipped2' asDirectoryEntry: FmGZipDirectoryEntry.
dir binaryAt: 'bin' put: #(1 2 3 12 34 56) asByteArray.
(dir binaryAt: 'bin') inspect.
dir at: 'text' put: 'This will be gzipped'.
(dir at: 'text') inspect.

I would be useful for storing/loading big contents in a simple dictionary-like manner. 

---
mu 5/4/2007 17:15!

!FmBackgroundDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/10/2008 15:29'!
at: localFileName put: contents 
	contents size > 1024 * 1024
		ifTrue: [self at: localFileName putBackground: contents]
		ifFalse: [super at: localFileName put: contents]! !

!FmBackgroundDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/27/2007 15:53'!
at: localFileName putBackground: contents 
	self assureExistence.
	[self setContentsOf: (self fileAccessor forceNewFileNamed: (self / localFileName) pathName) to: contents.
	self refreshChildren] forkAt: Processor userBackgroundPriority.
	^contents! !

!FmBackupDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 19:06'!
at: localFileName put: contents 
	(self includesKey: localFileName)
		ifTrue: [super at: localFileName , '.bak' put: (self at: localFileName)].
	super at: localFileName put: contents! !

!FmBackupDirectoryEntry methodsFor: 'dictionary-like-backup' stamp: 'mu 5/4/2007 19:08'!
backupAt: localFileName
	^self at: localFileName , '.bak' ! !

!FmBackupDirectoryEntry methodsFor: 'dictionary-like-backup' stamp: 'mu 5/4/2007 19:09'!
backupBinaryAt: localFileName
	^self binaryAt: localFileName , '.bak' ! !

!FmBackupDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 19:07'!
binaryAt: localFileName put: contents 
	(self includesKey: localFileName)
		ifTrue: [super binaryAt: localFileName , '.bak' put: (self binaryAt: localFileName)].
	super binaryAt: localFileName put: contents! !

!FmBackupDirectoryEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 19:04'!
directoryEntryClass
	^self! !

!FmBackupDirectoryEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 23:16'!
fileEntryClass
	^self! !

!FmGZipDirectoryEntry methodsFor: 'private' stamp: 'mu 5/4/2007 17:04'!
asSuffixedName: localFileName
	^(localFileName endsWith: '.gz') ifTrue: [localFileName] ifFalse: [localFileName, '.gz'].! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:05'!
at: localFileName
	| fName |
	fName := self asSuffixedName: localFileName.
	^(super at: fName) asString convertFromWithConverter: self textConverterClass new! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:05'!
at: localFileName put: contents 
	| fName |
	fName := self asSuffixedName: localFileName.
	super at: fName put: contents ! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:06'!
binaryAt: localFileName
	| fName |
	fName := self asSuffixedName: localFileName.
	^super binaryAt: fName! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:06'!
binaryAt: localFileName put: contents 
	| fName |
	fName := self asSuffixedName: localFileName.
	super binaryAt: fName put: contents ! !

!FmGZipDirectoryEntry methodsFor: 'private' stamp: 'mu 3/29/2009 23:55'!
contentsOf: aStream by: ignore
	| str |
	str := GZipReadStream on: aStream.
	^super contentsOf: str by: nil! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:31'!
includesKey: fileName
	^super includesKey: (self asSuffixedName: fileName)! !

!FmGZipDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 5/4/2007 17:31'!
removeKey: localFileName ifAbsent: failBlock
	super removeKey: (self asSuffixedName: localFileName) ifAbsent: failBlock! !

!FmGZipDirectoryEntry methodsFor: 'private' stamp: 'mu 3/29/2009 23:55'!
setContentsOf: aStream to: aStringOrBytes by: ignore
	| conts |
	aStringOrBytes isString
		ifTrue: [| stream | 
			stream := RWBinaryOrTextStream on: (String new: aStringOrBytes size).
			aStringOrBytes do: [:each | self textConverterClass new nextPut: each toStream: stream].
			conts := stream contents]
		ifFalse: [aStream binary.
			conts := aStringOrBytes].
	super
		setContentsOf: (GZipWriteStream on: aStream)
		to: conts
		by: nil! !

!FmGZipDirectoryEntry methodsFor: 'defaults' stamp: 'mu 5/4/2007 16:49'!
textConverterClass
	^UTF8TextConverter! !

!FmGZipDirectoryEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 18:58'!
directoryEntryClass
	^self! !

!FmGZipDirectoryEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 23:22'!
fileEntryClass
	^self! !
