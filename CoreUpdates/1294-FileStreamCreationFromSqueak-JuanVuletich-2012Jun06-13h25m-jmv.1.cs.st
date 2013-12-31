'From Cuis 4.0 of 21 April 2012 [latest update: #1293] on 6 June 2012 at 1:26:55 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 6/6/2012 11:52'!
asciiCode
	"Answer the value of the receiver that represents its ascii encoding."

	^value! !


!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:18'!
fileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist.
	Evaluate aBlock, and close the file"

	(self fileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:05'!
forceNewFileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking.
	Evaluate aBlock, and close the file"

	(self forceNewFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:06'!
newFileNamed: localFileName do: aBlock
	"Create a new file with the given name in this directory.
	If the file already exists, give the chance to pick another name or overwrite it
	Evaluate aBlock, and close the file"

	(self newFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:07'!
oldFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self oldFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:14'!
oldFileNamed: localFileName ifExistsDo: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, do nothing.
	If the file exists, evaluate aBlock, and close the file"

	(self oldFileOrNoneNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:11'!
readOnlyFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self readOnlyFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !


!SingleSetChangeSorter methodsFor: 'accessing' stamp: 'jmv 6/6/2012 11:45'!
changeSetDirtyFlags

	^{ (myChangeSet isForBaseSystem and: [ myChangeSet hasUnsavedChanges ])
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ] }! !


!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:18'!
fileNamed: localFileName
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist."

	^ FileStream concreteStream fileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:05'!
forceNewFileNamed: localFileName
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking."

	^ FileStream concreteStream forceNewFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:06'!
newFileNamed: localFileName
	"Create a new file with the given name in this directory.
	If the file already exists, give the chance to pick another name or overwrite it."

	^ FileStream concreteStream newFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:07'!
oldFileNamed: localFileName
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort."

	^ FileStream concreteStream oldFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:10'!
readOnlyFileNamed: localFileName
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort."

	^ FileStream concreteStream readOnlyFileNamed: (self fullNameFor: localFileName)
! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 6/6/2012 11:58'!
browseAllUnimplementedCalls
	"Create and schedule a message browser on each method that includes a 
	message that is not implemented in any object in the system.
	Smalltalk browseAllUnimplementedCalls
	"

	^self browseMessageList: self allUnimplementedCalls name: 'Unimplemented calls'! !

