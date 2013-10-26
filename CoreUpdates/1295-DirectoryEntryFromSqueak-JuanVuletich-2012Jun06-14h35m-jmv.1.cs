'From Cuis 4.0 of 21 April 2012 [latest update: #1294] on 6 June 2012 at 2:40:09 pm'!
!classDefinition: #DirectoryEntry category: #'System-Files'!
Object subclass: #DirectoryEntry
	instanceVariableNames: 'name creationTime modificationTime dirFlag fileSize directory '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
!classDefinition: #DirectoryEntryDirectory category: #'System-Files'!
DirectoryEntry subclass: #DirectoryEntryDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!DirectoryEntryDirectory commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a directory.!

!classDefinition: #DirectoryEntryFile category: #'System-Files'!
DirectoryEntry subclass: #DirectoryEntryFile
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!DirectoryEntryFile commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a file.!


!DirectoryEntry methodsFor: 'access' stamp: 'cmm 10/30/2009 15:50'!
baseName
	^ FileDirectory baseNameFor: self name! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:36'!
containingDirectory
	"Answer the FileDirectory in which I reside."
	^ directory! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
creationDateAndTime
	"The DateAndTime my entry in the file system was created."
	^DateAndTime fromSeconds: creationTime! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 9/21/2009 18:24'!
extension
	^ FileDirectory extensionFor: self name! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 2/15/2010 15:52'!
fileSizeString
	"Answer my file size as an easy-to-read String."
	^ self fileSize asBytesDescription! !

!DirectoryEntry methodsFor: 'access' stamp: 'bgf 9/9/2010 07:36'!
fullName
	"The fully-qualified name.
	 Since this method falls into the equality test, make it safe when directory is nil."
	^ directory 
		ifNotNil: [ directory fullNameFor: self name ] 
		ifNil: [ self name ]! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
modificationDateAndTime
	"The DateAndTime my entry in the file system was last modified."
	^ DateAndTime fromSeconds: modificationTime! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 8/29/2007 17:44'!
printOn: aStream 
	super printOn: aStream.
	aStream
		space ;
		nextPutAll: self name! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 2/18/2011 15:54'!
splitNameVersionExtension
	" answer an array with the root name, version # and extension."
	^ self directory splitNameVersionExtensionFor: self name! !

!DirectoryEntry methodsFor: 'private-initialization' stamp: 'cmm 9/12/2007 17:20'!
setDirectory: aFileDirectoryOrServerDirectory name: name0  creationTime: creationTime0  modificationTime: modificationTime0 fileSize: fileSize0
	directory := aFileDirectoryOrServerDirectory.
	name := name0.
	creationTime := creationTime0.
	modificationTime := modificationTime0.
	fileSize := fileSize0! !

!DirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/24/2011 19:17'!
= aDirectoryEntry 
	"Answer whether I am equivalent in all of my file-system attributes."
	super = aDirectoryEntry ifTrue: [^ true].
	self species = aDirectoryEntry species ifFalse: [^ false].
	^ self containingDirectory = aDirectoryEntry containingDirectory
		and: [self name = aDirectoryEntry name
				and: [self modificationTime = aDirectoryEntry modificationTime
						and: [self fileSize = aDirectoryEntry fileSize]]]! !

!DirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/21/2011 21:50'!
exists
	^ (self containingDirectory
		entryAt: self name
		ifAbsent: [ nil ]) notNil! !

!DirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/18/2011 16:04'!
hash
	"Hashing on directory + name should be sufficient."
	^ (self containingDirectory hash hashMultiply + self name hash) hashMultiply! !


!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 6/6/2012 13:47'!
directory: aFileDirectoryOrServerDirectory name: name0 creationTime: creationTime modificationTime: modificationTime fileSize: fileSize 
	^ self new 
		setDirectory: aFileDirectoryOrServerDirectory
		name: name0
		creationTime: creationTime
		modificationTime: modificationTime
		fileSize: fileSize! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 6/6/2012 13:47'!
fromArray: array directory: aFileDirectoryOrServerDirectory 
	| entryType |
	entryType := (array at: 4) 
		ifTrue: [ DirectoryEntryDirectory ]
		ifFalse: [ DirectoryEntryFile ].
	^ entryType 
		directory: aFileDirectoryOrServerDirectory
		name: (array at: 1)
		creationTime: (array at: 2)
		modificationTime: (array at: 3)
		fileSize: (array at: 5)! !


!DirectoryEntryDirectory methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does."
	^ true! !

!DirectoryEntryDirectory methodsFor: 'convert' stamp: 'jmv 6/6/2012 14:23'!
asFileDirectory
	"Answer a FileDirectory representing the same directory I represent."
	^directory on: (directory fullNameFor: self name)! !


!DirectoryEntryFile methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does not."
	^ false! !

!DirectoryEntryFile methodsFor: 'stream access' stamp: 'jmv 6/6/2012 14:21'!
readStreamDo: aBlock
	"Obtain a FileStream on my contents that can be read, but not written,
	and send it to aBlock."
	^ directory readOnlyFileNamed: self name do: aBlock! !


!DirectoryEntry methodsFor: 'access' stamp: 'cmm 2/15/2010 13:16'!
creationTime
	"The time the entry was created, as an Integer number of seconds offset from the DateAndTime epoch."
	^creationTime! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:25'!
isDirectory
	"whether this entry represents a directory"
	self subclassResponsibility! !

!DirectoryEntry methodsFor: 'access' stamp: 'cmm 1/25/2011 13:16'!
size
	"For API compatibility with byte objects (for streaming api)."
	^ self fileSize! !


!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/6/2012 14:00'!
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"FileDirectory default directoryNames"
	^ (self entries select: [ :entry |
		entry isDirectory ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/6/2012 13:42'!
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"FileDirectory default fileNames"
	^ (self entries select: [ :entry |
		entry isDirectory not ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'private' stamp: 'jmv 6/6/2012 13:53'!
directoryContentsFor: fullPath
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"FileDirectory default directoryContentsFor: ''"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[done] whileFalse: [
		entryArray _ self primLookupEntryIn: fullPath index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^(InvalidDirectoryError pathName: pathName) signal].
		entryArray
			ifNil: [done _ true]
			ifNotNil: [entries addLast: (DirectoryEntry fromArray: entryArray directory: self)].
		index _ index + 1].

	^ entries asArray! !


!AcornFileDirectory methodsFor: 'private' stamp: 'jmv 6/6/2012 13:49'!
directoryContentsFor: fullPath 
	"Return a collection of directory entries for the files and directories in 
	the directory with the given path. See primLookupEntryIn:index: for 
	further details."
	"FileDirectory default directoryContentsFor: ''"

	| entries extraPath |
	entries := super directoryContentsFor: fullPath.
	fullPath isEmpty
		ifTrue: [
			"For Acorn we also make sure that at least the parent of the current dir 
			is added - sometimes this is in a filing system that has not been (or 
			cannot be) polled for disc root names"
			extraPath := self class default containingDirectory.
			"Only add the extra path if we haven't already got the root of the current dir in the list"
			(entries anySatisfy: [:ent | extraPath fullName beginsWith: ent name]) 
				ifFalse: [entries := entries
								copyWith: (DirectoryEntryDirectory
										directory: self
										name: extraPath fullName
										creationTime: 0
										modificationTime: 0
										fileSize: 0)]].
	^ entries! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 6/6/2012 13:59'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ ((Date fromSeconds: entry modificationTime) printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			(Time fromSeconds: entry modificationTime \\ 86400)
				print24: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize asStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas + sizePad - sizeDigits.
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 6/6/2012 14:01'!
listForPatterns: anArray
	"Make the list be those file names which match the patterns."

	| sizePad newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	(fileSelectionBlock isKindOf: MessageSend) ifTrue: [
		fileSelectionBlock arguments: {directory entries}.
		newList _ fileSelectionBlock value.
		fileSelectionBlock arguments: #().
	] ifFalse: [
		newList _ Set new.
		anArray do: [ :pat |
			newList addAll: (directory entries select: [ :entry |
				entry isDirectory
					ifTrue: [showDirsInFileList]
					ifFalse: [fileSelectionBlock value: entry value: pat]]) ].
	].
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) asStringWithCommas size.
	newList _ newList collect: [ :e | self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !

!methodRemoval: DirectoryEntry class #fromArray:!
DirectoryEntry class removeSelector: #fromArray:!
!methodRemoval: DirectoryEntry class #name:creationTime:modificationTime:isDirectory:fileSize:!
DirectoryEntry class removeSelector: #name:creationTime:modificationTime:isDirectory:fileSize:!
!methodRemoval: DirectoryEntry #at:!
DirectoryEntry removeSelector: #at:!
!methodRemoval: DirectoryEntry #privateName:creationTime:modificationTime:isDirectory:fileSize:!
DirectoryEntry removeSelector: #privateName:creationTime:modificationTime:isDirectory:fileSize:!
!classDefinition: #DirectoryEntry category: #'System-Files'!
Object subclass: #DirectoryEntry
	instanceVariableNames: 'directory name creationTime modificationTime fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!DirectoryEntry reorganize!
('access' baseName containingDirectory creationDateAndTime creationTime extension fileSize fileSizeString fullName isDirectory modificationDateAndTime modificationTime name printOn: size splitNameVersionExtension)
('private-initialization' setDirectory:name:creationTime:modificationTime:fileSize:)
('testing' = exists hash)
!

