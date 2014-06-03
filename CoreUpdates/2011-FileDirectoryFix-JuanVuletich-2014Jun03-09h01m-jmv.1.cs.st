'From Cuis 4.2 of 25 July 2013 [latest update: #2010] on 3 June 2014 at 9:28:44.961316 am'!

!AcornFileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/3/2014 09:27'!
entries
	"Return a collection of directory entries for the files and directories in 
	the directory with the given path. See primLookupEntryIn:index: for 
	further details."
	"
	FileDirectory default entries
	"

	| entries extraPath |
	entries _ super entries.
	pathName isEmpty
		ifTrue: [
			"For Acorn we also make sure that at least the parent of the current dir 
			is added - sometimes this is in a filing system that has not been (or 
			cannot be) polled for disc root names"
			extraPath _  self class default containingDirectory.
			"Only add the extra path if we haven't already got the root of the current dir in the list"
			(entries anySatisfy: [:ent | extraPath fullName beginsWith: ent name]) 
				ifFalse: [
					entries _ entries
								copyWith: (DirectoryEntryDirectory
										directory: self
										name: extraPath fullName
										creationTime: 0
										modificationTime: 0
										fileSize: 0) ]].
	^ entries! !


!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/3/2014 09:26'!
entries
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"
	FileDirectory default entries
	"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[ done ] whileFalse: [
		entryArray _ self primLookupEntryIn: pathName index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ (InvalidDirectoryError pathName: pathName) signal ].
		entryArray
			ifNil: [ done _ true ]
			ifNotNil: [ entries addLast: (DirectoryEntry fromArray: entryArray directory: self) ].
		index _ index + 1 ].

	^ entries asArray! !

!methodRemoval: AcornFileDirectory #directoryContents!
AcornFileDirectory removeSelector: #directoryContents!
!methodRemoval: AcornFileDirectory #directoryContentsFor:!
AcornFileDirectory removeSelector: #directoryContentsFor:!

!AcornFileDirectory reorganize!
('enumeration' entries)
('file name utilities' checkName:fixErrors: fullPathFor:)
('testing' directoryExists:)
('path access' pathParts)
!

!methodRemoval: FileDirectory #directoryContents!
FileDirectory removeSelector: #directoryContents!
!methodRemoval: FileDirectory #directoryContentsFor:!
FileDirectory removeSelector: #directoryContentsFor:!
!methodRemoval: FileDirectory #statsForDirectoryTree:!
FileDirectory removeSelector: #statsForDirectoryTree:!

!FileDirectory reorganize!
('path access' fullPathFor: on: pathName pathNameDelimiter pathParts slash)
('file stream creation' fileNamed: fileNamed:do: forceNewFileNamed: forceNewFileNamed:do: newFileNamed: newFileNamed:do: oldFileNamed: oldFileNamed:do: oldFileNamed:ifExistsDo: oldFileOrNoneNamed: readOnlyFileNamed: readOnlyFileNamed:do:)
('enumeration' containingDirectory directoryEntry directoryEntryFor: directoryNamed: directoryNames entries fileAndDirectoryNames fileNames fullName fullNamesOfAllFilesInSubtree keysDo: localName withAllSubdirectoriesCollect:)
('testing' directoryExists: exists fileExists: includesKey: is: isAFileNamed: isCaseSensitive)
('file operations' copyFile:toFile: copyFileNamed:toFileNamed: copyFileWithoutOverwriteConfirmationNamed:toFileNamed: createDirectory: deleteDirectory: deleteFileNamed: deleteFileNamed:ifAbsent: deleteLocalFiles fileOrDirectoryExists: getMacFileTypeAndCreator: mimeTypesFor: putFile:named: putFile:named:retry: recursiveDelete rename:toBe: setMacFileNamed:type:creator:)
('file name utilities' checkName:fixErrors: fileNamesMatching: fullNameFor: isLegalFileName: lastNameFor:extension: nextNameFor:coda:extension: nextNameFor:extension: relativeNameFor: splitNameVersionExtensionFor:)
('printing' printOn:)
('private' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primGetMacFileNamed:type:creator: primLookupEntryIn:index: primRename:to: primSetMacFileNamed:type:creator: setPathName:)
('file status' entryAt: entryAt:ifAbsent:)
('file directory' assureExistence localNameFor:)
('comparing' = hash)
('searching' filesContaining:caseSensitive: withAllFilesDo:andDirectoriesDo:)
!

