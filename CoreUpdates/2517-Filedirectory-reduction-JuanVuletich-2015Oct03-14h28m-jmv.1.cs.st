'From Cuis 4.2 of 25 July 2013 [latest update: #2513] on 3 October 2015 at 2:40:46.780505 pm'!
!methodRemoval: StandardFileStream class #isAFileNamed:!
StandardFileStream class removeSelector: #isAFileNamed:!
!methodRemoval: StandardFileStream class #requestDropStream:!
StandardFileStream class removeSelector: #requestDropStream:!

!StandardFileStream class reorganize!
('file creation' crc16OfFileNamed: fileNamed: forceNewFileNamed: isAFileFullNamed: newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileFullNamed: readOnlyFileNamed:)
('registry' register: registry retryWithGC:until:forFileNamed: unregister:)
('error handling' fileDoesNotExistUserHandling: fileExistsUserHandling: readOnlyFileDoesNotExistUserHandling:)
('cached state access' releaseClassCachedState)
!

!methodRemoval: StandardFileStream #requestDropStream:!
StandardFileStream removeSelector: #requestDropStream:!
!methodRemoval: FileStream class #isAFileNamed:!
FileStream class removeSelector: #isAFileNamed:!
!methodRemoval: FileStream class #requestDropStream:!
FileStream class removeSelector: #requestDropStream:!
!methodRemoval: DosFileDirectory #relativeNameFor:!
DosFileDirectory removeSelector: #relativeNameFor:!
!methodRemoval: FileDirectory class #fileName:extension:!
FileDirectory class removeSelector: #fileName:extension:!
!methodRemoval: FileDirectory class #lookInUsualPlaces:!
FileDirectory class removeSelector: #lookInUsualPlaces:!
!methodRemoval: FileDirectory class #vmDirectory!
FileDirectory class removeSelector: #vmDirectory!

!FileDirectory class reorganize!
('instance creation' currentDirectory forFullFileName: on: root smalltalkImageDirectory)
('name utilities' baseNameFor: changeSuffix checkName:fixErrors: dirPathFor: extensionFor: imageSuffix localNameFor: sourceSuffix splitName:to: startUp)
('system start up' shutDown)
('platform specific' dot extensionDelimiter isCaseSensitive makeAbsolute: makeRelative: maxFileNameLength pathNameDelimiter slash)
('private' activeDirectoryClass isActiveDirectoryClass primPathNameDelimiter)
('cacher state access' releaseClassCachedState)
!

!methodRemoval: FileDirectory #copyFileNamed:toFileNamed:!
FileDirectory removeSelector: #copyFileNamed:toFileNamed:!
!methodRemoval: FileDirectory #directoryEntryFor:!
FileDirectory removeSelector: #directoryEntryFor:!
!methodRemoval: FileDirectory #fileNamed:do:!
FileDirectory removeSelector: #fileNamed:do:!
!methodRemoval: FileDirectory #filesContaining:caseSensitive:!
FileDirectory removeSelector: #filesContaining:caseSensitive:!
!methodRemoval: FileDirectory #forceNewFileNamed:do:!
FileDirectory removeSelector: #forceNewFileNamed:do:!
!methodRemoval: FileDirectory #fullNamesOfAllFilesInSubtree!
FileDirectory removeSelector: #fullNamesOfAllFilesInSubtree!
!methodRemoval: FileDirectory #isAFileNamed:!
FileDirectory removeSelector: #isAFileNamed:!
!methodRemoval: FileDirectory #isLegalFileName:!
FileDirectory removeSelector: #isLegalFileName:!
!methodRemoval: FileDirectory #lastNameFor:extension:!
FileDirectory removeSelector: #lastNameFor:extension:!
!methodRemoval: FileDirectory #newFileNamed:do:!
FileDirectory removeSelector: #newFileNamed:do:!
!methodRemoval: FileDirectory #oldFileNamed:do:!
FileDirectory removeSelector: #oldFileNamed:do:!
!methodRemoval: FileDirectory #oldFileNamed:ifExistsDo:!
FileDirectory removeSelector: #oldFileNamed:ifExistsDo:!
!methodRemoval: FileDirectory #putFile:named:!
FileDirectory removeSelector: #putFile:named:!
!methodRemoval: FileDirectory #putFile:named:retry:!
FileDirectory removeSelector: #putFile:named:retry:!
!methodRemoval: FileDirectory #readOnlyFileNamed:do:!
FileDirectory removeSelector: #readOnlyFileNamed:do:!
!methodRemoval: FileDirectory #relativeNameFor:!
FileDirectory removeSelector: #relativeNameFor:!
!methodRemoval: FileDirectory #withAllFilesDo:andDirectoriesDo:!
FileDirectory removeSelector: #withAllFilesDo:andDirectoriesDo:!

!FileDirectory reorganize!
('path access' fullPathFor: on: pathName pathNameDelimiter pathParts slash)
('file stream creation' fileNamed: forceNewFileNamed: newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileNamed:)
('enumeration' containingDirectory directoryEntry directoryNamed: directoryNames entries fileAndDirectoryNames fileNames fullName keysDo: localName)
('testing' directoryExists: exists fileExists: includesKey: is: isCaseSensitive)
('file operations' copyFile:toFile: copyFileWithoutOverwriteConfirmationNamed:toFileNamed: createDirectory: deleteDirectory: deleteFileNamed: deleteFileNamed:ifAbsent: deleteLocalFiles fileOrDirectoryExists: recursiveDelete rename:toBe:)
('file name utilities' checkName:fixErrors: fileNamesMatching: fullNameFor: nextNameFor:coda:extension: nextNameFor:extension: splitNameVersionExtensionFor:)
('printing' printOn:)
('private' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primLookupEntryIn:index: primRename:to: setPathName:)
('file status' entryAt: entryAt:ifAbsent:)
('file directory' assureExistence localNameFor:)
('comparing' = hash)
('*FileMan-Adaptor-FileDirectory' asDirectoryEntry)
!

!methodRemoval: DirectoryEntryFile #readStreamDo:!
DirectoryEntryFile removeSelector: #readStreamDo:!
