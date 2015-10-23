'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 4:38:13.272429 pm'!

!FileList methodsFor: 'file list' stamp: 'jmv 10/16/2015 16:22'!
selectedFileEntry
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^ directory ifNotNil: [ :dir | (dir fullNameFor: fileName) asFileEntry ]! !


!CodePackage methodsFor: 'naming' stamp: 'jmv 10/16/2015 16:32'!
packageFileName
	^ packageName, '.pck.st'! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 10/16/2015 16:08'!
path: aString 
	
	| contentPacks directory |

	self flag: #todo. "Consider renaming this method. --cbr"

	directory _ (FmDirectoryEntry smalltalkImageDirectory / aString) asDirectoryEntry.

	(self supportedFilesIn: directory) do: [ :i |
		| filename |
		filename _ directory pathName , '/', (i at: 1).
			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: i name 
			put: (self import: [ Form fromFileNamed: filename ]) "This may yet be a cross-cutting concern, and need to be refactored when other media types become present. --cbr"
	].

	contentPacks _ directory directoryNames collect: [ :i |	
		i ->  (ContentPack new path: (directory / i) pathName)
	].
	
	^ self union: (contentPacks as: Dictionary)! !

!ContentPack methodsFor: 'private' stamp: 'jmv 10/16/2015 16:04'!
supportedFilesIn: anFmdirectoryEntry

	| fileTypes supportedFiles |
		fileTypes _ (self class mapping as: Dictionary) values.
		supportedFiles _ Set new.
	
	fileTypes do: [ :type | 
		supportedFiles _ supportedFiles
			union: (anFmdirectoryEntry files select: [ :entry |
				'*.' , type match: entry name ])
	].

	^ supportedFiles! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 16:33'!
alternativeSourcesName
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (image folder), seek for them here (vm folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , SourceFileVersionString , '.sources'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 16:33'!
defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, FileDirectory slash, SourceFileVersionString, '.sources'! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 10/16/2015 16:33'!
runtimeDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."

	('error', Utilities dateTimeSuffix, '.log') asFileName asFileEntry writeStream: [ :file |
		Smalltalk timeStamp: file.
		(thisContext sender stackOfSize: 20) do: [ :ctx | file newLine. ctx printOn: file ]].

	Smalltalk snapshot: false andQuit: true clearAllClassState: false! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 10/16/2015 16:32'!
packageFileName
	"Answer a package name based on feature name"

	^ name asString, '.pck.st'! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/16/2015 16:13'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry _ directory / packageFileName.
		entry exists ifTrue: [
			pathName _ entry pathName.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/16/2015 16:18'!
inPackagesSubtreeOf: anFmDirectoryEntry do: aBlock

	| pckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: anFmDirectoryEntry.

	"Look in the usual Packages subfolders"
	pckDir _ (anFmDirectoryEntry / 'Packages') asDirectoryEntry.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ (anFmDirectoryEntry / 'CompatibilityPackages') asDirectoryEntry.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	anFmDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			aBlock value: entry.
			pckDir _ (entry / 'Packages') asDirectoryEntry.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ (entry / 'CompatibilityPackages') asDirectoryEntry.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/16/2015 16:12'!
placesToLookForPackagesDo: aBlock

	| base myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asDirectoryEntry.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ FmDirectoryEntry smalltalkImageDirectory.
	self inPackagesSubtreeOf: base do: aBlock.
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	self inPackagesSubtreeOf: base parent do: aBlock.

	"Also look in host OS current directory"
	base _ FmDirectoryEntry currentDirectory.
	self inPackagesSubtreeOf: base do: aBlock! !


!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 10/16/2015 16:32'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version |

	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [:file | self splitNameVersionExtensionFor: file].
	splits _ splits asArray sort: [:a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [version _ 1]
			ifFalse: [version _ (splits last at: 2) + 1].
	^ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName! !


!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 10/16/2015 16:35'!
extensionFor: pathName
	"Return the extension of given file name, if any.
	FileDirectory extensionFor: 'writings.txt'
	FileDirectory extensionFor: 'optionalstuff.pck.st'
	FileDirectory extensionFor: 'optionalstuff.pck'
	FileDirectory extensionFor: 'code.cs.st'
	FileDirectory extensionFor: 'code.cs'
	"

	| i |
	self splitName: pathName to: [ :path :filename |
		{ 'cs.st' . 'pck.st' } do: [ :specialExtension |
			(filename endsWith: '.', specialExtension)
				ifTrue: [ ^specialExtension ]].
		i _ filename findLast: [ :c | c = $. ].
		^ i = 0
			ifTrue: [ '']
			ifFalse: [ filename copyFrom: i + 1 to: filename size ]]! !

!methodRemoval: FileStream #directoryEntry!
FileStream removeSelector: #directoryEntry!
!methodMoveToSomePackage: FmFileIOAccessor #extensionDelimiter!
FmFileIOAccessor removeSelectorIfInBaseSystem: #extensionDelimiter!

!FmFileIOAccessor reorganize!
('actions' asAbsolutePathName: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: directoryExists: directoryExists:in: fileExists: fileExists:in: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn:from: lookupEntryIn:index: try:forFileNamed:)
('utilities' baseNameAndExtensionFor:do: baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: extensionFor: fileNamesIn: splitNameVersionExtensionFor:)
('accessing' concreteStreamClass currentDriveName drives pathNameDelimiter slash)
('file stream creation' fileNamed: forceNewFileNamed: newFileNamed: readOnlyFileNamed:)
('testing' isCaseSensitive isDriveSupported onMac onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
('*FileMan-StrictCompatibility' extensionDelimiter)
!

!methodMoveToSomePackage: FmDirectoryEntry #directory!
FmDirectoryEntry removeSelectorIfInBaseSystem: #directory!
!methodMoveToSomePackage: FmFileEntry #directory!
FmFileEntry removeSelectorIfInBaseSystem: #directory!
!methodRemoval: FileDirectory class #dot!
FileDirectory class removeSelector: #dot!
!methodRemoval: FileDirectory class #extensionDelimiter!
FileDirectory class removeSelector: #extensionDelimiter!
!methodRemoval: FileDirectory class #sourceSuffix!
FileDirectory class removeSelector: #sourceSuffix!
!methodRemoval: FileDirectory #directoryEntry!
FileDirectory removeSelector: #directoryEntry!
