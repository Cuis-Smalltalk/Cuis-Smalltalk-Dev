'From Cuis6.3 [latest update: #6222] on 21 February 2024 at 4:13:36 pm'!

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 2/21/2024 15:36:26'!
absolutePathComponentsFor: aString relativeTo: baseDirectory
	"aString is a relative path, and will be applied to baseDirectory
	See comment at #isAbsolutePathName"

	| tokens curDirPathComponents |

	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	(aString size > 0 and: [aString first = $~]) ifTrue: [
		Smalltalk at: #OSProcess
			ifPresent: [ :osProcess |
				(osProcess thisOSProcess environmentAt: 'HOME')
					ifNotNil: [ :homeVariable |
						^self absolutePathComponentsFor: homeVariable, (aString withoutPrefix: '~') relativeTo: DirectoryEntry currentDirectory]
					ifNil: [ self error: 'The $HOME variable could not be resolved' ]]
			ifAbsent: [
				self error: 'Use of tilde (~) to mean $HOME folder requires OSProcess' ]].

	tokens := aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens asArray ].

	curDirPathComponents := DirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents := curDirPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		strm nextPutAll: curDirPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]].! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 2/21/2024 15:37:28'!
expandTildeFor: aString

	aString size = 0 ifTrue: [ ^aString ].
	aString first = $~ifFalse: [ ^aString ].
	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	Smalltalk at: #OSProcess
		ifPresent: [ :osProcess |
			(osProcess thisOSProcess environmentAt: 'HOME')
				ifNotNil: [ :homeVariable |
					^homeVariable, (aString copyFrom: 2 to: aString size) ]
				ifNil: [ self error: 'The $HOME variable could not be resolved' ]]
		ifAbsent: [
			self error: 'Use of tilde (~) to mean $HOME folder requires OSProcess' ]! !


!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 2/21/2024 15:09:00'!
isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].
	
	"tilde means user's home folder, if possible.
	https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	self first = $~ ifTrue: [^ true ].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	^ false! !


!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/21/2024 15:19:01'!
pathString: aString
	"Argument can be an absolute or relative path"

	| tokens |
	tokens := aString isAbsolutePathName
		ifTrue: [ (FileIOAccessor default expandTildeFor: aString) asPathTokens ]
		ifFalse: [ FileIOAccessor default absolutePathComponentsFor: aString relativeTo: DirectoryEntry currentDirectory ].
	(tokens notEmpty and: [self fileAccessor isDriveSupported])
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil! !

!methodRemoval: FileIOAccessor #unresolvedHomeEnvironmentVariableDescription stamp: 'jmv 2/21/2024 16:11:43'!
FileIOAccessor removeSelector: #unresolvedHomeEnvironmentVariableDescription!
!methodRemoval: FileIOAccessor #tildeFolderRequiresOSProcessDescription stamp: 'jmv 2/21/2024 16:11:43'!
FileIOAccessor removeSelector: #tildeFolderRequiresOSProcessDescription!
!methodRemoval: FileIOAccessor #absolutePathComponentsFor: stamp: 'jmv 2/21/2024 16:11:43'!
FileIOAccessor removeSelector: #absolutePathComponentsFor:!
!methodRemoval: CharacterSequence #asAbsolutePathName stamp: 'jmv 2/21/2024 16:12:36'!
CharacterSequence removeSelector: #asAbsolutePathName!

!FileIOAccessor reorganize!
('actions' copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn: try:forFileNamed: updateEntry:)
('utilities' absolutePathComponentsFor:relativeTo: baseNameAndExtensionFor:do: baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: expandTildeFor: extensionFor: fileNamesIn: fileSize: splitNameVersionExtensionFor:)
('accessing' drives pathNameDelimiter slash)
('file stream creation' privateForceNewFile: privateNewFile: privateReadOnlyFile: privateWriteableFile:)
('testing' isCaseSensitive isDriveSupported onMacOsX onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primFixedNameLookupEntryIn:index: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
!

