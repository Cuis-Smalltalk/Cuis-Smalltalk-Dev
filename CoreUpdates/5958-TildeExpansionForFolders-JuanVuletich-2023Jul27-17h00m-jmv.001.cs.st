'From Cuis 6.0 [latest update: #5957] on 27 July 2023 at 5:14:59 pm'!

!FileIOAccessor methodsFor: 'error descriptions' stamp: 'jmv 7/27/2023 17:14:25'!
tildeFolderRequiresOSProcessDescription

	^'Use of tilde (~) to mean $HOME folder requires OSProcess'! !

!FileIOAccessor methodsFor: 'error descriptions' stamp: 'jmv 7/27/2023 17:05:35'!
unresolvedHomeEnvironmentVariableDescription

	^'The $HOME variable could not be resolved'! !


!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 7/27/2023 17:06:06'!
absolutePathComponentsFor: aString
	"Not complete, but OK in most cases.
	See comment at #isAbsolutePathName"

	| tokens curDirPathComponents |

	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	(aString size > 0 and: [aString first = $~]) ifTrue: [
		Smalltalk at: #OSProcess
			ifPresent: [ :osProcess |
				(osProcess thisOSProcess environmentAt: 'HOME')
					ifNotNil: [ :homeVariable |
						^self absolutePathComponentsFor: homeVariable, (aString withoutPrefix: '~')]
					ifNil: [ self error: self unresolvedHomeEnvironmentVariableDescription ]]
			ifAbsent: [
				self error: self tildeFolderRequiresOSProcessDescription ]].

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


!FileIOAccessor reorganize!
('actions' absolutePathComponentsFor: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn: try:forFileNamed: updateEntry:)
('utilities' baseNameAndExtensionFor:do: baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: extensionFor: fileNamesIn: fileSize: splitNameVersionExtensionFor:)
('accessing' drives pathNameDelimiter slash)
('file stream creation' privateForceNewFile: privateNewFile: privateReadOnlyFile: privateWriteableFile:)
('testing' isCaseSensitive isDriveSupported onMacOsX onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primFixedNameLookupEntryIn:index: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
('error descriptions' tildeFolderRequiresOSProcessDescription unresolvedHomeEnvironmentVariableDescription)
!

