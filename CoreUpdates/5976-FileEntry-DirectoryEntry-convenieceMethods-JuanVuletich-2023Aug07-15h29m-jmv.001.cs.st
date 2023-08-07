'From Cuis 6.0 [latest update: #5974] on 7 August 2023 at 3:34:39 pm'!

!DirectoryEntry methodsFor: 'convenience' stamp: 'jmv 8/7/2023 15:34:01'!
asDirectoryEntry
	"For convenience."
	^self! !


!FileEntry methodsFor: 'convenience' stamp: 'jmv 8/7/2023 15:34:16'!
asFileEntry
	"For convenience."
	^self! !


!DirectoryEntry reorganize!
('actions-path' / // concatPathComponentsAsDirectory: concatPathComponentsAsFile:)
('enumeration' allChildrenDo: allDirectoriesDo: allFilesDo: allFilesDo:matches: allRegularDirectoriesDo: childrenDo: directoriesDo: directoriesMatches: directoryMatching: directoryNamesMatching: fileMatching: fileNamesMatching: filesDo: filesMatches: latestFileMatches: oldestFileMatches: regularDirectoriesDo:)
('actions-directory' assureExistence copyTo: delete recursiveDelete rename:)
('private' assureExistenceDirNamed: basicRecursiveDelete initChildren updateFrom:entryInParent:)
('dictionary-like' at: at:ifAbsent: at:put: binaryAt: binaryAt:ifAbsent: binaryAt:put: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing' children directories directoryNames fileNames files)
('testing' isDirectory isFile isRegularDirectory updateExists)
('initialize-release' invalidate invalidateChildren)
('services' nextNameFor:coda:extension: nextNameFor:extension:)
('convenience' asDirectoryEntry)
!


!FileEntry reorganize!
('actions-rio' < <<)
('enumeration' assureExistence)
('actions-file' appendContents: binaryContents binaryContents: copyTo: delete fileContents fileContents: formContents rename: textContents textContents:)
('accessing-stream' appendStreamDo: forceWriteStreamDo: readStreamDo: writeStreamDo:)
('accessing' appendStream fileSize readStream writeStream)
('private' forceWriteStream primEntryInParent updateFrom:entryInParent:)
('testing' isFile updateExists)
('initialize-release' invalidate)
('actions-pipe' pipe:to: pipeRepeat:to: pipeRepeat:while:to:)
('convenience' asFileEntry)
!

