'From Cuis 5.0 of 7 November 2016 [latest update: #3038] on 18 January 2017 at 7:35:13 pm'!

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 1/18/2017 11:20:57'!
fileMatching: pattern
"
	DirectoryEntry smalltalkImageDirectory fileMatching: '*.image'.
	DirectoryEntry smalltalkImageDirectory fileMatching: 'x*.image'.
"
	self filesDo: [ :file |
		(pattern match: file name)
			ifTrue: [ ^ file ]].
	^ nil! !


!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 1/18/2017 11:13:59'!
directoriesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifFalse: [
			aBlock value: each ]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 1/18/2017 11:19:42'!
directoryMatching: pattern
"
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'.
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'xC*Pack*'.
"
	self directoriesDo: [ :directory |
		(pattern match: directory name)
			ifTrue: [ ^ directory ]].
	^ nil! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 1/18/2017 11:14:17'!
filesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifTrue: [
			aBlock value: each ]]! !


!DirectoryEntry reorganize!
('actions-path' / // concatPathComponentsAsDirectory: concatPathComponentsAsFile:)
('enumeration' allChildrenDo: allDirectoriesDo: allFilesDo: allFilesDo:matches: childrenDo: directoriesDo: directoriesMatches: directoryMatching: directoryNamesMatching: fileMatching: fileNamesMatching: filesDo: filesMatches: latestFileMatches: oldestFileMatches:)
('actions-directory' assureExistence copyTo: delete recursiveDelete rename:)
('private' assureExistenceDirNamed: basicRecursiveDelete initChildren)
('dictionary-like' at: at:ifAbsent: at:put: binaryAt: binaryAt:ifAbsent: binaryAt:put: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing' children directories directoryNames fileNames files)
('testing' exists isDirectory isFile)
('initialize-release' refresh refreshChildren)
('services' nextNameFor:coda:extension: nextNameFor:extension:)
!

