'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 1:16:14.492429 pm'!

!FileStream methodsFor: 'file accessing' stamp: 'jmv 10/16/2015 13:05'!
localName
	self subclassResponsibility! !

!methodRemoval: StandardFileStream #file!
StandardFileStream removeSelector: #file!
!methodRemoval: FileStream #file!
FileStream removeSelector: #file!
!methodRemoval: FileStream #printOn:!
FileStream removeSelector: #printOn:!
!methodMoveToSomePackage: FmDirectoryEntry #file!
FmDirectoryEntry removeSelectorIfInBaseSystem: #file!

!FmDirectoryEntry reorganize!
('actions-path' /)
('enumeration' allChildrenDo: allChildrenDo:filesMatches:directoryMatches: allDirectoriesDo: allFilesDo: allFilesDo:matches: childrenDo: childrenDo:filesMatches:directoryMatches: directoriesDo: directoriesMatches: filesDo: filesMatches: latestFileMatches: oldestFileMatches:)
('converting' asDirectoryEntry asFileEntry ensureAsDirectory ensureAsFile)
('actions-directory' assureExistence copyTo: delete recursiveDelete rename:)
('private' assureExistenceFileNamed: basicRecursiveDelete initChildren)
('dictionary-like' at: at:ifAbsent: at:put: binaryAt: binaryAt:ifAbsent: binaryAt:put: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing' children directories directory directoryNames fileNames files)
('testing' exists isDirectory)
('initialize-release' refresh refreshChildren)
('cuis specific' fileNamesMatching: nextNameFor:coda:extension: nextNameFor:extension:)
('*FileMan-StrictCompatibility' file)
!

!methodMoveToSomePackage: FmFileEntry class #example4!
FmFileEntry class removeSelectorIfInBaseSystem: #example4!

!FmFileEntry class reorganize!
('accessing' drives)
('instance creation' entry: pathComponents: pathComponents:drive: pathName:)
('examples' example1 example2 example3 example5 example6 example7)
('system startup')
('cached state access' releaseClassCachedState)
('*FileMan-StrictCompatibility' example4)
!

!methodMoveToSomePackage: FmFileEntry #file!
FmFileEntry removeSelectorIfInBaseSystem: #file!
