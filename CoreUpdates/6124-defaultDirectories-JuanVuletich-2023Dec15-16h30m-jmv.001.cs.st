'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:31:21 pm'!

!DirectoryEntry class methodsFor: 'default folders' stamp: 'jmv 12/15/2023 15:20:48'!
coreUpdatesDirectory
	^ DirectoryEntry cuisBaseDirectory / 'CoreUpdates'! !

!DirectoryEntry class methodsFor: 'default folders' stamp: 'jmv 12/15/2023 15:20:59'!
packagesDirectory
	^ DirectoryEntry cuisBaseDirectory / 'Packages'! !

!DirectoryEntry class methodsFor: 'default folders' stamp: 'jmv 12/15/2023 15:22:26'!
trueTypeFontsDirectory
	^ DirectoryEntry cuisBaseDirectory / 'TrueTypeFonts'! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/15/2023 14:54:21'!
fileOutsDirectory
	^ DirectoryEntry projectBaseDirectory / 'FileOuts' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/15/2023 14:55:28'!
logsDirectory
	^ DirectoryEntry projectBaseDirectory / 'Logs' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/15/2023 16:20:43'!
newPackagesDirectory
	^ DirectoryEntry projectBaseDirectory / 'NewPackages' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/15/2023 15:37:12'!
savedChangeSetsDirectory
	^ DirectoryEntry projectBaseDirectory / 'ChangeSets' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/15/2023 14:59:19'!
userChangesDirectory
	^ DirectoryEntry projectBaseDirectory / 'UserChanges' :: assureExistence! !

