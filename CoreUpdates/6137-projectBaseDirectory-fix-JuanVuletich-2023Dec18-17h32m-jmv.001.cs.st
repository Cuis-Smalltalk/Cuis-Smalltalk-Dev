'From Cuis6.0 [latest update: #6136] on 18 December 2023 at 5:33:22 pm'!

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/18/2023 17:32:17'!
projectBaseDirectory
	"The main directory that contains the #cuisBaseDirectory, but also other directories with user and project files.
	See #cuisBaseDirectory.
	projectBaseDirectory/SavedChangeSets 									Default folder for saving new ChangeSets
	projectBaseDirectory/Packages: 									Default folder for new code packages
	projectBaseDirectory/UserChanges: 									Folder to store user changes files
	projectBaseDirectory/FileOuts: 									Default folder for filed out code
	projectBaseDirectory/Logs: 									Default folder for error logs and other logs
	DirectoryEntry projectBaseDirectory
	"
	ProjectBaseDirectory notNil ifTrue: [
		^ ProjectBaseDirectory ].

	^ ProjectBaseDirectory := self cuisBaseDirectory parent.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DirectoryEntry releaseClassCachedState.!

