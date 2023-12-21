'From Cuis6.0 [latest update: #6145] on 20 December 2023 at 4:09:43 pm'!
!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children '
	classVariableNames: 'CuisBaseDirectory CurrentDirectory ImageDirectory ProjectBaseDirectory VMDirectory UserBaseDirectory '
	poolDictionaries: ''
	category: 'System-File System'!

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/20/2023 16:08:12'!
userBaseDirectory
	"The main directory that contains the #cuisBaseDirectory, but also other directories with user files.
	See #cuisBaseDirectory.
	userBaseDirectory/SavedChangeSets 									Default folder for saving new ChangeSets
	userBaseDirectory/Packages: 									Default folder for new code packages
	userBaseDirectory/UserChanges: 									Folder to store user changes files
	userBaseDirectory/FileOuts: 									Default folder for filed out code
	userBaseDirectory/Logs: 									Default folder for error logs and other logs
	DirectoryEntry userBaseDirectory
	"
	UserBaseDirectory notNil ifTrue: [
		^ UserBaseDirectory ].

	^ UserBaseDirectory := self cuisBaseDirectory parent / (self cuisBaseDirectory name, '-UserFiles').! !


!ContentPack class methodsFor: 'configuration' stamp: 'jmv 12/20/2023 16:01:11'!
exportDirectory
	
	^ DirectoryEntry userBaseDirectory / self defaultContentDirectory / 'Exported'! !

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 12/20/2023 16:01:15'!
importDirectory
	
	^ DirectoryEntry userBaseDirectory / self defaultContentDirectory / 'Import'! !


!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/20/2023 16:00:29'!
projectBaseDirectory
	"The main directory that contains the #cuisBaseDirectory, but also other directories with project files.
	See #cuisBaseDirectory.
	DirectoryEntry projectBaseDirectory
	"
	ProjectBaseDirectory notNil ifTrue: [
		^ ProjectBaseDirectory ].

	^ ProjectBaseDirectory := self cuisBaseDirectory parent.! !

!DirectoryEntry class methodsFor: 'cached state access' stamp: 'jmv 12/20/2023 16:00:46'!
releaseClassCachedState

	ImageDirectory := nil.
	VMDirectory := nil.
	CurrentDirectory := nil.
	CuisBaseDirectory := nil.
	ProjectBaseDirectory := nil.
	UserBaseDirectory := nil.! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/20/2023 16:01:18'!
fileOutsDirectory
	^ DirectoryEntry userBaseDirectory / 'FileOuts' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/20/2023 16:01:22'!
logsDirectory
	^ DirectoryEntry userBaseDirectory / 'Logs' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/20/2023 16:01:25'!
newPackagesDirectory
	^ DirectoryEntry userBaseDirectory / 'NewPackages' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/20/2023 16:01:29'!
savedChangeSetsDirectory
	^ DirectoryEntry userBaseDirectory / 'ChangeSets' :: assureExistence! !

!DirectoryEntry class methodsFor: 'ensured existence folders' stamp: 'jmv 12/20/2023 16:01:32'!
userChangesDirectory
	^ DirectoryEntry userBaseDirectory / 'UserChanges' :: assureExistence! !


!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/20/2023 16:01:39'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file line parts pref |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[
					pref := Preferences instanceAt: parts first asSymbol.
					pref value: parts second.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/20/2023 16:01:42'!
saveUserPrefs
	"
	PreferenceSet saveUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file forceWriteStreamDo: [ :stream |
		Preferences allPreferences keysAndValuesDo: [ :symbolName :preference |
			preference isSavedToDisk ifTrue: [
				stream
					nextPutAll: symbolName;
					nextPut: $=.
				preference value printOn: stream.
				stream newLine ]]].! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'jmv 12/20/2023 16:02:12'!
openFileList
	"
	FileListWindow openFileList
	"
	self openFileList: (FileList new directory: DirectoryEntry userBaseDirectory)! !

!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CuisBaseDirectory CurrentDirectory ImageDirectory ProjectBaseDirectory UserBaseDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-File System'!
