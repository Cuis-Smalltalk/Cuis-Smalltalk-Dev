'From Cuis6.3 [latest update: #6255] on 11 March 2024 at 1:28:15 pm'!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected showHidden topDirectories '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!

!FileList methodsFor: 'initialization' stamp: 'jmv 3/11/2024 13:18:25'!
initialDirectoryList

	topDirectories := (Preferences at: #initialFileListDirectories) 
		caseOf: {
			[ #cuis ]  		-> 	[ | topDirectoryToUse |
				topDirectoryToUse := directory. "must be inside the tree of some of the included!!"
				topDirectoryToUse := topDirectoryToUse commonAncestorWith: DirectoryEntry projectBaseDirectory.
				topDirectoryToUse := topDirectoryToUse commonAncestorWith: DirectoryEntry userBaseDirectory.
				topDirectoryToUse := topDirectoryToUse commonAncestorWith: DirectoryEntry cuisBaseDirectory.
				topDirectoryToUse := topDirectoryToUse commonAncestorWith: DirectoryEntry currentDirectory.
				{ topDirectoryToUse} ].
			[ #roots ] -> [ DirectoryEntry roots ].
			}.
	^topDirectories collect: [ :dir |
		FileDirectoryWrapper with: dir name: (dir name ifNil: [ '/' ]) model: self ]! !

!FileList methodsFor: 'private' stamp: 'jmv 3/11/2024 13:27:49'!
postOpen
"Ojo, ver que pasa con drive en Windows"
| top |
	directory ifNotNil: [
		top := topDirectories detect: [ :any | any isAncestorOf: directory ].
		top pathComponents size = 0 ifTrue: [
			^self changed: #(openPath), {directory drive}, directory pathComponents
			].
		self changed: #(openPath), 
		(
		directory pathComponents copyFrom: (top pathComponents size
		) to: directory pathComponents size)
		]! !

!FileList methodsFor: 'private' stamp: 'jmv 7/20/2016 10:34:35'!
setSelectedDirectoryTo: aFileDirectoryWrapper
	currentDirectorySelected := aFileDirectoryWrapper.
	self directory: aFileDirectoryWrapper withoutListWrapper.
	brevityState := #FileList.
	self changed: #fileList.
	self acceptedContentsChanged.
	self changed: #currentDirectorySelected! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 3/11/2024 13:18:41'!
prefSystem
	^ #( #(allowNonLocalReturnsInExceptionHandlers true )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(changeSetVersionNumbers true )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #cuis #(cuis roots) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(useUserPrefsFile true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
	)! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/11/2024 13:20:42'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu := MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	aMenu
		add: model showHidden asMenuItemTextPrefix, 'show hidden folders and files'
		action: #toggleShowHidden ::
		setBalloonText: 'show or hide files and directories that begin with a dot'.
	initialDirectoriesMenu := MenuMorph new.
	#(
		(cuis  		'Cuis top' 'Use the default Cuis top directory')
		(roots  		'OS roots' 'Use the host OS root directories. Drives on Windows; "/" on Unix')
	)
		do: [ :entry |
			initialDirectoriesMenu
				add: entry second
				target: Preferences
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !

!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected showHidden topDirectories'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences at: #initialFileListDirectories put: #cuis!

