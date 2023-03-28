'From Cuis 6.0 [latest update: #5731] on 28 March 2023 at 3:43:08 pm'!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected showHidden '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!

!FileList methodsFor: 'accessing' stamp: 'jmv 3/28/2023 15:30:52'!
showHidden
	^showHidden! !

!FileList methodsFor: 'user interface' stamp: 'jmv 3/28/2023 15:24:20'!
toggleShowHidden
	showHidden := showHidden not.
	self changed: #initialDirectoryList.
	self updateFileList.! !


!FileList methodsFor: 'initialization' stamp: 'jmv 3/28/2023 15:19:05'!
initialize

	showDirsInFileList := false.
	showHidden := false.! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 3/28/2023 15:37:43'!
doesPattern: aPattern allow: entry 

	(aPattern = '*' or: [ aPattern match: entry name ]) ifFalse: [ ^false ].

	(showHidden not and: [ entry name first = $. ]) ifTrue: [ ^false ].

	"Hide Mac resurce forks and folder service stores"
	(entry name = '.DS_Store') ifTrue: [ ^false ].
	('._*' match: entry name) ifTrue: [ ^false ].
	^true! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/28/2023 15:32:28'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu := MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	model currentDirectorySelected
		ifNil: [ aMenu add: 'initial directory' action: #yourself :: isEnabled: false ]
		ifNotNil: [ :selectedWrapper |
			aMenu 
				add: (PreferenceSet isInitialFileListDirectory: selectedWrapper item)
					asMenuItemTextPrefix, 'initial directory'
				action: #toggleInitialDirectory ::
					setBalloonText: 'The selected directory is an initial director for new file list windows' ].
	aMenu
		add: model showHidden asMenuItemTextPrefix, 'show hidden folders and files'
		action: #toggleShowHidden ::
		setBalloonText: 'show or hide files and directories that begin with a dot'.
	initialDirectoriesMenu := MenuMorph new.
	#(
		(roots  'default roots' 'Use the usual root directories. Drives on Windows; "/" on Unix')
	   	(image 'image directory' 'Use the directory with Smalltalk image')
		(vm 'VM directory' 'Use the virtual machine directory')
		(current 'current directory' 'Use the current directory; usually the directory the VM was started in')
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


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 3/28/2023 15:41:28'!
contents

	| names |
	names := item directoryNames asArray.
	model showHidden ifFalse:[
		names := names reject:[:each | each beginsWith: '.'] ].
	^(names sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: model ]! !

!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected showHidden'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
