'From Cuis 5.0 [latest update: #4836] on 5 September 2021 at 1:59:07 pm'!

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/5/2021 13:54:43'!
showingLineDiffsString
	"Answer a string representing whether I'm showing regular diffs"

	^ self showingLineDiffs asMenuItemTextPrefix,
		'lineDiffs'! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/5/2021 13:55:59'!
showingPrettyLineDiffsString
	"Answer a string representing whether I'm showing pretty diffs"

	^ self showingPrettyLineDiffs asMenuItemTextPrefix,
		'linePrettyDiffs'! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/5/2021 13:56:14'!
showingPrettyWordDiffsString
	"Answer a string representing whether I'm showing pretty diffs"

	^ self showingPrettyWordDiffs asMenuItemTextPrefix,
		'wordPrettyDiffs'! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/5/2021 13:56:25'!
showingWordDiffsString
	"Answer a string representing whether I'm showing regular diffs"

	^ self showingWordDiffs asMenuItemTextPrefix,
		'wordDiffs'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/5/2021 13:55:22'!
prettyPrintString
	"Answer whether the receiver is showing pretty-print"

	^ self showingPrettyPrint asMenuItemTextPrefix,
		'prettyPrint'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/5/2021 13:53:44'!
showingByteCodesString
	"Answer whether the receiver is showing bytecodes"

	^ self showingByteCodes asMenuItemTextPrefix,
		'byteCodes'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/5/2021 13:54:04'!
showingDecompileString
	"Answer a string characerizing whether decompilation is showing"

	^ self showingDecompile asMenuItemTextPrefix,
		'decompile'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/5/2021 13:54:20'!
showingDocumentationString
	"Answer a string characerizing whether documentation is showing"

	^ self showingDocumentation asMenuItemTextPrefix,
		'documentation'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/5/2021 13:55:44'!
showingPlainSourceString
	"Answer a string telling whether the receiver is showing plain source"

	^ self showingPlainSource asMenuItemTextPrefix,
		'source'! !


!Morph methodsFor: 'menus' stamp: 'jmv 9/5/2021 13:57:02'!
stickinessString
	"Answer the string to be shown in a menu to represent the  
	stickiness status"

	^ self isSticky asMenuItemTextPrefix,
		'resist being picked up'! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 9/5/2021 13:57:19'!
wrapString
	"Answer the string to put in a menu that will invite the user to 
	switch word wrap mode"
	^ wrapFlag asMenuItemTextPrefix,
		'text wrap to bounds'! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 9/5/2021 13:58:00'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	model currentDirectorySelected
		ifNil: [ aMenu add: 'initial directory' action: #yourself :: isEnabled: false ]
		ifNotNil: [ :selectedWrapper |
	aMenu 
		add: (Preferences isInitialFileListDirectory: selectedWrapper item)
			asMenuItemTextPrefix, 'initial directory'
		action: #toggleInitialDirectory ::
			setBalloonText: 'The selected directory is an initial director for new file list windows' ].
	initialDirectoriesMenu _ MenuMorph new.
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
				action: #initialFileListDirectories:
				argument: entry first ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !

