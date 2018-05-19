'From Cuis 5.0 of 7 November 2016 [latest update: #3328] on 14 May 2018 at 1:02:23 pm'!

!SystemWindow methodsFor: 'menu' stamp: 'jmv 5/14/2018 11:54:07'!
buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 				action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon;
		addLine;
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 	icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
											action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 						icon: #collapseIcon;
		add: 'expand / contract' 		action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize;
		add: 'resize full' 					action: #resizeFull;
		add: 'resize top' 					action: #resizeTop;
		add: 'resize left' 				action: #resizeLeft;
		add: 'resize bottom' 			action: #resizeBottom;
		add: 'resize right' 				action: #resizeRight;
		add: 'resize top left' 			action: #resizeTopLeft;
		add: 'resize top right' 			action: #resizeTopRight;
		add: 'resize bottom left' 		action: #resizeBottomLeft;
		add: 'resize bottom right' 		action: #resizeBottomRight.

	^ aMenu! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/14/2018 10:56:45'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove 	icon: #fileOutIcon 	enabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep 		icon: #fileOutIcon.
	aMenu addLine.

	aMenu add: 'Rename change set (r)' 			action: #rename 					icon: #saveAsIcon 	enabled: isForBaseSystem.
	aMenu add: 'Destroy change set (x)' 			action: #remove 					icon: #warningIcon 	enabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble 			icon: #textEditorIcon.
				aMenu add: 'Remove preamble' 	action: #removePreamble 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble 			icon: #listAddIcon ].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript 			icon: #textEditorIcon .
				aMenu add: 'Remove postscript' 	action: #removePostscript 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript 			icon: #listAddIcon ].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	(aMenu add: 'conflicts with other change sets' 	target: self 	action: #browseMethodConflicts 	icon: #emblemImportantIcon)
		setBalloonText: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	(aMenu add: 'trim history' 						action: #trimHistory 				icon: #clockIcon 		enabled: isForBaseSystem)
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #packageIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 5/14/2018 10:28:53'!
volumeMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon.
	^ aMenu! !


!TestRunnerWindow methodsFor: 'menu building' stamp: 'jmv 5/14/2018 11:10:46'!
listMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases'.
	aMenu add: 'select all' target: model action: #selectAll icon: #selectAllIcon.
	aMenu add: 'deselect all' target: model action: #deselectAll icon: #selectAllIcon.
	aMenu add: 'toggle selections' target: model action: #invertSelections icon: #switchIcon.
	aMenu add: 'filter' target: model action: #setFilter icon: #findIcon.
	model runButtonState ifTrue: [
		aMenu add: 'stop' target: model action: #terminateRun icon: #cancelIcon ].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls _ (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls _ cls asSymbol.
		cls _ (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			(aMenu add: 'browse' target: self action: #browse: argument: cls)
				setIcon: #editFindReplaceIcon.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult icon: #printerIcon.
	^aMenu! !


!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 5/14/2018 11:55:00'!
offerWindowMenu
	| aMenu |
	aMenu _ self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings
		icon: #warningIcon.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu popUpInWorld: self world! !

