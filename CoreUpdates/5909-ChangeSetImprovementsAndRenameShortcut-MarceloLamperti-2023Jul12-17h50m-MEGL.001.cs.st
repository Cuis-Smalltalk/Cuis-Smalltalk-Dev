'From Cuis 6.0 [latest update: #5908] on 12 July 2023 at 6:18:35 pm'!

!ChangeListElement methodsFor: 'accessing' stamp: 'MEGL 7/12/2023 17:59:12'!
changeType

	self subclassResponsibility ! !

!ChangeListElement methodsFor: 'accessing' stamp: 'MEGL 7/12/2023 18:00:00'!
string

	self subclassResponsibility ! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'MEGL 7/12/2023 18:17:08'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	self flag: #assumesKeyboardShortcuts.
	isForBaseSystem := model changeSet isForBaseSystem.
	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove 	icon: #fileOutIcon 	enabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep 		icon: #fileOutIcon.
	aMenu addLine.

	aMenu add: 'Rename change set (R)' 			action: #rename 					icon: #saveAsIcon 	enabled: isForBaseSystem.
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
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #packageIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'MEGL 7/12/2023 18:13:37'!
changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing]. 
	aChar = $o ifTrue: [^ model fileOutAndRemove].
	aChar = $k ifTrue: [^ model fileOutAndKeep].
	aChar = $R ifTrue: [^ model rename].
	aChar = $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'MEGL 7/12/2023 18:16:44'!
messageListKey: aChar from: view 
	
	aChar = $R ifTrue: [^ model rename ].
	
	^super messageListKey: aChar from: view 
! !


!ChangeRecord reorganize!
('accessing' category changeClass changeClassName changeType compilerClass dateAndTime fileName fileOutOn: isMetaClassChange methodSelector overridesASuperclassMethod prior stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: file:position:type:class:category:meta:stamp:prior: file:position:type:class:category:meta:stamp:prior:overrides: fileIn markAsTest:)
('testing' isDoIt isTestClassChange)
('printing' printOn:)
!

