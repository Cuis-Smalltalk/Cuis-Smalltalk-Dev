'From Cuis 5.0 of 7 November 2016 [latest update: #3315] on 6 May 2018 at 11:21:29 am'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:51:49'!
add: aString action: aSymbol icon: symbolOrFormOrNil
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:51:24'!
add: aString action: aSymbol icon: symbolOrFormOrNil enabled: aBoolean
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil;
			isEnabled: aBoolean! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:56:22'!
add: aString target: aTarget action: aSymbol icon: symbolOrFormOrNil
	^(self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/6/2018 10:56:43'!
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

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #clockIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/6/2018 11:20:40'!
classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'class list'.
	aMenu addStayUpIcons.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete class from change set (d)'.
				#object 			-> 		#model.
				#selector 		-> 		#forgetClass.
				#icon 			-> 		#warningIcon
			} asDictionary.
			{
				#label 			-> 		'remove class from system (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
		}`.
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/6/2018 11:12:18'!
messageMenu
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'message list'.
	aMenu addStayUpIcons.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete method from changeSet (d)'.
				#object 			-> 		#model.
				#selector 		-> 		#forget.
				#icon 			-> 		#warningIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method from system (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
		}`.
	^ aMenu! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 5/6/2018 09:38:40'!
addBlankIconsIfNecessary
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary ]].
"	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ]."
	withoutIcons do: [ :item | item setBlankIcon ]! !

!methodRemoval: MenuMorph #add:action:enabled:!
MenuMorph removeSelector: #add:action:enabled:!
