'From Cuis 6.0 [latest update: #5078] on 23 February 2022 at 10:53:19 am'!

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 2/23/2022 10:14:03'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
		(10	'browse'			browseMethodFull							'view this method in a browser')
		(11	'senders' 			browseSendersOfMessages							'browse senders of...' 						browseSendersOfMethod)
		(16	'implementors'			browseMessages							'browse implementors of...' 						browseImplementors)
		(12	'versions'			browseVersions							'browse versions')), 

		(Preferences decorateBrowserButtons
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'}}]
			ifFalse: [
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance')}]),

		#(
		(12	'hierarchy'			browseHierarchy							'browse class hierarchy')
		(10	'inst vars'			browseInstVarRefs							'inst var refs...')
		(11	'class vars'			browseClassVarRefs							'class var refs...')
		(10	'show...'			offerWhatToShowMenu							'menu of what to show in lower pane')).

	^ aList! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 2/23/2022 10:35:35'!
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
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #packageIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 2/23/2022 10:37:07'!
add: aString action: aSymbol icon: symbolOrFormOrNil enabled: aBoolean
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil;
			isEnabled: aBoolean! !

