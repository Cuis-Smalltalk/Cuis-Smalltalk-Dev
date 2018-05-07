'From Cuis 5.0 of 7 November 2016 [latest update: #3311] on 5 May 2018 at 8:47:10 pm'!

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:45:15'!
classCommentVersionsMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu addTitle: 'versions'.
	aMenu addStayUpIcons.
	aMenu buildFromDictionaries: `{
		{
			#label 			-> 			'compare to current'.
			#selector 		-> 			#compareToCurrentVersion.
			#balloonText 	-> 			'compare selected version to the current version'
		} asDictionary.
		{
			#label 			-> 			'revert to selected version'.
			#selector 		-> 			#fileInSelections.
			#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:45:08'!
methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu buildFromDictionaries: `{
				{
					#label 			-> 			'compare to current'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToCurrentVersion.
					#balloonText 	-> 			'compare selected version to the current version'
				} asDictionary.
				{
					#label 			-> 			'compare to version...'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToOtherVersion.
					#balloonText 	-> 			'compare selected version to another selected version'
				} asDictionary.
			}` ].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		aMenu buildFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version'.
				#object 			-> 			#model.
				#selector 		-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu buildFromDictionaries: `{
		{
			#label 			-> 			'edit current method (O)'.
			#selector 		-> 			#openSingleMessageBrowser.
			#balloonText 	-> 			'open a single-message browser on the current version of this method'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#object 			-> 			#model.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#object 			-> 			#model.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'senders (n)'.
			#selector 		-> 			#browseSenders.
			#balloonText 	-> 			'browse all senders of this selector'
		} asDictionary.
		{
			#label 			-> 			'implementors (m)'.
			#selector 		-> 			#browseImplementors.
			#balloonText 	-> 			'browse all implementors of this selector'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#object 			-> 			#model.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:27:50'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Change Set'.
	aMenu addStayUpIcons.

	(aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove) 	isEnabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep.
	aMenu addLine.

	(aMenu add: 'Rename change set (r)' 			action: #rename) 					isEnabled: isForBaseSystem.
	(aMenu add: 'Destroy change set (x)' 			action: #remove) 					isEnabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble.
				aMenu add: 'Remove preamble' 	action: #removePreamble]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript.
				aMenu add: 'Remove postscript' 	action: #removePostscript]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	(aMenu add: 'conflicts with other change sets' target: self action: #browseMethodConflicts)
		setBalloonText: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	(aMenu add: 'trim history' action: #trimHistory)
		isEnabled: isForBaseSystem;
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:27:52'!
classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'class list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
			-
			('delete class from change set (d)'		forgetClass					''		model)
			('remove class from system (x)'		removeClass					''		model)
			-
			('browse full (b)'						browseMethodFull)
			('browse hierarchy (h)'					browseHierarchy)
			('browse protocol (p)'					browseFullProtocol)
			-
			('inst var refs...'							browseInstVarRefs)
			('inst var defs...'						browseInstVarDefs)
			('class var refs...'						browseClassVarRefs)
			('class vars'								browseClassVariables)
			('class refs (N)'							browseClassRefs)).
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:27:55'!
messageMenu
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'message list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
			('delete method from changeSet (d)'	forget							''		model)
			-
			('remove method from system (x)'		removeMessage				''		model)
				-
			('browse full (b)'						browseMethodFull)
			('browse hierarchy (h)'					browseHierarchy)
			('browse method (O)'					openSingleMessageBrowser)
			('browse protocol (p)'					browseFullProtocol)
			-
			('fileOut'								fileOutMessage				''		model)
			-
			('senders of... (n)'						browseSendersOfMessages)
			('implementors of... (m)'					browseMessages)
			('inheritance (i)'							methodHierarchy)
			('versions (v)'							browseVersions)).
	^ aMenu! !


!TestRunnerWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:27:58'!
listMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases'.
	aMenu add: 'select all' target: model action: #selectAll.
	aMenu add: 'deselect all' target: model action: #deselectAll.
	aMenu add: 'toggle selections' target: model action: #invertSelections.
	aMenu add: 'filter' target: model action: #setFilter.
	model runButtonState ifTrue: [
		aMenu add: 'stop' target: model action: #terminateRun
		].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls _ (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls _ cls asSymbol.
		cls _ (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			aMenu add: 'browse' target: self action: #browse: argument: cls.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult.
	^aMenu! !

!methodRemoval: MenuMorph #title:!
MenuMorph removeSelector: #title:!
