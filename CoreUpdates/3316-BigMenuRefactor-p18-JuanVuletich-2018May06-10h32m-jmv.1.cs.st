'From Cuis 5.0 of 7 November 2016 [latest update: #3315] on 6 May 2018 at 10:33:27 am'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:34'!
addItemsFromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item wantsIcons |
	wantsIcons _ Preferences wantsMenuIcons.
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ self addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object ifAbsent: [defaultTarget].
				realTarget isSymbol ifTrue: [ realTarget _ defaultTarget perform: realTarget ].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						self
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						self
							add: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				wantsIcons ifTrue: [
					dict
						at: #icon
						ifPresent: [ :symbolOrFormOrNil |
							item setIcon: symbolOrFormOrNil ]].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]]! !


!TextEditor methodsFor: 'menu' stamp: 'jmv 5/6/2018 10:32:42'!
getMenu
	
	^(MenuMorph new defaultTarget: self)
		addTitle: self class name;
		addStayUpIcons;
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find...(f)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 		-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`! !


!SmalltalkEditor methodsFor: 'menu' stamp: 'jmv 5/6/2018 10:32:39'!
getMenu
	
	^(MenuMorph new defaultTarget: self)
		addTitle: self class name;
		addStayUpIcons;
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#label 			-> 		'Debug it (D)'.	
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#label 			-> 		'Profile it'.	
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingit.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Method Source with it'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#label 			-> 		'Cancel (l)'.
				#selector 		-> 		#cancelEdits.
				#icon 			-> 		#cancelIcon
			} asDictionary.
			{
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`! !

!SmalltalkEditor methodsFor: 'menu' stamp: 'jmv 5/6/2018 10:32:41'!
openMenu2
	
	(MenuMorph new defaultTarget: self)
		addTitle: self class name;
		addStayUpIcons;
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Find...(f)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`;
	popUpInWorld: morph world.! !


!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/6/2018 10:32:59'!
classCommentVersionsMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu addTitle: 'versions'.
	aMenu addStayUpIcons.
	aMenu addItemsFromDictionaries: `{
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

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/6/2018 10:33:10'!
methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu addItemsFromDictionaries: `{
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
		aMenu addItemsFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version'.
				#object 			-> 			#model.
				#selector 		-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu addItemsFromDictionaries: `{
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


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:44'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(MenuMorph new defaultTarget: self)
		addStayUpIcons;
		addItemsFromDictionaries: `{
				{
					#label 			-> 		'Open...'.
					#selector 		-> 		#openOpenMenu.
					#icon 			-> 		#openIcon.
					#balloonText 	-> 		'Submenu to open various system tools'.
				} asDictionary.
				{
					#label 			-> 		'New morph...'.
					#selector 		-> 		#newMorph.
					#icon 			-> 		#morphsIcon.
					#balloonText 	-> 		'Offers a variety of ways to create new objects'.
				} asDictionary.
				{
					#label 			-> 		'Preferences...'.
					#selector 		-> 		#openPreferencesMenu.
					#icon 			-> 		#preferencesIcon.
					#balloonText 	-> 		'Opens a menu offering many controls over appearance and system preferences.'.
				} asDictionary.
				{
					#label 			-> 		'Windows...'.
					#selector 		-> 		#openWindowsMenu.
					#icon 			-> 		#windowIcon.
					#balloonText 	-> 		'Manage open windows'.
				} asDictionary.
				{
					#label 			-> 		'Help...'.
					#selector 		-> 		#openHelpMenu.
					#icon 			-> 		#helpIcon.
					#balloonText 	-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
				} asDictionary.
				nil.
				{
					#label 			-> 		'Changes...'.
					#selector 		-> 		#openChangesMenu.
					#icon 			-> 		#changesIcon.
					#balloonText 	-> 		'Opens a menu of useful tools for dealing with changes'.
				} asDictionary.
				{
					#label 			-> 		'Debug...'.
					#selector 		-> 		#openDebugMenu.
					#icon 			-> 		#debugIcon.
					#balloonText 	-> 		'a menu of debugging items'.
				} asDictionary.
				{
					#label 			-> 		'Restore Display (r)'.
					#object 			-> 		#myWorld.
					#selector 		-> 		#restoreDisplay.
					#icon 			-> 		#displayIcon.
					#balloonText 	-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
				} asDictionary.
				nil.
				{
					#label 			-> 		'Save'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveSession.
					#icon 			-> 		#saveIcon.
					#balloonText 	-> 		'Save the current version of the image on disk'.
				} asDictionary.
				{
					#label 			-> 		'Save as...'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveAs.
					#icon 			-> 		#saveAsIcon.
					#balloonText 	-> 		'Save the current version of the image on disk under a new name.'.
				} asDictionary.
				{
					#label 			-> 		'Save as New Version'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveAsNewVersion.
					#icon 			-> 		#saveAsNewVersionIcon.
					#balloonText 	-> 		('Save the current image a new version-stamped name\and save it under that name on disk.\',
												'Clear all user preferences and user state (class vars).') withNewLines.
				} asDictionary.
				{
					#label 			-> 		'Save and Quit'.
					#selector 		-> 		#saveAndQuit.
					#icon 			-> 		#saveAndQuitIcon.
					#balloonText 	-> 		'Save the image and quit out of Cuis.'.
				} asDictionary.
				{
					#label 			-> 		'Quit'.
					#selector 		-> 		#quitSession.
					#icon 			-> 		#quitIcon.
					#balloonText 	-> 		'Quit out of Cuis.'.
				} asDictionary.
			}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:45'!
changesMenu
        "Build the changes menu for the world."

	^ (self menu: 'Changes...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Change Sorter'.
				#object 			-> 		ChangeSorterWindow.
				#selector 		-> 		#openChangeSorter.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Install New Updates'.
				#object 			-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Browse my Changes'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#label 			-> 		'Recently logged Changes...'.
				#object 			-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:47'!
debugMenu

	^ (self menu: 'Debug...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#inspect.
				#icon 			-> 		#inspectIcon.
			} asDictionary.
			{
				#label 			-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#explore.
				#icon 			-> 		#exploreIcon.
			} asDictionary.
			{
				#label 			-> 		'MessageTally all Processes'.
				#selector 		-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#label 			-> 		'MessageTally UI and Browse'.
				#selector 		-> 		#startThenBrowseMessageTally.
				#icon 			-> 		#editFindReplaceIcon.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#removeAllKnownFailing.
				#icon 			-> 		#graphicsIcon.
			} asDictionary.
			{
				#label 			-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#resumeAfterStepError.
				#icon 			-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#label 			-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 		-> 		#closeAllDebuggers.
				#icon 			-> 		#closeIcon.
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:49'!
helpMenu
	"Build the help menu for the world."

	^ (self menu: 'Help...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#aboutThisSystem.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'current version information.'
			} asDictionary.
			{
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 		-> 		#openTerseGuide.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#label 			-> 		'Class Comment Browser'.
				#selector 		-> 		#openCommentGuide.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCodeManagementInCuis.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCuisAndGitHub.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'GitHub usage pattern.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 	-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 	-> 		'a window full of useful expressions.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'VM Statistics'.
				#selector 		-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#label 			-> 		'Space Left'.
				#selector 		-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:52'!
openMenu
	"Build the open window menu for the world."
	| menu items groups firstGroup itemsSorted itemsBase |
	menu _ self menu: 'Open...'.
	itemsBase _ (Smalltalk allClassesImplementing: #worldMenuForOpenGroup)
		collect: [ :item |
			item class == Metaclass ifTrue: [
				item soleInstance
					worldMenuForOpenGroup ] ]
		thenSelect: [ :item |
			item notNil ].
	items _ OrderedCollection new.
	"A single class may add more than one item to a menu"
	itemsBase do: [ :item |
		item class == Dictionary
			ifTrue: [ items add: item ]
			ifFalse: [ items addAll: item ]].
	groups _ (items collect: [ :item |
		item at: #itemGroup ]) asSet asSortedCollection.
	itemsSorted _ OrderedCollection new.
	firstGroup _ true.
	groups do: [ :group |
		firstGroup
			ifTrue: [ firstGroup _ false ]
			ifFalse: [ itemsSorted add: nil ].
		((items select: [ :item |
			(item at: #itemGroup) = group ]) sort: [ :item1 :item2 |
			(item1 at: #itemOrder) < (item2 at: #itemOrder) ]) do: [ :item |
			itemsSorted add: item ]].
	menu addItemsFromDictionaries: itemsSorted.
	^ menu.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 10:32:55'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 		-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 		-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Font Sizes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 		-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 		-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set display depth...'.
				#selector 		-> 		#setDisplayDepth.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'choose how many bits per pixel.'
			} asDictionary.
			{
				#label 			-> 		'Set desktop color...'.
				#selector 		-> 		#changeBackgroundColor.
				#icon 			-> 		#wallpaperIcon.
				#balloonText 	-> 		'choose a uniform color to use as desktop background.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 		-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 		-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 5/6/2018 10:32:56'!
windowsMenu
        "Build the windows menu for the world."

	^ (self menu: 'Windows')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#label 			-> 		'Delete Both of the Above'.
				#selector 		-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !

!methodRemoval: MenuMorph #buildFromDictionaries:!
MenuMorph removeSelector: #buildFromDictionaries:!
