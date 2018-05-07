'From Cuis 5.0 of 7 November 2016 [latest update: #3309] on 5 May 2018 at 5:09:03 pm'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/5/2018 16:57:30'!
buildFromDictionaries: dataForMenuDicts
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


!TheWorldMenu methodsFor: 'mechanics' stamp: 'jmv 5/5/2018 17:03:51'!
myWorld
	^ myWorld! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:03:26'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(MenuMorph new defaultTarget: self)
		addStayUpIcons;
		buildFromDictionaries: `{
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:01:58'!
changesMenu
        "Build the changes menu for the world."

	^ (self menu: 'Changes...')
		buildFromDictionaries: `{
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:04:31'!
debugMenu

	^ (self menu: 'Debug...')
		buildFromDictionaries: `{
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:05:32'!
helpMenu
	"Build the help menu for the world."

	^ (self menu: 'Help...')
		buildFromDictionaries: `{
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:06:38'!
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
	menu buildFromDictionaries: itemsSorted.
	^ menu.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/5/2018 17:07:28'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		buildFromDictionaries: `{
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

!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 5/5/2018 17:08:12'!
windowsMenu
        "Build the windows menu for the world."

	^ (self menu: 'Windows')
		buildFromDictionaries: `{
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

!methodRemoval: TheWorldMenu #fillIn:fromDictionaries:!
TheWorldMenu removeSelector: #fillIn:fromDictionaries:!
!methodRemoval: TheWorldMenu #world!
TheWorldMenu removeSelector: #world!
