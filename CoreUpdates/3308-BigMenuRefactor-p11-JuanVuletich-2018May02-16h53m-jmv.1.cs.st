'From Cuis 5.0 of 7 November 2016 [latest update: #3307] on 2 May 2018 at 5:47:44 pm'!
!classDefinition: #UpdatingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #UpdatingMenuItemMorph
	instanceVariableNames: 'wordingProvider wordingSelector enablementSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!UpdatingMenuItemMorph methodsFor: 'world' stamp: 'jmv 5/2/2018 17:46:11'!
updateContents
	"Update the receiver's contents"

	| newString nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs _ wordingSelector numArgs.
		newString _ nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				nArgs = arguments size ifTrue: [
					wordingProvider perform: wordingSelector withArguments: arguments]].
		newString = (contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString ]]! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 16:57:32'!
changesMenu
        "Build the changes menu for the world."

	^ self
		fillIn: (self menu: 'Changes...')
		fromDictionaries: `{
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
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 16:58:03'!
debugMenu

	^ self
		fillIn: (self menu: 'Debug...')
		fromDictionaries: `{
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
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#label 			-> 		'MessageTally UI and Browse'.
				#object 			-> 		#theWorldMenu.
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

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 17:16:07'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item wantsIcons |
	wantsIcons _ Preferences wantsMenuIcons.
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object.
				realTarget == #myWorld ifTrue: [realTarget _ myWorld].
				realTarget == #theWorldMenu ifTrue: [realTarget _ self].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						aMenu
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						aMenu
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
			]].
	^ aMenu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 16:56:08'!
helpMenu
	"Build the help menu for the world."

	^ self
		fillIn: (self menu: 'Help...')
		fromDictionaries: `{
			{
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#aboutThisSystem.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'current version information.'
			} asDictionary.
			{
				#label 			-> 		'Terse Guide to Cuis'.
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#openTerseGuide.
				#icon 			->		#helpIcon.
				#balloonText 	-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#label 			-> 		'Class Comment Browser'.
				#object 			-> 		#theWorldMenu.
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
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#label 			-> 		'Space Left'.
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 17:12:47'!
preferencesMenu
	"Build the preferences menu for the world."

	^ self
		fillIn: (self menu: 'Preferences...')
		fromDictionaries: `{
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
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set display depth...'.
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#setDisplayDepth.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'choose how many bits per pixel.'
			} asDictionary.
			{
				#label 			-> 		'Set desktop color...'.
				#object 			-> 		#theWorldMenu.
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

!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 5/2/2018 17:42:52'!
windowsMenu
        "Build the windows menu for the world."

	^ self
		fillIn: (self menu: 'Windows')
		fromDictionaries: `{
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
				#object 			-> 		#theWorldMenu.
				#selector 		-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !

!methodRemoval: TheWorldMenu #doMenuItem:with:!
TheWorldMenu removeSelector: #doMenuItem:with:!
!methodRemoval: TheWorldMenu #fillIn:from:!
TheWorldMenu removeSelector: #fillIn:from:!
!methodRemoval: UpdatingMenuItemMorph #enablement!
UpdatingMenuItemMorph removeSelector: #enablement!
!methodRemoval: UpdatingMenuItemMorph #enablementSelector:!
UpdatingMenuItemMorph removeSelector: #enablementSelector:!
!classDefinition: #UpdatingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #UpdatingMenuItemMorph
	instanceVariableNames: 'wordingProvider wordingSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
