"
Instances of TheWorldMenu serve to present the primary Squeak menu obtained by clicking on open desktop.
"
Class {
	#name : 'TheWorldMenu',
	#superclass : 'Object',
	#instVars : [
		'myWorld',
		'myHand'
	],
	#classVars : [
		'ExtraOpenCommands'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'menu building' }
TheWorldMenu class >> additionalMenuOptions [

	^`{
		{
			#submenuOf  		-> 		TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		30.
			#label 				-> 		'Package Installer'.
			#selector 			-> 		#openPackageInstaller.
			#icon 				-> 		#packageIcon.
			#balloonText 		-> 		'A tool for installing Cuis packages from the local file system.'.
		} asDictionary.
		{
			#submenuOf  		-> 		TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		40.
			#label 				-> 		'Package Downloader'.
			#selector 			-> 		#openPackageDownloader.
			#icon 				-> 		#packageIcon.
			#balloonText 		-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary.
	}`	
	
]

{ #category : 'menu building' }
TheWorldMenu class >> changesLabel [

	^'Changes'
]

{ #category : 'menu building' }
TheWorldMenu class >> changesMenuOptions [

	^`{
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		10.
				#label 			-> 		'Change Sorter'.
				#object 		-> 		ChangeSorter.
				#selector 		-> 		#open.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		'Install New Updates'.
				#object 		-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		10.
				#label 			-> 		'Browse my Changes'.
				#object 		-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		20.
				#label 			-> 		'Recently logged Changes...'.
				#object 		-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		40.
				#itemOrder 	-> 		10.
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`
]

{ #category : 'menu building' }
TheWorldMenu class >> debugLabel [

	^'Debug'
]

{ #category : 'menu building' }
TheWorldMenu class >> debugMenuOptions [

	^`{
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#inspect.
				#icon 				-> 		#inspectIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#explore.
				#icon 				-> 		#exploreIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'MessageTally all Processes'.
				#selector 			-> 		#startMessageTally.
				#icon 				-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#removeAllKnownFailing.
				#icon 				-> 		#graphicsIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#resumeAfterStepError.
				#icon 				-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 				-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 			-> 		#closeAllDebuggers.
				#icon 				-> 		#closeIcon.
			} asDictionary.
		}`
]

{ #category : 'instance creation' }
TheWorldMenu class >> getMenuIn: aWorld at: anActiveHand [
	^ (self new world: aWorld hand: anActiveHand)
		buildWorldMenu
]

{ #category : 'menu building' }
TheWorldMenu class >> helpLabel [

	^'Help'
]

{ #category : 'menu building' }
TheWorldMenu class >> helpMenuOptions [

	^ `{
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#aboutThisSystem.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'current version information.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Terse Guide to Cuis'.
				#selector 			-> 		#openTerseGuide.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'Class Comment Browser'.
				#selector 			-> 		#openCommentGuide.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'Search & explore Cuis Class Comments. Requires cloning "Cuis-Smalltalk-Tools" git repo.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		40.
				#label 				-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCodeManagementInCuis.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		50.
				#label 				-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCuisAndGitHub.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 			-> 		#openHelp.
				#icon 				-> 		#keyboardShortcutsIcon.
				#balloonText 		-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openUsefulExpressions.
				#icon 				-> 		#chatIcon.
				#balloonText 		-> 		'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 				-> 		'VM Statistics'.
				#selector 			-> 		#vmStatistics.
				#icon 				-> 		#systemMonitorIcon.
				#balloonText 		-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 				-> 		'Space Left'.
				#selector 			-> 		#garbageCollect.
				#icon 				-> 		#removableMediaIcon.
				#balloonText 		-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`
]

{ #category : 'menu building' }
TheWorldMenu class >> openLabel [

	^'Open'
]

{ #category : 'menu building' }
TheWorldMenu class >> preferencesLabel [

	^'Preferences'
]

{ #category : 'menu building' }
TheWorldMenu class >> preferencesMenuOptions [

	^`{
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Focus follows Mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Focus when Click'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.

			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Show ST-80 assignments'.
				#selector 			-> 		#enableShowAssignmentAsLeftArrow.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Show variable assignment in Smalltalk-80 style (left arrow).'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Show ANSI assignments'.
				#selector 			-> 		#disableShowAssignmentAsLeftArrow.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Show variable assignment in ANSI style ( := ).'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 				-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label				-> 		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		60.
				#label				-> 		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		70.
				#label 				-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 				-> 		#worldIcon.
				#balloonText 		-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		80.
				#label 				-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 				-> 		#appearanceIcon.
				#balloonText 		-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 				-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 				-> 		#expandIcon.
				#balloonText 		-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 				-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 				-> 		#collapseIcon.
				#balloonText 		-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 				-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 				-> 		#viewFullscreenIcon.
				#balloonText 		-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		20.
				#label 				-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 				-> 		#exitFullscreenIcon.
				#balloonText 		-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 				-> 		'Save Prefs in UserPrefs.txt'.
				#selector 			-> 		#enableUsePrefsFile.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Preferences are saved without need to save the Smalltalk Image.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		20.
				#label 				-> 		'Save Prefs in the Image'.
				#selector 			-> 		#disableUsePrefsFile.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Preferences are lost unless the Smalltalk Image is saved.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		30.
				#label 				-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 				-> 		#usersIcon.
				#balloonText 		-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		40.
				#label 				-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 				-> 		#preferencesIcon.
				#balloonText 		-> 		'view and change various options.'
			} asDictionary.
		}`.
]

{ #category : 'menu building' }
TheWorldMenu class >> windowsLabel [

	^'Windows'
]

{ #category : 'menu building' }
TheWorldMenu class >> windowsMenuOptions [

	^`{
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Presents a list of browsers that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 				-> 		#newWindowIcon.
				#balloonText 		-> 		'Presents a list of all windows that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		->  					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 				-> 		#printerIcon.
				#balloonText 		-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 				-> 		#systemFileManagerIcon.
				#balloonText 		-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 				-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 				-> 		#changesIcon.
				#balloonText 		-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 				-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 				-> 		#inspectIcon.
				#balloonText 		-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 				-> 		'Tile open windows'.
				#object          		-> 		TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Tile open windows'.
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 				-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 				-> 		#collapseIcon.
				#balloonText 		-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 				-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 				-> 		#expandIcon.
				#balloonText 		-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 				-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 				-> 		#closeIcon.
				#balloonText 		-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 				-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 				-> 		#goBottomIcon.
				#balloonText 		-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		60.
				#label 				-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 				-> 		#displayIcon.
				#balloonText 		-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 				-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 				-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 				-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all unchanged windows and all non-window morphs on the world.'
			} asDictionary.
		}`
]

{ #category : 'menu building' }
TheWorldMenu class >> worldMainMenuOptions [

	^`{
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		10.
			#label 					-> 		TheWorldMenu openLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#openIcon.
			#balloonText			-> 		'Submenu to open various system tools'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		20.
			#label 					-> 		'New morph...'.
			#selector				-> 		#newMorph.
			#icon 					-> 		#morphsIcon.
			#balloonText 			-> 		'Offers a variety of ways to create new objects'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		30.
			#label 					-> 		TheWorldMenu preferencesLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#preferencesIcon.
			#balloonText 			-> 		'Opens a menu offering many controls over appearance and system preferences.'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		40.
			#label 					-> 		TheWorldMenu windowsLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#windowIcon.
			#balloonText 			-> 		'Manage open windows'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		50.
			#label 					-> 		TheWorldMenu helpLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#helpIcon.
			#balloonText 			-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		10.
			#label 					-> 		TheWorldMenu changesLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#changesIcon.
			#balloonText 			-> 		'Opens a menu of useful tools for dealing with changes'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		20.
			#label 					-> 		TheWorldMenu debugLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#debugIcon.
			#balloonText 			-> 		'a menu of debugging items'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		30.
			#label 					-> 		'Restore Display'.
			#object 				-> 		#myWorld.
			#selector 				-> 		#restoreDisplay.
			#icon 					-> 		#displayIcon.
			#balloonText 			-> 		'Repaint the screen -- useful for cleaning unwanted display artifacts.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		10.
			#label 					-> 		'Save Image'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveImage.
			#icon 					-> 		#saveIcon.
			#balloonText 			-> 		'Save the current state of the image on disk. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		20.
			#label 					-> 		'Save Image as...'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAs.
			#icon 					-> 		#saveAsIcon.
			#balloonText 			-> 		'Save the current state of the image on disk under a new name.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		30.
			#label 					-> 		'Save Image and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAndQuit.
			#icon 					-> 		#saveAndQuitIcon.
			#balloonText 			-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 			-> 		50.
			#itemOrder 			-> 		40.
			#label 					-> 		'Quit without saving'.
			#selector 				-> 		#quitSession.
			#icon 					-> 		#quitIcon.
			#balloonText 			-> 		('Quit out of Cuis without saving the image.\',
									'Ask for confirmation if there are unsaved changes.') withNewLines.
		} asDictionary.
	}`
]

{ #category : 'menu building' }
TheWorldMenu class >> worldMenuOptions [
	| options |
	options := `
		TheWorldMenu worldMainMenuOptions,
		"TheWorldMenu additionalMenuOptions,"
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions`.
		(Preferences at: #saveReleaseOptionsInWorldMenu) ifTrue: [
			options := options, TheWorldMenu worldSaveReleaseImagesMenuOptions ].
	^options
]

{ #category : 'menu building' }
TheWorldMenu class >> worldSaveReleaseImagesMenuOptions [
	"
	Preferences
		name: #saveReleaseOptionsInWorldMenu
		description: 'Include options for saving Stable and Rolling Release images'
		category: #system type: Boolean value: false
	"

	^`{
				{
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		10.
					#label 				-> 		'Save Stable Release'.
					#object 			-> 		Smalltalk.
					#selector 			-> 		#saveStableRelease.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 		-> 		('Save as a new official Stable Release of Cuis.\',
												'See naming in #saveStableRelease.\',
												'Clear all user preferences and class vars.\',
												'Write new compacted Sources file, an empty\',
												'Changes file, and save the Image on disk.\') withNewLines.
				} asDictionary.
				{
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		20.
					#label 				-> 		'Save Rolling Release, Quit'.
					#object 			-> 		Smalltalk.
					#selector 			-> 		#saveRollingReleaseAndQuit.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 		-> 		('Save as update of the Rolling Release of Cuis.\',
												'Clear all user preferences and class vars.\',
												'Save Image and Changes with update number.\',
												'Quit Cuis.\') withNewLines.
				} asDictionary.
			}`
]

{ #category : 'construction' }
TheWorldMenu >> alphabeticalMorphMenu [
	| list splitLists |
	list := Morph withAllSubclasses select: [ :m |
		m includeInNewMorphMenu ].
	list := list asArray sort: [ :c1 :c2 |
		c1 name < c2 name ].
	splitLists := self
		splitNewMorphList: list
		depth: self pvtAlphaSplitListDepth .
	^ self pvtNewMenuForSplitLists: splitLists 

]

{ #category : 'construction' }
TheWorldMenu >> buildWorldMenu [
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #worldMenuOptions)
		addStayUpIcons;
		addTitle: 'World';
		yourself
	
	
]

{ #category : 'commands' }
TheWorldMenu >> cleanUpWorld [
	(SelectionMenu confirm:
'This will remove all windows except those containing unsaved text.
It will also remove all non-window morphs on the desktop.
Are you sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonWindowRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]
]

{ #category : 'commands' }
TheWorldMenu >> disableFocusFollowsMouse [
	Preferences name: #focusFollowsMouse category: #gui value: false.
	Preferences saveToDisk: #focusFollowsMouse.
]

{ #category : 'commands' }
TheWorldMenu >> disableShowAssignmentAsLeftArrow [
	Preferences name: #showAssignmentAsLeftArrow category: #programming value: false.
	Preferences saveToDisk: #showAssignmentAsLeftArrow.
]

{ #category : 'commands' }
TheWorldMenu >> disableUsePrefsFile [
	Preferences name: #useUserPrefsFile category: #system value: false.
]

{ #category : 'popups' }
TheWorldMenu >> doPopUp: aMenu [

	aMenu popUpForHand: myHand in: myWorld.

]

{ #category : 'commands' }
TheWorldMenu >> enableFocusFollowsMouse [
	Preferences name: #focusFollowsMouse category: #gui value: true.
	Preferences saveToDisk: #focusFollowsMouse.
]

{ #category : 'commands' }
TheWorldMenu >> enableShowAssignmentAsLeftArrow [
	Preferences name: #showAssignmentAsLeftArrow category: #programming value: true.
	Preferences saveToDisk: #showAssignmentAsLeftArrow.
]

{ #category : 'commands' }
TheWorldMenu >> enableUsePrefsFile [
	Preferences name: #useUserPrefsFile category: #system value: true.
	PreferenceSet saveUserPrefs.
]

{ #category : 'commands' }
TheWorldMenu >> fullScreenOff [

	Display fullScreenMode: false.
]

{ #category : 'commands' }
TheWorldMenu >> fullScreenOn [

	Display fullScreenMode: true.
]

{ #category : 'commands' }
TheWorldMenu >> garbageCollect [
	"Do a garbage collection, and report results to the user."

	Utilities garbageCollectAndReport
]

{ #category : 'mechanics' }
TheWorldMenu >> menu: titleString [
	"Create a menu with the given title, ready for filling"

	| menu |
	(menu := MenuMorph entitled: titleString) 
		defaultTarget: self; 
		addStayUpIcons.
	^ menu

]

{ #category : 'mechanics' }
TheWorldMenu >> myWorld [
	^ myWorld
]

{ #category : 'construction' }
TheWorldMenu >> newMorph [
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu catDict |
	menu := self menu: 'Add a new morph'.
	menu
		
			add: 'From Clipboard'
			target: myHand
			action: #pasteMorph;
		
			add: 'From Alphabetical List'
			subMenu: self alphabeticalMorphMenu.
	menu addLine.
	"Add 'Classic' New Morph menu"
	catDict := Dictionary new.
	Morph allSubclassesDo: [ :eaSubclass |
		eaSubclass includeInNewMorphMenu ifTrue: [ | category |
			category := eaSubclass categoryInNewMorphMenu ifNil: [ 'Unknown'].				
			(catDict includesKey: category)
				ifTrue: [ (catDict at: category) add: eaSubclass ]
				ifFalse: [
					catDict
						at: category
						put: (OrderedCollection with: eaSubclass) ]]].
	catDict keys sort do: [ :categ | | morphsInCat |
		morphsInCat := (catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ].
		morphsInCat size > self pvtMaxItemsPerCategorySubMenu
			ifTrue: [
				menu
					add: categ
					subMenu:
						(self pvtNewMenuForSplitLists:
							(self
								splitNewMorphList: morphsInCat
								depth: self pvtCategorySplitListDepth )) ]
			ifFalse: [ | subMenu |
				subMenu := MenuMorph new.
				morphsInCat do: [ :cl |
					subMenu
						add: cl name
						target: self
						action: #newMorphOfClass:event:
						argument: cl ].
				menu
					add: categ
					subMenu: subMenu ]].
	self doPopUp: menu.
]

{ #category : 'commands' }
TheWorldMenu >> newMorphOfClass: morphClass event: evt [
	"Attach a new morph of the given class to the invoking hand."

	| m |
	m := morphClass initializedInstance.
	evt hand attachMorph: m
]

{ #category : 'menu' }
TheWorldMenu >> openCommentGuide [
	"In ClassCommentBrowser.pck"
	Smalltalk at: #CommentGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Class Comment Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'ClassCommentBrowser'] 
	].
	Smalltalk at: #CommentGuide ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package ClassCommentBrowser.pck.st', String newLineString, 'Feature require: #''ClassCommentBrowser'' '
]

{ #category : 'menu' }
TheWorldMenu >> openPackageDownloader [
	"In PackageDownloader.pck"
	Smalltalk at: #PackageDownloaderWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Downloader is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'PackageDownloader'] 
	].
	Smalltalk at: #PackageDownloaderWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageDownloader.pck.st', String newLineString, 'Feature require: #''PackageDownloader'' '
]

{ #category : 'menu' }
TheWorldMenu >> openPackageInstaller [
	"In PackageInstaller.pck"
	Smalltalk at: #PackageInstallerWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Installer is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #PackageInstaller ] 
	].
	Smalltalk at: #PackageInstallerWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageInstaller.pck.st', String newLineString, 'Feature require: #''PackageInstaller'' '
]

{ #category : 'menu' }
TheWorldMenu >> openTerseGuide [
	"In TerseGuide.pck"
	Smalltalk at: #TerseGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Terse Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'TerseGuide'] 
	].
	Smalltalk at: #TerseGuideWindow ifPresent: [ :cls | ^cls openTerseGuide ].
	self inform: 'Please install optional package TerseGuide.pck.st', String newLineString, 'Feature require: #''TerseGuide'' '
]

{ #category : 'private' }
TheWorldMenu >> pvtAlphaSplitListDepth [
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 3
]

{ #category : 'private' }
TheWorldMenu >> pvtCategorySplitListDepth [
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 2.
]

{ #category : 'private' }
TheWorldMenu >> pvtMaxItemsPerCategorySubMenu [
	"If the number of items exceeds this value, split the category submenu into sub-submenus"
	^ 15.
]

{ #category : 'private' }
TheWorldMenu >> pvtNewMenuForSplitLists: splitLists [
	| firstPrefix lastPrefix menu subMenu |
	menu := MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstPrefix := i = 1
				ifTrue: [ 'A' ]
				ifFalse: [ (splitLists at: i) first name first: 3 ].
			lastPrefix := i = splitLists size
				ifTrue: [ 'Z' ]
				ifFalse: [ (splitLists at: i) last name first: 3 ].
			subMenu := MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					action: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: firstPrefix , ' - ' , lastPrefix
				subMenu: subMenu ].
	^ menu.
]

{ #category : 'commands' }
TheWorldMenu >> quitSession [
	Smalltalk askConfirmationOnQuit ifFalse: [ ^ self ].
	Smalltalk quit
]

{ #category : 'commands' }
TheWorldMenu >> saveWorldInFile [
	"Save the world's submorphs, model, and stepList in a file.  "

	self request: 'File name for this morph?' do: [ :fileName |
		fileName isEmpty ifTrue: [^ self].  "abort"
		"Save only model, stepList, submorphs in this world"
		myWorld submorphsDo: [ :m | m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"
		(fileName, '.morph') asFileEntry writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream fileOutObject: myWorld ]]].
]

{ #category : 'commands' }
TheWorldMenu >> splitNewMorphList: list depth: d [
	| middle c prev next |
	d <= 0 ifTrue: [ ^ Array with: list ].
	middle := list size // 2 + 1.
	c := (list at: middle) name first: 3.
	prev := middle - 1.
	[
	prev > 0 and: [ ((list at: prev) name first: 3) = c ]] whileTrue: [ prev := prev - 1 ].
	next := middle + 1.
	[
	next <= list size and: [ ((list at: next) name first: 3) = c ]] whileTrue: [ next := next + 1 ].
	"Choose the better cluster"
	middle := middle - prev < (next - middle)
		ifTrue: [ prev + 1 ]
		ifFalse: [ next ].
	middle = 1 ifTrue: [ middle := next ].
	middle >= list size ifTrue: [ middle := prev + 1 ].
	(middle = 1 or: [ middle >= list size ]) ifTrue: [ ^ Array with: list ].
	^ Array streamContents: [ :out |
		out nextPutAll:
			(self
				splitNewMorphList:
					(list
						copyFrom: 1
						to: middle - 1)
				depth: d - 1).
		out nextPutAll:
			(self
				splitNewMorphList:
					(list
						copyFrom: middle
						to: list size)
				depth: d - 1) ].
]

{ #category : 'commands' }
TheWorldMenu >> startMessageTally [
	"Tally on all the processes in the system, and not only the UI"
	
	| d |
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		[
			d := Delay forMilliseconds: 100.
			AndreasSystemProfiler spyAllOn: [
				[Sensor peekMousePt y > 10] whileTrue: [d wait]]
			] forkAt: Processor userInterruptPriority
		]
]

{ #category : 'commands' }
TheWorldMenu >> vmStatistics [
	"Open a string view on a report of vm statistics"

	(TextModel new contents: Utilities vmStatisticsReportString)
		openLabel: 'VM Statistics'
]

{ #category : 'mechanics' }
TheWorldMenu >> world: aWorld hand: aHand [

	myWorld := aWorld.
	myHand := aHand.
]
