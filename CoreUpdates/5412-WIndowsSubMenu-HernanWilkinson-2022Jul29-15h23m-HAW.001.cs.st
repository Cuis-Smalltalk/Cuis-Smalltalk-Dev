'From Cuis 6.0 [latest update: #5411] on 29 July 2022 at 3:30:32 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 15:24:16'!
windowsLabel

	^'Windows'! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 15:28:20'!
windowsMenuOptions

	^`{
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Tile open windows'.
				#object          ->       					TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		60.
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 15:29:27'!
worldMainMenuOptions

	^`{
		{
			#itemGroup 				-> 		10.
			#itemOrder 				-> 		10.
			#label 				-> 		TheWorldMenu openLabel.
			#selector 				-> 		#yourself.
			#icon 				-> 		#openIcon.
			#balloonText				-> 		'Submenu to open various system tools'.
		} asDictionary.
		{
			#itemGroup 				-> 		10.
			#itemOrder 				-> 		20.
			#label 				-> 		'New morph...'.
			#selector				-> 		#newMorph.
			#icon 				-> 		#morphsIcon.
			#balloonText 				-> 		'Offers a variety of ways to create new objects'.
		} asDictionary.
		{
			#itemGroup 				-> 		10.
			#itemOrder 				-> 		30.
			#label 				-> 		TheWorldMenu preferencesLabel.
			#selector 				-> 		#yourself.
			#icon 				-> 		#preferencesIcon.
			#balloonText 				-> 		'Opens a menu offering many controls over appearance and system preferences.'.
		} asDictionary.
		{
			#itemGroup 				-> 		10.
			#itemOrder 				-> 		40.
			#label 				-> 		TheWorldMenu windowsLabel.
			#selector 				-> 		#yourself.
			#icon 				-> 		#windowIcon.
			#balloonText 				-> 		'Manage open windows'.
		} asDictionary.
		{
			#itemGroup 				-> 		10.
			#itemOrder 				-> 		50.
			#label 				-> 		'Help...'.
			#selector 				-> 		#openHelpMenu.
			#icon 				-> 		#helpIcon.
			#balloonText 				-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
		} asDictionary.
		{
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		10.
			#label 				-> 		'Changes...'.
			#selector 				-> 		#openChangesMenu.
			#icon 				-> 		#changesIcon.
			#balloonText 				-> 		'Opens a menu of useful tools for dealing with changes'.
		} asDictionary.
		{
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		20.
			#label 				-> 		'Debug...'.
			#selector 				-> 		#openDebugMenu.
			#icon 				-> 		#debugIcon.
			#balloonText 				-> 		'a menu of debugging items'.
		} asDictionary.
		{
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		30.
			#label 				-> 		'Restore Display'.
			#object 				-> 		#myWorld.
			#selector 				-> 		#restoreDisplay.
			#icon 				-> 		#displayIcon.
			#balloonText 				-> 		'Repaint the screen -- useful for cleaning unwanted display artifacts.'.
		} asDictionary.
		{
			#itemGroup 				-> 		30.
			#itemOrder 				-> 		10.
			#label 				-> 		'Save Image'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveImage.
			#icon 				-> 		#saveIcon.
			#balloonText 				-> 		'Save the current state of the image on disk. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 				-> 		30.
			#itemOrder 				-> 		20.
			#label 				-> 		'Save Image as...'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAs.
			#icon 				-> 		#saveAsIcon.
			#balloonText 				-> 		'Save the current state of the image on disk under a new name.'.
		} asDictionary.
		{
			#itemGroup 				-> 		30.
			#itemOrder 				-> 		30.
			#label 				-> 		'Save Release and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAsNewReleaseAndQuit.
			#icon 				-> 		#saveAsNewVersionIcon.
			#balloonText 				-> 		('Save as a new release of Cuis.\',
									'Clear all user preferences and user state (class vars).\',
									'Use an updated version-stamped name\',
									'and save the image with that name on disk.\',
									'Quit Cuis.') withNewLines.
		} asDictionary.
		{
			#itemGroup 				-> 		30.
			#itemOrder 				-> 		40.
			#label 				-> 		'Save Image and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAndQuit.
			#icon 				-> 		#saveAndQuitIcon.
			#balloonText 				-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 				-> 		30.
			#itemOrder 				-> 		50.
			#label 				-> 		'Quit without saving'.
			#selector 				-> 		#quitSession.
			#icon 				-> 		#quitIcon.
			#balloonText 				-> 		('Quit out of Cuis without saving the image.\',
									'Ask for confirmation if there are unsaved changes.') withNewLines.
		} asDictionary.
	}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 15:28:40'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions`! !

!methodRemoval: TheWorldMenu #openWindowsMenu stamp: 'HAW 7/29/2022 15:29:36'!
TheWorldMenu removeSelector: #openWindowsMenu!
!methodRemoval: TheWorldMenu #windowsMenu stamp: 'HAW 7/29/2022 15:29:41'!
TheWorldMenu removeSelector: #windowsMenu!
