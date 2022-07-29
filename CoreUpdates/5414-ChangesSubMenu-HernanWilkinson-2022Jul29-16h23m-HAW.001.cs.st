'From Cuis 6.0 [latest update: #5411] on 29 July 2022 at 5:23:54 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 16:24:37'!
changesLabel

	^'Changes'! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:22:32'!
changesMenuOptions

	^`{
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Change Sorter'.
				#object 			-> 		ChangeSorterWindow.
				#selector 		-> 		#openChangeSorter.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Install New Updates'.
				#object 			-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Browse my Changes'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Recently logged Changes...'.
				#object 			-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:23:08'!
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
			#label 				-> 		TheWorldMenu helpLabel.
			#selector 				-> 		#yourself.
			#icon 				-> 		#helpIcon.
			#balloonText 				-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
		} asDictionary.
		{
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		10.
			#label 				-> 		TheWorldMenu changesLabel.
			#selector 				-> 		#yourself.
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

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:23:21'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions`! !

!methodRemoval: TheWorldMenu #changesMenu stamp: 'HAW 7/29/2022 17:22:59'!
TheWorldMenu removeSelector: #changesMenu!
!methodRemoval: TheWorldMenu #openChangesMenu stamp: 'HAW 7/29/2022 17:23:03'!
TheWorldMenu removeSelector: #openChangesMenu!
