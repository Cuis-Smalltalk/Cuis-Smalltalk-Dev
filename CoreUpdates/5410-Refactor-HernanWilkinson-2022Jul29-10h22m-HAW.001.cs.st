'From Cuis 6.0 [latest update: #5409] on 29 July 2022 at 10:24:00 am'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 10:22:25'!
additionalMenuOptions

	^`{
		{
			#submenuOf  				-> 		TheWorldMenu openLabel.
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		10.
			#label 				->		'Package Downloader'.
			#selector 				-> 		#openPackageDownloader.
			#icon 				-> 		#packageIcon.
			#balloonText 				-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary.
	
	}`	
	! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 10:23:26'!
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
			#label 				-> 		'Windows...'.
			#selector 				-> 		#openWindowsMenu.
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

