'From Cuis 6.0 [latest update: #5401] on 28 July 2022 at 7:27:09 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/28/2022 19:24:51'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Focus follows mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Click to focus'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Use Unicode text'.
				#object 			-> 		TextModel.
				#selector 			-> 		#useUtf8Editors.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load the VectorGraphics package, use TrueType fonts, and Unicode text.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		60.
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		70.
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		80.
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`	
	! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/28/2022 19:25:14'!
worldMenuOptions

	^`{{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		10.
		#label 				-> 		TheWorldMenu openLabel.
		#selector 				-> 		#yourself.
		#icon 				-> 		#openIcon.
		#balloonText				-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#submenuOf  				-> 		TheWorldMenu openLabel.
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		10.
		#label 				->		'Package Downloader'.
		#selector 				-> 		#openPackageDownloader.
		#icon 				-> 		#packageIcon.
		#balloonText 				-> 		'A tool for downloading Cuis packages from the internet.'.
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
}, TheWorldMenu preferencesMenuOptions`! !

!methodRemoval: TheWorldMenu #openPreferencesMenu stamp: 'HAW 7/28/2022 19:26:13'!
TheWorldMenu removeSelector: #openPreferencesMenu!
!methodRemoval: TheWorldMenu #preferencesMenu stamp: 'HAW 7/28/2022 19:26:16'!
TheWorldMenu removeSelector: #preferencesMenu!
