'From Cuis 6.0 [latest update: #5411] on 29 July 2022 at 4:23:33 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 16:19:31'!
helpLabel

	^'Help'! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 16:21:32'!
helpMenuOptions
	
	^ `{
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#aboutThisSystem.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'current version information.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 		-> 		#openTerseGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'Class Comment Browser'.
				#selector 		-> 		#openCommentGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCodeManagementInCuis.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		50.
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCuisAndGitHub.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 	-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 	-> 		'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'VM Statistics'.
				#selector 		-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf -> TheWorldMenu helpLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Space Left'.
				#selector 		-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 16:22:30'!
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

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 16:21:43'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions`! !

!methodRemoval: TheWorldMenu #helpMenu stamp: 'HAW 7/29/2022 16:22:22'!
TheWorldMenu removeSelector: #helpMenu!
!methodRemoval: TheWorldMenu #openHelpMenu stamp: 'HAW 7/29/2022 16:22:26'!
TheWorldMenu removeSelector: #openHelpMenu!

!TheWorldMenu class reorganize!
('menu building' additionalMenuOptions helpLabel helpMenuOptions openLabel preferencesLabel preferencesMenuOptions windowsLabel windowsMenuOptions worldMainMenuOptions worldMenuOptions)
!

