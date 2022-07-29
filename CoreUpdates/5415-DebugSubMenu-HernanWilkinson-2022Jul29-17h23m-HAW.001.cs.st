'From Cuis 6.0 [latest update: #5411] on 29 July 2022 at 5:28:23 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:24:24'!
debugLabel

	^'Debug'! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:27:05'!
debugMenuOptions

	^`{
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#inspect.
				#icon 			-> 		#inspectIcon.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#explore.
				#icon 			-> 		#exploreIcon.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'MessageTally all Processes'.
				#selector 			-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#removeAllKnownFailing.
				#icon 			-> 		#graphicsIcon.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#resumeAfterStepError.
				#icon 			-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#submenuOf  ->					TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 			-> 		#closeAllDebuggers.
				#icon 			-> 		#closeIcon.
			} asDictionary.
		}`! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:27:50'!
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
			#label 				-> 		TheWorldMenu debugLabel.
			#selector 				-> 		#yourself.
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

!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/29/2022 17:27:19'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions `! !

!methodRemoval: TheWorldMenu #openDebugMenu stamp: 'HAW 7/29/2022 17:27:45'!
TheWorldMenu removeSelector: #openDebugMenu!
!methodRemoval: TheWorldMenu #debugMenu stamp: 'HAW 7/29/2022 17:27:37'!
TheWorldMenu removeSelector: #debugMenu!
