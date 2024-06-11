'From Cuis7.1 [latest update: #6462] on 11 June 2024 at 5:05:49 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 6/11/2024 17:01:18'!
helpMenuOptions

	^ `{
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#aboutThisSystem.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'current version information.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 			-> 		#openTerseGuide.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Class Comment Browser'.
				#selector 			-> 		#openCommentGuide.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'Search & explore Cuis Class Comments. Requires cloning "Cuis-Smalltalk-Tools" git repo.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCodeManagementInCuis.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCuisAndGitHub.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 			-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 			-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 			-> 		'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'VM Statistics'.
				#selector 			-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 			-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Space Left'.
				#selector 			-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 			-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 6/11/2024 17:01:37'!
worldMenuOptions
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
	^options! !

