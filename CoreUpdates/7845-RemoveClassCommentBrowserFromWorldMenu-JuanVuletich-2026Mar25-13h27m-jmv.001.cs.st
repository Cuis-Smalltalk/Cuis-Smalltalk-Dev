'From Cuis7.7 [latest update: #7844] on 25 March 2026 at 1:34:05 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 25/Mar/2026 13:27:53'!
helpMenuOptions

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
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 25/Mar/2026 13:32:46'!
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

!methodRemoval: TheWorldMenu #openCommentGuide stamp: 'jmv 25/Mar/2026 13:28:06'!
TheWorldMenu removeSelector: #openCommentGuide!
