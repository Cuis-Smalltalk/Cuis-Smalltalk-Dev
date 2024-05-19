'From Cuis6.3 [latest update: #6358] on 12 May 2024 at 6:22:26 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/12/2024 18:19:45'!
worldSaveReleaseImagesMenuOptions
	"
	Preferences
		name: #saveReleaseOptionsInWorldMenu
		description: 'Include options for saving Stable and Rolling Release images'
		category: #system type: Boolean value: false
	"

	^`{
				{
					#itemGroup 				-> 		40.
					#itemOrder 				-> 		10.
					#label 				-> 		'Save Official Release'.
					#object 				-> 		Smalltalk.
					#selector 				-> 		#saveNumberedRelease.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 				-> 		('Save as a new numbered release of Cuis.\',
											'Name after #saveNumberedRelease.\',
											'Clear all user preferences and class vars.\',
											'Write new compacted Sources file, an empty\',
											'Changes file, and save the Image on disk.\',
											'Quit Cuis.\') withNewLines.
				} asDictionary.
				{
					#itemGroup 				-> 		40.
					#itemOrder 				-> 		20.
					#label 				-> 		'Save Updated and Quit'.
					#object 				-> 		Smalltalk.
					#selector 				-> 		#saveRollingReleaseAndQuit.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 				-> 		('Save as update of the rolling release of Cuis.\',
											'Clear all user preferences and class vars.\',
											'Save Image and Changes with update number.\',
											'Quit Cuis.\') withNewLines.
				} asDictionary.
			}`! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 5/12/2024 18:13:27'!
prefSystem
	^ #( #(allowNonLocalReturnsInExceptionHandlers true )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(changeSetVersionNumbers true )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #cuis #(cuis roots) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(useUserPrefsFile true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
		#(saveReleaseOptionsInWorldMenu false)
	)! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/12/2024 18:15:22'!
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
			#label 				-> 		'Save Image and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAndQuit.
			#icon 				-> 		#saveAndQuitIcon.
			#balloonText 				-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 				-> 		50.
			#itemOrder 				-> 		40.
			#label 				-> 		'Quit without saving'.
			#selector 				-> 		#quitSession.
			#icon 				-> 		#quitIcon.
			#balloonText 				-> 		('Quit out of Cuis without saving the image.\',
									'Ask for confirmation if there are unsaved changes.') withNewLines.
		} asDictionary.
	}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/12/2024 18:20:09'!
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences
	name: #saveReleaseOptionsInWorldMenu
	description: 'Include options for saving Stable and Rolling Release images'
	category: #system type: Boolean value: false
!

