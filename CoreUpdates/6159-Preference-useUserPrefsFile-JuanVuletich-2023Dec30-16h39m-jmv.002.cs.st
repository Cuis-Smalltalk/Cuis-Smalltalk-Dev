'From Cuis6.3 [latest update: #6156] on 30 December 2023 at 4:51:21 pm'!

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/30/2023 16:45:54'!
disableUsePrefsFile
	Preferences name: #useUserPrefsFile category: #system value: false.! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/30/2023 16:45:36'!
enableUsePrefsFile
	Preferences name: #useUserPrefsFile category: #system value: true.
	PreferenceSet saveUserPrefs.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/30/2023 16:39:59'!
readAndApplyUserPrefs
	"
	Smalltalk readAndApplyUserPrefs
	"
	(Preferences at: #useUserPrefsFile) ifTrue: [
		PreferenceSet readAndApplyUserPrefs ].! !


!Preference methodsFor: 'accessing' stamp: 'jmv 12/30/2023 16:40:36'!
isSavedToDisk: aBoolean
	"True if this preference was modified by the user, or read from disk.
	If so, keep the User Preferences file updated."

	isSavedToDisk := aBoolean.
	isSavedToDisk ifTrue: [
		(Preferences at: #useUserPrefsFile) ifTrue: [
			PreferenceSet saveUserPrefs ]].! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 12/30/2023 16:43:01'!
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
		#(initialFileListDirectories #roots #(root image vm current) )
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
	)! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 12/30/2023 16:50:57'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Focus follows Mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Focus when Click'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.

			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show ST-80 assignments'.
				#selector 			-> 		#enableShowAssignmentAsLeftArrow.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Show ANSI assignments'.
				#selector 			-> 		#disableShowAssignmentAsLeftArrow.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		60.
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		70.
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		80.
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.




			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'Save Prefs in UserPrefs.txt'.
				#selector 			-> 		#enableUsePrefsFile.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Preferences are saved without need to save the Smalltalk Image.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'Save Prefs in the Image'.
				#selector 			-> 		#disableUsePrefsFile.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Preferences are lost unless the Smalltalk Image is saved.'
			} asDictionary.





			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		40.
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`.! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 12/30/2023 16:51:05'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions`.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #useUserPrefsFile category: #system value: true.!

