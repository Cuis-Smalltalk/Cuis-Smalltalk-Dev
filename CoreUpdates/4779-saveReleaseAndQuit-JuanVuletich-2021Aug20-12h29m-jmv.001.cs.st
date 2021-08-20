'From Cuis 5.0 [latest update: #4778] on 20 August 2021 at 12:33:23 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/20/2021 12:23:03'!
saveAsNewReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveAsNewReleaseAndQuit
	"
	| fileName newName changesName systemVersion |
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAndQuitAs: newName clearAllClassState: true! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 8/20/2021 12:32:32'!
worldMenuOptions

	^`{{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		10.
		#label 				-> 		'Open...'.
		#selector 				-> 		#openOpenMenu.
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
		#label 				-> 		'Preferences...'.
		#selector 				-> 		#openPreferencesMenu.
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
		#balloonText 				-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		10.
		#label 				-> 		'Save Image'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveImage.
		#icon 				-> 		#saveIcon.
		#balloonText 				-> 		'Save the current version of the image on disk. Overwrite existing file.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		20.
		#label 				-> 		'Save as...'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAs.
		#icon 				-> 		#saveAsIcon.
		#balloonText 				-> 		'Save the current version of the image on disk under a new name.'.
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
		#label 				-> 		'Save and Quit'.
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

!methodRemoval: SystemDictionary #saveAsNewVersion stamp: 'jmv 8/20/2021 12:29:36'!
SystemDictionary removeSelector: #saveAsNewVersion!
