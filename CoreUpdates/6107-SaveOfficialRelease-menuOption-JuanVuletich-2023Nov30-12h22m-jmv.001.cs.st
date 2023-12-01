'From Cuis6.0 [latest update: #6104] on 30 November 2023 at 12:58:57 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/30/2023 12:32:10'!
filenameForUpdatedImage: aBoolean
	"If aBoolean, include highest update number	"
	| fileName newName changesName systemVersion |
	systemVersion := SystemVersion current.
	fileName := String streamContents: [ :strm |
		strm nextPutAll: systemVersion versionString.
		aBoolean ifTrue: [
			strm
				nextPut: $-;
				print: systemVersion highestUpdate ].
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName := fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName := DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName := self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.'.
		^nil  ].
	^newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/30/2023 12:35:20'!
saveNumberedRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveNumberedRelease
	"
	| versionMajor versionMinor |
	versionMajor := 6.
	versionMinor := 1.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	Smalltalk condenseSources: SystemVersion current versionString.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		SystemVersion current beNumberedRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].
"inutil, parece. Ver"
false ifTrue: [
	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		SystemVersion current beRollingRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ]
]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/28/2023 15:58:56'!
saveRollingReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndQuit
	"
	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndQuitAs: newName clearAllClassState: true ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/28/2023 15:59:04'!
saveRollingReleaseAndStay
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndStay
	"
	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ]! !


!SystemVersion methodsFor: 'initialization' stamp: 'jmv 11/29/2023 09:30:22'!
beNumberedRelease
	updatesPrefix := String streamContents: [ :strm |
		strm nextPutAll: '[r'; print: versionMajor; nextPut: $.; print: versionMinor; nextPutAll: ']-' ].
	releaseDate := Date today.! !

!SystemVersion methodsFor: 'initialization' stamp: 'jmv 11/28/2023 15:54:38'!
beRollingRelease
	"i.e. non released"
	updatesPrefix := nil.
	releaseDate := nil.! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 11/30/2023 12:54:09'!
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
			#itemGroup 				-> 		30.
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
		{
			#itemGroup 				-> 		40.
			#itemOrder 				-> 		10.
			#label 				-> 		'Save Image'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveImage.
			#icon 				-> 		#saveIcon.
			#balloonText 				-> 		'Save the current state of the image on disk. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 				-> 		40.
			#itemOrder 				-> 		20.
			#label 				-> 		'Save Image as...'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAs.
			#icon 				-> 		#saveAsIcon.
			#balloonText 				-> 		'Save the current state of the image on disk under a new name.'.
		} asDictionary.
		{
			#itemGroup 				-> 		40.
			#itemOrder 				-> 		30.
			#label 				-> 		'Save Image and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAndQuit.
			#icon 				-> 		#saveAndQuitIcon.
			#balloonText 				-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 				-> 		40.
			#itemOrder 				-> 		40.
			#label 				-> 		'Quit without saving'.
			#selector 				-> 		#quitSession.
			#icon 				-> 		#quitIcon.
			#balloonText 				-> 		('Quit out of Cuis without saving the image.\',
									'Ask for confirmation if there are unsaved changes.') withNewLines.
		} asDictionary.
	}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 11/30/2023 12:54:11'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions`.! !

!methodRemoval: SystemDictionary #saveAsNewReleaseAndQuit stamp: 'jmv 11/30/2023 12:25:06'!
SystemDictionary removeSelector: #saveAsNewReleaseAndQuit!
!methodRemoval: SystemDictionary #filenameForUpdatedImage stamp: 'jmv 11/30/2023 12:32:43'!
SystemDictionary removeSelector: #filenameForUpdatedImage!
!methodRemoval: SystemDictionary #saveAsRollingReleaseAndStay stamp: 'jmv 11/30/2023 12:26:16'!
SystemDictionary removeSelector: #saveAsRollingReleaseAndStay!
!methodRemoval: SystemDictionary #saveAsNewReleaseAndStay stamp: 'jmv 11/30/2023 12:25:07'!
SystemDictionary removeSelector: #saveAsNewReleaseAndStay!
!methodRemoval: SystemDictionary #saveAsRollingReleaseAndQuit stamp: 'jmv 11/30/2023 12:26:36'!
SystemDictionary removeSelector: #saveAsRollingReleaseAndQuit!
