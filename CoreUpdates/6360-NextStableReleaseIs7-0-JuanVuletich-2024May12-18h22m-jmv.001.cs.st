'From Cuis6.3 [latest update: #6358] on 12 May 2024 at 6:35:52 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/12/2024 18:34:19'!
saveStableRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveNumberedRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 0.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	"While condensing sources, convert all variable assignment to ANSI Smalltalk form, i.e. :="
	previousValue := Preferences at: #fileOutANSIassignment.
	Preferences at: #fileOutANSIassignment put: true.
	self condenseSources: SystemVersion current versionString.
	Preferences at: #fileOutANSIassignment put: previousValue.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		SystemVersion current beStableRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].! !


!SystemVersion methodsFor: 'testing' stamp: 'jmv 5/12/2024 18:31:47'!
isStableRelease
	^releaseDate notNil! !

!SystemVersion methodsFor: 'initialization' stamp: 'jmv 5/12/2024 18:30:44'!
beStableRelease
	updatesPrefix := String streamContents: [ :strm |
		strm nextPutAll: '[r'; print: versionMajor; nextPut: $.; print: versionMinor; nextPutAll: ']-' ].
	releaseDate := Date today.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/12/2024 18:33:52'!
saveRollingReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndQuit
	"

	"If we are in a Stable Release, increment minor version.
	For instance, after releasing Cuis 6.2, next rolling release image should be Cuis 6.3.
	Note: Version number for next Stable Release is hardcoded at #saveStableRelease"
	| sv |
	sv := SystemVersion current.
	sv isStableRelease ifTrue: [
		SystemVersion
			newVersionMajor: sv versionMajor
			versionMinor: sv versionMinor + 1 ].

	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndQuitAs: newName clearAllClassState: true ]! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/12/2024 18:28:35'!
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

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/12/2024 18:28:06'!
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
					#label 				-> 		'Save Stable Release'.
					#object 				-> 		Smalltalk.
					#selector 				-> 		#saveStableRelease.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 				-> 		('Save as a new official Stable Release of Cuis.\',
											'See naming in #saveStableRelease.\',
											'Clear all user preferences and class vars.\',
											'Write new compacted Sources file, an empty\',
											'Changes file, and save the Image on disk.\') withNewLines.
				} asDictionary.
				{
					#itemGroup 				-> 		40.
					#itemOrder 				-> 		20.
					#label 				-> 		'Save Rolling Release, Quit'.
					#object 				-> 		Smalltalk.
					#selector 				-> 		#saveRollingReleaseAndQuit.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 				-> 		('Save as update of the Rolling Release of Cuis.\',
											'Clear all user preferences and class vars.\',
											'Save Image and Changes with update number.\',
											'Quit Cuis.\') withNewLines.
				} asDictionary.
			}`! !

!methodRemoval: SystemVersion #beNumberedRelease stamp: 'jmv 5/12/2024 18:31:01'!
SystemVersion removeSelector: #beNumberedRelease!
!methodRemoval: SystemVersion #isRelease stamp: 'jmv 5/12/2024 18:31:57'!
SystemVersion removeSelector: #isRelease!
!methodRemoval: SystemDictionary #saveNumberedRelease stamp: 'jmv 5/12/2024 18:26:13'!
SystemDictionary removeSelector: #saveNumberedRelease!
