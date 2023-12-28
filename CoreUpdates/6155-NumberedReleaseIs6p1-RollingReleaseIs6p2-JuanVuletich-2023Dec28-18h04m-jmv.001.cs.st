'From Cuis6.0 [latest update: #6154] on 28 December 2023 at 6:05:23 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/28/2023 18:04:28'!
saveNumberedRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveNumberedRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 6.
	versionMinor := 2.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	"While condensing sources, convert all variable assignment to ANSI Smalltalk form, i.e. :="
	previousValue := Preferences at: #fileOutANSIassignment.
	Preferences at: #fileOutANSIassignment put: true.
	Smalltalk condenseSources: SystemVersion current versionString.
	Preferences at: #fileOutANSIassignment put: previousValue.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		SystemVersion current beNumberedRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/28/2023 18:04:08'!
saveRollingReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndQuit
	"

	"If we are in a Numbered Release, increment minor version.
	For instance, after releasing Cuis 6.2, next rolling release image should be Cuis 6.3.
	Note: Version number for Numbered Release is hardcoded at #saveNumberedRelease"
	| sv |
	sv := SystemVersion current.
	sv isRelease ifTrue: [
		SystemVersion
			newVersionMajor: sv versionMajor
			versionMinor: sv versionMinor + 1 ].

	(self filenameForUpdatedImage: true) ifNotNil: [ :newName |
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndQuitAs: newName clearAllClassState: true ]! !

