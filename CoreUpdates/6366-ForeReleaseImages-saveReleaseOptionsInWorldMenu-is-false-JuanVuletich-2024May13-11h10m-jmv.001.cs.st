'From Cuis6.3 [latest update: #6365] on 13 May 2024 at 11:10:41 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/13/2024 11:10:32'!
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
		Preferences at: #saveReleaseOptionsInWorldMenu put: false.
		SystemVersion current beRollingRelease.
		self setDefaultGCParameters.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndQuitAs: newName clearAllClassState: true ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/13/2024 11:10:16'!
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
		Preferences at: #saveReleaseOptionsInWorldMenu put: false.
		SystemVersion current beStableRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].! !

