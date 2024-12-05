'From Cuis7.1 [latest update: #6891] on 5 December 2024 at 3:57:09 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/5/2024 15:55:47'!
saveStableRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveStableRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 2.
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

