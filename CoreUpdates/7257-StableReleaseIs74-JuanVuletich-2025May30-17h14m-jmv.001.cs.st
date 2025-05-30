'From Cuis7.3 [latest update: #7256] on 30 May 2025 at 5:14:30 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/30/2025 16:57:38'!
saveStableRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveStableRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 4.
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

