'From Cuis7.5 [latest update: #7775] on 19 December 2025 at 4:27:01 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 19/Dec/2025 16:25:26'!
saveStableRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveStableRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 6.
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

