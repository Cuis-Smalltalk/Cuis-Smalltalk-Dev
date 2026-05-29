'From Cuis7.7 [latest update: #7976] on 29 May 2026 at 2:54:17 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 29/May/2026 14:53:33'!
saveStableRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveStableRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 7.
	versionMinor := 8.
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

