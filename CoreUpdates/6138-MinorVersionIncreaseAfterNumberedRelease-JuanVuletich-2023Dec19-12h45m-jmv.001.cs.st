'From Cuis6.0 [latest update: #6137] on 19 December 2023 at 12:45:26 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/19/2023 12:42:50'!
saveRollingReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveRollingReleaseAndQuit
	"

	"If we are in a Numbered Release, increment minor version.
	For instance, after releasing Cuis 6.1, next rolling release image should be Cuis 6.2.
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

