'From Cuis7.1 [latest update: #6795] on 28 October 2024 at 9:48:29 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'RMV 10/28/2024 21:48:08'!
installNewUpdates: updatesFileDirectory

	| warnUser |
	self newUpdates: updatesFileDirectory do: [ :each |
		self install: each ].	
	warnUser := false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | providedFeatureInFile |
			providedFeatureInFile := CodePackageFile providedFeatureInFile: fullFileName asFullFileEntry.
			(providedFeatureInFile isLaterThan: package provides) ifTrue: [
				warnUser := true.
				package hasUnsavedChanges
					ifTrue: [ ('Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.') print ]
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: Smalltalk versionString, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ].! !

