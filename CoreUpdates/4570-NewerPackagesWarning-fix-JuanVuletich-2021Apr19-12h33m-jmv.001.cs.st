'From Cuis 5.0 [latest update: #4569] on 19 April 2021 at 12:34:32 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'jmv 4/19/2021 12:33:55'!
installNewUpdates: updatesFileDirectory

	| warnUser |
	(self newUpdates: updatesFileDirectory) do: [ :each |
		self install: each ].	

	warnUser _ false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | codePackageFile |
			codePackageFile _ CodePackageFile onFileEntry: fullFileName asFileEntry.
			(codePackageFile provides isLaterThan: package provides) ifTrue: [
				warnUser _ true.
				package hasUnsavedChanges
					ifTrue: [ ('Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.') print ]
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than the currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: SystemVersion current version, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ]! !

