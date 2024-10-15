'From Cuis7.1 [latest update: #6773] on 15 October 2024 at 9:42:34 am'!

!CodePackageFile class methodsFor: 'file contents' stamp: 'jmv 10/15/2024 09:39:41'!
providedFeatureInFile: aFileEntry
	| item |
	aFileEntry readStreamDo: [ :stream |
		[
			stream skipSeparators.
			stream atEnd ] whileFalse: [
				item := stream nextChunk.
				(item beginsWith:  'provides:') ifTrue: [
					^Feature fromTokens: (Scanner new scanTokens: item) ]]].
	^ nil! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/15/2024 09:41:34'!
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
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than the currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: Smalltalk versionString, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ].! !


!CodePackageFile class reorganize!
('instance creation' onFileEntry:)
('services' installPackage: packageNameFrom:)
('file list services' fileReaderServicesForFileEntry:suffix: serviceInstallPackage)
('file contents' providedFeatureInFile:)
!

