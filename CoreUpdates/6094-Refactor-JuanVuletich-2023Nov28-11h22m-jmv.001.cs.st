'From Cuis 6.0 [latest update: #6092] on 28 November 2023 at 11:28:47 am'!

!SystemVersion methodsFor: 'services' stamp: 'jmv 11/28/2023 11:27:35'!
updatesPrefixesDo: aBlock
	aBlock value: ''.! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/27/2023 11:35:36'!
newUpdates: updatesFileDirectory do: aBlock
	| updates |
	SystemVersion current updatesPrefixesDo: [ :prefix |
		updates := SortedCollection sortBlock: [ :a :b | a name < b name ].
		updatesFileDirectory filesDo: [ :fileEntry |
			(fileEntry name beginsWith: prefix) ifTrue: [
				(fileEntry name integerOrNilAt: prefix size + 1) ifNotNil: [ :updateNumber |
					updateNumber > SystemVersion current highestUpdate ifTrue: [
						updates add: fileEntry ]]]].
		updates do: aBlock ].! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/28/2023 11:28:09'!
installNewUpdates: updatesFileDirectory

	| warnUser |
	self newUpdates: updatesFileDirectory do: [ :each |
		self install: each ].	

	warnUser := false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | codePackageFile |
			codePackageFile := CodePackageFile onFileEntry: fullFileName asFileEntry.
			(codePackageFile provides isLaterThan: package provides) ifTrue: [
				warnUser := true.
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

!methodRemoval: ChangeSet class #newUpdates: stamp: 'jmv 11/28/2023 11:28:17'!
ChangeSet class removeSelector: #newUpdates:!
!methodRemoval: ChangeSet class #isNewUpdate: stamp: 'jmv 11/28/2023 11:28:20'!
ChangeSet class removeSelector: #isNewUpdate:!
