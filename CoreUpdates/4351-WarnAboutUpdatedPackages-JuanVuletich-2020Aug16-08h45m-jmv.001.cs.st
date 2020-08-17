'From Cuis 5.0 [latest update: #4349] on 16 August 2020 at 9:01:34 am'!

!Feature methodsFor: 'testing' stamp: 'jmv 8/16/2020 08:49:11'!
isLaterThan: aFeature
	^self version > aFeature version or: [
		self version = aFeature version and: [
			self revision > aFeature revision ]]! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 8/9/2020 19:02:18'!
packageFullNames

	^ packages collect: [ :each | each fullFileName ifNil: '---Never saved yet' ]! !


!CodePackage methodsFor: 'accessing' stamp: 'jmv 8/9/2020 19:02:15'!
fullFileName

	^fullFileName! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 8/16/2020 09:00:39'!
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
					ifTrue: [ 'Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.' print ]
					ifFalse: [ 'Package: ', pckName, '. There is a newer version than the currently loaded.' print ]]]].
	warnUser
		ifTrue: [
			self inform: SystemVersion current version, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk aboutThisSystem ]! !

