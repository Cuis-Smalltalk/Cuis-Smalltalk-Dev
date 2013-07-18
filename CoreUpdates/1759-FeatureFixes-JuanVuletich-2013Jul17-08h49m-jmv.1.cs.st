'From Cuis 4.1 of 12 December 2012 [latest update: #1758] on 17 July 2013 at 8:57:15.220508 am'!

!Feature methodsFor: 'requires' stamp: 'jmv 7/17/2013 08:56'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageNameString |
	pathName ifNotNil: [
		(FileDirectory default fileExists: pathName) ifTrue: [ ^ true ]].
	packageNameString _ self packageName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageNameString) ifTrue: [
			self pathName: (fileDirectory fullNameFor: packageNameString).
			^ true ]].
	^ false! !

!Feature methodsFor: 'requires' stamp: 'jmv 7/17/2013 01:45'!
requireUnlessIn: toLoad asReqOf: whoRequiresMe main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	(self isAlreadySatisfied
			or: [ self satisfiedInCollection: toLoad ] )
		ifFalse: [
			(self findPackageFileAsReqOf: mainFeatureOrNil)
				ifTrue: [
					whoRequiresMe
						ifNil: [ toLoad addFirst: self ]
						ifNotNil: [ toLoad add: self before: whoRequiresMe ].
					self requirements ifNotNil: [ :otherRequirements |
						otherRequirements do: [ :requires | 
							requires requireUnlessIn: toLoad asReqOf: self main: (mainFeatureOrNil ifNil: [self])]]]
				ifFalse: [ "@@FIXME: Signal exception for UI to handle"
					self error: 'Could not find package supplying feature: ' , self printString ]
		].

	^ toLoad! !

!Feature methodsFor: 'private' stamp: 'jmv 7/17/2013 08:54'!
placesToLookForPackagesDo: aBlock

	| base pckDir myDir |
	base _ FileDirectory default.
	aBlock value: base.
	pckDir _ base directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	myDir _ FileDirectory on: (FileDirectory dirPathFor: pathName).
	(myDir ~= base and: [ myDir ~= pckDir ]) ifTrue: [
		aBlock value: myDir ]! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/17/2013 00:43'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(Feature name: pkName)
		pathName: fullName;
		require! !


!Feature methodsFor: 'requires' stamp: 'jmv 7/17/2013 01:47'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new asReqOf: nil main: nil.
	packagesToLoad do: [ :provideLoadSpec |
		CodePackageFile basicInstallPackageStream: ( FileStream readOnlyFileNamed: (provideLoadSpec pathName) ).
	]! !

!methodRemoval: Feature class #placesToLookForPackagesDo:!
Feature class removeSelector: #placesToLookForPackagesDo:!
!methodRemoval: Feature #findProviderLoadSpec!
Feature removeSelector: #findProviderLoadSpec!
!methodRemoval: Feature #requireUnlessIn:!
Feature removeSelector: #requireUnlessIn:!

!Feature reorganize!
('accessing' codePackage name packageName pathName pathName: requirements revision version)
('requires' findPackageFileAsReqOf: require requireUnlessIn:asReqOf:main:)
('testing' isAlreadySatisfied satisfiedInCollection: satisfies:)
('initialize-release' name:version:revision:)
('printing' printOn:)
('private' placesToLookForPackagesDo:)
!

