'From Cuis 4.1 of 12 December 2012 [latest update: #1757] on 16 July 2013 at 6:14:09.53457 pm'!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName '
	classVariableNames: 'PlacesToLookForPackages '
	poolDictionaries: ''
	category: 'Package Support'!

!Feature class methodsFor: 'accessing' stamp: 'jmv 7/16/2013 18:10'!
placesToLookForPackagesDo: aBlock

	| base |
	base _ FileDirectory default.
	aBlock value: base.
	aBlock value: (base directoryNamed: 'Packages')! !


!Feature methodsFor: 'requires' stamp: 'jmv 7/16/2013 18:12'!
findProviderLoadSpec
	"Look in known places for packages providing required feature."

	| packageNameString |
	packageNameString _ self packageName.
	self class placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageNameString) ifTrue: [
			self pathName: (fileDirectory fullNameFor: packageNameString).
			^self ]].
	^nil! !

!methodRemoval: Feature class #addDirectoryToLookForPackages:!
Feature class removeSelector: #addDirectoryToLookForPackages:!
!methodRemoval: Feature class #initialize!
Feature class removeSelector: #initialize!
!methodRemoval: Feature class #placesToLookForPackages!
Feature class removeSelector: #placesToLookForPackages!
Feature initialize!

!Feature class reorganize!
('accessing' placesToLookForPackagesDo:)
('instance creation' baseSystemFeature name: name:version: name:version:revision:)
!

!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
