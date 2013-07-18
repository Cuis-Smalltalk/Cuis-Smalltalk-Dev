'From Cuis 4.1 of 12 December 2012 [latest update: #1751] on 14 July 2013 at 6:40:22.606979 pm'!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName '
	classVariableNames: 'FeaturesProvided PackageFeaturesTestMap PlacesToLookForPackages '
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 7/14/2013 18:16'!
requires

	^featureSpec requires! !


!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 7/14/2013 18:09'!
buildFileStream: aStream

	| fullFileName pkName |
	fullFileName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullFileName.
	^self buildFileStream: aStream packageName: pkName fullName: fullFileName! !


!CodePackage methodsFor: 'accessing' stamp: 'jmv 7/14/2013 17:59'!
provides

	^featureSpec provides! !


!Feature methodsFor: 'testing' stamp: 'jmv 7/14/2013 17:59'!
isAlreadySatisfied
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ]! !


!Feature class methodsFor: 'instance creation' stamp: 'jmv 7/14/2013 17:56'!
baseSystemFeature
	| cuisVersionAndRevision |
	cuisVersionAndRevision _ SystemVersion current versionRevision.
	^Feature
		name: #'Cuis-Base' 
		version: cuisVersionAndRevision first
		revision: cuisVersionAndRevision second! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 7/14/2013 18:09'!
browsePackageContents: aStream
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	aStream readOnly.
	changeList _ self new scanFile: aStream from: 0 to: aStream size.
	aStream reset.
	packageFile _ CodePackageFile buildFileStream: aStream.
	aStream close.
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream localName! !


!Feature methodsFor: 'accessing' stamp: 'jmv 7/14/2013 18:17'!
requirements
	"Answer my requirements"
	
	| packageFile |
	packageFile _ CodePackageFile buildFileStream: (FileStream readOnlyFileNamed: pathName).
	^ packageFile requires! !

!Feature methodsFor: 'requires' stamp: 'jmv 7/14/2013 17:59'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new.
	
	packagesToLoad do: [ :provideLoadSpec |
		CodePackageFile installPackageStream: ( FileStream readOnlyFileNamed: (provideLoadSpec pathName) ).
	]! !

!Feature methodsFor: 'requires' stamp: 'jmv 7/14/2013 18:04'!
requireUnlessIn: toLoad
	"See if all the requirements can be met and answer the load order"
	
	| providesLoadSpec otherRequirements |

	(self isAlreadySatisfied
			or: [ self satisfiedInCollection: toLoad ] )
		ifFalse: [
			providesLoadSpec := self findProviderLoadSpec.
			providesLoadSpec isNil
				ifTrue: [ "@@FIXME: Signal exception for UI to handle"
					self error: 'Could not find package supplying feature: ' , self printString ]
				ifFalse: [
				 	toLoad addFirst: providesLoadSpec.
					otherRequirements := providesLoadSpec requirements.
					(otherRequirements isNil)
						ifFalse: [
							otherRequirements do: [ :requires | requires requireUnlessIn: toLoad ]]]
		].

	^ toLoad! !


!Feature class methodsFor: 'class initialization' stamp: 'jmv 7/14/2013 18:39'!
initialize

	"
	Feature initialize
	"

	PlacesToLookForPackages := OrderedCollection new.
						
	self addDirectoryToLookForPackages: FileDirectory default.
			
	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: 'RecentlyExtractedPackages' ).
			
	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: 'Packages' )! !

!methodRemoval: Feature class #addFeature:!
Feature class removeSelector: #addFeature:!
!methodRemoval: Feature class #featuresProvided!
Feature class removeSelector: #featuresProvided!
!methodRemoval: Feature class #initializeTemporaryTestScaffold!
Feature class removeSelector: #initializeTemporaryTestScaffold!
!methodRemoval: Feature class #packageFeaturesTestMap!
Feature class removeSelector: #packageFeaturesTestMap!
Feature initialize!
!methodRemoval: Feature #isAlreadyLoaded!
Feature removeSelector: #isAlreadyLoaded!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName'
	classVariableNames: 'PlacesToLookForPackages'
	poolDictionaries: ''
	category: 'Package Support'!
