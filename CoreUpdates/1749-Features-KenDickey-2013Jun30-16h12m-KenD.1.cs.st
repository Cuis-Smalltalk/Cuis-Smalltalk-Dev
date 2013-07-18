'From Cuis 4.1 of 12 December 2012 [latest update: #1739] on 30 June 2013 at 4:12:05.177126 pm'!
'Description A Package may specify a Feature it supplies and a Set of Features it requires.

Loading such a package should autoload all required packages if not already loaded.
'!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName'
	classVariableNames: 'FeaturesProvided PackageFeaturesTestMap PlacesToLookForPackages'
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: 'Feature class' category: #'Package Support'!
Feature class
	instanceVariableNames: ''!

!classDefinition: #FeatureSpec category: #'Package Support'!
Object subclass: #FeatureSpec
	instanceVariableNames: 'provides requires'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: 'FeatureSpec class' category: #'Package Support'!
FeatureSpec class
	instanceVariableNames: ''!


!Feature commentStamp: '<historical>' prior: 0!
A named Feature is something which identifies the primary role of a Package.

  name -- a symbol naming the feature
  version -- an integer which increments at each inconistent change in package interface
  revision -- an integer which increments with each package change
  path -- nil or the (cached)  pathName of a file containing the package with this feature.



TRY:
  (Feature name: #'Sound' version: 1 revision: 0) require. 

This should load packages #Compression, #'Graphic-Files-Additional', and #Sound!

!FeatureSpec commentStamp: '<historical>' prior: 0!
This class just packages two pieces of information useful for packages:

	provides  -- the Feature which our containing package supplies
	requires -- nil or a Set of Features required by our containing package!

!Feature methodsFor: 'accessing' stamp: 'KenD 6/27/2013 08:12'!
codePackage
	"Answer a CodePackage based on my name or nil"

	| baseName |
	baseName := self name asString.
	
	^ CodePackage installedPackages detect: [ :cp | cp name = baseName ] ifNone: [ nil ]! !

!Feature methodsFor: 'requires' stamp: 'KenD 6/30/2013 14:13'!
findProviderLoadSpec
	"Look in known places for packages providing required feature."

	| placesToLook packageNameString foundDir |
	placesToLook := Feature placesToLookForPackages. "A collection of FileDirectory"
	packageNameString := self packageName.
	
	foundDir := placesToLook detect: [ :fileDir | fileDir fileExists: packageNameString] ifNone: [ ^ nil ].
	
	self pathName: (foundDir fullNameFor: packageNameString).
	
	^ self
! !

!Feature methodsFor: 'testing' stamp: 'KenD 6/30/2013 16:11'!
isAlreadyLoaded
	"Answer true if requirement feature name exists in installedPackages"

	| featureString |
	featureString := self name asString.
	
	"@@FIXME: Add version, revision check"
	^ CodePackage installedPackages anySatisfy: [ :codePack | codePack name = featureString ]! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/26/2013 16:57'!
name
	
	^ name! !

!Feature methodsFor: 'initialize-release' stamp: 'KenD 6/26/2013 16:54'!
name: theName version: theVersion revision: theRevision

	name     := theName.
	version  := theVersion.
	revision := theRevision! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/27/2013 15:24'!
packageName
	"Answer a package name based on feature name"

	^ ( self name asString , '.pck.st' )! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/30/2013 14:06'!
pathName
	
	^ pathName! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/30/2013 14:07'!
pathName: aPathNameString
	
	pathName := aPathNameString! !

!Feature methodsFor: 'requires' stamp: 'KenD 6/30/2013 14:13'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new.
	
	packagesToLoad do: [ :provideLoadSpec |
		CodePackageFile installPackageStream: ( FileStream oldFileNamed: (provideLoadSpec pathName) ).
		Feature addFeature: provideLoadSpec
	]! !

!Feature methodsFor: 'requires' stamp: 'KenD 6/30/2013 14:14'!
requireUnlessIn: toLoad
	"See if all the requirements can be met and answer the load order"
	
	| providesLoadSpec otherRequirements |

	( (  (self satisfiedInCollection: Feature featuresProvided) 
		or: [ self satisfiedInCollection: toLoad ] )
			or: [self isAlreadyLoaded] )  "@@FIXME: skips version.revision"
		ifFalse: [ providesLoadSpec := self findProviderLoadSpec.
				(providesLoadSpec notNil)
				ifFalse: [ "@@FIXME: Signal exception for UI to handle"
					self error: 'Could not find package supplying feature: ' , self name asString 
				]
				ifTrue: [
				 	toLoad addFirst: providesLoadSpec.
					otherRequirements := providesLoadSpec requirements.
					(otherRequirements isNil)
						ifFalse: [
							otherRequirements do: [ :requires | requires requireUnlessIn: toLoad ]
						]
				]
		].

	^ toLoad! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/30/2013 15:24'!
requirements

	"Answer my requirements"
	
	| featureSpec |
	
	"@@@FIXME: The real implementation would look in the package file on disk"
	
	 featureSpec := Feature packageFeaturesTestMap at: self name ifAbsent: [ ^nil ].

	^  featureSpec requires ! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/26/2013 16:57'!
revision

	^ revision! !

!Feature methodsFor: 'testing' stamp: 'KenD 6/30/2013 14:11'!
satisfiedInCollection: aCollection
	"Answer true if requirement is satisfied in aCollection of ProvidesFeatureSpec"

	^ aCollection anySatisfy: [ :provides | provides satisfies: self ]! !

!Feature methodsFor: 'testing' stamp: 'KenD 6/30/2013 14:41'!
satisfies: requirement

	"Does this provided Feature satisfy the required Feature?"

	^(requirement isKindOf: Feature)
		and: [ (self name = requirement name)
			and: [ (self version <= requirement version)
				and: [ self revision >= requirement revision ]
			]
		]! !

!Feature methodsFor: 'accessing' stamp: 'KenD 6/26/2013 16:57'!
version

	^ version
! !

!Feature class methodsFor: 'accessing' stamp: 'KenD 6/26/2013 17:22'!
addDirectoryToLookForPackages: aDirectory

	PlacesToLookForPackages 
		ifNil: [ PlacesToLookForPackages := OrderedCollection new ].
		
	(aDirectory isKindOf: FileDirectory) 
		ifFalse: [ self error: 'aDirectory must be a FileDirectory instance'].
		
	PlacesToLookForPackages add: aDirectory! !

!Feature class methodsFor: 'accessing' stamp: 'KenD 6/30/2013 14:25'!
addFeature: providedFeature

	(providedFeature isKindOf: Feature) 
		ifFalse: [ self error: 'The provided Feature must be a Feature instance']. 
		
	FeaturesProvided add: providedFeature! !

!Feature class methodsFor: 'accessing' stamp: 'KenD 6/26/2013 17:31'!
featuresProvided

	^ FeaturesProvided! !

!Feature class methodsFor: 'class initialization' stamp: 'KenD 6/30/2013 14:33'!
initialize

	"
	self initialize.
	"
	
	FeaturesProvided := OrderedCollection new.
	PlacesToLookForPackages := OrderedCollection new.
	
	self addFeature: (Feature
						name: #'Cuis-Base' 
						version: 41 	"Nota Bene:  SystemVersion current version"
						revision: (Smalltalk lastUpdateString asInteger)).
						
	self addDirectoryToLookForPackages: (FileDirectory default).
			
	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: 'RecentlyExtractedPackages' ).
		
	self initializeTemporaryTestScaffold.
	! !

!Feature class methodsFor: 'class initialization' stamp: 'KenD 6/30/2013 14:34'!
initializeTemporaryTestScaffold

	"Test hack to try things out before any (deep) changes.	"
	
	| packageMap |
	
	packageMap := self packageFeaturesTestMap.
	
	packageMap 
		at: #'Compression' 
		put:  (FeatureSpec 
				provides: (Feature name: #Compression version: 1 revision: 0)
				requires: nil
			 ).
	packageMap at: #'Graphics-Files-Additional'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Graphics-Files-Additional' version: 1 revision: 0)
						requires: ( { (Feature name: #Compression  version: 1 revision: 0).
								  	} asSet )
					).

	packageMap at: #'Sound'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Sound' version: 1 revision: 0)
						requires: ({ (Feature name: #'Graphics-Files-Additional'  version: 1 revision: 0).
								  	} asSet )
					).

	packageMap at: #'Theme-Themes'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Theme-Themes' version: 1 revision: 0)
						requires: ( { (Feature name: #Sound  version: 1 revision: 0).
								    } asSet )
					).

	packageMap at: #'Network-Kernel'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Network-Kernel' version: 1 revision: 0)
						requires: ( { (Feature name: #'Theme-Themes' version: 1 revision: 0).
								    } asSet )
					).

	packageMap at: #'LinearAlgebra'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'LinearAlgebra' version: 1 revision: 0)
						requires: ( { (Feature name: #'Network-Kernel'  version: 1 revision: 0).
								    } asSet )
					).

	packageMap at: #'Tests'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Tests' version: 1 revision: 0)
						requires: ( { (Feature name: #'LinearAlgebra' version: 1 revision: 0).
								    } asSet )
					).
					
	packageMap at: #'Morphic-Games-Solitaire'
				 put:  (FeatureSpec 
						provides:  (Feature name: #'Morphic-Games-Solitaire' version: 3 revision: 6)
						requires: ( { (Feature name: #'Graphics-Files-Additional' version: 1 revision: 0).
								    } asSet )
					).

	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: '../Cuis-Solitaire' ).
! !

!Feature class methodsFor: 'instance creation' stamp: 'KenD 6/26/2013 20:11'!
name: aSymbol version: integerVersion revision: integerRevision

	| newInst |
	newInst := super new initialize.
	newInst name: aSymbol asSymbol version: integerVersion asInteger revision: integerRevision asInteger.
	
	^ newInst! !

!Feature class methodsFor: 'accessing' stamp: 'KenD 6/30/2013 15:23'!
packageFeaturesTestMap

	"PackageFeaturesTestMap is a map from CodePackage to FeatureSpec.
	This is used to test Features before any changes to CodePackages."

	PackageFeaturesTestMap ifNil: [ PackageFeaturesTestMap := Dictionary new ].
	
	^ PackageFeaturesTestMap ! !

!Feature class methodsFor: 'accessing' stamp: 'KenD 6/26/2013 20:32'!
placesToLookForPackages

	^ PlacesToLookForPackages ! !

!FeatureSpec methodsFor: 'accessing' stamp: 'KenD 6/30/2013 15:19'!
name
	"Answer the name of the Feature provided"

	^ self provides name! !

!FeatureSpec methodsFor: 'accessing' stamp: 'KenD 6/26/2013 19:34'!
provides

	^ provides! !

!FeatureSpec methodsFor: 'initialize-release' stamp: 'KenD 6/26/2013 19:20'!
provides: providesFeatureSpec requires: setOfRequiresFeatureSpec

	provides := providesFeatureSpec.
	requires := setOfRequiresFeatureSpec.! !

!FeatureSpec methodsFor: 'accessing' stamp: 'KenD 6/26/2013 19:34'!
requires

	^ requires! !

!FeatureSpec class methodsFor: 'instance creation' stamp: 'KenD 6/30/2013 14:23'!
provides: providedFeature requires: requiredFeaturesSet

	| newInst |
	
	(providedFeature isKindOf: Feature) 
		ifFalse: [ self error: 'The provided Feature must be a Feature instance']. 
		
	(requiredFeaturesSet isNil)
		ifFalse: [ 
			(requiredFeaturesSet isKindOf: Set) ifFalse: [self error: 'requiredFeaturesSet must be a Set'].
			(requiredFeaturesSet allSatisfy: [ :rf | rf isKindOf: Feature ])
				ifFalse: [
					self error: 'requiredFeaturesSet must be a set of  Feature instances' 
				].
		].
		
	newInst := self new initialize.
	newInst provides: providedFeature requires: requiredFeaturesSet.
	
	^ newInst
	
! !
Feature initialize!
