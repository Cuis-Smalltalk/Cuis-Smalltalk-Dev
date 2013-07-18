'From Cuis 4.1 of 12 December 2012 [latest update: #1744] on 7 July 2013 at 10:57:34.514314 pm'!

!Feature commentStamp: '<historical>' prior: 0!
A named Feature is something which identifies the primary role of a Package.

  name -- a symbol naming the feature
  version -- an integer which increments at each incompatible change in package interface
  revision -- an integer which increments with each backwards compatible package change
  path -- nil or the (cached)  pathName of a file containing the package with this feature



Try any of:
  (Feature name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (Feature name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (Feature name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"

This should load packages #Compression, #'Graphic-Files-Additional', and #Sound!


!Feature methodsFor: 'printing' stamp: 'jmv 7/7/2013 22:33'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: name;
		nextPut: $ .
	version
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ version printOn: aStream ].
	aStream nextPut: $..
	revision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ revision printOn: aStream ].
	aStream
		nextPut: $)! !


!FeatureSpec methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:37'!
provides: aFeature

	provides _ aFeature! !

!FeatureSpec methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:37'!
requires: aFeature

	requires add: aFeature! !

!FeatureSpec methodsFor: 'initialize-release' stamp: 'jmv 7/7/2013 22:36'!
initialize
	requires _ Set new! !

!FeatureSpec methodsFor: 'printing' stamp: 'jmv 7/7/2013 22:55'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	provides printOn: aStream.
	aStream nextPutAll: ' requires: '.
	requires printOn: aStream.
	aStream nextPut: $)! !


!Feature methodsFor: 'accessing' stamp: 'jmv 7/7/2013 17:35'!
codePackage
	"Answer a CodePackage based on my name or nil"

	| baseName |
	baseName := name asString.
	
	^CodePackage named: baseName createIfAbsent: false registerIfNew: false! !

!Feature methodsFor: 'accessing' stamp: 'jmv 7/6/2013 17:42'!
packageName
	"Answer a package name based on feature name"

	^ (name asString , '.pck.st' )! !

!Feature methodsFor: 'accessing' stamp: 'jmv 7/6/2013 17:44'!
requirements

	"Answer my requirements"
	
	| featureSpec |
	
	"@@@FIXME: The real implementation would look in the package file on disk"
	
	 featureSpec := Feature packageFeaturesTestMap at: name ifAbsent: [ ^nil ].

	^  featureSpec requires ! !

!Feature methodsFor: 'requires' stamp: 'jmv 7/7/2013 17:40'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new.
	
	packagesToLoad do: [ :provideLoadSpec |
		CodePackageFile installPackageStream: ( FileStream readOnlyFileNamed: (provideLoadSpec pathName) ).
		Feature addFeature: provideLoadSpec
	]! !

!Feature methodsFor: 'requires' stamp: 'jmv 7/7/2013 17:43'!
requireUnlessIn: toLoad
	"See if all the requirements can be met and answer the load order"
	
	| providesLoadSpec otherRequirements |

	( (  (self satisfiedInCollection: Feature featuresProvided) 
		or: [ self satisfiedInCollection: toLoad ] )
			or: [self isAlreadyLoaded] )  "@@FIXME: skips version.revision"
		ifFalse: [ providesLoadSpec := self findProviderLoadSpec.
				(providesLoadSpec notNil)
				ifFalse: [ "@@FIXME: Signal exception for UI to handle"
					self error: 'Could not find package supplying feature: ' , self printString
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

!Feature methodsFor: 'testing' stamp: 'jmv 7/7/2013 17:35'!
isAlreadyLoaded
	"Answer true if requirement feature name exists in installedPackages"
	
	"@@FIXME: Add version, revision check"
	^self codePackage notNil! !

!Feature methodsFor: 'testing' stamp: 'jmv 7/7/2013 17:26'!
satisfies: requirement
	"Does this provided Feature satisfy the required Feature?"
	^ (requirement isKindOf: Feature) and: [
		name = requirement name and: [
			requirement version isNil or: [
				version = requirement version and: [
					requirement revision isNil or: [ revision >= requirement revision ]]]]]! !


!Feature class methodsFor: 'class initialization' stamp: 'jmv 7/7/2013 18:05'!
initialize

	"
	self initialize.
	"
	
	| cuisVersionAndRevision |
	FeaturesProvided := OrderedCollection new.
	PlacesToLookForPackages := OrderedCollection new.
	
	cuisVersionAndRevision := SystemVersion current versionRevision.
	self addFeature: (Feature
						name: #'Cuis-Base' 
						version: cuisVersionAndRevision first
						revision: cuisVersionAndRevision second).
						
	self addDirectoryToLookForPackages: (FileDirectory default).
			
	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: 'RecentlyExtractedPackages' ).
		
	self initializeTemporaryTestScaffold.
	! !

!Feature class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 22:27'!
name: aSymbol version: integerVersionOrNil revision: integerRevisionOrNil

	| newInst |
	newInst := self new.
	newInst name: aSymbol asSymbol version: integerVersionOrNil revision: integerRevisionOrNil.
	
	^ newInst! !


!FeatureSpec methodsFor: 'accessing' stamp: 'jmv 7/6/2013 17:45'!
name
	"Answer the name of the Feature provided"

	^provides name! !


!FeatureSpec class methodsFor: 'instance creation' stamp: 'jmv 7/6/2013 17:45'!
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
		
	newInst := self new.
	newInst provides: providedFeature requires: requiredFeaturesSet.
	
	^ newInst
	
! !


!FeatureSpec reorganize!
('accessing' name provides provides: requires requires:)
('initialize-release' initialize provides:requires:)
('printing' printOn:)
!

Feature initialize!
