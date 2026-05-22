"
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion
  path -- nil or the (cached)  pathName of a file containing the package with this feature

Usually use the convenience methods:
	Feature require: 'Sound'
	Feature require: 'Tests' version: 1.0

Try any of:
  (FeatureRequirement name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) require 		"Specific version.revision, or any greater version, up to max"

This should load packages #Compression and #Sound


  (FeatureRequirement name: #'Sound') print
  (FeatureRequirement name: #'Sound' version: 1) print
  (FeatureRequirement name: #'Sound' version: 1 revision: 12) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: nil) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: nil maxVersion: 3) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) print
"
Class {
	#name : 'FeatureRequirement',
	#superclass : 'Object',
	#instVars : [
		'name',
		'minVersion',
		'minRevision',
		'maxVersion',
		'pathName',
		'codePackageFile'
	],
	#category : 'System-Package Support',
	#package : 'System',
	#tag : 'Package Support'
}

{ #category : 'instance creation' }
FeatureRequirement class >> fromTokens: tokens [
	^self 
		name: tokens second 
		minVersion: tokens third 
		minRevision: tokens fourth 
		maxVersion: (tokens size > 4 ifTrue: [tokens fifth]).
]

{ #category : 'instance creation' }
FeatureRequirement class >> name: aSymbol [
	"Ask for any version, and any revision"
	^self name: aSymbol minVersion: nil minRevision: nil maxVersion: nil
]

{ #category : 'instance creation' }
FeatureRequirement class >> name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil [

	| newInst |
	(minVersionOrNil isNil or: [ minVersionOrNil isInteger ]) ifFalse: [
		self error: 'Version numbers must be Integer numbers. Specify also Revision number if needed.' ].
	(minRevisionOrNil isNil or: [ minRevisionOrNil isInteger ]) ifFalse: [
		self error: 'Revision numbers must be Integer numbers.' ].
	(maxVersionOrNil isNil or: [ maxVersionOrNil isInteger ]) ifFalse: [
		self error: 'Version numbers must be Integer numbers. Specify also Revision number if needed.' ].
	newInst := self new.
	newInst name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil.
	
	^ newInst
]

{ #category : 'instance creation' }
FeatureRequirement class >> name: aSymbol version: integerVersion [
	"Ask for a specific version, but any revision"
	^self name: aSymbol minVersion: integerVersion minRevision: nil maxVersion: integerVersion
]

{ #category : 'instance creation' }
FeatureRequirement class >> name: aSymbol version: integerVersion revision: integerRevision [
	"Ask for a specific version, and revision"
	^self name: aSymbol minVersion: integerVersion minRevision: integerRevision maxVersion: integerVersion
]

{ #category : 'comparing' }
FeatureRequirement >> = another [
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	^name = another name and: [
		minVersion = another minVersion and: [
		minRevision = another minRevision and: [
		maxVersion = another maxVersion ]]]
]

{ #category : 'private' }
FeatureRequirement >> addToLoad: toLoad withMyRequirements: requirements [

	"Add self to OrderedCollection 'toLoad' before any requirements I have"
	| myRequirements |
	myRequirements := self codePackageFile requires.
	toLoad do: [ :featureReq | 
		(myRequirements anySatisfy: [ :req | featureReq sameNameAs: req])
		ifTrue: [
			"Transcript show: ('adding ', self name asPlainString, ' before: ', featureReq name asPlainString); newLine."
			toLoad add: self before: featureReq.
			^ toLoad
		]
	].
	"Transcript show: ('adding ', self name asPlainString); newLine."
	toLoad addLast: self.
	^ toLoad
		
]

{ #category : 'testing' }
FeatureRequirement >> checkRequirement [
	"Answer if I am satisfied by package found at pathName"

	^ self codePackageFile provides satisfies: self.
]

{ #category : 'accessing' }
FeatureRequirement >> codePackageFile [
	codePackageFile isNil ifTrue: [
		codePackageFile := CodePackageFile onFileEntry: pathName asFullFileEntry.
		"?"
		pathName := nil ].
	^codePackageFile
]

{ #category : 'requires' }
FeatureRequirement >> findPackageFileAsReqOf: mainFeatureOrNil [
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName |
	pathName ifNotNil: [
		pathName asFullFileEntry exists ifTrue: [ ^ true ]].
	packageFileName := self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory | | entry |
		entry := directory // packageFileName.
		entry exists ifTrue: [
			"Try this one. If success, keep it."
			self pathName: entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			"Nope. Don't keep it."
			self pathName: nil ]].
	^ false
]

{ #category : 'comparing' }
FeatureRequirement >> hash [
	"Hash is reimplemented because = is implemented."

	^name hash
]

{ #category : 'requires' }
FeatureRequirement >> install [
	"Preconditions have been satisfied.  Install the required package."

	| existing |
	existing := CodePackage named: self codePackageFile packageName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		self codePackageFile install.
		codePackageFile := nil.
	]
]

{ #category : 'testing' }
FeatureRequirement >> isAlreadySatisfied [
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	"VectorGraphics is now part of the base Cuis image, and therefore already satisfied."
	name = 'VectorGraphics' ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ].
]

{ #category : 'accessing' }
FeatureRequirement >> maxVersion  [

	^maxVersion 
]

{ #category : 'accessing' }
FeatureRequirement >> minRevision [

	^minRevision 
]

{ #category : 'accessing' }
FeatureRequirement >> minVersion  [

	^minVersion 
]

{ #category : 'accessing' }
FeatureRequirement >> name [
	
	^ name
]

{ #category : 'initialization' }
FeatureRequirement >> name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer [

	name := featureName asSymbol.
	minVersion := minVer.
	minRevision := rev.
	maxVersion := maxVer
]

{ #category : 'accessing' }
FeatureRequirement >> packageFileName [
	"Answer a package name based on feature name"

	^ name asPlainString, '.pck.st'
]

{ #category : 'accessing' }
FeatureRequirement >> pathName: aPathNameString [
	
	pathName := aPathNameString.
	codePackageFile := nil
]

{ #category : 'private' }
FeatureRequirement >> placesToLookForPackagesDo: aBlock [

	| myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir := pathName asFullFileEntry parent.
		aBlock value: myDir ].

	"Look in codePackageFile folder"
	codePackageFile ifNotNil: [
		myDir := codePackageFile fullName asFullFileEntry parent.
		aBlock value: myDir ].

	"Packages that come included with Cuis"
	aBlock value: DirectoryEntry packagesDirectory.
	DirectoryEntry packagesDirectory allRegularDirectoriesDo: aBlock.

	"Packages created by user"
	aBlock value: DirectoryEntry newPackagesDirectory.

	"Packages in other folders or repos in the project directory"
	aBlock value: DirectoryEntry projectBaseDirectory.
	DirectoryEntry projectBaseDirectory allRegularDirectoriesDo: aBlock.
]

{ #category : 'printing' }
FeatureRequirement >> printDetailsOn: aStream [
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersion
		ifNil: [
			aStream nextPutAll: '*.*)'.
			^self ]
		ifNotNil: [ minVersion printOn: aStream ].
	aStream nextPut: $..
	minRevision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ minRevision printOn: aStream ].
	(minRevision notNil or: [ maxVersion isNil or: [maxVersion > minVersion ]]) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersion
			ifNil: [ aStream nextPut: $* ]
			ifNotNil: [ maxVersion printOn: aStream ].
		maxVersion = minVersion
			ifTrue: [ aStream nextPutAll: '.999' ]
			ifFalse: [ aStream nextPutAll: '.*' ]
		]
]

{ #category : 'printing' }
FeatureRequirement >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)
]

{ #category : 'requires' }
FeatureRequirement >> require [
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: nil
			requiringFeature: nil
			allRequiringFeatures: #().
	
	"Transcript show: 'packagesToLoad: '; newLine.
	packagesToLoad do: [ :x |Transcript show: x; newLine ].
	Transcript newLine."
	
	"[2] Check to see that each requirement is actually satisfied"
	packagesToLoad do: [ :fReq | fReq checkRequirement ].

	"[3] Load required packages before packages that require them"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]
]

{ #category : 'requires' }
FeatureRequirement >> requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil allRequiringFeatures: chainRequiringMe [
	"See if all the requirements can be met and answer the load order"

	(chainRequiringMe includes: self) ifTrue: [
		self error: 'Circular dependence detected. Can not load required packages.' ].
	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq sameNameAs: self]) ifFalse: [
			(self findPackageFileAsReqOf: mainFeatureOrNil)
				ifTrue: [ | otherRequirements |
					otherRequirements := self codePackageFile requires.
					otherRequirements ifNotNil: [
						otherRequirements do: [ :requires | 
							requires
								requireUnlessIn: toLoad
								main: (mainFeatureOrNil ifNil: [self])
								requiringFeature: self
								allRequiringFeatures: (chainRequiringMe copyWith: self) ]].
					self == mainFeatureOrNil ifFalse: [
						self addToLoad: toLoad withMyRequirements: otherRequirements ].
					]
				ifFalse: [
					| failureMessage |
					failureMessage := name = #'Cuis-Base'
						ifTrue: [
							(self maxVersion notNil and: [(SystemVersion current versionRevision first) > self maxVersion])
							ifTrue: [
								'Installing [', requiringFeatureOrNil name, '] has maximum version ',  
									self maxVersion asString, 						String newLineString,
									' which is older than the current Cuis-Base version ',
									 (SystemVersion current versionRevision first) asString
								]
							ifFalse: [
								'Installing [', requiringFeatureOrNil name,
									'] requires base system updated to #', self minRevision printString, String newLineString,
								'But this system is updated to #', SystemVersion current versionRevision second printString, String newLineString,
								'Please install Cuis base system updates and retry.' ]						
						]
						ifFalse: [
							requiringFeatureOrNil notNil
								ifTrue: [
									'Could not find code package file for [', name, '].', String newLineString,
									'Installation of [', requiringFeatureOrNil name, '] failed.']
								ifFalse: [
									'Could not find code package file for [', name, '].', String newLineString,
									'Installation failed.']].
					FeatureRequirementUnsatisfied signal: failureMessage.
				]]].

	^ toLoad
]

{ #category : 'testing' }
FeatureRequirement >> sameNameAs: anotherFeatureRequirement [

	"Answer true if anotherFeatureRequirement has same name as me"
	^(self name = anotherFeatureRequirement name)
]

{ #category : 'requires' }
FeatureRequirement >> satisfyRequirementsAndInstall [
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: self
			requiringFeature: self
			allRequiringFeatures: #().
	
	"Check to see that each requirement is actually satisfied"
	packagesToLoad do: [ :fReq | fReq checkRequirement ].
	
	"All requirements are satisfied; do the deed"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install
]
