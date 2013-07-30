'From Cuis 4.2 of 25 July 2013 [latest update: #1769] on 28 July 2013 at 11:17:30.858225 pm'!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision pathName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!Feature commentStamp: 'jmv 7/28/2013 23:15' prior: 0!
A package has a ProvidedFeature which specifies its version and revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  version -- the integer version/  This increments with each incompatable interface change.
  revision -- the integer revision which increments each time a package is saved.!

!classDefinition: #FeatureRequirement category: #'Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name version revision pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!FeatureRequirement commentStamp: 'jmv 7/28/2013 23:16' prior: 0!
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion
  path -- nil or the (cached)  pathName of a file containing the package with this feature


Try any of:
  (FeatureRequirement name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"

This should load packages #Compression and #Sound!


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
name
	
	^ name! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
packageName
	"Answer a package name based on feature name"

	^ (name asString , '.pck.st' )! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
pathName
	
	^ pathName! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
pathName: aPathNameString
	
	pathName := aPathNameString! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
requirements
	"Answer my requirements"
	
	| packageFile |
	packageFile _ CodePackageFile buildFileStream: (FileStream readOnlyFileNamed: pathName).
	^ packageFile requires! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
revision

	^ revision! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/28/2013 22:50'!
version

	^ version
! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/28/2013 22:50'!
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

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/28/2013 23:10'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad := self requireUnlessIn: OrderedCollection new asReqOf: nil main: nil.
	packagesToLoad do: [ :requirement |
		CodePackageFile basicInstallPackageStream: ( FileStream readOnlyFileNamed: (requirement pathName) ).
	]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/28/2013 23:04'!
requireUnlessIn: toLoad asReqOf: whoRequiresMe main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	(self isAlreadySatisfied
			or: [ toLoad includes: self] )
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

!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 7/28/2013 22:50'!
isAlreadySatisfied
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ]! !

!FeatureRequirement methodsFor: 'initialize-release' stamp: 'jmv 7/28/2013 22:50'!
name: theName version: theVersion revision: theRevision

	name     := theName.
	version  := theVersion.
	revision := theRevision! !

!FeatureRequirement methodsFor: 'printing' stamp: 'jmv 7/28/2013 22:50'!
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

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/28/2013 22:50'!
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


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 22:50'!
name: aSymbol
	^self name: aSymbol version: nil revision: nil! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 22:50'!
name: aSymbol version: integerVersionOrNil
	^self name: aSymbol version: integerVersionOrNil revision: nil! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 22:50'!
name: aSymbol version: integerVersionOrNil revision: integerRevisionOrNil

	| newInst |
	newInst := self new.
	newInst name: aSymbol asSymbol version: integerVersionOrNil revision: integerRevisionOrNil.
	
	^ newInst! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/28/2013 22:52'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem feature |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:']
	or: [item includesSubString: 'provides']
	or: [item includesSubString: 'requires'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk actualScannerClass new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference asStringOrText ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp].

		secondToken == #commentStamp: ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self ].

		firstToken == #provides: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].

		firstToken == #requires: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #requires
					feature: (FeatureRequirement name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].
	]! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/28/2013 22:52'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(FeatureRequirement name: pkName)
		pathName: fullName;
		require! !


!Feature methodsFor: 'testing' stamp: 'jmv 7/28/2013 23:12'!
satisfies: featureRequirement
	"Does this provided Feature satisfy the FeatureRequirement?"
	^ name = featureRequirement name and: [
		featureRequirement version isNil or: [
			version = featureRequirement version and: [
				featureRequirement revision isNil or: [ revision >= featureRequirement revision ]]]]! !

!methodRemoval: FeatureRequirement class #baseSystemFeature!
FeatureRequirement class removeSelector: #baseSystemFeature!
!methodRemoval: FeatureRequirement #codePackage!
FeatureRequirement removeSelector: #codePackage!
!methodRemoval: FeatureRequirement #satisfiedInCollection:!
FeatureRequirement removeSelector: #satisfiedInCollection:!
!methodRemoval: FeatureRequirement #satisfies:!
FeatureRequirement removeSelector: #satisfies:!
!methodRemoval: Feature class #name:!
Feature class removeSelector: #name:!
!methodRemoval: Feature class #name:version:!
Feature class removeSelector: #name:version:!
!methodRemoval: Feature #codePackage!
Feature removeSelector: #codePackage!
!methodRemoval: Feature #findPackageFileAsReqOf:!
Feature removeSelector: #findPackageFileAsReqOf:!
!methodRemoval: Feature #isAlreadySatisfied!
Feature removeSelector: #isAlreadySatisfied!
!methodRemoval: Feature #packageName!
Feature removeSelector: #packageName!
!methodRemoval: Feature #pathName!
Feature removeSelector: #pathName!
!methodRemoval: Feature #pathName:!
Feature removeSelector: #pathName:!
!methodRemoval: Feature #placesToLookForPackagesDo:!
Feature removeSelector: #placesToLookForPackagesDo:!
!methodRemoval: Feature #require!
Feature removeSelector: #require!
!methodRemoval: Feature #requireUnlessIn:asReqOf:main:!
Feature removeSelector: #requireUnlessIn:asReqOf:main:!
!methodRemoval: Feature #requirements!
Feature removeSelector: #requirements!
!methodRemoval: Feature #satisfiedInCollection:!
Feature removeSelector: #satisfiedInCollection:!
!classDefinition: #Feature category: #'Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
