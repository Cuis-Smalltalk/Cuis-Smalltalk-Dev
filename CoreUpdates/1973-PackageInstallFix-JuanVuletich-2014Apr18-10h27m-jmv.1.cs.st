'From Cuis 4.2 of 25 July 2013 [latest update: #1972] on 18 April 2014 at 10:29:09.710006 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/18/2014 10:28'!
isInstalling: aCodePackage

	^Installing = ('Install-', aCodePackage packageName)! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/18/2014 10:09'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad _ self requireUnlessIn: OrderedCollection new asReqOf: nil main: self.
	packagesToLoad do: [ :requirement | requirement install ]! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 4/18/2014 10:09'!
installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(FeatureRequirement name: pkName)
		pathName: fullName;
		satisfyRequirementsAndInstall "Install with all requirements"! !


!CodePackage methodsFor: 'accessing' stamp: 'jmv 4/18/2014 10:28'!
hasUnsavedChanges: aBoolean

	"Not during package install"
	(ChangeSet isInstalling: self) ifFalse: [
		hasUnsavedChanges = aBoolean ifFalse: [
			hasUnsavedChanges _ aBoolean.
			hasUnsavedChanges ifTrue: [
				featureSpec provides newRevision ].
			self triggerEvent: #dirtyFlagChanged ]]! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/18/2014 10:04'!
requireUnlessIn: toLoad asReqOf: whoRequiresMe main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	((self isAlreadySatisfied not
			and: [ (toLoad includes: self) not ]) or: [
				"If we are the main feature being installed, install me even if already satisfied"
				self == mainFeatureOrNil ])
		ifTrue: [
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

