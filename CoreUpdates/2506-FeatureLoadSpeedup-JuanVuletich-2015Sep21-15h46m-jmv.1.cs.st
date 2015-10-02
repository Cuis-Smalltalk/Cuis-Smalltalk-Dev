'From Cuis 4.2 of 25 July 2013 [latest update: #2505] on 21 September 2015 at 3:47:46.343687 pm'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/21/2015 15:43'!
requireUnlessIn: toLoad main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	self isAlreadySatisfied ifFalse: [
		(self findPackageFileAsReqOf: mainFeatureOrNil)
			ifTrue: [
				self == mainFeatureOrNil ifFalse: [
					toLoad add: self ].
				self requirements ifNotNil: [ :otherRequirements |
					otherRequirements do: [ :requires | 
						requires requireUnlessIn: toLoad main: (mainFeatureOrNil ifNil: [self])]]]
			ifFalse: [ "@@FIXME: Signal exception for UI to handle"
				self error: 'Could not find package supplying feature: ' , self printString ]].

	^ toLoad! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/21/2015 15:47'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil.
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/21/2015 15:47'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
	
	"Preflight before load"
	packagesToLoad _ self requireUnlessIn: OrderedCollection new main: self.
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install! !

!methodRemoval: FeatureRequirement #requireUnlessIn:asReqOf:main:!
FeatureRequirement removeSelector: #requireUnlessIn:asReqOf:main:!
