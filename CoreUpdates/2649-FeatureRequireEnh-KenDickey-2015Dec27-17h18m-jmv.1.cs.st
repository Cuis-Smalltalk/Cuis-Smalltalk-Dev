'From Cuis 4.2 of 25 July 2013 [latest update: #2648] on 27 December 2015 at 5:24:40.888917 pm'!
!classDefinition: #FeatureRequirementUnsatisfied category: #'Package Support'!
Error subclass: #FeatureRequirementUnsatisfied
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!FeatureRequirementUnsatisfied methodsFor: 'exceptionDescription' stamp: 'jmv 12/27/2015 17:24'!
defaultAction
	"The default action taken if the exception is signaled."

	self messageText print.
	PopUpMenu inform: 
		self messageText, 
		String newLineString, String newLineString, 
		'I checked ./* ./Packages/* and ../Cuis-Smalltalk*',
		String newLineString, String newLineString, 
		'You can view loaded Packages and their requirements via',
		String newLineString,
		'        World menu > Open.. > Installed Packages',
		String newLineString
	
! !


!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 12/15/2015 14:20'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 12/27/2015 17:20'!
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
			ifFalse: [
				FeatureRequirementUnsatisfied 
					signal: 'Could not find package supplying: ',
							String newLineString, '        ',
							self printString 
			]].

	^ toLoad! !

!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 12/15/2015 14:30'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
	
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: self]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	"All requirements are satisfied; do the deed"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install! !

