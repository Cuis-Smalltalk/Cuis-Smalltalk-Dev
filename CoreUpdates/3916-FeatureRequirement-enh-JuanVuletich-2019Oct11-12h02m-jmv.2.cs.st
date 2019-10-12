'From Cuis 5.0 [latest update: #3913] on 11 October 2019 at 12:35:03 pm'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/11/2019 12:34:45'!
requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq sameNameAs: self]) ifFalse: [
			(self findPackageFileAsReqOf: mainFeatureOrNil)
				ifTrue: [ | otherRequirements |
					otherRequirements := self requirements.
					otherRequirements ifNotNil: [
						otherRequirements do: [ :requires | 
							requires requireUnlessIn: toLoad main: (mainFeatureOrNil ifNil: [self]) requiringFeature: self ]].
					self == mainFeatureOrNil ifFalse: [
						self addToLoad: toLoad withMyRequirements: otherRequirements ].
					]
				ifFalse: [ | failureMessage |
					failureMessage := 'Could not find package supplying: ', self printString, String newLineString,
						'Required by: ', (requiringFeatureOrNil ifNil: [ self ]) printString, String newLineString,
						'For installing: ', (mainFeatureOrNil ifNil: [ self ]) printString, String newLineString.
					FeatureRequirementUnsatisfied signal: failureMessage.
				]]].

	^ toLoad! !


!PackageRequirementsList methodsFor: 'accessing' stamp: 'jmv 10/11/2019 12:16:09'!
updateSelectedRequirement

	| selectedPackage requiredPackage featureSpec requirementToUpdate updatedRequirement newRequires selectedName |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	requirementToUpdate := self selection.
	updatedRequirement := (selectedName _ requirementToUpdate name) = Feature baseSystemFeature name
		ifTrue: [ Feature baseSystemFeature requirementOfMe ]
		ifFalse: [
			requiredPackage := CodePackage installedPackages at: selectedName.
			requiredPackage hasUnsavedChanges
				ifTrue: [ self notify: 'Please save package ', requiredPackage packageName, ' first. Requirement version of an unsaved package can not be updated.'. ^self ].
			requiredPackage requirementOfMe ].
	newRequires := (featureSpec requires copyWithout: requirementToUpdate) copyWith: updatedRequirement.
	featureSpec 
		provides: featureSpec provides 
		requires: newRequires.
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/11/2019 12:25:39'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil requiringFeature: nil]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"Transcript show: 'packagesToLoad: '; newLine.
	packagesToLoad do: [ :x |Transcript show: x; newLine ].
	Transcript newLine."
	
	"[2] Check to see that each requirement is actually satisfied"
	[packagesToLoad do: [ :fReq | fReq checkRequirement ]]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].

	"[3] Load required packages before packages that require them"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/11/2019 12:26:13'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: self requiringFeature: self]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"Check to see that each requirement is actually satisfied"
	[packagesToLoad do: [ :fReq | fReq checkRequirement ]]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"All requirements are satisfied; do the deed"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install! !

!methodRemoval: FeatureRequirement #requireUnlessIn:main:!
FeatureRequirement removeSelector: #requireUnlessIn:main:!
