'From Cuis 6.0 [latest update: #5772] on 23 April 2023 at 8:18:19 pm'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/23/2023 20:17:23'!
requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil allRequiringFeatures: chainRequiringMe
	"See if all the requirements can be met and answer the load order"

	(chainRequiringMe includes: self) ifTrue: [
		self error: 'Circular dependence detected. Can not load required packages.' ].
	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq sameNameAs: self]) ifFalse: [
			(self findPackageFileAsReqOf: mainFeatureOrNil)
				ifTrue: [ | otherRequirements |
					otherRequirements := self requirements.
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
							'Installing [', requiringFeatureOrNil name,
								'] requires base system updated to #', self minRevision printString, String newLineString,
							'But this system is updated to #', SystemVersion current versionRevision second printString, String newLineString,
							'Please install Cuis base system updates and retry.' ]						
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

	^ toLoad! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/23/2023 20:16:11'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	[packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: nil
			requiringFeature: nil
			allRequiringFeatures: #()]
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

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/23/2023 20:16:27'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	[packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: self
			requiringFeature: self
			allRequiringFeatures: #()]
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

!methodRemoval: FeatureRequirement #requireUnlessIn:main:requiringFeature: stamp: 'jmv 4/23/2023 20:16:29'!
FeatureRequirement removeSelector: #requireUnlessIn:main:requiringFeature:!
