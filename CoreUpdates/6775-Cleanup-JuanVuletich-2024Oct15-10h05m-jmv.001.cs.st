'From Cuis7.1 [latest update: #6774] on 15 October 2024 at 10:06:43 am'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/15/2024 09:50:17'!
requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil allRequiringFeatures: chainRequiringMe
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
							(SystemVersion current versionRevision first) > self maxVersion
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

	^ toLoad! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/15/2024 09:50:13'!
addToLoad: toLoad withMyRequirements: requirements

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
		! !

!methodRemoval: FeatureRequirement #requirements stamp: 'jmv 10/15/2024 10:06:28'!
FeatureRequirement removeSelector: #requirements!
