'From Cuis7.1 [latest update: #6808] on 1 November 2024 at 10:51:30 am'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 11/1/2024 10:51:25'!
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

	^ toLoad! !

