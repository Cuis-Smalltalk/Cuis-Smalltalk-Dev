'From Cuis 5.0 [latest update: #4619] on 20 July 2021 at 2:33:16 pm'!

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/20/2021 14:32:53'!
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
				ifFalse: [
					| failureMessage |
					failureMessage _ name = #'Cuis-Base' ifTrue: [
							'Installing ', requiringFeatureOrNil name, String newLineString,
							'requires base system updated to #', self minRevision printString, String newLineString,
							'But this system is updated to #', SystemVersion current versionRevision second printString, String newLineString,
							'Please install Cuis base system updates' ]						
						ifFalse: [
							'Installing', requiringFeatureOrNil name, String newLineString,
							'Requires: ', self printString ].
					FeatureRequirementUnsatisfied signal: failureMessage.
				]]].

	^ toLoad! !

