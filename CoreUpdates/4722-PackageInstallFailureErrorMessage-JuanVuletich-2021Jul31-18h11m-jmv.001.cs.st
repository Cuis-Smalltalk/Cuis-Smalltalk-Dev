'From Cuis 5.0 [latest update: #4689] on 31 July 2021 at 8:35:26 pm'!

!FeatureRequirementUnsatisfied methodsFor: 'exceptionDescription' stamp: 'jmv 7/31/2021 20:26:38'!
defaultAction
	"The default action taken if the exception is signaled."

	self messageText print.
	PopUpMenu inform: self messageText.! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/31/2021 20:34:40'!
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
					failureMessage _ name = #'Cuis-Base'
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

