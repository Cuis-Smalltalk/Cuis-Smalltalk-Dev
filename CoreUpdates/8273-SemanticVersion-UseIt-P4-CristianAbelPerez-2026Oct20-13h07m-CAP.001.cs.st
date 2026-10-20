'From Cuis7.9 [latest update: #8271] on 20 October 2026 at 1:12:50 pm'!

!FeatureRequirement class methodsFor: 'private' stamp: 'CAP 19/Oct/2026, 6:33:53 pm (UTC)'!
semanticVersionFromLegacyCuisBaseVersion: aVersionNumber revision: aRevisionNumber
	"Before Semantic Versioning, Cuis 7.9 updated to #8257 was written as version 79, revision 8257"

	^SemanticVersion major: aVersionNumber // 10 minor: aVersionNumber \\ 10 patch: aRevisionNumber! !


!Feature methodsFor: 'accessing' stamp: 'CAP 18/Oct/2026, 10:48:15 pm (UTC)'!
requirementOfMe

	^ FeatureRequirement 
		name: self name 
		minVersion: self version 
		maxVersion: self version anyMinor! !

!Feature methodsFor: 'testing' stamp: 'CAP 16/Oct/2026, 5:43:16 pm (UTC)'!
isLaterThan: aFeature

	^self version > aFeature version! !

!Feature methodsFor: 'testing' stamp: 'CAP 16/Oct/2026, 5:22:24 pm (UTC)'!
satisfies: featureRequirement

	^ (name sameAs: featureRequirement name) and: [
		version between: featureRequirement minVersion and: featureRequirement maxVersion ]
		! !


!Feature class methodsFor: 'instance creation' stamp: 'CAP 19/Oct/2026, 12:44:24 am (UTC)'!
baseSystemFeature

	^Feature name: #'Cuis-Base' version: SystemVersion current version! !

!Feature class methodsFor: 'instance creation' stamp: 'CAP 17/Oct/2026, 11:39:25 pm (UTC)'!
fromTokens: tokens

	| version |

	version := tokens third isString
		ifTrue: [ SemanticVersion fromString: tokens third ]
		ifFalse: [
			"Compatibility with previous non-semantic versioning"
			SemanticVersion major: tokens third minor: tokens fourth
		].
	^self
		name: tokens second
		version: version! !


!FeatureRequirement methodsFor: 'requires' stamp: 'CAP 19/Oct/2026, 12:44:40 am (UTC)'!
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
							(SystemVersion current version > self maxVersion)
							ifTrue: [
								'Installing [', requiringFeatureOrNil name, '] has maximum Cuis-Base version ',  
									self maxVersion printString, String newLineString,
									' which is older than the current Cuis-Base version ',
									SystemVersion current version printString
								]
							ifFalse: [
								'Installing [', requiringFeatureOrNil name,
									'] requires Cuis-Base version ', self minVersion printString, String newLineString,
								'But this system is Cuis-Base version ', SystemVersion current version printString, String newLineString,
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

!FeatureRequirement methodsFor: 'printing' stamp: 'CAP 17/Oct/2026, 9:07:41 pm (UTC)'!
printDetailsOn: aStream
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersion printOn: aStream.
	(maxVersion > minVersion) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersion printOn: aStream ]! !

!FeatureRequirement methodsFor: 'comparing' stamp: 'CAP 16/Oct/2026, 5:58:53 pm (UTC)'!
= another
	self class == another class ifFalse: [ ^false ].
	^name = another name and: [
		minVersion = another minVersion and: [
			maxVersion = another maxVersion ]]! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'CAP 19/Oct/2026, 6:35:11 pm (UTC)'!
fromTokens: tokens

	| minVersion maxVersion |
	
	tokens third isString
		ifTrue: [
			minVersion := SemanticVersion fromString: tokens third.
			maxVersion := SemanticVersion fromString: tokens fourth.
		]
		ifFalse: [
			"Compatibility with previous non-semantic versioning"
			tokens third ifNil: [ ^self name: tokens second ].
			tokens second = 'Cuis-Base'
				ifTrue: [
					minVersion := self semanticVersionFromLegacyCuisBaseVersion: (tokens third ifNil: [0]) revision: (tokens fourth ifNil: [0]).
					maxVersion := (tokens size > 4 and: [ tokens fifth notNil ])
						ifTrue: [ (self semanticVersionFromLegacyCuisBaseVersion: tokens fifth revision: 0) anyPatch ]
						ifFalse: [ minVersion anyMinor ] ]
				ifFalse: [
					minVersion := SemanticVersion major: (tokens third ifNil: [0]) minor: (tokens fourth ifNil: [0]).
					maxVersion := (tokens size > 4 and: [ tokens fifth notNil ])
						ifTrue: [ SemanticVersion major: tokens fifth minor: Float infinity patch: Float infinity ]
						ifFalse: [ minVersion anyMinor ] ] ].
		
	
			
	^self 
		name: tokens second 
		minVersion: minVersion
		maxVersion: maxVersion.! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'CAP 17/Oct/2026, 11:06:12 pm (UTC)'!
name: aName
	"Ask for any version"
	
	^self name: aName minVersion: SemanticVersion zero maxVersion: SemanticVersion infinity
! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'CAP 18/Oct/2026, 10:39:20 pm (UTC)'!
name: aName version: aVersion
	"Ask for a specific version, up to any later one within its major"

	^self name: aName minVersion: aVersion maxVersion: aVersion anyMinor! !

