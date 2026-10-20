'From Cuis7.9 [latest update: #8267] on 20 October 2026 at 11:22:12 am'!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision versionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName codePackageFile minVersionNumber maxVersionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!

!Feature methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 1:58:46 pm (UTC)'!
versionNumber

	^ versionNumber
! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 2:00:56 pm (UTC)'!
maxVersionNumber 

	^maxVersionNumber ! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 2:00:20 pm (UTC)'!
minVersionNumber 

	^minVersionNumber ! !


!CodePackage methodsFor: 'saving' stamp: 'CAP 20/Oct/2026, 2:01:01 pm (UTC)'!
writeFeatureSpecOn: aStream
	| provides |
	provides := featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asPlainString printString, ' ', provides versionNumber printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'requires: ', requires name asPlainString printString, ' ', requires minVersionNumber printString, ' ', requires minRevision printString, ' ', requires maxVersionNumber printString;
			newLine ]! !


!Feature methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 1:59:08 pm (UTC)'!
requirementOfMe

	^ FeatureRequirement 
		name: self name 
		minVersion: self versionNumber 
		minRevision: self revision 
		maxVersion: nil! !

!Feature methodsFor: 'testing' stamp: 'CAP 20/Oct/2026, 1:59:04 pm (UTC)'!
isLaterThan: aFeature
	^self versionNumber > aFeature versionNumber or: [
		self versionNumber = aFeature versionNumber and: [
			self revision > aFeature revision ]]! !

!Feature methodsFor: 'testing' stamp: 'CAP 20/Oct/2026, 2:01:08 pm (UTC)'!
satisfies: featureRequirement
	"Does this provided Feature satisfy the FeatureRequirement?"
	
	"Must match name."
	^ (name sameAs: featureRequirement name) and: [

		"If no specific version req, we are done. Ok."
		featureRequirement minVersionNumber isNil or: [

		"If our version is exactly the min req version, we must also satisfy minRevision"
		versionNumber = featureRequirement minVersionNumber and: [
			featureRequirement minRevision isNil or: [ revision >= featureRequirement minRevision ]]] or: [
		
		"If we are past min req version, ignore minRevision, but check we are not beyond max req version"
		versionNumber > featureRequirement minVersionNumber and: [
			featureRequirement maxVersionNumber isNil or: [ versionNumber <= featureRequirement maxVersionNumber ]]]]! !

!Feature methodsFor: 'printing' stamp: 'CAP 20/Oct/2026, 1:57:43 pm (UTC)'!
printVersionRevisionOn: aStream
	versionNumber printOn: aStream.
	aStream nextPut: $..
	revision printOn: aStream! !

!Feature methodsFor: 'incrementing' stamp: 'CAP 20/Oct/2026, 2:20:27 pm (UTC)'!
newVersion
	"Increment major version number."

	versionNumber := versionNumber + 1.
	revision := 0! !

!Feature methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:21:07 pm (UTC)'!
name: theName version: theVersion revision: theRevision

	name := theName.
	versionNumber := theVersion.
	revision := theRevision.! !


!FeatureRequirement methodsFor: 'requires' stamp: 'CAP 20/Oct/2026, 2:01:20 pm (UTC)'!
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
							(self maxVersionNumber notNil and: [(SystemVersion current versionRevision first) > self maxVersionNumber])
							ifTrue: [
								'Installing [', requiringFeatureOrNil name, '] has maximum version ',  
									self maxVersionNumber asString, 						String newLineString,
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

!FeatureRequirement methodsFor: 'printing' stamp: 'CAP 20/Oct/2026, 1:58:10 pm (UTC)'!
printDetailsOn: aStream
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersionNumber
		ifNil: [
			aStream nextPutAll: '*.*)'.
			^self ]
		ifNotNil: [ minVersionNumber printOn: aStream ].
	aStream nextPut: $..
	minRevision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ minRevision printOn: aStream ].
	(minRevision notNil or: [ maxVersionNumber isNil or: [maxVersionNumber > minVersionNumber ]]) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersionNumber
			ifNil: [ aStream nextPut: $* ]
			ifNotNil: [ maxVersionNumber printOn: aStream ].
		maxVersionNumber = minVersionNumber
			ifTrue: [ aStream nextPutAll: '.999' ]
			ifFalse: [ aStream nextPutAll: '.*' ]
		]! !

!FeatureRequirement methodsFor: 'comparing' stamp: 'CAP 20/Oct/2026, 2:01:11 pm (UTC)'!
= another
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	^name = another name and: [
		minVersionNumber = another minVersionNumber and: [
		minRevision = another minRevision and: [
		maxVersionNumber = another maxVersionNumber ]]]! !

!FeatureRequirement methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:19:52 pm (UTC)'!
name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersionNumber := minVer.
	minRevision := rev.
	maxVersionNumber := maxVer.! !

!methodRemoval: FeatureRequirement #maxVersion stamp: 'CAP 20/Oct/2026, 2:18:18 pm (UTC)'!
FeatureRequirement removeSelector: #maxVersion!
!methodRemoval: FeatureRequirement #minVersion stamp: 'CAP 20/Oct/2026, 2:18:23 pm (UTC)'!
FeatureRequirement removeSelector: #minVersion!
!methodRemoval: Feature #version stamp: 'CAP 20/Oct/2026, 2:18:27 pm (UTC)'!
Feature removeSelector: #version!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name revision versionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minRevision pathName codePackageFile minVersionNumber maxVersionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
