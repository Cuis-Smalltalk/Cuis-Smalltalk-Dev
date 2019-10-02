'From Cuis 5.0 [latest update: #3892] on 30 September 2019 at 3:25:41 pm'!

!FeatureRequirement methodsFor: 'testing' stamp: 'KenD 9/30/2019 08:12:46'!
checkRequirement
	"Answer if I am satisfied by package found at pathName"
	
	pathName asFileEntry readStreamDo: [ :stream | | fSpec |
		fSpec := ((CodePackageFile buildFileStream: stream) featureSpec).
		((fSpec isNil) or: [(fSpec provides satisfies: self) not])
		ifTrue: [
			FeatureRequirementUnsatisfied 
					signal: pathName,
							String newLineString,
							' could not satisfy ', self printString.
			^false
		]
		ifFalse: [ ^true ]
	]
							 
! !

!FeatureRequirement methodsFor: 'testing' stamp: 'KenD 9/30/2019 09:51:40'!
sameNameAs: anotherFeatureRequirement

	"Answer true if anotherFeatureRequirement has same name as me"
	^(self name = anotherFeatureRequirement name)! !


!CodePackageFile methodsFor: 'accessing' stamp: 'KenD 9/29/2019 21:28:27'!
featureSpec

	^ featureSpec! !


!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 9/30/2019 15:12:46'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil]
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

!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 9/30/2019 15:23:40'!
requireUnlessIn: toLoad main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq sameNameAs: self])
		ifFalse: [
		(self findPackageFileAsReqOf: mainFeatureOrNil)
			ifTrue: [ | otherRequirements |
				otherRequirements := self requirements.
				otherRequirements ifNotNil: [
					otherRequirements do: [ :requires | 
						requires requireUnlessIn: toLoad main: (mainFeatureOrNil ifNil: [self])]].
				self == mainFeatureOrNil ifFalse: [
					self addToLoad: toLoad withMyRequirements: otherRequirements ].
				]
			ifFalse: [
				FeatureRequirementUnsatisfied 
					signal: 'Could not find package supplying: ',
							String newLineString, '        ',
							self printString 
			]]].

	^ toLoad! !

!FeatureRequirement methodsFor: 'private' stamp: 'KenD 9/30/2019 15:13:19'!
addToLoad: toLoad withMyRequirements: requirements

	"Add self to OrderedCollection 'toLoad' before any requirements I have"
	| myRequirements |
	myRequirements := self requirements.
	toLoad do: [ :featureReq | 
		(myRequirements anySatisfy: [ :req | featureReq sameNameAs: req])
		ifTrue: [
			"Transcript show: ('adding ', self name asString, ' before: ', featureReq name asString); newLine."
			toLoad add: self before: featureReq.
			^ toLoad
		]
	].
	"Transcript show: ('adding ', self name asString); newLine."
	toLoad addLast: self.
	^ toLoad
		! !

!methodRemoval: FeatureRequirement #satisfies:!
FeatureRequirement removeSelector: #satisfies:!
