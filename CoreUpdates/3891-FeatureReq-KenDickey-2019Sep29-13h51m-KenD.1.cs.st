'From Cuis 5.0 [latest update: #3890] on 29 September 2019 at 1:52:09 pm'!

!FeatureRequirement methodsFor: 'testing' stamp: 'KenD 9/26/2019 16:16:32'!
satisfies: anotherFeatureRequirement

	"Answer true if anotherFeatureRequirement is satisfied by me"
	(self name = anotherFeatureRequirement name) ifFalse: [^false].
	"FIXME: add version,revision checks"
	^true! !

!FeatureRequirement methodsFor: 'private' stamp: 'KenD 9/28/2019 09:03:11'!
addToLoad: toLoad withMyRequirements: requirements

	"Add self to OrderedCollection toLoad before any requirements I have"
	| requires |
	requires := requirements collect: [:r| r name]. "a set"
	toLoad do: [ :featureReq | 
		(requires includes: featureReq name)
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


!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 9/28/2019 09:03:54'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	"Transcript show: 'packagesToLoad: '; newLine.
	packagesToLoad do: [ :x |Transcript show: x; newLine ].
	Transcript newLine .
self halt." 
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 9/27/2019 16:09:25'!
requireUnlessIn: toLoad main: mainFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq satisfies: self])
		ifFalse: [
		(self findPackageFileAsReqOf: mainFeatureOrNil)
			ifTrue: [ | otherRequirements |
				otherRequirements := self requirements.
				self == mainFeatureOrNil ifFalse: [
					self addToLoad: toLoad withMyRequirements: otherRequirements ].
				otherRequirements ifNotNil: [
					otherRequirements do: [ :requires | 
						requires requireUnlessIn: toLoad main: (mainFeatureOrNil ifNil: [self])]]]
			ifFalse: [
				FeatureRequirementUnsatisfied 
					signal: 'Could not find package supplying: ',
							String newLineString, '        ',
							self printString 
			]]].

	^ toLoad! !

