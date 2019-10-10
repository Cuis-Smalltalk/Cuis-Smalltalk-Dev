'From Cuis 5.0 [latest update: #3905] on 8 October 2019 at 8:44:06 am'!

!FeatureRequirement methodsFor: 'requires' stamp: 'KenD 10/7/2019 10:59:06'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: self]
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

