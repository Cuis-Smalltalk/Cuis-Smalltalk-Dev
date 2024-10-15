'From Cuis7.1 [latest update: #6771] on 14 October 2024 at 5:48:18 pm'!

!Feature class methodsFor: 'instance creation' stamp: 'jmv 10/14/2024 17:45:57'!
fromTokens: tokens
	^self 
		name: tokens second
		version: tokens third
		revision: tokens fourth.! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 10/14/2024 17:45:02'!
fromTokens: tokens
	^self 
		name: tokens second 
		minVersion: tokens third 
		minRevision: tokens fourth 
		maxVersion: (tokens size > 4 ifTrue: [tokens fifth]).! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 10/14/2024 17:47:23'!
scanFeatureProvision: tokens

	| changeRecord feature |
	feature := Feature fromTokens: tokens.
	changeRecord := FeatureChangeRecord new
		type: #provides
		feature: feature.
	self addItem: changeRecord text: changeRecord string.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 10/14/2024 17:47:05'!
scanFeatureRequirement: tokens

	| changeRecord requirement |
	requirement := FeatureRequirement fromTokens: tokens.
	changeRecord := FeatureChangeRecord new
		type: #requires
		feature: requirement.
	self addItem: changeRecord text: changeRecord string.! !

