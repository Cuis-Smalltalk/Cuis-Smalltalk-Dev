'From Cuis 5.0 of 7 November 2016 [latest update: #3791] on 11 June 2019 at 11:56:29 am'!

!Number methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:41:36'!
= aNumber
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility! !

!Number methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:41:27'!
hash
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility! !


!TextDoIt methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:43:43'!
hash
	"Hash is reimplemented because = is implemented."

	^evalString hash! !


!TextURL methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:43:54'!
hash
	"Hash is reimplemented because = is implemented."

	^url hash! !


!TextAnchor methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:44:13'!
hash
	"Hash is reimplemented because = is implemented."

	^anchoredFormOrMorph hash! !


!FeatureRequirement methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:45:31'!
hash
	"Hash is reimplemented because = is implemented."

	^name hash! !


!CharacterBlock methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:42:38'!
hash
	"Hash is reimplemented because = is implemented."

	^stringIndex hash! !


!MouseScrollEvent methodsFor: 'comparing' stamp: 'jmv 6/11/2019 11:54:11'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	direction = aMorphicEvent direction ifFalse: [ ^ false ].
	^ true! !

!methodRemoval: MorphicEvent #hash!
MorphicEvent removeSelector: #hash!
