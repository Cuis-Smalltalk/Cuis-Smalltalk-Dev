'From Cuis 5.0 of 7 November 2016 [latest update: #3490] on 20 November 2018 at 12:51:56 pm'!

!Message methodsFor: 'comparing' stamp: 'jmv 11/20/2018 12:51:32'!
= aMessage

	"Any object is equal to itself"
	self == aMessage ifTrue: [ ^ true ].

	self class == aMessage class ifFalse: [ ^false ].
	selector = aMessage selector ifFalse: [ ^false ].
	lookupClass = aMessage lookupClass ifFalse: [ ^false ].
	^args = aMessage arguments! !

!Message methodsFor: 'comparing' stamp: 'jmv 11/20/2018 12:47:30'!
hash
	"Hash is reimplemented because = is implemented."
	^selector hash bitXor: args hash! !

