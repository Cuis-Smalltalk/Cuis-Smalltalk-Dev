'From Cuis 5.0 [latest update: #3946] on 7 November 2019 at 2:09:32 pm'!

!LargePositiveInteger methodsFor: 'enumerating' stamp: 'sqr 11/2/2019 20:17:15'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| toGo thisRound |
	toGo := self.
	[toGo > 0] whileTrue:
		[
			thisRound := toGo min: SmallInteger maxVal.
			thisRound timesRepeat: aBlock.
			toGo := toGo - thisRound
		]! !


!Integer methodsFor: 'enumerating'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| count |
	count _ 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count _ count + 1]! !

!Integer methodsFor: 'testing' stamp: 'dtl 1/23/2010 14:16'!
isPrime
	"Answer true if the receiver is a prime number. See isProbablyPrime for a probabilistic
	implementation that is much faster for large integers, and that is correct to an extremely
	high statistical level of confidence (effectively deterministic)."
	
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2].
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 ifTrue: [ ^false ] ].
	^true! !

!methodRemoval: SmallInteger #timesRepeat: stamp: 'jmv 11/7/2019 14:09:10'!
SmallInteger removeSelector: #timesRepeat:!
