'From Cuis 5.0 [latest update: #3948] on 7 November 2019 at 5:55:14 pm'!
'Description '!
!provides: 'PerformanceImprovements' 1 2!



!Integer methodsFor: '*performanceImprovements' stamp: 'a s + sqr 10/11/2019 23:51:08'!
isPrime

	self < 3 ifTrue: [^self = 2].
	self even ifTrue: [^false].
	self \\ 3 = 0 ifTrue: [^self = 3].
	self \\ 5 = 0 ifTrue: [^self = 5].
	self < 8281 ifTrue: [
		"Approximate sqrtFloor to avoid computational expense"
		self \\ 7 = 0 ifTrue: [^self = 7].
		self \\ 11 = 0 ifTrue: [^self = 11].
		self \\ 13 = 0 ifTrue: [^self = 13].
		12 to: (self bitShift: -6) + 11 by: 6 do: [:each |
			self \\ (each+5) = 0 ifTrue: [^false].
			self \\ (each+7) = 0 ifTrue: [^false]
		].
		^true
	].
	"Now 2, 3 and 5 do not divide self. So, self is of the form
	30*k + {1, 7, 11, 13, 17, 19, 23, 29} for integer k >= 0.
	The 31 case below is the 30k+1 case, excluding k = 0"
	0 to: self sqrtFloor by: 30 do: [:each |
		self \\ (each+7) = 0 ifTrue: [^false].
		self \\ (each+11) = 0 ifTrue: [^false].
		self \\ (each+13) = 0 ifTrue: [^false].
		self \\ (each+17) = 0 ifTrue: [^false].
		self \\ (each+19) = 0 ifTrue: [^false].
		self \\ (each+23) = 0 ifTrue: [^false].
		self \\ (each+29) = 0 ifTrue: [^false].
		self \\ (each+31) = 0 ifTrue: [^false]
	].
	^true! !

!LargePositiveInteger methodsFor: '*performanceImprovements' stamp: 'sqr 11/2/2019 20:17:15'!
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

!SmallInteger methodsFor: '*performanceImprovements' stamp: 'sqr 11/2/2019 20:45:34'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	self < 1 ifTrue: [^self].
	1 to: (self bitAnd: 31) do: [:x | aBlock value].
	1 to: (self bitAnd: -32) by: 32 do:
		[:x |
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value
		]! !
