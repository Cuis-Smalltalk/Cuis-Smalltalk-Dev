'From Cuis 4.2 of 25 July 2013 [latest update: #2847] on 19 July 2016 at 11:22:03 am'!
'Description Please enter a description for this package'!
!provides: 'ProbabilityDistributions' 1 1!
!classDefinition: #ProbabilityDistribution category: #ProbabilityDistributions!
Object subclass: #ProbabilityDistribution
	instanceVariableNames: 'uniformRandomGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProbabilityDistributions'!
!classDefinition: 'ProbabilityDistribution class' category: #ProbabilityDistributions!
ProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #ExponentialProbabilityDistribution category: #ProbabilityDistributions!
ProbabilityDistribution subclass: #ExponentialProbabilityDistribution
	instanceVariableNames: 'lambda'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProbabilityDistributions'!
!classDefinition: 'ExponentialProbabilityDistribution class' category: #ProbabilityDistributions!
ExponentialProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #NormalProbabilityDistribution category: #ProbabilityDistributions!
ProbabilityDistribution subclass: #NormalProbabilityDistribution
	instanceVariableNames: 'mean standardDeviation cachedValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProbabilityDistributions'!
!classDefinition: 'NormalProbabilityDistribution class' category: #ProbabilityDistributions!
NormalProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #RayleighProbabilityDistribution category: #ProbabilityDistributions!
ProbabilityDistribution subclass: #RayleighProbabilityDistribution
	instanceVariableNames: 'sigma'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProbabilityDistributions'!
!classDefinition: 'RayleighProbabilityDistribution class' category: #ProbabilityDistributions!
RayleighProbabilityDistribution class
	instanceVariableNames: ''!


!ProbabilityDistribution commentStamp: '<historical>' prior: 0!
ProbabilityDistributions can also generate random numbers!

!ExponentialProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:18' prior: 0!
| r h |
r _ ExponentialProbabilityDistribution lambda: 3.
h _ Histogram binCount: 500 minValue: 0 maxValue: 120.
100000 timesRepeat: [ h addValue: r nextRandomNumber ].
h plot.!

!NormalProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:19' prior: 0!
| r h |
r _ NormalProbabilityDistribution new.
h _ Histogram binCount: 500 minValue: -5 maxValue: 5.
100000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.

| r h |
r _ NormalProbabilityDistribution mean: 2.5 standardDeviation: 0.5.
h _ Histogram binCount: 500 minValue: -5 maxValue: 5.
100000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.!

!RayleighProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:22' prior: 0!
| r h |
r _ RayleighProbabilityDistribution sigma: 3.
h _ Histogram binCount: 500 minValue: 0 maxValue: 20.
1000000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.!

!ProbabilityDistribution methodsFor: 'accessing' stamp: 'jmv 4/6/2015 12:03'!
uniformRandomGenerator
	^uniformRandomGenerator ifNil: [ uniformRandomGenerator _ Random new ]! !

!ExponentialProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:16'!
lambda: aNumber

	lambda _ aNumber! !

!ExponentialProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:16'!
nextRandomNumber
	"Answer the next value"

	^(1 / (1 - self uniformRandomGenerator next)) ln * 2 * lambda squared! !

!ExponentialProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:14'!
lambda: lambda
	^self basicNew lambda: lambda! !

!ExponentialProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:15'!
new
	^self lambda: 1.0! !

!NormalProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:08'!
mean: mu standardDeviation: sigma
	mean _ mu.
	standardDeviation _ sigma! !

!NormalProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:06'!
nextRandomNumber
	"Two random values are generated. So, answer one and cache the other."

	^self nextStandardNormalRandomNumber * standardDeviation  + mean! !

!NormalProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:06'!
nextStandardNormalRandomNumber
	"Two random values are generated. So, answer one and cache the other."

	| x1 x2 t1 t2 y1 y2 |
	cachedValue notNil
		ifTrue: [
			y1 _ cachedValue.
			cachedValue _ nil.
			^ y1 ].
	x1 _ self uniformRandomGenerator next.
	x2 _ self uniformRandomGenerator next.
	t1 _ (-2 * x1 ln) sqrt.
	t2 _ 2 * Float pi * x2.
	y1 _ t1 * t2 cos.
	y2 _ t1 * t2 sin.
	cachedValue _ y2.
	^ y1! !

!NormalProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:08'!
mean: mu standardDeviation: sigma
	^self basicNew mean: mu standardDeviation: sigma! !

!NormalProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:09'!
new
	^self mean: 0.0 standardDeviation: 1.0! !

!RayleighProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:21'!
nextRandomNumber
	"Answer the next value"

	^((1 / (1 - self uniformRandomGenerator next)) ln * 2 * sigma squared) sqrt! !

!RayleighProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:21'!
sigma: aNumber

	sigma _ aNumber! !

!RayleighProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:20'!
new
	^self sigma: 1.0! !

!RayleighProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:20'!
sigma: sigma
	^self basicNew sigma: sigma! !
