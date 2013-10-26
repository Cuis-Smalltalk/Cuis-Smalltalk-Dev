'From Cuis 4.0 of 21 April 2012 [latest update: #1313] on 18 July 2012 at 3:42:52 pm'!
!classDefinition: #ExponentialRandom category: #'Kernel-Numbers'!
Random subclass: #ExponentialRandom
	instanceVariableNames: 'parameter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ExponentialRandom commentStamp: '<historical>' prior: 0!
My instances are random variables with Exponential distribution!


!NormalRandom commentStamp: '<historical>' prior: 0!
My instances are random variables with Normal distribution (mean 0 and variance 1), using the Box-Muller method!

!classDefinition: #RayleighRandom category: #'Kernel-Numbers'!
Random subclass: #RayleighRandom
	instanceVariableNames: 'parameter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!RayleighRandom commentStamp: '<historical>' prior: 0!
My instances are random variables with Rayleigh distribution!


!ExponentialRandom methodsFor: 'generation' stamp: 'JMV 7/3/2000 20:28'!
next
	"Answer the next value"

	^(1 / (1 - super next)) ln * 2 * parameter squared! !

!ExponentialRandom methodsFor: 'initialization' stamp: 'JMV 7/3/2000 20:24'!
parameter: aNumber

	^parameter _ aNumber! !


!ExponentialRandom class methodsFor: 'instance creation' stamp: 'JMV 7/3/2000 20:25'!
newWithParameter: aNumber

	^super new
		parameter: aNumber;
		yourself! !


!RayleighRandom methodsFor: 'initialization' stamp: 'JMV 7/3/2000 20:24'!
parameter: aNumber

	^parameter _ aNumber! !

!RayleighRandom methodsFor: 'generation' stamp: 'JMV 7/3/2000 20:28'!
next
	"Answer the next value"

	^((1 / (1 - super next)) ln * 2 * parameter squared) sqrt! !


!RayleighRandom class methodsFor: 'instance creation' stamp: 'JMV 7/3/2000 20:25'!
newWithParameter: aNumber

	^super new
		parameter: aNumber;
		yourself! !

