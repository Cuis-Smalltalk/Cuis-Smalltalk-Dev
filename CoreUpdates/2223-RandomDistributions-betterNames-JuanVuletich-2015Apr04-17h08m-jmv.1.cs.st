'From Cuis 4.2 of 25 July 2013 [latest update: #2219] on 4 April 2015 at 5:13:00.947655 pm'!
!classDefinition: #ExponentialRandom category: #'Kernel-Numbers'!
Random subclass: #ExponentialRandom
	instanceVariableNames: 'parameter lambda '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
!classDefinition: #RayleighRandom category: #'Kernel-Numbers'!
Random subclass: #RayleighRandom
	instanceVariableNames: 'parameter sigma '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ExponentialRandom methodsFor: 'initialization' stamp: 'jmv 4/3/2015 23:02'!
lambda: aNumber

	^lambda _ aNumber! !


!ExponentialRandom class methodsFor: 'instance creation' stamp: 'jmv 4/3/2015 23:02'!
lambda: aNumber

	^self new
		lambda: aNumber;
		yourself! !


!RayleighRandom methodsFor: 'initialization' stamp: 'jmv 4/3/2015 23:04'!
sigma: aNumber

	^sigma _ aNumber! !


!RayleighRandom class methodsFor: 'instance creation' stamp: 'jmv 4/3/2015 23:04'!
sigma: aNumber

	^self new
		sigma: aNumber;
		yourself! !


!ExponentialRandom methodsFor: 'generation' stamp: 'jmv 4/3/2015 23:06'!
next
	"Answer the next value"

	^(1 / (1 - super next)) ln * 2 * lambda squared! !


!RayleighRandom methodsFor: 'generation' stamp: 'jmv 4/3/2015 23:04'!
next
	"Answer the next value"

	^((1 / (1 - super next)) ln * 2 * sigma squared) sqrt! !

!methodRemoval: RayleighRandom class #newWithParameter:!
RayleighRandom class removeSelector: #newWithParameter:!
!methodRemoval: RayleighRandom #parameter:!
RayleighRandom removeSelector: #parameter:!
!classDefinition: #RayleighRandom category: #'Kernel-Numbers'!
Random subclass: #RayleighRandom
	instanceVariableNames: 'sigma'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
!methodRemoval: ExponentialRandom class #newWithParameter:!
ExponentialRandom class removeSelector: #newWithParameter:!
!methodRemoval: ExponentialRandom #parameter:!
ExponentialRandom removeSelector: #parameter:!
!classDefinition: #ExponentialRandom category: #'Kernel-Numbers'!
Random subclass: #ExponentialRandom
	instanceVariableNames: 'lambda'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ExponentialRandom reorganize!
('initialization' lambda:)
('generation' next)
!

