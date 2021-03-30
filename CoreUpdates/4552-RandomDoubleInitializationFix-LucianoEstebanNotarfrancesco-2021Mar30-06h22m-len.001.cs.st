'From Cuis 5.0 [latest update: #4551] on 30 March 2021 at 6:33:01 am'!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: ''
	classVariableNames: 'ConcreteSubclass DefaultInstance MutexForDefaultInstance '
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Random class methodsFor: 'instance creation' stamp: 'len 3/30/2021 06:24:49'!
new
	^ self seed: Time millisecondClockValue! !

!Random class methodsFor: 'instance creation' stamp: 'len 3/30/2021 06:26:50'!
seed: anInteger
	^ (self = Random ifTrue: [ParkMiller93Random] ifFalse: [self]) basicNew seed: anInteger! !

!Random class methodsFor: 'cached state access' stamp: 'len 3/30/2021 06:27:11'!
releaseClassCachedState
	DefaultInstance _ nil.
	MutexForDefaultInstance _ nil! !


!LaggedFibonacciRandom methodsFor: 'private' stamp: 'len 3/30/2021 06:31:16'!
seed: anInteger
	ring isNil ifTrue: [ring _ self newRing].
	self last: 1.
	self initializeRingWith: (ParkMiller93Random seed: anInteger)! !

!methodRemoval: ParkMiller88Random #initialize stamp: 'len 3/30/2021 06:28:51'!
ParkMiller88Random removeSelector: #initialize!
!methodRemoval: LaggedFibonacciRandom #initialize stamp: 'len 3/30/2021 06:31:30'!
LaggedFibonacciRandom removeSelector: #initialize!
!methodRemoval: Random class #concreteRandomClass stamp: 'len 3/30/2021 06:26:59'!
Random class removeSelector: #concreteRandomClass!
!methodRemoval: Random class #newDefault stamp: 'len 3/30/2021 06:25:46'!
Random class removeSelector: #newDefault!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: ''
	classVariableNames: 'DefaultInstance MutexForDefaultInstance'
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
