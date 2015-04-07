'From Cuis 4.2 of 25 July 2013 [latest update: #2222] on 4 April 2015 at 9:10:04.470735 pm'!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: 'seed a m q r '
	classVariableNames: 'ConcreteSubclass MutexForDefaultInstance DefaultInstance '
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
!classDefinition: #ParkMillerRandom category: #'Kernel-Numbers'!
Random subclass: #ParkMillerRandom
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Random class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:12'!
concreteRandomClass
	^ConcreteSubclass! !

!Random class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:01'!
new
	^self concreteRandomClass basicNew initialize! !

!Random class methodsFor: 'default instance' stamp: 'jmv 4/4/2015 20:24'!
withDefaultDo: aBlock
	"
	Random withDefaultDo: [ :r | r next ]
	"
	^MutexForDefaultInstance critical: [
		aBlock value: DefaultInstance ]! !

!Random class methodsFor: 'class initialization' stamp: 'jmv 4/4/2015 20:20'!
initialize
	"Set up a Random number generator to be used by atRandom when the 
	user does not feel like creating his own Random generator."
	"
	Random initialize
	"
	ConcreteSubclass _ ParkMillerRandom.
	DefaultInstance _ Random new.
	MutexForDefaultInstance _ Semaphore forMutualExclusion! !


!Random class methodsFor: 'testing' stamp: 'jmv 4/4/2015 20:01'!
bucketTest: randy
	"Execute this:
	Random bucketTest: Random new
	"
	" A quick-and-dirty bucket test. Prints nbuckets values on the Transcript.
	  Each should be 'near' the value of ntries. Any run with any value 'far' from ntries
	  indicates something is very wrong. Each run generates different values.
	  For a slightly better test, try values of nbuckets of 200-1000 or more; go get coffee.
	  This is a poor test; see Knuth.   Some 'OK' runs:
		1000 1023 998 969 997 1018 1030 1019 1054 985 1003
		1011 987 982 980 982 974 968 1044 976
		1029 1011 1025 1016 997 1019 991 954 968 999 991
		978 1035 995 988 1038 1009 988 993 976
"
	| nbuckets buckets ntrys slot |
	nbuckets := 20.
	buckets := Array new: nbuckets.
	buckets atAllPut: 0.
	ntrys :=  100.
	ntrys*nbuckets timesRepeat: [
		slot := (randy next * nbuckets) floor + 1.
		buckets at: slot put: (buckets at: slot) + 1 ].
	Transcript newLine.
	1 to: nbuckets do: [ :nb |
		Transcript show: (buckets at: nb) printString, ' ' ]! !

!Random class methodsFor: 'testing' stamp: 'jmv 4/4/2015 20:02'!
theItsCompletelyBrokenTest
	"
	Random theItsCompletelyBrokenTest
	"
	"The above should print as...
	(0.149243269650845 0.331633021743797 0.75619644800024 0.393701540023881 0.941783181364547 0.549929193942775 0.659962596213428 0.991354559078512 0.696074432551896 0.922987899707159 )
	If they are not these values (accounting for precision of printing) then something is horribly wrong: DO NOT USE THIS CODE FOR ANYTHING. "
	| rng |
	rng := Random new.
	rng seed: 2345678901.
	^ (1 to: 10) collect: [:i | rng next]! !

Random initialize!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: 'seed a m q r'
	classVariableNames: 'ConcreteSubclass DefaultInstance MutexForDefaultInstance'
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Random reorganize!
('accessing' next nextInt:)
('initialization' initialize seed:)
('private' nextValue seed)
!

