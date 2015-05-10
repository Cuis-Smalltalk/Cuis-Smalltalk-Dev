'From Cuis 4.2 of 25 July 2013 [latest update: #2292] on 2 May 2015 at 4:26:21.930691 pm'!

!Random class methodsFor: 'cached state access' stamp: 'jmv 5/2/2015 16:25'!
releaseClassCachedState

	ConcreteSubclass _ nil.
	DefaultInstance _ nil.
	MutexForDefaultInstance _ nil! !


!Random class methodsFor: 'instance creation' stamp: 'jmv 5/2/2015 16:24'!
concreteRandomClass

	ConcreteSubclass ifNil: [
		ConcreteSubclass _ ParkMillerRandom ].
	^ConcreteSubclass! !

!Random class methodsFor: 'default instance' stamp: 'jmv 5/2/2015 16:25'!
withDefaultDo: aBlock
	"
	Random withDefaultDo: [ :r | r next ]
	"
	DefaultInstance ifNil: [
		DefaultInstance _ Random new.
		MutexForDefaultInstance _ Semaphore forMutualExclusion ].
	^MutexForDefaultInstance critical: [
		aBlock value: DefaultInstance ]! !

!methodRemoval: Random class #initClassState!
Random class removeSelector: #initClassState!
!methodRemoval: Random class #initialize!
Random class removeSelector: #initialize!
Random initialize!

!Random class reorganize!
('instance creation' concreteRandomClass new seed:)
('testing' bucketTest: theItsCompletelyBrokenTest)
('default instance' withDefaultDo:)
('class initialization')
('cached state access' releaseClassCachedState)
!

