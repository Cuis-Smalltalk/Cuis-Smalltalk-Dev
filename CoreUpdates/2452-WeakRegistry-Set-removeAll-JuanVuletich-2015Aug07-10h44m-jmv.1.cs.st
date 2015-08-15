'From Cuis 4.2 of 25 July 2013 [latest update: #2449] on 7 August 2015 at 11:03:26.898594 am'!

!Set methodsFor: 'removing' stamp: 'jmv 8/7/2015 11:03'!
removeAll
	self init: array size! !


!WeakRegistry methodsFor: 'removing' stamp: 'jmv 8/7/2015 10:56'!
removeAll

	self protected: [
		valueDictionary removeAll ]! !

