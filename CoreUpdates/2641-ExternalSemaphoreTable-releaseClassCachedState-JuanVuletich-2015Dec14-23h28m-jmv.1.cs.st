'From Cuis 4.2 of 25 July 2013 [latest update: #2636] on 14 December 2015 at 11:36:22.186492 pm'!

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:34'!
accessProtect
	ProtectTable ifNil: [
		ProtectTable _ Semaphore forMutualExclusion ].
	^ProtectTable! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:35'!
releaseClassCachedState

	ProtectTable _ nil! !


!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:34'!
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."

	self accessProtect critical: [Smalltalk specialObjectsArray at: 39 put: Array new].
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:34'!
externalObjects
	^self accessProtect critical: [Smalltalk specialObjectsArray at: 39]! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:34'!
registerExternalObject: anObject
	^ self accessProtect critical: [self safelyRegisterExternalObject: anObject]
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:35'!
unregisterExternalObject: anObject
	self accessProtect critical: [self safelyUnregisterExternalObject: anObject]
! !

!methodRemoval: ExternalSemaphoreTable class #initialize!
ExternalSemaphoreTable class removeSelector: #initialize!
ExternalSemaphoreTable initialize!
