'From Cuis 4.2 of 25 July 2013 [latest update: #2334] on 24 May 2015 at 9:51:54.376438 am'!

!Object methodsFor: 'finalization' stamp: 'SqR 5/23/2015 17:04'!
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	^self! !


!ObjectFinalizer methodsFor: 'finalization' stamp: 'SqR 5/23/2015 17:05'!
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	[receiver perform: selector withArguments: arguments] 
		on: Error do:[:ex| ex return].
! !

