'From Cuis 4.2 of 25 July 2013 [latest update: #2120] on 5 December 2014 at 11:28:32.111368 am'!

!Set methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:02'!
postCopy

	array _ array copy! !


!Dictionary methodsFor: 'private' stamp: 'jmv 12/5/2014 11:02'!
postCopy
	"Must copy the associations, or later store will effect both the
original and the copy"

	array _ (array collect: [:assoc |
		assoc ifNil: [nil]
			ifNotNil: [Association key: assoc key value: assoc value]])! !

!methodRemoval: Dictionary #copy!
Dictionary removeSelector: #copy!
!methodRemoval: Set #copy!
Set removeSelector: #copy!
