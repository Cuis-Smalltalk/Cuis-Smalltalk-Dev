'From Cuis 5.0 of 7 November 2016 [latest update: #3779] on 31 May 2019 at 11:34:28 am'!

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/31/2019 11:33:02'!
sendersFrom: methodReferences

	senders := methodReferences collect: [:aMethodReference | aMethodReference compiledMethod ]! !

