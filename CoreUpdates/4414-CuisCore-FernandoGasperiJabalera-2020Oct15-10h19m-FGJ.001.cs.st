'From Cuis 5.0 [latest update: #4360] on 15 October 2020 at 10:24:51 am'!

!ReturnNode methodsFor: 'testing' stamp: 'FGJ 10/15/2020 10:19:23'!
isImplicitSelfReturnIn: aMethodNode

	self isReturnSelf ifFalse: [^false]. 
	aMethodNode encoder rangeForNode: self ifAbsent: [^true].
	
	^false! !

