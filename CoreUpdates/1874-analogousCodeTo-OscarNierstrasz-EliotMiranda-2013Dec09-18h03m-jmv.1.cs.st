'From Cuis 4.2 of 25 July 2013 [latest update: #1873] on 9 December 2013 at 6:06:20.906827 pm'!

!Association methodsFor: 'testing' stamp: 'jmv 12/3/2013 22:14'!
analogousCodeTo: anObject
"
From:  	Oscar Nierstrasz <oscar.nierstrasz@gmail.com>Add oscar.nierstrasz@gmail.com to my Address Book
Subject:  	[Pharo-dev] CompiledMethod>>#= is broken

The following code breaks:

(SortedCollectionTest>>#testDo) = (SortedCollectionTest>>#testStoreOn).

because it makes use of AdditionalMethodState>>#analogousCodeTo: which tries to send #analogousCodeTo: to a property which is an Association.

It seems to me the solution is to implement Association>>#analogousCodeTo: with an equality test:

analogousCodeTo: anObject
   ^self class == anObject class
     and: [ self = anObject ]

Does this make sense?

See:

https://pharo.fogbugz.com/f/cases/12077/MNU-in-AdditionalMethodState-analogousCodeTo

Oscar Nierstrasz

"
   ^self class == anObject class
     and: [ self = anObject ]! !


!AdditionalMethodState methodsFor: 'testing' stamp: 'eem 12/3/2013 11:42'!
analogousCodeTo: aMethodProperties
	| bs |
	self class == aMethodProperties class ifFalse:
		[^false].
	(bs := self basicSize) = aMethodProperties basicSize ifFalse:
		[^false].
	1 to: bs do:
		[:i|
		((self basicAt: i) analogousCodeTo: (aMethodProperties basicAt: i)) ifFalse:
			[^false]].
	^true! !

