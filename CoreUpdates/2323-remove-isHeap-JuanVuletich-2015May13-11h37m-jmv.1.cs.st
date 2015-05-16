'From Cuis 4.2 of 25 July 2013 [latest update: #2322] on 13 May 2015 at 11:37:31.944023 am'!

!Heap methodsFor: 'comparing' stamp: 'jmv 5/13/2015 11:36'!
= otherCollection

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].

	sortBlock = otherCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: otherCollection ! !

!methodRemoval: Heap #isHeap!
Heap removeSelector: #isHeap!
!methodRemoval: Object #isHeap!
Object removeSelector: #isHeap!
