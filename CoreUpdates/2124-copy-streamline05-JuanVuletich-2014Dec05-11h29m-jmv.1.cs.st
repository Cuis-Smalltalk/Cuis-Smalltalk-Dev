'From Cuis 4.2 of 25 July 2013 [latest update: #2120] on 5 December 2014 at 11:36:08.143368 am'!

!Bag methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:10'!
postCopy

	contents _ contents copy! !


!Heap methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:36'!
copy

	| answer |
	answer _ self species sortBlock: sortBlock.
	answer  addAll: self.
	^ answer! !


!Interval methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:33'!
copy
	"Answer another instance just like the receiver."

	^self shallowCopy postCopy! !

!methodRemoval: Interval #shallowCopy!
Interval removeSelector: #shallowCopy!

!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('enumerating' collect: do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:by: species valuesInclude:)
('testing' includes: isInterval)
('arithmetic' + -)
('copying' copy)
!

!methodRemoval: Heap #postCopy!
Heap removeSelector: #postCopy!
!methodRemoval: Bag #copy!
Bag removeSelector: #copy!
