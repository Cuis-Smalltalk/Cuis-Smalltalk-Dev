'From Cuis7.1 [latest update: #6784] on 18 October 2024 at 11:52:56 am'!

!Collection methodsFor: 'enumerating' stamp: 'jfr 10/17/2024 17:14:50'!
associationsDo: elementBlock separatedBy: separatorBlock
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst := true.
	self associationsDo: [ :element |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]! !

