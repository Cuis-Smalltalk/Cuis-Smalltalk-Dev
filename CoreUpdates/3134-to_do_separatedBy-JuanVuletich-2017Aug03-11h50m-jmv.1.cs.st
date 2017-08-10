'From Cuis 5.0 of 7 November 2016 [latest update: #3133] on 3 August 2017 at 12:49:11 pm'!

!Number methodsFor: 'intervals' stamp: 'jmv 8/3/2017 11:57:08'!
to: stop do: elementBlock separatedBy: separatorBlock
	"
	String streamContents: [ :strm |
		1 to: 10 do: [ :i | i printOn: strm ] separatedBy: [ strm nextPutAll: ' -- ' ]]
	"
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst _ true.
	self to: stop do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst _ false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]! !

