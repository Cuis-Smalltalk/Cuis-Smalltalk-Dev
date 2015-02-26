'From Cuis 4.2 of 25 July 2013 [latest update: #2177] on 19 February 2015 at 10:51:23.354494 am'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 2/19/2015 10:50'!
select: aBlock 
	"Refer to the comment in Collection>>select: ."
	^self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			(aBlock value: (self at: index))
				ifTrue: [ strm nextPut: (self at: index) ]]]! !

