'From Cuis 5.0 of 7 November 2016 [latest update: #3254] on 19 January 2018 at 10:28:10 am'!

!Collection methodsFor: 'enumerating' stamp: 'jmv 1/19/2018 08:57:00'!
select: selectBlock thenDo: doBlock
	"Equivalent to 
		(self select: selectBlock) do: doBlock
	but avoid creating an extra collection."

	self do: [ :each | (selectBlock value: each) ifTrue: [ doBlock value ]]! !

