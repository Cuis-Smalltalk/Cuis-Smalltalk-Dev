'From Cuis 5.0 of 7 November 2016 [latest update: #3589] on 4 February 2019 at 11:11:35 pm'!

!Collection methodsFor: 'enumerating' stamp: 'AY 2/4/2019 23:11:18'!
select: selectBlock thenDo: doBlock
	"Equivalent to 
		(self select: selectBlock) do: doBlock
	but avoid creating an extra collection."

	self do: [ :each | (selectBlock value: each) ifTrue: [ doBlock value: each ]]! !

