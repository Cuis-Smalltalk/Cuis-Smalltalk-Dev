'From Cuis 5.0 of 7 November 2016 [latest update: #3389] on 3 August 2018 at 11:45:47 am'!

!Browser methodsFor: 'class list' stamp: 'HAW 8/3/2018 11:45:20'!
selectedClassName
	"Answer the name of the current class. Answer nil if no selection exists."

	^selectedClassName ifNotNil: [ 
		"I send #defaultClassList and no #classList because when showing classes hierarchically we should remove spaces to see
		if class name is in the list and that consumes more time - Hernan"
		(self defaultClassList includes: selectedClassName) ifTrue: [ selectedClassName ]]! !

