'From Cuis 6.0 [latest update: #6073] on 17 November 2023 at 10:03:50 am'!

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/17/2023 10:02:34'!
markIndex: anIndex pointIndex: anotherIndex
	"Called, for example, when selecting text with shift+arrow keys"
	self
		markIndex: anIndex;
		pointIndex: anotherIndex! !

