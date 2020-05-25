'From Cuis 5.0 [latest update: #4167] on 23 May 2020 at 5:08:44 pm'!

!Inspector methodsFor: 'accessing' stamp: 'sqr 5/23/2020 17:05:55'!
object: anObject 
	"Set anObject to be the object being inspected by the receiver."

	| oldIndex |
	anObject == object ifTrue: [^self update].
	oldIndex := self selectionIsUnmodifiable ifTrue: [selectionIndex] ifFalse: [0].
	self inspect: anObject.
	oldIndex := oldIndex min: self fieldList size.
	self changed: #inspectObject.
	oldIndex > 0 ifTrue: [self toggleIndex: oldIndex].
	self changed: #fieldList.
	self acceptedContentsChanged! !

!Inspector methodsFor: 'accessing' stamp: 'sqr 5/23/2020 17:06:28'!
update
	"Reshow contents, assuming selected value may have changed."

	selectionIndex = 0 ifFalse: [self changedSelectionIndex]! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/23/2020 17:06:21'!
toggleIndex: anInteger
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. If anInteger is the index of this variable, then deselect it. 
	Otherwise, make the variable whose index is anInteger be the selected 
	item."

	selectionIndex := selectionIndex = anInteger ifTrue: [0] ifFalse: [anInteger].
	self changedSelectionIndex! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/23/2020 17:05:06'!
changedSelectionIndex

	acceptedContentsCache _ self selectionPrintString.
	self acceptedContentsChanged.
	self changed: #selectionIndex! !

