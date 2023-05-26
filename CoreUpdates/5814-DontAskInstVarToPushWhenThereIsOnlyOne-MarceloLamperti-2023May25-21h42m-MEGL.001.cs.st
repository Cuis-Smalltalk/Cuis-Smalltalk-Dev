'From Cuis 6.0 [latest update: #5812] on 25 May 2023 at 9:46:06 pm'!

!TransferInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 5/25/2023 21:43:20'!
selectVariableLabel
	
	self subclassResponsibility ! !


!TransferInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 5/25/2023 21:42:54'!
chooseInstanceVariable

	instanceVariableName ifNotNil: [ ^self ].

	classToRefactor instVarNames size = 1 ifTrue: [^instanceVariableName := classToRefactor instVarNames first].

	classToRefactor
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anInstanceVariable | ^instanceVariableName := anInstanceVariable ].

	self endRequest ! !

