'From Cuis 5.0 [latest update: #3958] on 16 November 2019 at 1:10:59 pm'!

!NewInstanceVariablePrecondition methodsFor: 'evaluating' stamp: 'HAW 11/13/2019 20:46:26'!
valueForSuperclass
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefinedInSuperclasses.

	! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 11/13/2019 20:47:54'!
assertIsNotAlreadyDefinedInSuperclasses
	
	^ (classToAddInstVar classThatDefinesInstanceVariable: instVarName) 
		ifNotNil: [ :definingClasses | self signalAlreadyDefinedInAll: definingClasses ]
		! !

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 11/13/2019 20:43:43'!
assertIsNotDefinedInMethods
	
	| methodsDefiningNewVariable |
	
	methodsDefiningNewVariable := self methodsDefiningNewVariable.
	
	methodsDefiningNewVariable notEmpty ifTrue: [ self signalNewVariable: instVarName willBeHiddenAtAll: methodsDefiningNewVariable ].! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'HAW 11/16/2019 13:09:55'!
visitMessageNode: aMessageNode

	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do: [:argument| argument accept: self]! !


!NewInstanceVariablePrecondition methodsFor: 'evaluating' stamp: 'HAW 11/13/2019 20:43:43'!
value
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefined.

	self assertIsNotDefinedInMethods.! !

!methodRemoval: NewInstanceVariablePrecondition #assertIsDefinedInMethods stamp: 'HAW 11/13/2019 20:43:43'!
NewInstanceVariablePrecondition removeSelector: #assertIsDefinedInMethods!

!NewInstanceVariablePrecondition reorganize!
('initialization' initializeOf:for:)
('evaluating' value valueForSuperclass)
('pre-conditions' assertIsNotAReservedName assertIsNotAlreadyDefined assertIsNotAlreadyDefinedInSuperclasses assertIsNotDefinedInMethods assertIsNotEmpty assertIsValidInstanceVariableName)
('exceptions' signalAlreadyDefinedInAll: signalInvalidInstanceVariable signalNewInstanceVariableCanNotBeAReservedName signalNewVariableCanNotBeEmpty)
('warnings' methodsDefiningNewVariable signalNewVariable:willBeHiddenAtAll:)
!

