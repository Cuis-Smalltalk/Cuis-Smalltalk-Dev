'From Cuis 5.0 [latest update: #3976] on 17 December 2019 at 5:20:19 pm'!

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 12/17/2019 17:09:06'!
declareBlockTempAction
	
	^[ parser declareTemp: name at: #block ]! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 12/17/2019 17:09:53'!
declareInstanceVariableAction

	^[ parser declareInstVar: name ]! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 12/17/2019 17:09:25'!
declareMethodTempAction
	
	^[ parser declareTemp: name at: #method ].! !


!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 12/17/2019 17:09:43'!
addLocalVariableOptionsTo: labels actions: actions

	labels add: 'declare block-local temp'.
	actions add: self declareBlockTempAction.
			
	labels add: 'declare method temp'.
	actions add: self declareMethodTempAction.
			
	parser canDeclareInstanceVariable ifTrue: [
		labels add: 'declare instance'.
		actions add: self declareInstanceVariableAction]! !

