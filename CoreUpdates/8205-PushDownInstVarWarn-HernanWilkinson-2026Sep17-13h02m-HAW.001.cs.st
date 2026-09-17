'From Cuis7.9 [latest update: #8184] on 17 September 2026 at 1:16:29 pm'!

!TransferInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 4:07:58 pm (UTC)'!
removeInstanceVariableFrom: aClass

	"The class builder does not let a class declare the variable while another of its hierarchy does,
	so the methods using it are compiled with it undeclared until declaring it where it goes compiles
	them again. Resuming with false keeps the name out of Undeclared."

	[ aClass removeInstVarName: instanceVariableToMove ]
		on: UndeclaredVariableWarning
		do: [ :warning |
			warning variableName = instanceVariableToMove
				ifTrue: [ warning resume: false ]
				ifFalse: [ warning pass ]]! !


!PushDownInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 4:07:58 pm (UTC)'!
removeInstanceVariable

	self removeInstanceVariableFrom: classToRefactor! !


!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 4:07:58 pm (UTC)'!
removeInstanceVariableFromAllSubclasses

	rootClass allSubclasses
		select: [ :subclass | subclass definesInstanceVariableNamed: instanceVariableToMove ]
		thenDo: [ :subclass |
			self removeInstanceVariableFrom: subclass.
			changes add: subclass ]! !

!methodRemoval: PushUpInstanceVariable #removeInstanceVariableFrom: stamp: 'HAW 17/Sep/2026, 4:07:58 pm (UTC)'!
PushUpInstanceVariable removeSelector: #removeInstanceVariableFrom:!
