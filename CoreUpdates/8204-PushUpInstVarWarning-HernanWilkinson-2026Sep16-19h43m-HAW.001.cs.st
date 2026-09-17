'From Cuis7.9 [latest update: #8184] on 17 September 2026 at 1:02:32 pm'!

!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 3:10:42 pm (UTC)'!
removeInstanceVariableFrom: aSubclass

	"The class builder does not let the root class declare the variable while a subclass does, so
	the methods using it are compiled with it undeclared until adding it to the root class compiles
	them again. Resuming with false keeps the name out of Undeclared."

	[ aSubclass removeInstVarName: instanceVariableToMove ]
		on: UndeclaredVariableWarning
		do: [ :warning |
			warning variableName = instanceVariableToMove
				ifTrue: [ warning resume: false ]
				ifFalse: [ warning pass ]].
	changes add: aSubclass! !


!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 3:10:42 pm (UTC)'!
removeInstanceVariableFromAllSubclasses

	rootClass allSubclasses
		select: [ :subclass | subclass definesInstanceVariableNamed: instanceVariableToMove ]
		thenDo: [ :subclass | self removeInstanceVariableFrom: subclass ]! !

