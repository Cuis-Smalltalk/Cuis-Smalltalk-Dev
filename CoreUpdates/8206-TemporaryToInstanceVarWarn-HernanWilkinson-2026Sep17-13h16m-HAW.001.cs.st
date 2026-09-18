'From Cuis7.9 [latest update: #8184] on 17 September 2026 at 6:51:42 pm'!

!Refactoring methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 9:47:27 pm (UTC)'!
ignoreUndeclared: aVariableName during: aBlock

	"For a refactoring that cannot declare a variable before it compiles the methods using it, which the
	declaration compiles again. Resuming with false keeps the name out of Undeclared."

	^aBlock
		on: UndeclaredVariableWarning
		do: [ :warning |
			warning variableName = aVariableName
				ifTrue: [ warning resume: false ]
				ifFalse: [ warning pass ]]! !


!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'HAW 17/Sep/2026, 9:47:27 pm (UTC)'!
apply

	| newSourceCode |

	self
		ignoreUndeclared: variable
		during: [
			newSourceCode := self removeTemporary.
			self addInstanceVariable ].
	
	^newSourceCode! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'HAW 17/Sep/2026, 4:30:59 pm (UTC)'!
removeTemporary

	| remover |
	
	remover := TemporaryVariableDeclarationRemover in: method for: variable. 
	method accept: remover.
	^remover newSourceCode! !


!TransferInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 17/Sep/2026, 9:47:27 pm (UTC)'!
removeInstanceVariableFrom: aClass

	self
		ignoreUndeclared: instanceVariableToMove
		during: [ aClass removeInstVarName: instanceVariableToMove ].! !
