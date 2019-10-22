'From Cuis 5.0 [latest update: #3913] on 18 October 2019 at 6:43:17 pm'!

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/18/2019 18:39:55'!
logClassDefinition
		
	Smalltalk
		logChange: classToRefactor definition 
		preamble: classToRefactor definitionPreamble.! !


!RenameInstanceVariable methodsFor: 'applying' stamp: 'HAW 10/18/2019 18:40:03'!
apply

	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		logClassDefinition;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		! !


!RenameInstanceVariable reorganize!
('applying - private' changeInstanceVariableName keepMethodToChangeNamed:in: logClassDefinition lookForMethodsReferencingOldVariable lookForMethodsReferencingOldVariableIn: newSourceOf: renameReferencesToOldVariable renameReferencesToOldVariableInMethod:)
('applying' apply)
('initialization' initializeFrom:to:in:)
!

