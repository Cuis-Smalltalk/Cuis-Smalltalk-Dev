Class {
	#name : 'RenameTemporaryTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
RenameTemporaryTest >> classToRefactorName [

	^#ClassToRenameTemp
]

{ #category : 'class factory' }
RenameTemporaryTest >> methodNodeOf: aSourceCode [

	^self methodNodeOf: aSourceCode in: self class
]

{ #category : 'class factory' }
RenameTemporaryTest >> methodNodeOf: aSourceCode in: aClass [

	^aClass methodNodeFor: aSourceCode 
]

{ #category : 'tests' }
RenameTemporaryTest >> test01VariableToRenameHasToBeDefined [

	| newVariable oldVariable methodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1'.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ RenameTemporary errorMessageForTemporaryVariable: oldVariable notDefinedIn: methodNode ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test02NewVariableNameCanNotBeEmpty [

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := ' '.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test03NewVariableHasToBeValid [

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := 'a b'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	
]

{ #category : 'tests' }
RenameTemporaryTest >> test04NewVariableNameCanNotBeDefinedInMethod [

	| methodNode oldVariable |
	
	oldVariable := 'old'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: oldVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: oldVariable isAlreadyDefinedIn: methodNode ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test05FailsIfNewTemporaryIsEqualToInstanceVariableInClass [

	| methodNode oldVariable newVariable classToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test06FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass [

	| methodNode oldVariable newVariable classToRefactor superclassToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	superclassToRefactor := self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: superclassToRefactor.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test07RenameCreatesNewTemporaryVariableAndDeletesOldOne [

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable)
]

{ #category : 'tests' }
RenameTemporaryTest >> test08RenameChangesReferencesFromOldVariableToNewVariable [

	| methodNode oldVariable newVariable rename newMethodNode assigmentNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ', oldVariable, ' := 1 + ', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	assigmentNode := newMethodNode block statements first.
	self assert: newVariable equals: assigmentNode variable name.
	self assert: newVariable equals: assigmentNode value arguments first name.
]

{ #category : 'tests' }
RenameTemporaryTest >> test09RenamesArguments [

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1: ', oldVariable, ' ^', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	self assert: newVariable equals: newMethodNode block statements first expr name.
]

{ #category : 'tests' }
RenameTemporaryTest >> test10NewVariableNameCanNotBeDefinedAsBlockArgument [

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | [:', newVariable, ' | ^1 ]'.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]
]

{ #category : 'tests' }
RenameTemporaryTest >> test11RenamesBlockArguments [

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [:', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first arguments first name.
	"asserts it renamed reference to block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name
]

{ #category : 'tests' }
RenameTemporaryTest >> test12RenamesBlockTemporaries [

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [ |', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first temporaries first name.
	"asserts it renamed reference to block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name
]

{ #category : 'tests' }
RenameTemporaryTest >> test13OldNodeMustBePartOfMethodNode [

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	oldVariableNode := methodNode tempNodes anyOne.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode copy to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeNotPartOfMethodNodeErrorDescription ]	
]

{ #category : 'tests' }
RenameTemporaryTest >> test14OldVariableNodeMustBeArgOrTempNode [

	| methodNode newVariable oldVariableNode |
	
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 self'.
	oldVariableNode := methodNode block statements first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeMustBeArgOrTempNodeErrorDescription ]	
]

{ #category : 'tests' }
RenameTemporaryTest >> test15RenameVariablesWithSameNameInDifferentBlocksCorrectly [

	| methodNode oldVariable newVariable oldVariableNode newMethodNode rename |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',oldVariable,'| ^',oldVariable, '].[|',oldVariable,'| ^',oldVariable, '].'.
	oldVariableNode := methodNode block statements first temporaries first.
	
	rename := RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self 
		assert: 'm1 [|',newVariable,'| ^',newVariable, '].[|',oldVariable,'| ^',oldVariable, '].'
		equals: newMethodNode sourceText
]

{ #category : 'tests' }
RenameTemporaryTest >> test16CantRenameVariableToANameAlreadyUsedInAnUpperBlock [

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',newVariable,'| [|',oldVariable,'| ^', oldVariable,']. ^',newVariable,' ]'.
	oldVariableNode := methodNode block statements first statements first temporaries first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [
			NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]
	
]

{ #category : 'tests' }
RenameTemporaryTest >> test17RenamesOneCharVariableAfterAReturnWithoutCharsAfterThatCorrectly [

	| methodNode oldVariable newVariable rename newSource |
	
	"This test is due to a fixed error in the parser. 
	See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat"
	
	oldVariable := 'o'.
	newVariable := 'n'.

	methodNode := self methodNodeOf: 'm1 |',oldVariable,'| ^',oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newSource := rename apply.
	
	self 
		assert: 'm1 |',newVariable,'| ^',newVariable
		equals: newSource
		
]

{ #category : 'tests' }
RenameTemporaryTest >> test18CantRenameVariableToAReservedName [

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	
	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		methodNode := self methodNodeOf: 'm1 |' , oldVariable , '| ^ ' , oldVariable.

		self 
			assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]
]
