'From Cuis 5.0 of 7 November 2016 [latest update: #3765] on 26 May 2019 at 3:47:55 pm'!

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/25/2019 00:38:49'!
sourceCodeWithoutAnyTemporaryDeclarations
	| firstPipeIndex secondPipeIndex |
	firstPipeIndex _ method sourceText findString: '|'.
	secondPipeIndex _ method sourceText findString: '|' startingAt: firstPipeIndex + 1.
	^method sourceText copyReplacing: {firstPipeIndex to: secondPipeIndex} with: ' ' ! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/25/2019 00:39:01'!
sourceCodeWithoutTemporaryDeclaration
	| temporaryVariablePositions variableDeclarationPosition | 
	temporaryVariablePositions _ method positionsForTemporaryVariable: variable ifAbsent: [].
	variableDeclarationPosition _ {temporaryVariablePositions first}.
	^method sourceText copyReplacing: variableDeclarationPosition with: ''! !


!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/25/2019 00:39:01'!
removeTemporary

	| newSourceCode |
	newSourceCode _ method temporaries size = 1 
		ifTrue: [ self sourceCodeWithoutAnyTemporaryDeclarations ]
		ifFalse: [ self sourceCodeWithoutTemporaryDeclaration].
	
	method methodClass compile: newSourceCode.

	^newSourceCode.
	! !


!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 5/26/2019 15:40:50'!
assertNoOtherMethodIn: aClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName

 	| methodsDefiningTemporaryInHierarchy |

	methodsDefiningTemporaryInHierarchy := OrderedCollection new.
	aClass withAllSubclassesDo: [ :subclass | 
		methodsDefiningTemporaryInHierarchy addAll: (subclass methodsWithArgumentOrTemporaryNamed: aTemporaryVariableName) ].

	methodsDefiningTemporaryInHierarchy size > 1 ifTrue: [
		self 
			canNotRefactorDueToReferencesError: self temporaryExistsInOtherMethodsErrorDescription
			references: (methodsDefiningTemporaryInHierarchy collect: [ :implementor | MethodReference method: implementor ])
			to: aTemporaryVariableName. ].! !


!TemporaryToInstanceVariable reorganize!
('initialization' initializeNamed:fromMethod:)
('applying' addInstanceVariable apply removeTemporary sourceCodeWithoutAnyTemporaryDeclarations sourceCodeWithoutTemporaryDeclaration)
!

