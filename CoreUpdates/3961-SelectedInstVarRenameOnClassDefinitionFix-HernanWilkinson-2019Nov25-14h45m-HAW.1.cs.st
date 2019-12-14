'From Cuis 5.0 [latest update: #3958] on 25 November 2019 at 2:49:34 pm'!

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 11/25/2019 14:47:40'!
contextualRenameInClassDefinitionOf: aClassDefinitionNode in: aSelectedClass 
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtSuperclass: cursorPosition) 
		ifTrue: [ ^self renameClassOn: self codeProvider for: analyzer superclass ].
	
	(analyzer isAtClassName: cursorPosition)
		ifTrue: [ ^self renameClassOn: self codeProvider for: aSelectedClass ].
		
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ |selection variableToRename|
			selection := self selectedString.
			variableToRename := selection isEmpty ifTrue: [ self wordUnderCursor ] ifFalse: [ selection ].
			^self renameInstanceVariableOn: self codeProvider for: variableToRename at: aSelectedClass ].
		
	(analyzer isAtCategory: cursorPosition)
		ifTrue: [ 
			"I'm sure codeProvider is a Browser - Hernan"
			^self codeProvider renameSystemCategory ].
		
	morph flash
	
! !

