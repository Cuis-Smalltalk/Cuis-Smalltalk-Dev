'From Cuis 5.0 of 7 November 2016 [latest update: #3751] on 19 May 2019 at 5:39:14 pm'!

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'HAW 5/19/2019 17:27:10'!
temporaryToInstanceVariable

	self 
		withNodeUnderCursorDo: [ :nodeUnderCursor |
			nodeUnderCursor isTemp 
				ifTrue: [ TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name :: value ] 
				ifFalse: [ morph flash ]] 
		ifAbsent: [ morph flash ].! !

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'HAW 5/19/2019 17:38:26'!
temporaryToInstanceVariable: aKeyboardEvent

	self temporaryToInstanceVariable.

	^true.! !

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'HAW 5/19/2019 17:27:59'!
withNodeUnderCursorDo: aDoBlock ifAbsent: anAbsentBlock

	self 
		withMethodNodeAndClassDo: [ :currentMethodNode :currentClass |
			currentMethodNode 
				withParseNodeIncluding: self startIndex
				do: aDoBlock
				ifAbsent: anAbsentBlock ] 
		ifErrorsParsing: [ :arg1 | anAbsentBlock value ].! !


!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'HAW 5/19/2019 17:26:03'!
addInstanceVariable

	AddInstanceVariable named: variable to: method methodClass :: apply.! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'HAW 5/19/2019 17:25:59'!
apply

	| newSourceCode |

	newSourceCode _ self removeTemporary.
	self addInstanceVariable.
	
	^newSourceCode.! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'HAW 5/19/2019 17:26:16'!
removeTemporary

	| temporaryVariablePositions newSourceCode variableDeclarationPosition |

	temporaryVariablePositions _ method positionsForTemporaryVariable: variable ifAbsent: [].
	variableDeclarationPosition _ {temporaryVariablePositions first}.
	newSourceCode _ method sourceText copyReplacing: variableDeclarationPosition with: ''.
	method methodClass compile: newSourceCode.

	^newSourceCode.
	! !


!TemporaryToInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 5/19/2019 17:23:35'!
named: aTemporaryVariableName fromMethod: aMethodNode

	| methodClass |

	methodClass _ aMethodNode methodClass.
	
	self assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName.
	self assertNoOtherMethodIn: methodClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName. 
	self assertSubclassesOf: methodClass haventGotInstanceVariableNamed: aTemporaryVariableName.
	
	^self new initializeNamed: aTemporaryVariableName fromMethod: aMethodNode.! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 5/19/2019 17:23:40'!
inexistentTemporaryErrorDescription

	^'The temporary variable does not exist.'! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 5/19/2019 17:23:44'!
temporaryExistsAsInstVarInSubclassesErrorDescription

	^'The temporary variable exists as an instance variable in a subclass.'! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 5/19/2019 17:23:47'!
temporaryExistsInOtherMethodsErrorDescription

	^'Temporary variable exists in other methods; remove those first.'! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'HAW 5/19/2019 17:23:56'!
assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName

	aMethodNode hasLocalNamed: aTemporaryVariableName :: ifFalse: [
		self refactoringError: self inexistentTemporaryErrorDescription ].! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'HAW 5/19/2019 17:24:11'!
assertNoOtherMethodIn: aClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName

 	| methodsDefiningTemporaryInHierarchy |

	methodsDefiningTemporaryInHierarchy := OrderedCollection new.
	aClass withAllSubclassesDo: [ :subclass | 
		methodsDefiningTemporaryInHierarchy addAll: (subclass methodsWithArgumentOrTemporaryNamed: aTemporaryVariableName) ].

	methodsDefiningTemporaryInHierarchy size > 1 ifTrue: [
		self refactoringError: self temporaryExistsInOtherMethodsErrorDescription ].! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'HAW 5/19/2019 17:24:43'!
assertSubclassesOf: aClass haventGotInstanceVariableNamed: anInstanceVariableName
	
	aClass allSubclassesDo: [ :subclass | 
		subclass instVarNames includes: anInstanceVariableName :: ifTrue: [
			self refactoringError: self temporaryExistsAsInstVarInSubclassesErrorDescription ]].! !


!TemporaryToInstanceVariableApplier methodsFor: 'initialization' stamp: 'HAW 5/19/2019 17:28:47'!
initializeOn: aSmalltalkEditor for: aTemporaryVariableName

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := classToRefactor methodNodeFor: smalltalkEditor actualContents string.
	variableName := aTemporaryVariableName 
	! !

!TemporaryToInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 5/19/2019 17:26:37'!
createRefactoring

	^TemporaryToInstanceVariable named: variableName fromMethod: methodNode.! !

!TemporaryToInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 5/19/2019 17:29:06'!
showChanges

	smalltalkEditor actualContents: changes.
	! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 5/19/2019 17:37:43'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 	-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 5/19/2019 17:38:03'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor')
		#(		$O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable'))
! !


!TemporaryToInstanceVariableApplier class reorganize!
('instance creation' on:for:)
!

