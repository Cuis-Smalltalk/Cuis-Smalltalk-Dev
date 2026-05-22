Class {
	#name : 'RenameObject',
	#superclass : 'Refactoring',
	#instVars : [
		'system',
		'newName',
		'oldName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'accessing' }
RenameObject >> newName [

	^newName 
]

{ #category : 'applying - private' }
RenameObject >> newSourceCodeOf: aCompiledMethod [

	| ranges newSource |
	ranges := [ self rangesToReplaceOf: aCompiledMethod ]
		on: UndeclaredVariableWarning
		do: [ :ex |
			ex variableName = oldName
				ifTrue: [
					"Ignore the UndeclaredVariableWarning due to the fact that we have
					already renamed a class / global, so the old code will always have 
					references to now invalid names, exactly those we are updating."
					ex resume ]
				ifFalse: [ ex pass ]].
	newSource := aCompiledMethod sourceCode copyReplacing: ranges with: newName.
	^newSource
]

{ #category : 'applying - private' }
RenameObject >> rangesForLiteralOf: methodNode [

	| literalRanges |

	literalRanges := methodNode positionsForLiteralNode: oldName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].

	^literalRanges 
]

{ #category : 'applying - private' }
RenameObject >> rangesForLiteralVariableOf: methodNode [

	^methodNode positionsForLiteralVariableNode: oldName ifAbsent: [ #() ]
	
]

{ #category : 'applying - private' }
RenameObject >> rangesToReplaceOf: aCompiledMethod [

	| methodNode ranges |

	methodNode := aCompiledMethod notOptimizedMethodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].

	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).

	^ranges 
]

{ #category : 'applying - private' }
RenameObject >> references: aMethodReference classVarNamed: aName [

	^(aMethodReference actualClass theNonMetaClass definesClassVariableNamedInHierarchy: aName)
		or: [ aMethodReference actualClass theMetaClass definesClassVariableNamedInHierarchy: aName ]
]

{ #category : 'applying - private' }
RenameObject >> referencesOldName: aMethodReference [

	^self references: aMethodReference classVarNamed: oldName
]

{ #category : 'accessing' }
RenameObject >> referencesToOldName [

	^system allCallsOn: oldName
]

{ #category : 'applying - private' }
RenameObject >> referencesToRename [

	self subclassResponsibility
]

{ #category : 'applying - private' }
RenameObject >> renameReference: aReferencingMethod [

	| newSource |

	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource 
]

{ #category : 'applying - private' }
RenameObject >> renameReferences [

	| references |

	references := self referencesToRename.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	"See #newSourceCodeOf: . These spurious undeclared entries may be a problem later."
	Smalltalk cleanOutUndeclared.
	^references
]
