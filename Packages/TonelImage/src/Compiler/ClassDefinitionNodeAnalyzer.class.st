Class {
	#name : 'ClassDefinitionNodeAnalyzer',
	#superclass : 'Object',
	#instVars : [
		'classDefinitionNode',
		'classCreationMessageNode',
		'superClassNode'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'parameters positions' }
ClassDefinitionNodeAnalyzer class >> categoryPosition [
	
	^5
]

{ #category : 'instance creation' }
ClassDefinitionNodeAnalyzer class >> for: aClassDefinitionMethodNode  [
	
	^self new initializeFor: aClassDefinitionMethodNode 
]

{ #category : 'parameters positions' }
ClassDefinitionNodeAnalyzer class >> instanceVariableNamesPositionForClassDefinition [

	^2
]

{ #category : 'parameters positions' }
ClassDefinitionNodeAnalyzer class >> instanceVariableNamesPositionForMetaclassDefinition [

	^1
]

{ #category : 'initialization' }
ClassDefinitionNodeAnalyzer >> initializeFor: aClassDefinitionMethodNode  [

	classDefinitionNode := aClassDefinitionMethodNode.
	classCreationMessageNode := classDefinitionNode block statements first expr.
	superClassNode := classCreationMessageNode receiver.

]

{ #category : 'parameters positions' }
ClassDefinitionNodeAnalyzer >> instanceVariableNamesPosition [
	
	^self isClassDefinition ifTrue: [ self class instanceVariableNamesPositionForClassDefinition ] ifFalse: [ self class instanceVariableNamesPositionForMetaclassDefinition ]
]

{ #category : 'testing - private' }
ClassDefinitionNodeAnalyzer >> is: anIndex atStringParameterNumber: aParameterPosition [

	| parameterRange |
	
	parameterRange := (classDefinitionNode rangeForNode: (classCreationMessageNode arguments at: aParameterPosition) ifAbsent: [ ^ false ]) first.
	
	^anIndex between: parameterRange first + 1 and: parameterRange last - 1
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtCategory: anIndex [

	^self isClassDefinition and: [ self is: anIndex atStringParameterNumber: self class categoryPosition ]
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtClassName: anIndex [

	^self isClassDefinition and: [ self isAtClassNameInClassDefinition: anIndex ]
	
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtClassNameInClassDefinition: anIndex [

	^(classDefinitionNode rangeForNode: classCreationMessageNode arguments first ifAbsent: [ ^ false ]) first includes: anIndex
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtInstanceVariables: anIndex [

	^self is: anIndex atStringParameterNumber: self instanceVariableNamesPosition
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtSuperclass: anIndex [

	^self isClassDefinition and: [ self isAtSuperclassInClassDefinition: anIndex ]
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isAtSuperclassInClassDefinition: anIndex [

	^(classDefinitionNode rangeForNode: superClassNode ifAbsent: [ ^ false ]) first includes: anIndex 
]

{ #category : 'testing' }
ClassDefinitionNodeAnalyzer >> isClassDefinition [
	
	^classDefinitionNode encoder classEncoding isMeta not
]

{ #category : 'accessing' }
ClassDefinitionNodeAnalyzer >> superclass [
	
	^superClassNode key value 
]

{ #category : 'accessing' }
ClassDefinitionNodeAnalyzer >> systemCategory [

	self isClassDefinition 	ifFalse: [ self error: 'Cannot get system category from metaclass definition' ].
	
	^(classCreationMessageNode arguments at: self class categoryPosition) literalValue 
]
