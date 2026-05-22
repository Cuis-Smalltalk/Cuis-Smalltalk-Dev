Class {
	#name : 'ClassReferencesCollector',
	#superclass : 'Object',
	#instVars : [
		'classToLookForReferences',
		'referencesToClass',
		'referencedAsClass',
		'referencesToName',
		'referencedAsName',
		'withAllSubclassesNames'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ClassReferencesCollector class >> of: aClassToLookForReferences [

	^self new initializeOf: aClassToLookForReferences
]

{ #category : 'evaluating' }
ClassReferencesCollector class >> valueOf: aClassToLookForReferences [

	^(self of: aClassToLookForReferences) value
]

{ #category : 'evaluating - private' }
ClassReferencesCollector >> add: aClass asReferencedToClassWith: referencesToVariableBinding [
	
	referencedAsClass add: aClass.
	referencesToClass addAll: referencesToVariableBinding 
]

{ #category : 'evaluating - private' }
ClassReferencesCollector >> add: aClass asReferencedToNameWith: referencesToClassName [

	referencedAsName add: aClass.
	referencesToName addAll: referencesToClassName 
	
]

{ #category : 'evaluating - private' }
ClassReferencesCollector >> collectReferencesOf: aClass  [
	
	| allReferences referencesOutsideHierarchy referencesToVariableBinding referencesToClassName |
	
	allReferences := aClass allCallsOn.
	referencesOutsideHierarchy := allReferences reject: [ :aReference | withAllSubclassesNames includes: aReference classSymbol ].
	referencesToVariableBinding := referencesOutsideHierarchy select: [ :aReference | aReference hasVariableBindingTo: aClass ].
	referencesToClassName := referencesOutsideHierarchy difference: referencesToVariableBinding.
	
	referencesToVariableBinding notEmpty ifTrue: [ self add: aClass asReferencedToClassWith: referencesToVariableBinding ].	
	referencesToClassName notEmpty ifTrue: [ self add: aClass asReferencedToNameWith: referencesToClassName ].
	
]

{ #category : 'testing' }
ClassReferencesCollector >> hasReferencesToClass [
	
	^referencesToClass notEmpty
]

{ #category : 'testing' }
ClassReferencesCollector >> hasReferencesToName [
	
	^referencesToName notEmpty 
]

{ #category : 'evaluating - private' }
ClassReferencesCollector >> initializeCollectionFor: withAllSubclasses [
	
	withAllSubclassesNames := withAllSubclasses collect: [:aClass | aClass name ].
	referencesToClass := OrderedCollection new.
	referencedAsClass := OrderedCollection new.
	referencesToName := OrderedCollection new.
	referencedAsName := OrderedCollection new
]

{ #category : 'initialization' }
ClassReferencesCollector >> initializeOf: aClassToLookForReferences [

	classToLookForReferences := aClassToLookForReferences 
]

{ #category : 'accessing' }
ClassReferencesCollector >> referencedAsClass [
	
	^referencedAsClass
]

{ #category : 'accessing' }
ClassReferencesCollector >> referencedAsName [
	
	^referencedAsName
]

{ #category : 'accessing' }
ClassReferencesCollector >> referencesToClass [
	
	^referencesToClass
]

{ #category : 'accessing' }
ClassReferencesCollector >> referencesToName [
	
	^referencesToName
]

{ #category : 'evaluating' }
ClassReferencesCollector >> value [

	| withAllSubclasses |
	
	withAllSubclasses := classToLookForReferences withAllSubclasses.
	self initializeCollectionFor: withAllSubclasses.
		
	withAllSubclasses do: [ :aClass | self collectReferencesOf: aClass ].
		
	
]
