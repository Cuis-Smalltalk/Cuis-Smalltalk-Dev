Class {
	#name : 'PushDownInstanceVariable',
	#superclass : 'TransferInstanceVariable',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
PushDownInstanceVariable class >> assert: aClassToRefactor isNotAccessingInstanceVariable: anInstanceVariable [

	| selectorsReferencingInstVar |
	
	selectorsReferencingInstVar := aClassToRefactor whichSelectorsAccess: anInstanceVariable.
	selectorsReferencingInstVar ifNotEmpty: [
		self 
			canNotRefactorDueToReferencesError: (
				self errorMessageForInstanceVariable: anInstanceVariable isAccessedInMethodsOf: aClassToRefactor)
			references: (selectorsReferencingInstVar collect: [ :selector | 
				MethodReference class: aClassToRefactor selector: selector ]) asArray
			to: anInstanceVariable ]
]

{ #category : 'pre-conditions' }
PushDownInstanceVariable class >> assertCanMove: anInstanceVariable from: aClassToRefactor [

	self assert: aClassToRefactor hasInstanceVariable: anInstanceVariable.
	self assert: aClassToRefactor isNotAccessingInstanceVariable: anInstanceVariable.
	
	
]

{ #category : 'error descriptions' }
PushDownInstanceVariable class >> errorMessageForInstanceVariable: anInstanceVariable isAccessedInMethodsOf: aClassToRefactor  [
	
	^ 'Cannot push down ', anInstanceVariable, ' because it is accessed in methods of ', aClassToRefactor name
]

{ #category : 'applying' }
PushDownInstanceVariable >> apply [

	classToRefactor removeInstVarName: instanceVariableToMove.
	self pushDownInstanceVariableToAllSubclasses
]

{ #category : 'applying - private' }
PushDownInstanceVariable >> pushDownInstanceVariableToAllSubclasses [

	classToRefactor subclassesDo: [ :subClass |
		subClass addInstVarName: instanceVariableToMove ]
]
