Class {
	#name : 'PushUpInstanceVariable',
	#superclass : 'TransferInstanceVariable',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
PushUpInstanceVariable class >> assertCanMove: anInstanceVariable from: aClassToRefactor [

	self assert: aClassToRefactor hasInstanceVariable: anInstanceVariable.
	self assertNoOtherMethodIn: aClassToRefactor superclass definesTemporaryNamed: anInstanceVariable.
]

{ #category : 'pre-conditions' }
PushUpInstanceVariable class >> assertNoOtherMethodIn: aSuperclass definesTemporaryNamed: anInstanceVariableName [

 	| methodsDefiningTemporaryInSuperclass |

	methodsDefiningTemporaryInSuperclass := OrderedCollection new.
	methodsDefiningTemporaryInSuperclass addAll: (
		aSuperclass methodsWithArgumentOrTemporaryNamed: anInstanceVariableName).

	methodsDefiningTemporaryInSuperclass ifNotEmpty: [
		self 
			canNotRefactorDueToReferencesError: (
				self errorMessageForInstanceVariable: anInstanceVariableName isDefinedInMethodsOf: aSuperclass)
			references: (
				methodsDefiningTemporaryInSuperclass collect: [ :aMethod | MethodReference method: aMethod ])
			to: anInstanceVariableName ]
]

{ #category : 'error descriptions' }
PushUpInstanceVariable class >> errorMessageForInstanceVariable: anInstanceVariableName isDefinedInMethodsOf: aSuperclass  [
	
	^ anInstanceVariableName, ' exist as temporary in methods of ', aSuperclass name
]

{ #category : 'applying' }
PushUpInstanceVariable >> apply [

	self removeSubclassesInstanceVariable.
	classToRefactor superclass addInstVarName: instanceVariableToMove.
]

{ #category : 'applying - private' }
PushUpInstanceVariable >> removeSubclassesInstanceVariable [

	classToRefactor superclass allSubclassesDo: [ :subclass |
		(subclass definesInstanceVariableNamed: instanceVariableToMove) ifTrue: [ subclass removeInstVarName: instanceVariableToMove].
	].
]
