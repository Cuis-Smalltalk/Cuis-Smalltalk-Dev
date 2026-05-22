"
I am responsible for checking if a new instance variable can be introduced in a specific class. If that is not possible, I raise a refactoring error.
"
Class {
	#name : 'NewInstanceVariablePrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'classToAddInstVar',
		'instVarName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
NewInstanceVariablePrecondition class >> errorMessageForInvalidInstanceVariable: aName [

	^ '''' , aName , ''' is not a valid instance variable name'.
]

{ #category : 'error messages' }
NewInstanceVariablePrecondition class >> errorMessageForNewInstanceVariable: aName alreadyDefinedInAll: classes [

	^ 'Instance variable ''' , aName , ''' is already defined in ' , classes asCommaStringAnd.
]

{ #category : 'error messages' }
NewInstanceVariablePrecondition class >> errorMessageForNewInstanceVariableCanNotBeAReservedName: aName [

	^'''', aName, ''' can not be used as instance variable name because it is a reserved name'
]

{ #category : 'warning messages' }
NewInstanceVariablePrecondition class >> errorMessageForNewVariable: newVariable willBeHiddenAtAll: methods [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: newVariable;
			nextPutAll: ' can not be named as temporary/parameter in '.
		methods asCommaSeparated: [:aMethod | aMethod printClassAndSelectorOn: stream ] on: stream ]
]

{ #category : 'error messages' }
NewInstanceVariablePrecondition class >> newVariableCanNotBeEmptyErrorMessage [
	
	^'New variable can not be empty'
]

{ #category : 'instance creation' }
NewInstanceVariablePrecondition class >> of: anInstanceVariableName for: aClass [

	^self new initializeOf: anInstanceVariableName for: aClass 
]

{ #category : 'evaluating' }
NewInstanceVariablePrecondition class >> valueOf: anInstanceVariableName for: aClass [

	^(self of: anInstanceVariableName for: aClass) value
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsNotAReservedName [
	
	(ClassBuilder reservedNames includes: instVarName) ifTrue: [ self signalNewInstanceVariableCanNotBeAReservedName ]
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsNotAlreadyDefined [
	
	^ classToAddInstVar 
		withClassesThatDefineInHierarchyInstanceVariable: instVarName 
		do: [ :definingClasses | self signalAlreadyDefinedInAll: definingClasses ]
		ifNone: nil.
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsNotAlreadyDefinedInSuperclasses [

	^ (classToAddInstVar classThatDefinesInstanceVariable: instVarName) 
		ifNotNil: [ :definingClass | self signalAlreadyDefinedInAll: {definingClass} ]
		
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsNotDefinedInMethods [
	
	| methodsDefiningNewVariable |
	
	methodsDefiningNewVariable := self methodsDefiningNewVariable.
	
	methodsDefiningNewVariable notEmpty ifTrue: [ self signalNewVariable: instVarName willBeHiddenAtAll: methodsDefiningNewVariable ].
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsNotEmpty [

	instVarName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty]
]

{ #category : 'pre-conditions' }
NewInstanceVariablePrecondition >> assertIsValidInstanceVariableName [

	| scannedNames |

	scannedNames := Scanner new scanFieldNames: instVarName .
	scannedNames size = 1 ifFalse: [ self signalInvalidInstanceVariable ].
	scannedNames first = instVarName ifFalse: [ self signalInvalidInstanceVariable ].
]

{ #category : 'initialization' }
NewInstanceVariablePrecondition >> initializeOf: anInstanceVariableName for: aClass [

	instVarName := anInstanceVariableName withBlanksTrimmed.
	classToAddInstVar := aClass.
]

{ #category : 'warnings' }
NewInstanceVariablePrecondition >> methodsDefiningNewVariable [
	
	| methodsDefiningNewVariableInHierarchy |
	
	methodsDefiningNewVariableInHierarchy := OrderedCollection new.
	
	classToAddInstVar withAllSubclassesDo: [ :class | 
		methodsDefiningNewVariableInHierarchy addAll: (class methodsWithArgumentOrTemporaryNamed: instVarName) ].

	^methodsDefiningNewVariableInHierarchy 
		
	
]

{ #category : 'exceptions' }
NewInstanceVariablePrecondition >> signalAlreadyDefinedInAll: classes [

	^ self refactoringError: (self class errorMessageForNewInstanceVariable: instVarName alreadyDefinedInAll: classes).
]

{ #category : 'exceptions' }
NewInstanceVariablePrecondition >> signalInvalidInstanceVariable [

	^ self refactoringError: (self class errorMessageForInvalidInstanceVariable: instVarName).
]

{ #category : 'exceptions' }
NewInstanceVariablePrecondition >> signalNewInstanceVariableCanNotBeAReservedName [

	self refactoringError: (self class errorMessageForNewInstanceVariableCanNotBeAReservedName: instVarName)
]

{ #category : 'warnings' }
NewInstanceVariablePrecondition >> signalNewVariable: newVariable willBeHiddenAtAll: methods [

	^ self refactoringError: (self class errorMessageForNewVariable: newVariable willBeHiddenAtAll: methods).
]

{ #category : 'exceptions' }
NewInstanceVariablePrecondition >> signalNewVariableCanNotBeEmpty [
	
	self refactoringError: self class newVariableCanNotBeEmptyErrorMessage
]

{ #category : 'evaluating' }
NewInstanceVariablePrecondition >> value [
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefined.

	self assertIsNotDefinedInMethods.
]

{ #category : 'evaluating' }
NewInstanceVariablePrecondition >> valueForSuperclass [
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefinedInSuperclasses.

	
]
