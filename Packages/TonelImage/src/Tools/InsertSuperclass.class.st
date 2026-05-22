Class {
	#name : 'InsertSuperclass',
	#superclass : 'Refactoring',
	#instVars : [
		'classToRefactor',
		'superclassName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InsertSuperclass class >> newClassPreconditionClass [

	^NewClassPrecondition 
]

{ #category : 'instance creation' }
InsertSuperclass class >> to: aClass named: aSuperclassName [
	
	^self to: aClass named: aSuperclassName in: Smalltalk undeclared: Undeclared
]

{ #category : 'instance creation' }
InsertSuperclass class >> to: aClass named: aSuperclassName in: aSystem undeclared: anUndeclared [

	self newClassPreconditionClass valueFor: aSuperclassName in: aSystem undeclared: anUndeclared.
	
	^self new initializeTo: aClass theNonMetaClass named: aSuperclassName 
]

{ #category : 'applying' }
InsertSuperclass >> apply [

	| newSuperclass |
	
	newSuperclass := self createSuperclass.
	self changeSuperclassTo: newSuperclass.

	^newSuperclass 
]

{ #category : 'applying - private' }
InsertSuperclass >> changeSuperclassTo: newSuperclass [
	
	newSuperclass subclass: classToRefactor name
		instanceVariableNames: classToRefactor instanceVariablesString 
		classVariableNames: classToRefactor classVariablesString 
		poolDictionaries: classToRefactor sharedPoolsString
		category: classToRefactor category.
]

{ #category : 'applying - private' }
InsertSuperclass >> createSuperclass [
	
	^classToRefactor superclass subclass: superclassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: classToRefactor category.
]

{ #category : 'initialization' }
InsertSuperclass >> initializeTo: aClass named: aSuperclassName  [

	classToRefactor := aClass.
	superclassName := aSuperclassName.
]
