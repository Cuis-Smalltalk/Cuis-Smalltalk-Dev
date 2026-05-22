Class {
	#name : 'UndeclaredVariableNode',
	#superclass : 'VariableNode',
	#instVars : [
		'tag'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
UndeclaredVariableNode >> accept: aVisitor [
	^aVisitor visitUndeclaredVariableNode: self
]

{ #category : 'testing' }
UndeclaredVariableNode >> isUndeclared [
	^true
]

{ #category : 'accessing' }
UndeclaredVariableNode >> tag [
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	^tag
]

{ #category : 'accessing' }
UndeclaredVariableNode >> tag: anObject [
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	tag := anObject
]
