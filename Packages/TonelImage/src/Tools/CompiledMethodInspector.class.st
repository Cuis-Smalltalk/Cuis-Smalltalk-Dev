Class {
	#name : 'CompiledMethodInspector',
	#superclass : 'Inspector',
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'accessing' }
CompiledMethodInspector >> fieldList [

	| keys |
	keys := OrderedCollection new.
	keys add: 'self'.
	keys add: 'all bytecodes'.
	keys add: 'header'.
	1 to: object numLiterals do: [ :i |
		keys add: 'literal', i printString ].
	object initialPC to: object size do: [ :i |
		keys add: i printString ].
	^ keys asArray
	
]

{ #category : 'constants' }
CompiledMethodInspector >> lastMetaField [
	"This value must be synchronized with the base field list, the
	string selection indices, and the senders of this message"

	^3
]

{ #category : 'selecting' }
CompiledMethodInspector >> selection [

	| bytecodeIndex |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= object numLiterals
		ifTrue: [ ^ object objectAt: selectionIndex - self lastMetaField + 1 ].
	bytecodeIndex := selectionIndex - object numLiterals - self lastMetaField.
	^ object at: object initialPC + bytecodeIndex - 1
]

{ #category : 'selecting' }
CompiledMethodInspector >> selectionIsUnmodifiable [
	"Answer if the current selected variable is unmodifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^ true
]

{ #category : 'selecting' }
CompiledMethodInspector >> selectionMeta [

	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object ].
	self selectionIndex = 2 ifTrue: [^ self safelyPrintWith: [object symbolic]].
	self selectionIndex = 3 ifTrue: [^ self safelyPrintWith: [object headerDescription]].
	^self selectionMetaUnknown
]
