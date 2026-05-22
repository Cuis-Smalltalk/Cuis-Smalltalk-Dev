"
I represent a query path into the internal representation of a ContextPart. Typically this is a context at a point in the query path of a Debugger. As a TextProvider, the string I represent is the value of the currently selected variable of the observed temporary variable of the context.
"
Class {
	#name : 'ContextVariablesInspector',
	#superclass : 'Inspector',
	#instVars : [
		'fieldList'
	],
	#category : 'Tools-Debugger',
	#package : 'Tools',
	#tag : 'Debugger'
}

{ #category : 'shout styling' }
ContextVariablesInspector >> bindingNamesDo: aBlock [
	self fieldList do: aBlock
]

{ #category : 'selecting' }
ContextVariablesInspector >> contentsIsString [
	"Hacked so contents empty when deselected and = long printString when item 3"

	^ (selectionIndex = 3) | (selectionIndex = 0)
]

{ #category : 'code' }
ContextVariablesInspector >> doItContext [

	^object
]

{ #category : 'code' }
ContextVariablesInspector >> doItReceiver [

	^object receiver
]

{ #category : 'accessing' }
ContextVariablesInspector >> fieldList  [
	"Refer to the comment in Inspector|fieldList."

	object
		ifNil: [^ Array with: 'thisContext'].
	^fieldList ifNil:[fieldList := (Array with: 'thisContext' with: 'stack top' with: 'all temp vars') , object tempNames]
]

{ #category : 'shout styling' }
ContextVariablesInspector >> hasBindingOf: aString [
	^ self fieldList includes: aString
]

{ #category : 'accessing' }
ContextVariablesInspector >> inspect: anObject  [
	"Initialize the receiver so that it is inspecting anObject. There is no 
	current selection.
	
	Because no object's inspectorClass method answers this class, it is OK for this method to
	override Inspector >> inspect: "
	fieldList := nil.
	object := anObject.
	self initialize.
	
]

{ #category : 'accessing' }
ContextVariablesInspector >> printedSelectionIndices [

	^#(0 3)
]

{ #category : 'selecting' }
ContextVariablesInspector >> replaceSelectionValue: anObject  [
	"Refer to the comment in Inspector|replaceSelectionValue:."

	^selectionIndex = 1
		ifTrue: [object]
		ifFalse: [object namedTempAt: selectionIndex - 3 put: anObject]
]

{ #category : 'selecting' }
ContextVariablesInspector >> selection  [
	"Refer to the comment in Inspector|selection."
	selectionIndex = 0 ifTrue:[^''].
	selectionIndex = 1 ifTrue: [^object].
	selectionIndex = 2 ifTrue: [^object stackPtr > 0 ifTrue: [object top]].
	selectionIndex = 3 ifTrue: [^object tempsAndValues].
	^object debuggerMap namedTempAt: selectionIndex - 3 in: object
]
