Class {
	#name : 'UndeclaredVariableWarning',
	#superclass : 'Warning',
	#instVars : [
		'name',
		'selector',
		'class'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'accessing' }
UndeclaredVariableWarning >> classImplementingSelector [

	^class 
]

{ #category : 'exceptionDescription' }
UndeclaredVariableWarning >> defaultAction [
	"The user should be notified of the occurrence of an exceptional occurrence and
	 given an option of continuing or aborting the computation. The description of the
	 occurrence should include any text specified as the argument of the #signal: message."
	
	selector
		ifNotNil: [Transcript newLine; nextPutAll: class name, '>>', selector, ' ']
		ifNil: [Transcript newLine ].
	Transcript show: '(' , name , ' is Undeclared) '.
	^true
]

{ #category : 'initialization' }
UndeclaredVariableWarning >> defaultResumeValue [
	"Answer the value that by default should be returned if the exception is resumed"
	^true
]

{ #category : 'initialization' }
UndeclaredVariableWarning >> name: aString selector: aSymbolOrNil class: aBehavior [
	name := aString.
	selector := aSymbolOrNil.
	class := aBehavior
]

{ #category : 'accessing' }
UndeclaredVariableWarning >> selector [

	^selector 
]

{ #category : 'accessing' }
UndeclaredVariableWarning >> variableName [

	^name 
]
