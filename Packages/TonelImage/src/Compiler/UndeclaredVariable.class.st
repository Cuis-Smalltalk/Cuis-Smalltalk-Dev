Class {
	#name : 'UndeclaredVariable',
	#superclass : 'ParserNotification',
	#instVars : [
		'parser',
		'interval'
	],
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'as yet unclassified' }
UndeclaredVariable class >> signalFor: aParser name: aString inRange: anInterval  [
	^ (self new setParser: aParser name: aString range: anInterval) signal
]

{ #category : 'exception handling - private' }
UndeclaredVariable >> addAlternativesTo: labels actions: actions icons: icons [
	
	| alternatives |
	
	alternatives := parser possibleVariablesFor: name.
	alternatives do: [ :each |
		labels add: each.
		actions add: [ parser substituteVariable: each atInterval: interval ].
		icons add: nil ].

]

{ #category : 'exception handling - private' }
UndeclaredVariable >> addCancelTo: labels actions: actions icons: icons [
	
	labels add: 'cancel'.
	actions add: nil.
	icons add: #cancelIcon.
	

]

{ #category : 'exception handling - private' }
UndeclaredVariable >> addGlobalVariableOptionsTo: labels actions: actions [
	
	labels add: 'define new class'.
	actions add: [ parser defineClass: name ].
			
	labels add: 'declare global'.
	actions add: [ parser declareGlobal: name ].
			
	parser canDeclareClassVariable ifTrue: [
		labels add: 'declare class variable'.
		actions add: [ parser declareClassVar: name ]]

]

{ #category : 'exception handling - private' }
UndeclaredVariable >> addLocalVariableOptionsTo: labels actions: actions [

	labels add: 'declare block-local temp'.
	actions add: self declareBlockTempAction.
			
	labels add: 'declare method temp'.
	actions add: self declareMethodTempAction.
			
	parser canDeclareInstanceVariable ifTrue: [
		labels add: 'declare instance'.
		actions add: self declareInstanceVariableAction]
]

{ #category : 'exception handling - private' }
UndeclaredVariable >> addOptionsTo: labels actions: actions icons: icons [

	name first isLowercase
		ifTrue: [ self addLocalVariableOptionsTo: labels actions: actions ]
		ifFalse: [ self addGlobalVariableOptionsTo: labels actions: actions ].
	labels size timesRepeat: [ icons add: #listAddIcon ].
		

]

{ #category : 'exception handling - private' }
UndeclaredVariable >> createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines [
	
	self addOptionsTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addAlternativesTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addCancelTo: labels actions: actions icons: icons.
]

{ #category : 'exception handling - private' }
UndeclaredVariable >> declareBlockTempAction [
	
	^[ parser declareTemp: name at: #block ]
]

{ #category : 'exception handling - private' }
UndeclaredVariable >> declareInstanceVariableAction [

	^[ parser declareInstVar: name ]
]

{ #category : 'exception handling - private' }
UndeclaredVariable >> declareMethodTempAction [
	
	^[ parser declareTemp: name at: #method ].
]

{ #category : 'exception handling' }
UndeclaredVariable >> defaultAction [
	
	| labels actions lines caption choice icons |
	
	labels := OrderedCollection new.
	actions := OrderedCollection new.
	lines := OrderedCollection new.
	icons := OrderedCollection new.
	
	self createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	
	self resume: (actions at: choice ifAbsent: nil).
]

{ #category : 'parser info' }
UndeclaredVariable >> setParser: aParser name: aString range: anInterval  [
	parser := aParser.
	name := aString.
	interval := anInterval
]
