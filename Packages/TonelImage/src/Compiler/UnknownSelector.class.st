Class {
	#name : 'UnknownSelector',
	#superclass : 'ParserNotification',
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'exception handling' }
UnknownSelector >> defaultAction  [

	| alternatives labels lines caption choice icons |

	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents: [:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	icons := Array new: labels size.
	icons at: 1 put: #acceptIcon.
	icons at: icons size put: #cancelIcon.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withNewLines.
	
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1 ifAbsent: nil)
]
