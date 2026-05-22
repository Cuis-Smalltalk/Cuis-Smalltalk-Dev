"
My instances are user commands that can be undone and redone.
"
Class {
	#name : 'TextCommand',
	#superclass : 'Object',
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'actions' }
TextCommand >> doOn: aTextModel [
	"Perform the command, used for initial execution or for redo after undoing"
	^nil
]

{ #category : 'actions' }
TextCommand >> undoOn: aTextModel [
	"Undo the command, bringing the text model to the state it had prior to doing it"
	^nil
]
