"
My instances are text replace commands, such as typing, pasting, deleting, etc.
"
Class {
	#name : 'TextReplaceCommand',
	#superclass : 'TextCommand',
	#instVars : [
		'old',
		'new',
		'position'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'instance creation' }
TextReplaceCommand class >> old: oldStringOrText new: newStringOrText at: anInteger [
	^self basicNew old: oldStringOrText new: newStringOrText at: anInteger
]

{ #category : 'updating' }
TextReplaceCommand >> appendToNew: aStringOrText [
	new := new, aStringOrText
]

{ #category : 'actions' }
TextReplaceCommand >> doOn: aTextModel [
	"Perform the command, used for initial execution or for redo after undoing.
	Answer a new position for the text cursor"

	aTextModel basicReplaceFrom: position to: position + old size-1 with: new.
	^position + new size
]

{ #category : 'initialization' }
TextReplaceCommand >> old: oldStringOrText new: newStringOrText at: anInteger [
	old := oldStringOrText.
	new := newStringOrText.
	position := anInteger
]

{ #category : 'printing' }
TextReplaceCommand >> printOn: aStream [
	old isEmpty ifTrue: [
		^aStream nextPutAll: 'typed: '; nextPutAll: new asPlainString withDescriptiveLineEndings surroundedBySingleQuotes].
	new isEmpty ifTrue: [
		^aStream nextPutAll: 'deleted: '; nextPutAll: old asPlainString withDescriptiveLineEndings surroundedBySingleQuotes ].
	aStream
		nextPutAll: 'replaced: ';
		nextPutAll: old asPlainString withDescriptiveLineEndings surroundedBySingleQuotes;
		nextPutAll: ' with: ';
		nextPutAll: new asPlainString withDescriptiveLineEndings surroundedBySingleQuotes 
]

{ #category : 'accessing' }
TextReplaceCommand >> stopPosition [
	^position + new size.
]

{ #category : 'actions' }
TextReplaceCommand >> undoOn: aTextModel [
	"Undo the command, bringing the text model to the state it had prior to doing it.
	Answer a new position for the text cursor"

	aTextModel basicReplaceFrom: position to: position + new size-1 with: old.
	^position + old size
]
