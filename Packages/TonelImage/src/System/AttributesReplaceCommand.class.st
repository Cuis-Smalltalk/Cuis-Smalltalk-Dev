Class {
	#name : 'AttributesReplaceCommand',
	#superclass : 'TextCommand',
	#instVars : [
		'old',
		'new',
		'start',
		'stop'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'instance creation' }
AttributesReplaceCommand class >> old: oldAttributes new: newAttributes start: startIndex stop: stopIndex [
	^self basicNew old: oldAttributes new: newAttributes start: startIndex stop: stopIndex
]

{ #category : 'actions' }
AttributesReplaceCommand >> doOn: aTextModel [
	"Perform the command, used for initial execution or for redo after undoing"

	aTextModel basicReplaceAttributesFrom: start to: stop with: new.
	^nil
]

{ #category : 'initialization' }
AttributesReplaceCommand >> old: oldAttributes new: newAttributes start: startIndex stop: stopIndex [
	old := oldAttributes.
	new := newAttributes.
	start := startIndex.
	stop := stopIndex
]

{ #category : 'printing' }
AttributesReplaceCommand >> printOn: aStream [
	aStream
		nextPutAll: 'replaced attributes'
]

{ #category : 'actions' }
AttributesReplaceCommand >> undoOn: aTextModel [
	"Perform the command, used for initial execution or for redo after undoing"

	aTextModel basicReplaceAttributesFrom: start to: stop with: old.
	^nil
]
