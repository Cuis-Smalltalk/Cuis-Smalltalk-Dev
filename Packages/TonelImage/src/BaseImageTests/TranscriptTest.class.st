"
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1

"
Class {
	#name : 'TranscriptTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Support',
	#package : 'BaseImageTests',
	#tag : 'System-Support'
}

{ #category : 'assertions' }
TranscriptTest >> assertTranscriptContentsAdded: expectedAddedString after: aBlock [
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.
]

{ #category : 'assertions' }
TranscriptTest >> assertTranscriptContentsAdded2: expectedAddedString after: aBlock [
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	{transcriptContentsBeforeBlockEvaluation. expectedAddedString.Transcript contents} inspect.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.
]

{ #category : 'assertions' }
TranscriptTest >> assertTranscriptContentsDoesNotChangeAfter: aBlock [
	
	self assertTranscriptContentsAdded: '' after: aBlock
]

{ #category : 'tests' }
TranscriptTest >> testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript newLineWhen: false ]
	
]

{ #category : 'tests' }
TranscriptTest >> testNewLineWhenConditionIsTruePutsANewLineInStream [

	self assertTranscriptContentsAdded: String newLineString, '.' after: [ Transcript newLineWhen: true; nextPut: $. ]
]

{ #category : 'tests' }
TranscriptTest >> testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPutAll: 'other' when: false ]
	
]

{ #category : 'tests' }
TranscriptTest >> testNextPutAllWhenConditionIsTruePutsCollectionInStream [

	self assertTranscriptContentsAdded: 'other' after: [ Transcript nextPutAll: 'other' when: true ]
	
	
]

{ #category : 'tests' }
TranscriptTest >> testNextPutWhenConditionIsFalseDoesNotPutObjectInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPut: $t when: false ]

]

{ #category : 'tests' }
TranscriptTest >> testNextPutWhenConditionIsTruePutsObjectInStream [

	self assertTranscriptContentsAdded: 't' after: [ Transcript nextPut: $t when: true ]
	
	
]

{ #category : 'tests' }
TranscriptTest >> testPrintWhenConditionIsFalseDoesNotPrintObjectInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript print: Object when: false ]

]

{ #category : 'tests' }
TranscriptTest >> testPrintWhenConditionIsTruePrintsObjectInStream [

	self assertTranscriptContentsAdded: Object printString after: [ Transcript print: Object when: true ]
	
]

{ #category : 'tests' }
TranscriptTest >> testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript spaceWhen: false ]
	
]

{ #category : 'tests' }
TranscriptTest >> testSpaceWhenConditionIsTruePutsASpaceInStream [

	self assertTranscriptContentsAdded: ' ' after: [ Transcript spaceWhen: true ]
	
]

{ #category : 'tests' }
TranscriptTest >> testTabWhenConditionIsFalseDoesNotPutATabInStream [

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript tabWhen: false ]
]

{ #category : 'tests' }
TranscriptTest >> testTabWhenConditionIsTruePutsATabInStream [

	self assertTranscriptContentsAdded: Character tab asString after: [ Transcript tabWhen: true ]
]
