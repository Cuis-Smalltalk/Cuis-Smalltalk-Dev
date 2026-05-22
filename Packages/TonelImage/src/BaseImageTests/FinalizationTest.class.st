Class {
	#name : 'FinalizationTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Finalization',
	#package : 'BaseImageTests',
	#tag : 'System-Finalization'
}

{ #category : 'tests' }
FinalizationTest >> testFinalization [
	| object finalizationWasRun finalizationAction |
	object := Object new.
	finalizationWasRun := false.
	finalizationAction := [ finalizationWasRun := true ].
	object toFinalizeSend: #value to: finalizationAction.
	self deny: finalizationWasRun.
	object := nil.
	self triggerFinalization.
	self assert: finalizationWasRun.
]

{ #category : 'tests' }
FinalizationTest >> testFinalization2 [
	| object finalizationWasRun finalizationAction |
	object := FinalizationTestObject new.
	finalizationWasRun := false.
	finalizationAction := [ finalizationWasRun := true ].
	object finalizeAction: finalizationAction.
	object finalizationRegistry add: object.
	self deny: finalizationWasRun.
	object := nil.
	self triggerFinalization.
	self assert: finalizationWasRun.
]

{ #category : 'helpers' }
FinalizationTest >> triggerFinalization [

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.
]
