"
To be used by BecomeTest
"
Class {
	#name : 'BecomeTestExperiment',
	#superclass : 'Object',
	#instVars : [
		'a',
		'b'
	],
	#category : 'BaseImageTests-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Kernel'
}

{ #category : 'as yet unclassified' }
BecomeTestExperiment class >> threeVars [

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'c a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'BaseImageTests-Kernel'
]

{ #category : 'as yet unclassified' }
BecomeTestExperiment class >> twoVars [

	Object subclass: #BecomeTestExperiment
		instanceVariableNames: 'a b'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'BaseImageTests-Kernel'
]

{ #category : 'as yet unclassified' }
BecomeTestExperiment >> initialize [
	a _ 1.
	b _ 2.
]

{ #category : 'as yet unclassified' }
BecomeTestExperiment >> messStuffUp [
"In a Workspace

BecomeTestExperiment twoVars.
e _ BecomeTestExperiment new.
e messStuffUp.

e _ nil. Smalltalk garbageCollect.
BecomeTestExperiment allInstances

Processor invalidMethodInStackDueToClass: BecomeTestExperiment
"

	BecomeTestExperiment threeVars. "New ivar c is added before a and be"
	^a + b
]

{ #category : 'as yet unclassified' }
BecomeTestExperiment >> messStuffUpWBecome [
"In a Workspace

BecomeTestExperiment twoVars.
e _ BecomeTestExperiment new.
e messStuffUpWBecome.

e _ nil. Smalltalk garbageCollect.
BecomeTestExperiment allInstances

Processor invalidMethodInStackDueToClass: BecomeTestExperiment
"

	self becomeForward: {22}.
	^a + b
]
