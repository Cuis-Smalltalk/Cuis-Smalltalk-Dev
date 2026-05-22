"
Tests that common uses of #become: and related methods don't crash the system when some method could resume execution on a 'becomed' instance.
"
Class {
	#name : 'BecomeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'testing' }
BecomeTest >> testBecome [
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUpWBecome ] raise: MethodInCallStackToBecomeInvalid
]

{ #category : 'testing' }
BecomeTest >> testShapeMutation [
	| e |
	BecomeTestExperiment twoVars.
	e _ BecomeTestExperiment new.
	self should: [ e messStuffUp ] raise: MethodInCallStackToBecomeInvalid
]
