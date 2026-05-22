"
A become operation tries to mutate an object that is the receiver ('self') in a method currently in execution, and part of the stack of calls of some process. This would render the method invalid and is potentially catastrophic.
"
Class {
	#name : 'MethodInCallStackToBecomeInvalid',
	#superclass : 'Exception',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exception handling' }
MethodInCallStackToBecomeInvalid >> defaultAction [
	"defaultAction aborts the become or clas reshape.
	See references to MethodInCallStackToBecomeInvalid.
	To go ahead with the become or class reshape, do something like:
	[
		BorderedBoxMorph subclass: #PluggableMorph
			instanceVariableNames: 'model menuNshortcutsKey newIvarThatBreaksRunningMethods'
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Morphic-Widgets'.
	] on: MethodInCallStackToBecomeInvalid do: [ :ex | ex resume: true ]
	"
	self resume: false
]
