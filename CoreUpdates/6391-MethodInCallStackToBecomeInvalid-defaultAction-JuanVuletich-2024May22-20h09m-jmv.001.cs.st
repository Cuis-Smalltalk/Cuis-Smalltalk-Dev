'From Cuis7.1 [latest update: #6388] on 22 May 2024 at 8:22:00 pm'!

!MethodInCallStackToBecomeInvalid methodsFor: 'exception handling' stamp: 'jmv 5/22/2024 20:18:03'!
defaultAction
	"defaultAction aborts the become or clas reshape.
	See references to MethodInCallStackToBecomeInvalid.
	To go ahead with the become or class reshape, do something like:
	[
		BoxedMorph subclass: #PluggableMorph
			instanceVariableNames: 'model menuNshortcutsKey newIvarThatBreaksRunningMethods'
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Morphic-Widgets'.
	] on: MethodInCallStackToBecomeInvalid do: [ :ex | ex resume: true ]
	"
	self resume: false! !

