'From Cuis 5.0 of 7 November 2016 [latest update: #3674] on 19 March 2019 at 6:57:48 am'!

!MessageSet methodsFor: 'message list' stamp: 'HAW 3/19/2019 06:56:35'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].
	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment].
		selector == #Definition ifTrue: [
			^ class definition].
		selector == #Hierarchy ifTrue: [^ class printHierarchy]].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed! !

