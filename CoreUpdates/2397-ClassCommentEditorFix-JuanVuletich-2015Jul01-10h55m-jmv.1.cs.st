'From Cuis 4.2 of 25 July 2013 [latest update: #2396] on 1 July 2015 at 10:56:06.062325 am'!

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/30/2015 12:01'!
editorClassFor: textGetter
	(#(acceptedContents classCommentText) statePointsTo: textGetter) ifFalse: [
		^super editorClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor! !

