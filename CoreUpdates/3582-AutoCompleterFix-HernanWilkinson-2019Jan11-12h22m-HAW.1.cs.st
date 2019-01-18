'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 17 January 2019 at 4:48:31 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/17/2019 16:33:43'!
returnTypeOfEnclosedExpressionAt: aRange 

	Smalltalk isLiveTypingInstalled ifFalse: [ ^nil ].
	
	^self 
		withParserSourceMethodNodeDo: [ :methodNode | self returnTypeOfEnclosedExpresionIn: methodNode at: aRange end - 1 ]
		ifError: [ :anError | nil ]! !

