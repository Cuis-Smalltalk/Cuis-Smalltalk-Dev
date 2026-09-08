'From Cuis7.9 [latest update: #8160] on 8 September 2026 at 7:21:53 pm'!

!Compiler class methodsFor: 'evaluating' stamp: 'jmv 8/Sep/2026, 10:14:46 pm (UTC)'!
evaluate: textOrString 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString for: nil logged: false! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'jmv 8/Sep/2026, 10:20:32 pm (UTC)'!
classOfLiteral: aLiteral in: aClass 
	
	| compilerClass |
	
	compilerClass := aClass ifNil: [ Compiler ] ifNotNil: [ aClass compilerClass ].
	^([compilerClass evaluate: aLiteral]
		on: SyntaxErrorNotification 
		do: [ :anError | anError return: nil ]) class! !

