'From Cuis7.9 [latest update: #8209] on 21 September 2026 at 10:37:02 am'!

!ExtractMethodCodeModifier methodsFor: 'private' stamp: 'jmv 21/Sep/2026, 1:35:42 pm (UTC)'!
parseIgnoringUndeclared: aSourceCode

	^[ Parser parse: aSourceCode class: methodNode methodClass noPattern: true ]
		on: UndeclaredVariableWarning
		do: [ :warning | warning resume: false ] ! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'jmv 21/Sep/2026, 1:35:42 pm (UTC)'!
sourceCodeToExtractParsed

	"The piece is parsed on its own to look at its shape, where the arguments and temporaries of the
	method it comes from are not declared. Resuming with false keeps their names out of Undeclared."

	^[ Parser parse: sourceCodeToExtract class: self sourceClass noPattern: true ]
		on: UndeclaredVariableWarning
		do: [ :warning | warning resume: false ] ! !

!methodRemoval: Parser class #parseIgnoringUndeclared:class:noPattern: stamp: 'jmv 21/Sep/2026, 1:35:42 pm (UTC)'!
Parser class removeSelector: #parseIgnoringUndeclared:class:noPattern:!
