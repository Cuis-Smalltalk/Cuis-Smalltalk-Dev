'From Cuis7.5 [latest update: #7408] on 13 August 2025 at 10:28:31 am'!

!Parser class methodsFor: 'parsing' stamp: 'jmv 8/13/2025 10:20:19'!
parse: sourceCode class: aClass noPattern: aBoolean

	^(self new
		parse: sourceCode class: aClass noPattern: aBoolean)
			sourceText: sourceCode;
			yourself! !

