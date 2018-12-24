'From Cuis 5.0 of 7 November 2016 [latest update: #3543] on 23 December 2018 at 11:25:59 am'!

!Behavior methodsFor: 'methods' stamp: 'KLG 12/23/2018 11:18:27'!
methodNodeFor: aSourceCode noPattern: aBoolean

	|  parser methodNode |

	parser := self parserClass new.

	methodNode := parser parse: aSourceCode class: self noPattern: aBoolean.
	methodNode sourceText: aSourceCode.

	^methodNode
			! !

