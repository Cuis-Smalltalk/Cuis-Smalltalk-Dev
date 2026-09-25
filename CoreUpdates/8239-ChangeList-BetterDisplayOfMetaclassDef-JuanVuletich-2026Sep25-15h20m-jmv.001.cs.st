'From Cuis7.9 [latest update: #8237] on 25 September 2026 at 3:20:56 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 25/Sep/2026, 6:13:14 pm (UTC)'!
scanClassDefinition: tokens

	| record text |
	record := self classDefinitionRecordFrom: tokens.
	text := 'classDefinition: ', record changeClassName.
	record isMetaClassChange ifTrue: [
		text := text, ' class' ].
	self addItem: record text: text.
! !

