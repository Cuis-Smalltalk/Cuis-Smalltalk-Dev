'From Cuis 6.0 [latest update: #5775] on 24 April 2023 at 10:52:46 am'!
!classDefinition: #TextReplaceCommand category: #'System-Text'!
TextCommand subclass: #TextReplaceCommand
	instanceVariableNames: 'old new position newSizeShouted '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!TextCommand methodsFor: 'updating' stamp: 'jmv 4/24/2023 10:40:43'!
newSizeShouted: aNumber! !


!TextReplaceCommand methodsFor: 'updating' stamp: 'jmv 4/24/2023 10:38:51'!
newSizeShouted: aNumber
	"Syntax Highlight has changed the effective size of the new text. 
	Adjust for that."
	newSizeShouted := aNumber! !


!TextModel methodsFor: 'user interface support' stamp: 'jmv 4/24/2023 10:47:01'!
convertAndStyleIfNeededWith: anSHTextStyler
	| after before |
	anSHTextStyler ifNotNil: [
		(self shouldStyle: self actualContents with: anSHTextStyler) ifTrue: [
			before := self actualContents size.
			anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.
			after := anSHTextStyler convertedAndStyledText size.
			self basicActualContents: anSHTextStyler convertedAndStyledText.
			after = before ifFalse: [
				undoRedoCommands peekLast ifNotNil: [ :lastCommand |
					lastCommand newSizeShouted: after-before]].
			 ]]! !


!TextReplaceCommand methodsFor: 'actions' stamp: 'jmv 4/24/2023 10:45:42'!
undoOn: aTextModel
	"Undo the command, bringing the text model to the state it had prior to doing it.
	Answer a new position for the text cursor"

	| to |
	to := position + new size-1.
	newSizeShouted notNil ifTrue: [
		to := to + newSizeShouted ].
	aTextModel basicReplaceFrom: position to: to with: old.
	^position + old size! !

!classDefinition: #TextReplaceCommand category: #'System-Text'!
TextCommand subclass: #TextReplaceCommand
	instanceVariableNames: 'old new position newSizeShouted'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!TextReplaceCommand reorganize!
('accessing' stopPosition)
('actions' doOn: undoOn:)
('printing' printOn:)
('initialization' old:new:at:)
('updating' appendToNew: newSizeShouted:)
!

