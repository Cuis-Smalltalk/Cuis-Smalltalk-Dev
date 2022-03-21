'From Cuis 6.0 [latest update: #5093] on 21 March 2022 at 10:46:27 am'!

!StringRequestMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:00:32'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!PluggableButtonMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:05:05'!
categoryInNewMorphMenu

	^ 'User Interface'! !

!PluggableButtonMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:06:38'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"
	
	^ true! !

!PluggableButtonMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:05:26'!
initializedInstance

	^ self example! !


!TextParagraphMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:15:12'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!FillInTheBlankMorph class methodsFor: 'examples' stamp: 'KenD 3/14/2022 14:51:54'!
example1
	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		emptyTextDisplayMessage: 'Enter answer here';
		setQuery: 'Ask Question Here'
		initialAnswer: 'Enter answer here'
		acceptOnCR: true.
	aFillInTheBlankMorph responseUponCancel: 'returnOnCancel'.
	^ aFillInTheBlankMorph.! !

!FillInTheBlankMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 14:55:00'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!AutoCompleterMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:16:19'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!ProgressMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 15:00:20'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!UpdatingLabelMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 14:57:58'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!FillInTheBlankMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 14:34:24'!
initializedInstance
	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		emptyTextDisplayMessage: 'Enter answer here';
		setQuery: 'Ask Question Here'
		initialAnswer: 'Enter answer here'
		acceptOnCR: true.
	aFillInTheBlankMorph responseUponCancel: 'returnOnCancel'.
	^ aFillInTheBlankMorph.! !


!HoverHelpMorph class methodsFor: 'new-morph participation' stamp: 'KenD 3/14/2022 14:56:58'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!PluggableButtonMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu includeInNewMorphMenu initializedInstance)
('example' example)
('instance creation' model:action: model:action:label: model:stateGetter:action: model:stateGetter:action:label:)
!


!FillInTheBlankMorph class reorganize!
('instance creation' request: request:initialAnswer: request:initialAnswer:centerAt:onCancelReturn:acceptOnCR: request:initialAnswer:onCancel: request:onCancel:)
('examples' example1)
('new-morph participation' categoryInNewMorphMenu includeInNewMorphMenu initializedInstance)
!

