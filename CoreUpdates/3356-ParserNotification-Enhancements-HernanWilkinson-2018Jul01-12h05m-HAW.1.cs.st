'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 1 July 2018 at 1:15:10 pm'!

!UndeclaredVariable methodsFor: 'exception handling' stamp: 'HAW 7/1/2018 13:05:18'!
defaultAction
	
	| labels actions lines caption choice icons |
	
	labels _ OrderedCollection new.
	actions _ OrderedCollection new.
	lines _ OrderedCollection new.
	icons _ OrderedCollection new.
	
	self createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines.
	caption _ 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice _ (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	
	self resume: (actions at: choice ifAbsent:[ nil ]).! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:59:52'!
addAlternativesTo: labels actions: actions icons: icons
	
	| alternatives |
	
	alternatives _ parser possibleVariablesFor: name.
	alternatives do: [ :each |
		labels add: each.
		actions add: [ parser substituteVariable: each atInterval: interval ].
		icons add: nil ].
! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:55:34'!
addCancelTo: labels actions: actions icons: icons
	
	labels add: 'cancel'.
	actions add: nil.
	icons add: #cancelIcon.
	
! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:50:36'!
addGlobalVariableOptionsTo: labels actions: actions
	
	labels add: 'define new class'.
	actions add: [ parser defineClass: name ].
			
	labels add: 'declare global'.
	actions add: [ parser declareGlobal: name ].
			
	parser canDeclareClassVariable ifTrue: [
		labels add: 'declare class variable'.
		actions add: [ parser declareClassVar: name ]]
! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:49:43'!
addLocalVariableOptionsTo: labels actions: actions

	labels add: 'declare block-local temp'.
	actions add: [ parser declareTemp: name at: #block ].
			
	labels add: 'declare method temp'.
	actions add: [ parser declareTemp: name at: #method ].
			
	parser canDeclareInstanceVariable ifTrue: [
		labels add: 'declare instance'.
		actions add: [ parser declareInstVar: name ]]! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:52:33'!
addOptionsTo: labels actions: actions icons: icons

	name first isLowercase
		ifTrue: [ self addLocalVariableOptionsTo: labels actions: actions ]
		ifFalse: [ self addGlobalVariableOptionsTo: labels actions: actions ].
	labels size timesRepeat: [ icons add: #listAddIcon ].
		
! !

!UndeclaredVariable methodsFor: 'exception handling - private' stamp: 'HAW 7/1/2018 12:59:24'!
createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines
	
	self addOptionsTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addAlternativesTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addCancelTo: labels actions: actions icons: icons.! !


!UndefinedVariable methodsFor: 'exception handling' stamp: 'HAW 7/1/2018 12:17:22'!
defaultAction

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\undefined at this point.\Proceed anyway?' withNewLines.
	^ self resume: shouldResume ! !


!UnknownSelector methodsFor: 'exception handling' stamp: 'HAW 7/1/2018 13:07:30'!
defaultAction 

	| alternatives labels lines caption choice icons |

	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents: [:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	icons := Array new: labels size.
	icons at: 1 put: #acceptIcon.
	icons at: icons size put: #cancelIcon.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withNewLines.
	
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1 ifAbsent: [ nil ])
	
! !


!UnusedVariable methodsFor: 'exception handling' stamp: 'HAW 7/1/2018 12:14:01'!
defaultAction
	
	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\unused in this method.\OK to remove it?' withNewLines.
	self resume: shouldResume ! !

!methodRemoval: UnusedVariable #openMenuIn:!
UnusedVariable removeSelector: #openMenuIn:!
!methodRemoval: UnknownSelector #openMenuIn:!
UnknownSelector removeSelector: #openMenuIn:!
!methodRemoval: UndefinedVariable #openMenuIn:!
UndefinedVariable removeSelector: #openMenuIn:!
!methodRemoval: UndeclaredVariable #addAlternative:to:actions:icons:!
UndeclaredVariable removeSelector: #addAlternative:to:actions:icons:!
!methodRemoval: UndeclaredVariable #openMenuIn:!
UndeclaredVariable removeSelector: #openMenuIn:!
!methodRemoval: ParserNotification #defaultAction!
ParserNotification removeSelector: #defaultAction!
!methodRemoval: ParserNotification #openMenuIn:!
ParserNotification removeSelector: #openMenuIn:!

!ParserNotification reorganize!
('name' setName:)
!


!UndeclaredVariable reorganize!
('parser info' setParser:name:range:)
('*DenotativeObject' addCollaboratorLabelsTo:andActionsTo: addDenotativeObjectLabelsTo:andActionsTo:in: defineDenotativeObjectNamed:in: handleForDenotativeObjectIn:)
('exception handling' defaultAction)
('exception handling - private' addAlternativesTo:actions:icons: addCancelTo:actions:icons: addGlobalVariableOptionsTo:actions: addLocalVariableOptionsTo:actions: addOptionsTo:actions:icons: createMenuOptionsAddingTo:actions:icons:lines:)
!


!UndefinedVariable reorganize!
('exception handling' defaultAction)
!


!UnknownSelector reorganize!
('exception handling' defaultAction)
!


!UnusedVariable reorganize!
('exception handling' defaultAction)
!

