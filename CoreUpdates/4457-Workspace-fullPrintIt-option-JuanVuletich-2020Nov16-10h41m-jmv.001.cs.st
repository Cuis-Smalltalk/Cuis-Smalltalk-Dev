'From Cuis 5.0 [latest update: #4456] on 16 November 2020 at 10:59:06 am'!
!classDefinition: #Workspace category: #'System-Text'!
TextModel subclass: #Workspace
	instanceVariableNames: 'bindings mustDeclareVariables shouldStyle fullPrintIt '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 11/16/2020 10:57:17'!
fullPrintIt
	^true! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 11/16/2020 10:48:58'!
fullPrintIt

	^fullPrintIt ifNil: [ Preferences fullPrintItInWorkspaces]! !

!Workspace methodsFor: 'user interface support' stamp: 'jmv 11/16/2020 10:51:34'!
toggleFullPrintIt

	fullPrintIt _ self fullPrintIt not.! !

!Workspace methodsFor: 'user interface support' stamp: 'jmv 11/16/2020 10:50:46'!
toggleFullPrintItLabel

	^self fullPrintIt 
		ifTrue: [ '<on> full printIt' ]
		ifFalse: [ '<off> full printIt' ]! !


!Preferences class methodsFor: 'personalization' stamp: 'jmv 11/16/2020 10:48:24'!
fullPrintItInWorkspaces
	^ self
		valueOfFlag: #fullPrintItInWorkspaces
		ifAbsent: [false]! !


!Workspace methodsFor: 'initialization' stamp: 'jmv 11/16/2020 10:41:49'!
initialize
	
	super initialize.
	self initializeBindings.
	mustDeclareVariables _ false.
	fullPrintIt _ false.! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/16/2020 10:58:25'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	| rpt |
	self
		evaluateSelectionAndDo: [ :result |
			rpt _ model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			self afterSelectionInsertAndSelect: 
				((' ', rpt, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false.! !


!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 11/16/2020 10:50:55'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aCustomMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aCustomMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aCustomMenu
		addUpdating: #toggleFullPrintItLabel
		target: model
		action: #toggleFullPrintIt.! !

!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 11/16/2020 10:53:20'!
offerWindowMenu
	| aMenu |
	aMenu _ self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings
		icon: #warningIcon.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu
		addUpdating: #toggleFullPrintItLabel
		target: model
		action: #toggleFullPrintIt.
	aMenu popUpInWorld: self world! !

!classDefinition: #Workspace category: #'System-Text'!
TextModel subclass: #Workspace
	instanceVariableNames: 'bindings mustDeclareVariables shouldStyle fullPrintIt'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
