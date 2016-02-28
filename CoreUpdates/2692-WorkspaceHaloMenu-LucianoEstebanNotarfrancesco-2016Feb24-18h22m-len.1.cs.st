'From Cuis 4.2 of 25 July 2013 [latest update: #2690] on 24 February 2016 at 6:23:33.287562 pm'!

!WorkspaceWindow methodsFor: 'menu' stamp: 'len 1/20/2016 19:29'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aCustomMenu
		addUpdating: #allowsMorphDropWording
		target: self
		action: #toggleAcceptDroppedMorphs.
	aCustomMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aCustomMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling! !

