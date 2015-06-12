'From Cuis 4.2 of 25 July 2013 [latest update: #2344] on 9 June 2015 at 8:46:05.438197 pm'!
!classDefinition: #WorkspaceWindow category: #'Morphic-Tools'!
SystemWindow subclass: #WorkspaceWindow
	instanceVariableNames: 'acceptDroppedMorphs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!WorkspaceWindow methodsFor: 'menu' stamp: 'KenD 6/9/2015 20:44'!
allowsMorphDropWording

	^ self allowsMorphDrop
		ifTrue:  ['<yes> morphs dropped on me create a variable reference']
		ifFalse: ['<no> morphs dropped on me create a variable reference']! !

!WorkspaceWindow methodsFor: 'access' stamp: 'KenD 6/9/2015 20:30'!
acceptDroppedMorphs

	^ acceptDroppedMorphs! !

!WorkspaceWindow methodsFor: 'access' stamp: 'KenD 6/9/2015 20:30'!
acceptDroppedMorphs: aBoolean

	acceptDroppedMorphs := aBoolean! !

!WorkspaceWindow methodsFor: 'access' stamp: 'KenD 6/9/2015 20:35'!
toggleAcceptDroppedMorphs

	self acceptDroppedMorphs: self acceptDroppedMorphs not! !

!WorkspaceWindow methodsFor: 'initialization' stamp: 'KenD 6/9/2015 20:31'!
initialize

	super initialize.
	self acceptDroppedMorphs: false.! !


!WorkspaceWindow methodsFor: 'menu' stamp: 'KenD 6/9/2015 20:42'!
offerWindowMenu
	| aMenu |
	aMenu _ self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aMenu
		addUpdating: #allowsMorphDropWording
		target: self
		action: #toggleAcceptDroppedMorphs.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu popUpInWorld: self world! !

!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'KenD 6/9/2015 20:37'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ self acceptDroppedMorphs! !

!classDefinition: #WorkspaceWindow category: #'Morphic-Tools'!
SystemWindow subclass: #WorkspaceWindow
	instanceVariableNames: 'acceptDroppedMorphs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!WorkspaceWindow reorganize!
('menu' allowsMorphDropWording offerWindowMenu)
('drag n drop' allowsMorphDrop wantsDroppedMorph:event:)
('access' acceptDroppedMorphs acceptDroppedMorphs: toggleAcceptDroppedMorphs)
('initialization' initialize)
!

