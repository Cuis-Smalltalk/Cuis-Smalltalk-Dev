'From Cuis 5.0 of 7 November 2016 [latest update: #3145] on 28 August 2017 at 1:50:13 pm'!
!classDefinition: #BrowserWindow category: #'Morphic-Tools'!
CodeWindow subclass: #BrowserWindow
	instanceVariableNames: 'codePane '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/28/2017 13:44:30'!
disableCodePaneEdition

	codePane ifNotNil: [ codePane disableEdition ]! !

!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/28/2017 13:41:24'!
editSelectionChanged
	
	self isEditSelectionNone
		ifTrue: [ self disableCodePaneEdition ]
		ifFalse: [ self enableCodePaneEdition]! !

!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/28/2017 13:43:20'!
enableCodePaneEdition

	codePane ifNotNil: [ codePane enableEdition ]! !

!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/28/2017 13:43:44'!
isEditSelectionNone
		
	^ model editSelection = #none! !

!BrowserWindow methodsFor: 'updating' stamp: 'HAW 8/28/2017 13:41:51'!
update: anEvent
	super update: anEvent.
	anEvent = #editSelection ifTrue: [self editSelectionChanged ] ! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 8/28/2017 13:39:41'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."

	codePane _ super buildMorphicCodePane.
	^codePane! !

!classDefinition: #BrowserWindow category: #'Morphic-Tools'!
CodeWindow subclass: #BrowserWindow
	instanceVariableNames: 'codePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!
