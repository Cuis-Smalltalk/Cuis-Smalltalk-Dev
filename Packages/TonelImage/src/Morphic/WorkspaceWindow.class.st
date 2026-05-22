"
Morphic view for Workspace models. See category 'GUI building'.
"
Class {
	#name : 'WorkspaceWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu' }
WorkspaceWindow >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
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
		action: #toggleFullPrintIt.
]

{ #category : 'menu' }
WorkspaceWindow >> offerWindowMenu [
	| aMenu |
	aMenu := self buildWindowMenu.
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
	aMenu popUpInWorld: self world
]
