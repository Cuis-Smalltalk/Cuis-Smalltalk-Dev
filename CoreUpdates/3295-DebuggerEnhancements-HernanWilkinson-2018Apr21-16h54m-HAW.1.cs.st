'From Cuis 5.0 of 7 November 2016 [latest update: #3293] on 22 April 2018 at 5:32:30 pm'!

!DebuggerWindow methodsFor: 'user interface' stamp: 'HAW 4/21/2018 20:43:49'!
ifOkToChangeCodePaneDo: aBlock

	^self okToChangeCodePane ifTrue: aBlock 
! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/22/2018 17:30:34'!
createMethod

	^ self ifOkToChangeCodePaneDo: [ model createMethod ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:44:10'!
doStep

	^ self ifOkToChangeCodePaneDo: [ model doStep ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:45:19'!
fullStack

	^ self ifOkToChangeCodePaneDo: [ model fullStack ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:45:26'!
proceed

	^ self ifOkToChangeCodePaneDo: [ model proceed ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:45:36'!
restart 
	
	^ self ifOkToChangeCodePaneDo: [ model restart ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/22/2018 17:31:44'!
send

	^ self ifOkToChangeCodePaneDo: [ model send ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:45:56'!
stepIntoBlock

	^ self ifOkToChangeCodePaneDo: [ model stepIntoBlock ]! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 4/21/2018 20:46:06'!
where

	^ self ifOkToChangeCodePaneDo: [ model where ]! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 4/21/2018 20:41:02'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons _ OrderedCollection new.
	buttonColor _ self buttonColor.
	"button with target = self"
	button _ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button _ PluggableButtonMorph 
					model: self
					stateGetter: nil
					action: tuple second.
		button color: buttonColor.
		button label: tuple first asString.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'HAW 4/21/2018 20:39:59'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList: #(
			('fullStack (f)'					fullStack	)
			('restart (r)'					restarl)
			('proceed (p)'					proceed)
			('step (t)'						doStep)
			('step through (T)'				stepIntoBlock)
			('send (e)'						send)
			('where (w)'					where)
			('peel to first like this'			peelToFirst)
			-
			('return entered value'		returnValue)
			('toggle break on entry'		toggleBreakOnEntry 	''		model)
		).
	^aMenu! !

!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 4/21/2018 20:38:49'!
contextStackKey: aChar from: view
	"Respond to a keystroke in the context list"

	aChar == $e ifTrue: [^ self send].
	aChar == $t ifTrue: [^ self doStep].
	aChar == $T ifTrue: [^ self stepIntoBlock].
	aChar == $p ifTrue: [^ self proceed].
	aChar == $r ifTrue: [^ self restart].
	aChar == $f ifTrue: [^ self fullStack].
	aChar == $w ifTrue: [^ self where].

	^ self messageListKey: aChar from: view! !


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicCodePane buildMorphicWindow customButtonRow customButtonSpecs initialExtent optionalButtonTuples windowColor)
('menu building' contextFieldListMenu contextStackMenu receiverFieldListMenu)
('menu commands' contextClassHierarchy exploreContextSelection exploreReceiverSelection receiverClassHierarchy returnValue)
('keyboard shortcuts' browseClassRefsIn: browseFullProtocolIn: browseHierarchyIn: browseMethodFullIn: contextStackKey:from: exploreSelectionIn: inspectSelectionIn: inspectorKey:from:)
('open/close' closeView delete model:)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack proceed restart send stepIntoBlock where)
!

