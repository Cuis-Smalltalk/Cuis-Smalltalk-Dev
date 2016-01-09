'From Cuis 4.2 of 25 July 2013 [latest update: #2657] on 9 January 2016 at 8:38:55.952133 pm'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString interruptedProcessUI sendProceeds '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 1/6/2016 21:45'!
proceed
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self triggerEvent: #closeViews.
		"Resume instead"
		self resumeProcess ]! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 1/6/2016 22:58'!
initialize
	sendProceeds _ false! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 1/6/2016 22:59'!
sendProceeds
	sendProceeds _ true! !


!DebuggerWindow methodsFor: 'open/close' stamp: 'jmv 1/6/2016 21:43'!
closeView
	self model: nil.
	self delete! !

!DebuggerWindow methodsFor: 'open/close' stamp: 'jmv 1/6/2016 21:42'!
model: aDebugger
	super model: aDebugger.
	aDebugger ifNotNil: [
		aDebugger when: #closeViews send: #closeView to: self ]! !


!PreDebugWindow methodsFor: 'open/close' stamp: 'jmv 1/6/2016 21:43'!
closeView
	self model: nil.
	self delete! !

!PreDebugWindow methodsFor: 'open/close' stamp: 'jmv 1/6/2016 21:42'!
model: aDebugger
	super model: aDebugger.
	aDebugger ifNotNil: [
		aDebugger when: #closeViews send: #closeView to: self ]! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 1/6/2016 22:59'!
send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send."
	sendProceeds ifTrue: [ ^ self proceed ].
	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 1/6/2016 22:59'!
debug: aCompiledMethod receiver: anObject in: evalContext

	| guineaPig debugger context |
	debugger _ Debugger new.
	guineaPig _ [
		aCompiledMethod
			valueWithReceiver: anObject
			arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ]).
		debugger sendProceeds.
	] newProcess name: 'debugIt'.
	context _ guineaPig suspendedContext.
	debugger process: guineaPig context: context.
	debugger openFullNoSuspendLabel: 'Debug it'.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 1/6/2016 21:46'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons _ OrderedCollection new.
	buttonColor _ self buttonColor.
	"button with target = self"
	button _ PluggableButtonMorph 
		model: model
		stateGetter: nil
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button _ PluggableButtonMorph 
					model: model
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

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 1/6/2016 22:38'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send					'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Full Stack'	fullStack			'show full stack')
		('Where'		where				'select current pc range'))! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 1/6/2016 21:46'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList: #(
			('fullStack (f)'					fullStack					''		model)
			('restart (r)'					restart						''		model)
			('proceed (p)'					proceed 					'' 		model)
			('step (t)'						doStep						''		model)
			('step through (T)'			stepIntoBlock				''		model)
			('send (e)'						send							''		model)
			('where (w)'					where						''		model)
			('peel to first like this'		peelToFirst					''		model)
			-
			('return entered value'		returnValue)
			('toggle break on entry'		toggleBreakOnEntry 	''		model)
		).
	^aMenu! !

!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 1/6/2016 21:45'!
contextStackKey: aChar from: view
	"Respond to a keystroke in the context list"

	aChar == $e ifTrue: [^ model send].
	aChar == $t ifTrue: [^ model doStep].
	aChar == $T ifTrue: [^ model stepIntoBlock].
	aChar == $p ifTrue: [^ model proceed].
	aChar == $r ifTrue: [^ model restart].
	aChar == $f ifTrue: [^ model fullStack].
	aChar == $w ifTrue: [^ model where].

	^ self messageListKey: aChar from: view! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 1/6/2016 21:35'!
preDebugButtonSpec

	^{
		{'Proceed'.		#proceed. 	'continue execution' }.
		{'Abandon'.		#abandon. 	'abandon this execution by closing this window' }.
		{'Debug'.			#debug.		'bring up a debugger' }
	}! !

!PreDebugWindow methodsFor: 'button actions' stamp: 'jmv 1/6/2016 21:43'!
debug
	"Open a full DebuggerView."
	| m |
	m _ model.
	self closeView.
	m openFullMorphicLabel: self label! !

!PreDebugWindow methodsFor: 'button actions' stamp: 'jmv 1/6/2016 21:41'!
proceed
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	| m |
	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		m _ model.
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self closeView.
		"Resume instead"
		m resumeProcess ]! !

!methodRemoval: DebuggerWindow #proceed!
DebuggerWindow removeSelector: #proceed!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString interruptedProcessUI sendProceeds'
	classVariableNames: 'ErrorRecursion'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
