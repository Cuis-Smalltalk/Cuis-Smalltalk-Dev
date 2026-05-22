"
Morphic view for Debugger models. See category 'GUI building'.
"
Class {
	#name : 'PreDebugWindow',
	#superclass : 'CodeWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'instance creation' }
PreDebugWindow class >> open: model label: aString message: messageString [
	|  window |

	window := self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString.
	aString ifNotNil: [ window setLabel: aString ].

	window openInWorld.
]

{ #category : 'button actions' }
PreDebugWindow >> abandon [
	"abandon the debugger from its pre-debug notifier"
	self delete
]

{ #category : 'GUI building' }
PreDebugWindow >> buildMorphicWindowMessage: messageString [
	| notifyPane |
	model expandStack.
	messageString
		ifNil: [
			notifyPane := PluggableListMorph
				withModel: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: #openFullAt: ]
		ifNotNil: [
			notifyPane := TextModelMorph withText: messageString.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8
]

{ #category : 'GUI building' }
PreDebugWindow >> buttonRowForPreDebugWindow: aDebugger [
	| row aButton triads buttons |
	buttons := OrderedCollection new.
	triads := OrderedCollection withAll: self preDebugButtonSpec.
	aDebugger shouldBeAbleToCreateMethod ifTrue: [ triads add: { 'Create'. #createMethod. 'create the missing method' }].
	
	triads do: [ :triad |
		aButton := PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: `Color transparent`.
	row separation: 1.
	row addMorphs: buttons.
	^row
]

{ #category : 'open/close' }
PreDebugWindow >> closeView [
	self model: nil.
	self delete
]

{ #category : 'button actions' }
PreDebugWindow >> createMethod [
	"Should only be called when this Debugger was created in response to a
	MessageNotUnderstood exception. Create a stub for the method that was
	missing and proceed into it."
	
	model createMethod.
	self debug

]

{ #category : 'button actions' }
PreDebugWindow >> debug [
	"Open a full DebuggerView."
	| m |
	m := model.
	self closeView.
	m openFullMorphicLabel: self label
]

{ #category : 'button actions' }
PreDebugWindow >> delete [
	model ifNotNil: [ model windowIsClosing ].
	super delete
]

{ #category : 'GUI building' }
PreDebugWindow >> initialExtent [
	^ `640 @ 320` * (Preferences at: #standardCodeFont) lineSpacing // 14
]

{ #category : 'initialization' }
PreDebugWindow >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model when: #closeViews send: #closeView to: self.
]

{ #category : 'GUI building' }
PreDebugWindow >> preDebugButtonSpec [

	^{
		{'Proceed'.		#proceed. 	'continue execution' }.
		{'Abandon'.		#abandon. 	'abandon this execution by closing this window' }.
		{'Debug'.		#debug.	'bring up a debugger' }
	}
]

{ #category : 'button actions' }
PreDebugWindow >> proceed [
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	| m |
	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		m := model.
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self closeView.
		"Resume instead"
		m resumeProcess ]
]

{ #category : 'GUI building' }
PreDebugWindow >> windowColor [
	^ Theme current debugger
]
