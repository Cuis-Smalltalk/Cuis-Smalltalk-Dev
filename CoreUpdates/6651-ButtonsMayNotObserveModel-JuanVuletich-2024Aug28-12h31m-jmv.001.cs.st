'From Cuis7.1 [latest update: #6652] on 28 August 2024 at 12:36:50 pm'!

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 12:33:51'!
actionTarget: anObject
	"Set my model.
	We will not observe the model. We will not receive #update: or other notifications from them.
	Call #model: if observing the model for updates is required."

	model ifNotNil: [model removeActionsWithReceiver: self].
	model := anObject.! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 12:27:01'!
enableSelector: aSymbol

	enableSelector := aSymbol.
	enableSelector notNil ifTrue: [
		model ifNotNil: [
			"So we get #update: called"
			model addDependent: self]].! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 12:32:13'!
model: anObject stateGetter: getStateSel action: actionSel label: aString

	stateSelector := getStateSel.
	stateArgument := nil.
	actionSelector := actionSel.
	actionArgument := nil.
	stateSelector notNil
		ifTrue: [
			"So we get #update: called"
			self model: anObject ]
		ifFalse: [ self actionTarget: anObject ].
	self label: aString.! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 12:32:26'!
model: anObject stateGetter: getStateSel stateArgument: getStateArg action: actionSel actionArgument: actionArg label: aString

	stateSelector := getStateSel.
	stateArgument := getStateArg.
	actionSelector := actionSel.
	actionArgument := actionArg.
	stateSelector notNil
		ifTrue: [
			"So we get #update: called"
			self model: anObject ]
		ifFalse: [ self actionTarget: anObject ].
	self label: aString.! !

!PluggableButtonMorph methodsFor: 'updating' stamp: 'jmv 8/28/2024 12:23:12'!
update: aSymbol
	super update: aSymbol.
	"Note: if any of these selectors is not nil, then we need to observe on the model,
	setting it with #model: and not with #actionTarget:"
	(aSymbol == stateSelector or: [aSymbol == enableSelector ]) ifTrue: [
		self redrawNeeded ].! !

!methodRemoval: PluggableMorph #actionTarget: stamp: 'jmv 8/28/2024 12:33:44'!
PluggableMorph removeSelector: #actionTarget:!
