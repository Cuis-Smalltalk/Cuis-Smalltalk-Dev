'From Cuis 5.0 of 7 November 2016 [latest update: #3584] on 21 January 2019 at 12:19:58 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 1/21/2019 12:17:19'!
fixReceiverInspector
	"Make receiver inspector work on current context receiver.
	Create a new inspector if needed"

	| currentReceiver requiredInspectorClass oldInspectorClass |
	currentReceiver _ self receiver.
	requiredInspectorClass _ currentReceiver inspectorClass.
	oldInspectorClass _ receiverInspector class.
	
	oldInspectorClass ~= requiredInspectorClass ifTrue: [
		oldInspectorClass format = requiredInspectorClass format
			ifTrue: [receiverInspector primitiveChangeClassTo: requiredInspectorClass basicNew]
			ifFalse: [receiverInspector becomeForward: (requiredInspectorClass basicNew copyFrom: receiverInspector)]].
	
	receiverInspector object: currentReceiver! !


!Debugger methodsFor: 'private' stamp: 'jmv 1/21/2019 12:17:40'!
contextStackIndex: arg1 oldContextWas: arg2
	| temp3 temp4 temp5 |
	contextStackIndex _ arg1.
	arg1 = 0 ifTrue: [
		currentCompiledMethod _ nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		self fixReceiverInspector.
		^ self ].
	temp4 _ contextVariablesInspector selectedSlotName.
	temp3 _ arg2
		ifNil: [ true ]
		ifNotNil: [ arg2 method ~~ (currentCompiledMethod _ self selectedContext method) ].
	temp3 ifTrue: [
		self acceptedContentsChanged.
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((temp5 _ contextVariablesInspector fieldList indexOf: temp4) ~= 0 and: [
		temp5 ~= contextVariablesInspector selectionIndex ]) ifTrue: [
			contextVariablesInspector toggleIndex: temp5 ].
	self fixReceiverInspector.
	temp3 ifFalse: [ self changed: #contentsSelection ].! !


!Inspector methodsFor: 'initialization' stamp: 'jmv 1/21/2019 12:12:06'!
inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no current selection."
	
	object := anObject. 
	self initialize! !

