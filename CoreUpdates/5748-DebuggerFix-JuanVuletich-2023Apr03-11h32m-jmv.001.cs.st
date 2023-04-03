'From Cuis 6.0 [latest update: #5747] on 3 April 2023 at 11:32:50 am'!

!Debugger methodsFor: 'private' stamp: 'jmv 4/3/2023 11:32:26'!
contextStackIndex: anInteger oldContextWas: oldContext
	"Change the context stack index to anInteger, perhaps in response to user selection."

	| isNewMethod selectedContextSlotName index |
	contextStackIndex := anInteger.
	anInteger = 0 ifTrue: [
		currentCompiledMethod := nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		self fixReceiverInspector.
		^ self ].
	selectedContextSlotName := contextVariablesInspector selectedSlotName.
	isNewMethod := oldContext == nil
					or: [ oldContext method ~~ (currentCompiledMethod := self selectedContext method) ].
	self acceptedContentsChanged.
	isNewMethod ifTrue: [
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((index := contextVariablesInspector fieldList indexOf: selectedContextSlotName) ~= 0 and: [
		index ~= contextVariablesInspector selectionIndex ]) ifTrue: [
			contextVariablesInspector toggleIndex: index ].
	self fixReceiverInspector.
	isNewMethod ifFalse: [ self changed: #contentsSelection ].! !

