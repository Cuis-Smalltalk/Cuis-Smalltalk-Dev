'From Cuis 5.0 [latest update: #3826] on 5 August 2019 at 10:56:29 am'!

!Debugger methodsFor: 'private' stamp: 'jmv 8/5/2019 10:51:08'!
contextStackIndex: anInteger oldContextWas: oldContext
	"Change the context stack index to anInteger, perhaps in response to user selection."

	| isNewMethod selectedContextSlotName index |
	contextStackIndex _ anInteger.
	anInteger = 0 ifTrue: [
		currentCompiledMethod _ nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		self fixReceiverInspector.
		^ self ].
	selectedContextSlotName _ contextVariablesInspector selectedSlotName.
	isNewMethod _ oldContext == nil
					or: [ oldContext method ~~ (currentCompiledMethod _ self selectedContext method) ].
	isNewMethod ifTrue: [
		self acceptedContentsChanged.
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((index _ contextVariablesInspector fieldList indexOf: selectedContextSlotName) ~= 0 and: [
		index ~= contextVariablesInspector selectionIndex ]) ifTrue: [
			contextVariablesInspector toggleIndex: index ].
	self fixReceiverInspector.
	isNewMethod ifFalse: [ self changed: #contentsSelection ].! !

