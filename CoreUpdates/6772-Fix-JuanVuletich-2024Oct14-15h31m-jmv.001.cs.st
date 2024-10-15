'From Cuis7.1 [latest update: #6771] on 14 October 2024 at 3:33:34 pm'!

!SmalltalkEditor methodsFor: 'private' stamp: 'jmv 10/14/2024 15:33:24'!
lineOrNodeSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, either
		- expand it to be the entire current line
		- or expand it to a meaningful chunk of code.
	If after that's done the selection is still empty, then evaluate the returnBlock."

	self hasSelection ifFalse: [
		(model actualContents isLineEndAt: self pointIndex)
			ifTrue: [ self selectLine ]
			ifFalse: [
				self
					withMethodNodeAndClassDo: [ :methodNode :selectedClass |
						self selectNodeUnderCursorForInspectionIn: methodNode ]
					ifErrorsParsing: [ :anError | ]]].
	self selectionIsEmptyOrSeparators ifTrue: [
		"
		morph flash.
		^returnBlock value.
		"
		self selectLine.
		].! !

