'From Cuis 6.0 [latest update: #5598] on 19 December 2022 at 2:21:52 pm'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 12/19/2022 12:57:47'!
currentMorph
	^currentMorph! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/19/2022 14:15:58'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] on: Error, Halt do: [ :ex | | erroredMorph |
		"Handle a drawing error"
		erroredMorph _ (ex signalerContext findContextSuchThat: [ :context | (context receiver is: #Morph) ])
			ifNotNil: [ :context | context receiver ]
			ifNil: [ canvas currentMorph ].
		erroredMorph drawingFails.
		self setMainCanvas.
		"Install the old error handler, so we can re-raise the error.
		This means, open a Debugger."
		ex receiver error: ex description.
	]! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 13:02:57'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		[self fullUpdateCurrentProtrudingBounds] on: Error, Halt do: [ :ex | | erroredMorph |
			"Handle a drawing error"
			erroredMorph _ (ex signalerContext findContextSuchThat: [ :context | (context receiver is: #Morph) ])
				ifNotNil: [ :context | context receiver ]
				ifNil: [ currentMorph ].
			erroredMorph drawingFails.
			"In this case, for finding bounds, don't open a debugger.
			Just find bounds for the 'drawingFails' drawing. See other senders of #drawingFails."
			ex retry ].
		self outOfMorph ].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 13:03:01'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		[self fullUpdateCurrentProtrudingBounds] on: Error, Halt do: [ :ex | | erroredMorph |
			"Handle a drawing error"
			erroredMorph _ (ex signalerContext findContextSuchThat: [ :context | (context receiver is: #Morph) ])
				ifNotNil: [ :context | context receiver ]
				ifNil: [ currentMorph ].
			erroredMorph drawingFails.
			"In this case, for finding bounds, don't open a debugger.
			Just find bounds for the 'drawingFails' drawing. See other senders of #drawingFails."
			ex retry ].
		self outOfMorph ].! !

!methodRemoval: HybridCanvas #currentMorphDrawingFails stamp: 'jmv 12/19/2022 14:19:41'!
HybridCanvas removeSelector: #currentMorphDrawingFails!
!methodRemoval: MorphicCanvas #currentMorphDrawingFails stamp: 'jmv 12/19/2022 14:19:41'!
MorphicCanvas removeSelector: #currentMorphDrawingFails!
