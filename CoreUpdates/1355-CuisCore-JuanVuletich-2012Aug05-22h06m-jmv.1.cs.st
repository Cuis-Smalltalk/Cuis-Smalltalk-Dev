'From Cuis 4.0 of 21 April 2012 [latest update: #1351] on 5 August 2012 at 10:12:45 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:08'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	
	self flag: #jmvVer2.
	"Ir convirtiendo todos los usos (no las asignaciones!!) a las vars nuevas.
	Despues eliminar las asignaciones y las propias ivars (bounds y fullBounds)"
	bounds _ self defaultBounds.

	position _ bounds topLeft.
	extent _ bounds extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	color _ self defaultColor.
	layoutNeeded _ false! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 22:11'!
layoutSubmorphsAndComputeFullBounds
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.
	layoutNeeded ifNil: [ layoutNeeded _ false ].
	layoutNeeded ifTrue: [

		self layoutSubmorphs.
	
		"useful someday?"
		self computeFullBounds.

		layoutNeeded _ false ]! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 22:11'!
someSubmorphPositionOrExtentChanged
	"In some submorph or in self"
	layoutNeeded ifFalse: [
		layoutNeeded _ true.	"layout will be recomputed so don't bother"
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]]! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/5/2012 22:09'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	self invalidRect: (layoutNeeded
		ifFalse: [ self morphFullBoundsInWorld ]
		ifTrue: [ self morphBoundsInWorld ])! !

