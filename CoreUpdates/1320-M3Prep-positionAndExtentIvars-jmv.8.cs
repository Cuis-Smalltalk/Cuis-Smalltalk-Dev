'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 2:37:37 pm'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs fullBounds color extension position extent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 12/12/2011 11:15'!
initPosExtent
"
Morph allSubInstancesDo: [ :m | m initPosExtent ]
"
	position ifNil: [
		position _ owner
			ifNil: [ bounds topLeft ]
			ifNotNil: [
				owner initPosExtent.
				owner internalizeFromWorld: bounds topLeft ]].
	extent ifNil: [ extent _ bounds extent ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 23:00'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."
	^owner
		ifNotNil: [ owner internalizeFromWorld: aPoint + position ]
		ifNil: [ aPoint + position ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 22:59'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	^(owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ])
			- position! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 22:59'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."
	^self isWorldMorph
		ifTrue: [ aPoint ]
		ifFalse: [ super externalizeToWorld: aPoint ]! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 22:59'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	^self isWorldMorph
		ifTrue: [ aPoint ]
		ifFalse: [ super internalizeFromWorld: aPoint ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 19:05'!
basicExtent: aPoint

	bounds extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	bounds _ bounds topLeft extent: aPoint.
	extent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 13:22'!
morphPosition: aPoint
	"Change the position of this morph and and all of its submorphs."

	"Detesto el falso polimorfismo con streams. Ponerle otro nombre a esto!!
	Igual, todavia esta #position, el getter, que es igual de feo..."
	
	"
	VER SENDERS. Acomodar. el argumento es en coord del owner o del world?
	Convertir los senders a senders de #zzpositionInOwner: o #zzpositionInWorld
	Espero que pocos hablen en coordenadas del world!!
	"

	| delta |
	delta _ aPoint - bounds topLeft.
	(delta x = 0 and: [delta y = 0]) ifTrue: [^ self].  "Null change"
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.
	self privateFullMoveBy: delta.

	position _ owner
		ifNil: [ bounds topLeft ]
		ifNotNil: [ owner internalizeFromWorld: bounds topLeft ].

	self redrawNeeded
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/11/2011 19:08'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	bounds _ self defaultBounds.
	position _ bounds topLeft.
	extent _ bounds extent.
	color _ self defaultColor! !

!Morph methodsFor: 'private' stamp: 'jmv 12/12/2011 11:16'!
privateBounds: boundsRect
	"Private!! Use position: and/or extent: instead."

	fullBounds _ nil.
	bounds _ boundsRect.
	position _ owner
		ifNil: [ bounds topLeft ]
		ifNotNil: [ owner internalizeFromWorld: bounds topLeft ].
	extent _ bounds extent.! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 12/11/2011 19:22'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	bounds _ 0@0 extent: CursorWithMask normal extent.
	position _ 0@0.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 12/11/2011 19:20'!
initialize
	super initialize.
	bounds _ 0@0 corner: 40@10.
	position _ 0@0.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil
! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/11/2011 19:21'!
extent: aPoint

	bounds extent = aPoint ifFalse: [
		self redrawNeeded.
		bounds _ bounds topLeft extent: aPoint.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = bounds ifFalse: [
				worldState canvas: nil.
				worldState viewBox: bounds ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 12/12/2011 11:19'!
viewBox: newViewBox
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	super morphPosition: newViewBox topLeft.
	fullBounds _ bounds _ newViewBox.
	position _ owner
		ifNil: [ bounds topLeft ]
		ifNotNil: [ owner internalizeFromWorld: bounds topLeft ].
	extent _ bounds extent.! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs fullBounds color extension position extent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

| p |
World allMorphsDo: [ :m | m initPosExtent ].
Morph allSubInstancesDo: [ :m | m initPosExtent ].
p _ Processor activeProcess.
p == ProjectX uiProcessX ifTrue: [
	[
		p terminate.
		p _ nil.
		ProjectX spawnNewProcessX.
		Display restore ] forkAt: Processor userInterruptPriority ]!
