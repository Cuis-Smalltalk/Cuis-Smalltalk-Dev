'From Cuis 4.1 of 12 December 2012 [latest update: #1652] on 1 April 2013 at 3:44:59 pm'!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData '
	classVariableNames: 'DisableDeferredUpdates WindowEventHandler '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!Morph methodsFor: 'drawing' stamp: 'jmv 4/1/2013 15:35'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"
	| c |
	submorphs isEmpty ifTrue: [ ^ self ].
	c _ self clipsSubmorphs
		ifTrue: [ aCanvas copyClipRect: self clippingBoundsInWorld ]
		ifFalse: [ aCanvas ].
	submorphs reverseDo: [ :m |
		c fullDraw: m ]! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 4/1/2013 15:42'!
doDeferredUpdatingFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."
	| properDisplay |
	(Display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	self flag: #jmvVer.
	true "aWorld == World" ifTrue: [  "this world fills the entire Display"
		properDisplay ifFalse: [
			aWorld viewBox: Display boundingBox.    "do first since it may clear canvas"
			self canvas: Display getCanvas.
		]
	] ifFalse: [  "this world is inside an MVC window"
		self revisar.
		self halt.
	].
	^ true! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 4/1/2013 15:40'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ OrderedCollection new.
	(damageRecorder invalidRectsFullBounds: aWorld viewBox) do: [ :r |
		r ifNotNil: [ initialRectsToRepair addLast: r ]].
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].

	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			aWorld drawOn: (canvas copyClipRect: r) ]].
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could merge all xrects into just one call... Most likely, that would be slower, though."
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas copyClipRect: r) fullDraw: m
		].
"		(canvas copyClipRect: rr) fullDrawMorph: m "
	].
	
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!methodRemoval: PasteUpMorph class #disableDeferredUpdates!
PasteUpMorph class removeSelector: #disableDeferredUpdates!
!methodRemoval: PasteUpMorph class #disableDeferredUpdates:!
PasteUpMorph class removeSelector: #disableDeferredUpdates:!
!methodRemoval: PasteUpMorph #assuredNonDisplayCanvas!
PasteUpMorph removeSelector: #assuredNonDisplayCanvas!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData'
	classVariableNames: 'WindowEventHandler'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!methodRemoval: FormCanvas #clipBy:during:!
FormCanvas removeSelector: #clipBy:during:!
