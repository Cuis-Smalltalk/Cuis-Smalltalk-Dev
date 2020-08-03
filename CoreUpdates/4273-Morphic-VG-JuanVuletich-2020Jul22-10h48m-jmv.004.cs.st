'From Cuis 5.0 [latest update: #4269] on 22 July 2020 at 11:39:02 am'!

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 7/22/2020 10:48:50'!
submorphsMightProtrude
	"Handles and label are usually outside our bounds."

	^true! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/21/2020 11:32:17'!
updatingMorphBoundsDo: aBlock

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 11:31:29'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addMyBounds trySubmorphs |
	aMorph visible ifTrue: [
		addMyBounds _ aMorph valueOfProperty: #needsRedraw ifAbsent: [ false ].
		trySubmorphs _ aMorph valueOfProperty: #submorphNeedsRedraw ifAbsent: [ false ].
		(trySubmorphs or: [addMyBounds]) ifTrue: [
			self into: aMorph.
			addMyBounds ifTrue: [
				currentMorph setProperty: #needsRedraw toValue: false.
				"find bounds.
				agregar mi rect"
				currentMorph drawOn: self.
				world notNil ifTrue: [
"ATENCION: Este rectangulo deberia tomarse interseccion con el del clipping morph si lo hay.
Registrar el clippingMorph, o al menos su rect, en otra ivar."
					aDamageRecorder recordInvalidRect: self boundingRectOfCurrentMorphAfterDraw ].
			].
			trySubmorphs ifTrue: [
				"llamar recursivo a mis submorphs"
				currentMorph setProperty: #submorphNeedsRedraw toValue: false.
				currentMorph clippedSubmorph ifNotNil: [ :clipped |
					self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
				currentMorph unclippedSubmorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]
			].
			self outOfMorph
		]]! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/21/2020 11:33:43'!
updatingMorphBoundsDo: aBlock

	| prevEngine |
	prevEngine _ engine.
	[
		engine _ nil.
		aBlock value.
	] ensure: [ engine _ prevEngine ]! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 7/22/2020 10:49:44'!
submorphsDrawingOutsideReverseDo: aBlock
	"Might be redefined in subclasses that know that its submorphs are never outside itself"

	self submorphsMightProtrude ifTrue: [
		self unclippedSubmorphsReverseDo: aBlock ].! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/22/2020 11:07:35'!
isRedrawNeeded
	"Report that the area occupied by this morph should be redrawn."
"Creo que es mejor transformarlo en una coleccion. Ver."

	self visible ifFalse: [ ^false ].
	^ self valueOfProperty: #needsRedraw ifAbsent: [ false ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/22/2020 11:38:50'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
"BTW, no method should call #redrawNeeded twice. Just once, before any updates."
	"these properties... better store flags in 'id' "
	self isRedrawNeeded ifFalse: [
		self setProperty: #needsRedraw toValue: true.
		self allOwnersDo: [ :m | m setProperty: #submorphNeedsRedraw toValue: true ].
		self displayFullBounds
			ifNil: [
				"Dummy, so #updateIsNeeded answers true if some morph dirty"
				"Check if this is really needed. If so, find a cleaner way."
				self invalidateDisplayRect: (10@10 extent: 2@2) from: nil ]
			ifNotNil: [ :r |
				self invalidateDisplayRect: r from: nil ]].

"hacer esto en todos los casos? o solo si invalido r?"
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!HaloMorph methodsFor: 'event handling' stamp: 'jmv 7/22/2020 11:00:46'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."
"No debería alcanzar?"
true not ifTrue: [ ^super redrawNeeded ].
	self
		invalidateDisplayRect:
			((self morphBoundsInWorld
				outsetBy: self class handleSize*2)
					extendBy: 0@(FontFamily defaultLineSpacing+self class handleSize))
		from: nil.
	"Label could be wider than us..."
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/22/2020 11:22:38'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 11:36:38'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount morphBounds |

false
ifFalse: [
"Add dirty rectangles for all dirty morphs"
aPasteUpMorph allMorphsDo: [ :morph |
	morph isRedrawNeeded ifTrue: [
		morph setProperty: #needsRedraw toValue: false.
		morph displayFullBounds ifNotNil: [ :r |
			aDamageRecorder recordInvalidRect: r ]]].

"Aca agregar el seoudodibujado para actualizar los bounds que sean necesarios"

"Add dirty rectangles for all dirty morphs"
aPasteUpMorph allMorphsDo: [ :morph |
	morph isRedrawNeeded ifTrue: [
		morph displayFullBounds ifNotNil: [ :r |
			aDamageRecorder recordInvalidRect: r ]]].
]
ifTrue: [
"quizas llamarlo #findBoundsDo: o algo asi"
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
	].
].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			aPasteUpMorph submorphsReverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
				morph visible ifTrue: [
					reuse _ previousRectsToRepair.
					previousRectsToRepair _ rectsToRepair.
					rectsToRepair _ reuse removeAll.
					thisMorphRectangle _ nil.
					thisMorphBounds _ morph morphFullBoundsInWorld.
					previousRectsToRepair do: [ :r |
						(thisMorphBounds intersects: r)
							ifTrue: [ thisMorphRectangle _ r quickMerge: thisMorphRectangle ]
							ifFalse: [ rectsToRepair add: r ]].
					thisMorphRectangle
						ifNotNil: [rectsToRepair add: thisMorphRectangle]]]].
	].

	"Draw World"
	rectsToRepair do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Draw morphs"
	aPasteUpMorph submorphsReverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

!methodRemoval: MorphicCanvas #fullAddRectsToRepair: stamp: 'jmv 7/22/2020 11:27:11'!
MorphicCanvas removeSelector: #fullAddRectsToRepair:!
!methodRemoval: SystemWindow #submorphsDrawingOutsideReverseDo: stamp: 'jmv 7/22/2020 10:49:59'!
SystemWindow removeSelector: #submorphsDrawingOutsideReverseDo:!
