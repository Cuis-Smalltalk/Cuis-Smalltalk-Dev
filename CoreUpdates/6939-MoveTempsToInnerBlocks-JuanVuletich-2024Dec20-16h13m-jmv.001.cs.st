'From Cuis7.3 [latest update: #6938] on 20 December 2024 at 4:29:46 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 12/20/2024 16:13:32'!
methodInheritance
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta aClass sel |
	aClass := model selectedClassOrMetaClass.
	sel := model selectedMessageName.
	aClass ifNil: [ ^ nil ].
	sel ifNil: [ ^ nil ].
	aClassNonMeta := aClass theNonMetaClass.
	isMeta := aClassNonMeta ~~ aClass.
	list := OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			| theClassOrMeta |
			theClassOrMeta := isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	^ Smalltalk
		browseMessageList: list
		name: 'Implementors of ' , sel, ' in ', aClass name, ' and super / sub classes'.! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/20/2024 16:15:07'!
changePriority
	| rule |
	rule := (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [self inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	self request: 'New priority'
		initialAnswer: model selectedProcess priority printString
		verifying: [:aString| ([aString asNumber asInteger] on: Error do: []) isInteger]
		do: [ :aString |
			| newPriority |
			newPriority := aString asNumber asInteger.
			(newPriority between: 1 and: Processor highestPriority)
			ifTrue:
				[model class setProcess: model selectedProcess toPriority: newPriority.
				model updateProcessList]
			ifFalse: [self inform: 'Bad priority']]! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 12/20/2024 16:16:27'!
adjustExtent
	"And reposition submorphs"
	| w y |
	"make all items wide, so selection indicator is wide too"
	w := self desiredWidth.
	y := 0.
	self submorphsDo: [ :m | | h |
		h := m morphHeight.
		m position: 0@y extent: w@h.
		y := y + h ].
	self morphExtent: w@y! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 12/20/2024 16:17:49'!
drawRoots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."
	"Iterate from back to front."

	| allDamage newDamageFromMorphsBelow |
	"Don't need a copy just because nobody will use it afterwards"
	newDamageFromMorphsBelow := backgroundDamage.
	allDamage := Rectangle merging: newDamageFromMorphsBelow.
	rootMorphs size to: 1 by: -1 do: [ :i | | morph morphDamage |
		morph := rootMorphs at: i.
		morphDamage := rootsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			newDamageFromMorphsBelow add: morphDamage.
			allDamage
				ifNil: [ allDamage := morphDamage copy ]
				ifNotNil: [ allDamage updateMerging: morphDamage ]]].
	^allDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 12/20/2024 16:18:28'!
updateAllDamage: allDamage roots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage
	"Detect any morph whose damage would grow because of morphs beneath it.
	If one found, add the new larger damage to allDamage and exit.
	See sender."
	"Iterate from back to front."

	| newDamageFromMorphsBelow |
	newDamageFromMorphsBelow := backgroundDamage copy.
	rootMorphs size to: 1 by: -1 do: [ :i |
		| morph morphPreviousDamage morphDamage |
		morph := rootMorphs at: i.
		morphPreviousDamage := rootsDamage at: i.
		morphDamage := morphPreviousDamage copy.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			newDamageFromMorphsBelow add: morphDamage ].
		morphDamage = morphPreviousDamage ifFalse: [
			allDamage add: morphDamage.
			^true 	]].
	^false! !


!AbstractVectorCanvas class methodsFor: 'private' stamp: 'jmv 12/20/2024 16:19:33'!
scalePathData: aFloat32Array scale: scale
	"aFloatArray is a #pathSequence: argument. It encodes a sequence of path commands in a Float32Array.
	See #pathSequence:"

	^Float32Array streamContents: [ :scaledStrm | | unscaledStrm kind |
		unscaledStrm := aFloat32Array readStream.
		[ unscaledStrm atEnd ] whileFalse: [
			kind := unscaledStrm next.
			scaledStrm nextPut: kind.
			(kind asInteger *2 max: 2) timesRepeat: [
				scaledStrm nextPut: unscaledStrm next * scale ]]
		].! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/19/2024 12:41:19'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		BitBltCanvas accessProtect critical: [ | f |
			f := BitBltCanvas buildAuxWith: aForm multipliedWith: aColor.
			self image: f at: aPoint sourceRect: aForm boundingBox ]]! !


!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 12/20/2024 16:25:41'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	| dx dy trx try tcx tcy center angle d hops |
	dx := geometryTransformation externalizeDelta: rx@0.
	dy := geometryTransformation externalizeDelta: 0@ry.
	trx := dx r.
	try := dy r.
	hops := ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d := sweepAngle / hops.
	center := geometryTransformation externalizePosition: cx@cy.
	tcx := center x.
	tcy := center y.
	(trx = try and: [
		d * hops > 6.28]) ifTrue: [ 	"If a circle, not an ellipse, and a complete circle, or pretty close"
			spanLeft := spanLeft min: tcx - trx.
			spanRight := spanRight max: tcx + trx.
			spanTop := spanTop min: tcy - trx.
			spanBottom := spanBottom max: tcy + trx ]

		ifFalse: [						"Else go the more expensive way"
			angle := startAngle.
			hops := hops+1.
			hops timesRepeat: [
				| x xp y yp |
				xp := angle cos * trx.
				yp := angle sin * try.
				x := thetaCos * xp - (thetaSin * yp) + tcx.
				y := thetaSin * xp + (thetaCos * yp) + tcy.
				spanLeft := spanLeft min: x.
				spanRight := spanRight max: x.
				spanTop := spanTop min: y.
				spanBottom := spanBottom max: y.
				angle := angle + d. ]].! !


!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 9/15/2021 14:47:40'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	| dx dy trx try tcx tcy center angle xp yp x y d hops |
	dx := geometryTransformation externalizeDelta: rx@0.
	dy := geometryTransformation externalizeDelta: 0@ry.
	trx := dx r.
	try := dy r.
	hops := ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d := hops asFloat.
	center := geometryTransformation externalizePosition: cx@cy.
	tcx := center x.
	tcy := center y.
	0 to: hops do: [ :h |
		angle := h / d * sweepAngle + startAngle.
		xp := angle cos * trx.
		yp := angle sin * try.
		x := thetaCos * xp - (thetaSin * yp) + tcx.
		y := thetaSin * xp + (thetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y. ].! !


!VectorEngineWithPluginSubPixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 12/19/2024 12:43:08'!
dashedStroke_doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Ask a VectorEngineSmalltalk for help"


	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	self class protected: [ | smalltalkVGE |
		self initializePath.
		smalltalkVGE := VectorEngineSubPixel new dashedStroke_ToFollow: self.
		smalltalkVGE initializePath.
		canvas instVarNamed: 'engine' put: smalltalkVGE.
		pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[smalltalkVGE]).
		self dashedStroke_finishPath: ensureClosePath smalltalkVGE: smalltalkVGE.
		canvas instVarNamed: 'engine' put: self.
		].
! !


!VectorEngineWithPluginWholePixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 12/19/2024 12:43:13'!
dashedStroke_doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Ask a VectorEngineSmalltalk for help"


	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	self class protected: [ | smalltalkVGE |
		self initializePath.
		smalltalkVGE := VectorEngineWholePixel new dashedStroke_ToFollow: self.
		smalltalkVGE initializePath.
		canvas instVarNamed: 'engine' put: smalltalkVGE.
		pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[smalltalkVGE]).
		self dashedStroke_finishPath: ensureClosePath smalltalkVGE: smalltalkVGE.
		canvas instVarNamed: 'engine' put: self.
		].
! !

