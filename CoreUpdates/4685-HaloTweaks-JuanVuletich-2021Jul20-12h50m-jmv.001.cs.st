'From Cuis 5.0 [latest update: #4683] on 20 July 2021 at 1:48:24 pm'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector '
	classVariableNames: 'CircleForm '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 7/20/2021 12:14:54'!
haloHandleSize
	^ Preferences standardListFont pointSize * 5 // 3 max: 16! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 12:11:58'!
drawOn: aCanvas

	aCanvas
		ellipseCenter: extent // 2 radius: extent // 2 borderWidth: 0 borderColor: nil fillColor: color! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 12:30:56'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx |
	haloTargetTx _ MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx _ haloTargetTx composedWith: o location ].
	haloTargetTx _ haloTargetTx composedWith: target location.

	x0 _ x1 _ y0 _ y1 _ 0.
	target displayFullBounds corners collect: [ :pt | | p |
		p _ haloTargetTx inverseTransform: pt .
		x0 _ x0 min: p x.
		x1 _ x1 max: p x.
		y0 _ y0 min: p y.
		y1 _ y1 max: p y.].
	x1-x0 < 1000 ifTrue: [
		stepX _ 50.
		stepY _ 20 ]
	ifFalse: [
		stepX _ 100.
		stepY _ 50 ].

	prevTx _ aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c _ `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: 2 color: c.
	aCanvas line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		aCanvas line: x @ -5 to: x @ 5 width: 2 color: c.
	   	aCanvas drawString: x printString atCenterXBaselineY: x @ -10 font: nil color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		aCanvas line: -5 @ y to: 5 @ y width: 2 color: c.
	   	aCanvas drawString: y printString, ' ' atWaistRight: -5 @ y font: nil color: c ].
	  aCanvas drawString: 'y' atWaistRight: -5 @ (y1 - 20) font: nil color: c.

	aCanvas geometryTransformation: prevTx.! !

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 12:36:34'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].

	Preferences halosShowCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.3`.
			b _ target displayBounds.
			b = fb ifFalse: [
				b area > 0 ifTrue: [
					aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.3` ]].
			self drawCoordinateSystemOn: aCanvas ]].! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/20/2021 12:17:13'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e _ Preferences haloHandleSize asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form _ form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 2 ].
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!methodRemoval: HaloHandleMorph class #circleForm: stamp: 'jmv 7/20/2021 13:47:41'!
HaloHandleMorph class removeSelector: #circleForm:!
!methodRemoval: HaloHandleMorph class #releaseClassCachedState stamp: 'jmv 7/20/2021 13:47:41'!
HaloHandleMorph class removeSelector: #releaseClassCachedState!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
KernelMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!HaloHandleMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu)
!

