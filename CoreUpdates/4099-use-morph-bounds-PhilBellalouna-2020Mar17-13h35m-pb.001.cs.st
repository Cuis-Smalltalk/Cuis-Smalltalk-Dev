'From Cuis 5.0 [latest update: #4069] on 17 March 2020 at 1:55:28 pm'!

!Form methodsFor: 'analyzing' stamp: 'pb 3/17/2020 13:39:41'!
primCountBits
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue: [
		^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: self boundingBox;
		combinationRule: 32;
		copyBits! !

!Form methodsFor: 'converting' stamp: 'pb 3/17/2020 13:38:02'!
asGrayForm: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a GrayForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit GrayForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayForm: componentIndex ].
	
	result _ GrayForm extent: self extent.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox ;
		destOrigin: `0@0`;
		copyBits.
	^ result! !

!Form methodsFor: 'converting' stamp: 'pb 3/17/2020 13:39:15'!
asGrayScaleAndTransparent: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayScaleAndTransparent: componentIndex ].

	result _ ColorForm grayScaleAndTransparentExtent: self extent.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		copyBits.

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	^ result! !


!Morph methodsFor: 'geometry' stamp: 'pb 3/17/2020 13:51:15'!
morphPositionInWorld

	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!!"

	^self externalizeToWorld: self morphTopLeft ! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'pb 3/17/2020 13:41:52'!
invalidateDisplayRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self morphLocalBounds ) ]
                ifFalse: [ super invalidateDisplayRect: damageRect from: aMorph ]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'pb 3/17/2020 13:42:03'!
viewBox

	^ worldState
		ifNotNil: [
			 self morphLocalBounds ]
		ifNil: [
			self world viewBox ]! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'pb 3/17/2020 13:36:07'!
morphContainsPoint: aLocalPoint

	| radius other delta xOverY |
	(self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [^ false].  "quick elimination"
	extent > `1@1`
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ extent y asFloat / 2.
	other _ extent x asFloat / 2.
	delta _ aLocalPoint - (other@radius).
	xOverY _ extent x asFloat / extent y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:42:18'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphLocalBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:42:43'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ self morphLocalBounds insetBy: `1@3`.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ self morphLocalBounds insetBy: `1@3`.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'pb 3/17/2020 13:43:00'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	(self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ extent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]! !


!SystemWindow methodsFor: 'change reporting' stamp: 'pb 3/17/2020 13:53:05'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (self morphTopLeft extent: extent x @ (self labelHeight + borderWidth))! !

!SystemWindow methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:43:27'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: titleColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:43:37'!
drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: self morphLocalBounds
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color! !


!ScrollBar methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:43:16'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self morphLocalBounds
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:35:12'!
drawContainingRectangle: aCanvas
	
	aCanvas frameAndFillRectangle: self morphLocalBounds fillColor: self color borderWidth: borderWidth borderColor: borderColor.
! !


!ImageMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:48:15'!
drawOn: aCanvas

	aCanvas image: image at: self morphTopLeft! !


!StringMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:52:32'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self morphTopLeft 
		font: self fontToUse
		color: color! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:40:18'!
drawOn: aCanvas

	| tRect colorToUse sLeft aForm centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
			
	tRect _ self toggleRectangle.
	aForm _ isExpanded 
		ifTrue: [ container expandedForm ]
		ifFalse: [ container notExpandedForm ].
	centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.

	complexContents hasContents ifTrue: [
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		at: sLeft@0
		font: self fontToUse
		color: colorToUse! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:41:40'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self morphLocalBounds color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		| iconForm w h factor magnifiedExtent magnifiedIcon |
		iconForm _ isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
		magnifiedIcon _ iconForm.
		w _ iconForm width.
		h _ iconForm height.
		w*h = 0 ifFalse: [
			factor _ extent y * 0.8 / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (iconForm extent * factor) rounded.
				magnifiedIcon _ iconForm magnifyTo: magnifiedExtent ]].
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge _ magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:41:10'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self morphLocalBounds
		color: color! !


!LayoutMorph methodsFor: 'layout' stamp: 'pb 3/17/2020 13:41:27'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self morphLocalBounds! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:49:31'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: color
		at: self morphTopLeft ! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'pb 3/17/2020 13:50:18'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > self morphTopLeft and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:40:29'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:40:40'!
drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: highlightedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: ((Theme current listHighlightFocused: owner hasKeyboardFocus) alpha: 0.3)! !

!InnerListMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:40:50'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:41:01'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: self morphLocalBounds
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:36:24'!
drawOn: aCanvas

	Theme current roundWindowCorners
		ifTrue: [
			aCanvas
				roundRect: self morphLocalBounds
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:54:36'!
drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: self morphLocalBounds.
	aCanvas image: form at: self morphTopLeft.
	Transcript
		bounds: self morphBoundsInWorld;
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:50:47'!
drawOn: aCanvas 
	| baseColor |
	baseColor _ owner color.
	aCanvas
		fillRectangle: (self morphTopLeft corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter! !

