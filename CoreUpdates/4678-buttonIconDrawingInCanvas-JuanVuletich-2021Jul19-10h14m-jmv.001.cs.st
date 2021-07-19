'From Cuis 5.0 [latest update: #4677] on 19 July 2021 at 10:48:28 am'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 10:42:16'!
iconDrawSelector
	"Must be understood by drawing canvas."

	^iconName! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 10:41:45'!
iconDrawSelector: aSymbol
	"aSymbol must be understood by drawing canvas."

	iconName _ aSymbol! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:34:59'!
drawCloseIcon
	| icon |
	icon _ self class windowButtonIcon: #closeIcon size: currentMorph morphWidth.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:03'!
drawCollapseIcon
	| icon |
	icon _ self class windowButtonIcon: #collapseIcon size: currentMorph morphWidth.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:07'!
drawDownIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:11'!
drawExpandIcon
	| icon |
	icon _ self class windowButtonIcon: #expandIcon size: currentMorph morphWidth.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:14'!
drawLeftIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:18'!
drawMenuIcon
	| icon |
	icon _ self class windowButtonIcon: #windowMenuIcon size: currentMorph morphWidth.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:22'!
drawPushPinIcon
	| icon |
	icon _ self class windowButtonIcon: #pushPinIcon size: currentMorph morphWidth.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:25'!
drawRightIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 10:35:29'!
drawUpIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	self pvtDrawButtonIcon: icon.! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/19/2021 10:34:41'!
pvtDrawButtonIcon: icon
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	| iconColor |
	iconColor _ currentMorph isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			currentMorph mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].
	self
		image: icon
		multipliedBy: iconColor
		at: (currentMorph morphExtent - icon extent //2).! !


!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'jmv 7/18/2021 19:57:10'!
windowButtonIcon: aSymbol size: finalSizeInteger
	^self cachedForms
		at: { aSymbol . finalSizeInteger }
		ifAbsentPut: [
			| icon w h factor magnifiedExtent magnifiedIcon |
			icon _ Theme current perform: aSymbol.	
			magnifiedIcon _ icon.
			w _ icon width.
			h _ icon height.
			w*h = 0 ifFalse: [
				factor _ 1.0 * finalSizeInteger / w min: 1.0 * finalSizeInteger / h.
				factor = 1.0 ifFalse: [
					magnifiedExtent _ (icon extent * factor) rounded.
					magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
			magnifiedIcon ]! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/18/2021 20:25:15'!
icon: aForm
"estos 2 lo quiero eliminar. Es todo mambo del canvas, no del boton."
	icon _ aForm.
	magnifiedIcon _ nil! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 7/18/2021 20:27:22'!
magnifiedIcon
"que quede solo en MenuItemMorph, de donde debe volar tambien eventualmente."
	| factor magnifiedExtent w h |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		w _ icon width.
		h _ icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * extent x / w min: 1.0 * extent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (icon extent * factor) rounded.
				magnifiedIcon _ icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 7/18/2021 20:27:02'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."
"este vuela, claro."
	super extentChanged: oldExtent.
	magnifiedIcon _ nil! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2021 20:26:09'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	(self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
"mhhhhhhh"
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ extent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:44:22'!
updateDownButton: aPluggableButtonMorph
	"Update the argument as a downButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawDownIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:44:13'!
updateLeftButton: aPluggableButtonMorph
	"Update the argument as a leftButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawLeftIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:44:04'!
updateRightButton: aPluggableButtonMorph
	"Update the argument as a rightButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawRightIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:43:51'!
updateUpButton: aPluggableButtonMorph
	"Update the argument as an upButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawUpIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:42:53'!
createCloseButton
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		iconDrawSelector: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:42:58'!
createCollapseButton
	^(PluggableButtonMorph model: self action: #collapse)
		iconDrawSelector: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:43:02'!
createExpandButton
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		iconDrawSelector: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/19/2021 10:43:06'!
createMenuButton
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		iconDrawSelector: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: self titleBarButtonsExtent! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 7/19/2021 10:42:49'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Preferences windowTitleFont pointSize.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: pinButton fixedWidth: buttonHW).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !


!BitBltCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/19/2021 10:42:23'!
drawButtonIcon: aPluggableButtonMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	aPluggableButtonMorph iconDrawSelector ifNotNil: [ :selector |
(#(drawLeftIcon drawRightIcon drawUpIcon drawDownIcon drawCloseIcon drawCollapseIcon drawExpandIcon drawMenuIcon drawPushPinIcon) includes: selector)
ifTrue: [
		self perform: selector.
		^true ].
].

	aPluggableButtonMorph magnifiedIcon ifNotNil: [ :theIcon | | iconColor |
		iconColor _ aPluggableButtonMorph isPressed
			ifTrue: [ `Color gray: 0.75` ]
			ifFalse: [
				aPluggableButtonMorph mouseIsOver
					ifTrue: [ `Color gray: 0.75` ]
					ifFalse: [ `Color white` ]].
		self
			image: theIcon
			multipliedBy: iconColor
			at: (aPluggableButtonMorph morphExtent - theIcon extent //2).
		^true ].
	^false! !

!methodRemoval: BitBltCanvas #drawIcon:for: stamp: 'jmv 7/19/2021 10:35:34'!
BitBltCanvas removeSelector: #drawIcon:for:!
!methodRemoval: PluggableButtonMorph #iconName: stamp: 'jmv 7/19/2021 10:44:32'!
PluggableButtonMorph removeSelector: #iconName:!
!methodRemoval: PluggableButtonMorph #iconName stamp: 'jmv 7/19/2021 10:42:35'!
PluggableButtonMorph removeSelector: #iconName!

!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawLeftIcon drawMenuIcon drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIcon:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: pvtDrawButtonIcon: setClipRect: setForm:preferSubPixelAntiAliasing: setPaintColor: updatingMorphBoundsDo:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs morph:isAtPoint:)
!


!BitBltCanvas class reorganize!
('cached forms' bottomLeftCorner:height:gradientTop:gradientBottom: bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: bottomRightCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: steButtonForm: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom: verticalGrayGradient:gradientTop:gradientBottom:)
('cached button forms' arrowOfDirection:size: buildArrowOfDirection:size: buildArrowWith:borderForm: windowButtonIcon:size:)
('cached state access' accessProtect cachedForms releaseClassCachedState releaseClassState)
!

