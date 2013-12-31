'From Cuis 4.0 of 21 April 2012 [latest update: #1403] on 21 August 2012 at 10:45:35 pm'!

!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 22:45'!
initialize
	"initialize the state of the receiver"
	super initialize.
	"initialize the receiver state related to border"
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 8/21/2012 22:38'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ RectangleLikeMorph new.
	outerBox
		morphExtent: finalSizeInteger asPoint * resizeFactor;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox morphBoundsInWorld.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPositionInOwner: 12@8.	"not a clue why these numbers work..."
	
	
	f _ outerBox imageForm: 32.
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 22:38'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphExtent y + 5).
	nameBackground morphBoundsInWorld: (nameMorph morphBoundsInWorld outsetBy: 2).
	^nameMorph! !


!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/21/2012 22:39'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphBoundsInWorld left = (pane morphBoundsInWorld left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow morphBoundsInWorld left = (row morphBoundsInWorld left + 5).
	self assert: (innerRow morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - innerRow morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 5).
	self assert: (i1 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i1 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 20).
	self assert: (i2 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i2 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 35).
	self assert: (i3 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/21/2012 22:41'!
addStayUpIcons
	| closeBox pinBox w |
	
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 42.
	self addMorphFront: 
		(LayoutMorph newRow
			morphHeight: (titleMorph morphHeight max: 19);
			morphWidth: w;	"Make room for buttons"
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/21/2012 22:40'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s.
		s morphPositionInOwner: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphHeight: pp y; morphWidth: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 8/21/2012 22:41'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleLikeMorph new.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbed.
	slider dragSelector: #scrollAbsolute:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!methodRemoval: MenuMorph #setTitleParametersFor:!
MenuMorph removeSelector: #setTitleParametersFor:!
