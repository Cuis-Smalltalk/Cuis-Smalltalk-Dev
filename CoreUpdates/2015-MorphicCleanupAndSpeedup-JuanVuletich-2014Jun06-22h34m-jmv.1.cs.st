'From Cuis 4.2 of 25 July 2013 [latest update: #2014] on 6 June 2014 at 10:40:05.022942 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/6/2014 12:16'!
addMorphFront: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint! !

!Morph methodsFor: 'private' stamp: 'jmv 6/6/2014 12:28'!
privateAddMorph: aMorph atIndex: index position: aPoint

	| oldIndex myWorld itsWorld oldOwner |
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue:[
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	aMorph privatePosition: aPoint.
	myWorld ifNotNil: [aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

!Morph methodsFor: 'private' stamp: 'jmv 6/6/2014 12:26'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	location position = aPoint ifTrue: [
		^ self ].		"Null change"

	location setPosition: aPoint! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:08'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	(location position = newPos and: [extent = newExtent]) ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.
	location setPosition: newPos.
	(self privateExtent: newExtent) ifTrue: [
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 17:59'!
privateExtent: newExtent
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	^extent = newExtent
		ifFalse: [ extent _ newExtent ]; not! !


!MagnifierMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:06'!
privateExtent: aPoint
	"Round to multiples of magnification"

	srcExtent _ (aPoint - (2 * borderWidth)) // magnification.
	^ super privateExtent: self defaultExtent! !


!MenuMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 12:12'!
fitInWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| delta |
	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self morphPosition: ((self morphPosition x + 10 - extent x) @ self morphPosition y) ].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight:
			((owner world morphExtentInWorld y) max: (self morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:01'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.

			worldState ifNotNil: [
				worldState viewBox ifNotNil: [
					worldState viewBox = self morphBoundsInWorld ifFalse: [
						worldState clearCanvas.
						worldState viewBox: self morphBoundsInWorld ]]]]; yourself! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:06'!
privateExtent: newExtent

	magnifiedIcon _ nil.
	^ super privateExtent: newExtent! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:02'!
privateExtent: newExtent
	
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ self vIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	minW _ self hIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	^ (super privateExtent: (newExtent max: (minW@minH)))
		ifTrue: [
			"Now reset widget sizes"
			scroller adjustExtent.
			self updateScrollBarsBounds.
			self setScrollDeltas ]; yourself! !


!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:06'!
privateExtent: aPoint

	maxHeight _ aPoint y.
	^ super privateExtent: aPoint! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:05'!
privateExtent: newExtent

	| newExtentToUse |
	newExtentToUse _ self isHorizontal
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	^ (super privateExtent: newExtentToUse)
		ifTrue: [
			self flag: #jmv.
			"Most times it is not necessary to recreate the buttons"
			self recreateSubmorphs ]; yourself! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:04'!
privateExtent: aPoint 
	"Set the receiver's extent to value provided. Honor my minimumExtent."

	^ super privateExtent: (aPoint max: self minimumExtent)! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:07'!
privateExtent: aPoint
	| newExtent |

	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ false ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = extent x ifTrue: [ ^ false ].

	^ (super privateExtent: newExtent)
		ifTrue: [
			self resetTextComposition.
			self editor recomputeSelection.	
			self updateFromTextComposition ]; yourself! !


!StringMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 17:53'!
privateExtent: aPoint 
	"Set the receiver's extent to value provided. Honor my minimumExtent."

	^ super privateExtent: (aPoint max: self minimumExtent)! !


!TranscriptMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:05'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			(form isNil or: [ form extent ~= aPoint ]) ifTrue: [
				form _ Form extent: aPoint depth: Display depth ]]; yourself! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 6/6/2014 11:12'!
invalidateRect: aRectangle

	| rectInOwner rectInOwns |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

 	rectInOwns _ self clipsSubmorphs
		ifTrue: [ aRectangle intersect: (0@0 extent: self morphExtent) ]
		ifFalse: [ aRectangle ].
	owner ifNotNil: [
		rectInOwner _ location displayBoundsOfTransformOf: rectInOwns.
		owner invalidateRect: rectInOwner ]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:02'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"
	self flag: #jmvVer2.
	extent = newExtent ifFalse: [
		self redrawNeeded.
		(self privateExtent: newExtent) ifTrue: [
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/6/2014 11:58'!
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
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: 19);
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/6/2014 11:46'!
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
		s morphPosition: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 11:31'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	self adjustSubmorphsLayout.
	sourceItem world addMorphFront: self.
	self world startSteppingSubmorphsOf: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 12:18'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self adjustSubmorphsLayout.
	self runningWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 12:18'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	self adjustSubmorphsLayout.
	aWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 11:21'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 1.
	p _ 5 @ 5.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 6/6/2014 11:47'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPosition: extent x - t - borderWidth @ borderWidth
		extent: t @ self vScrollBarHeight.
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:47'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPosition: extent - borderWidth - e extent: e@e;
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:47'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPosition: borderWidth@borderWidth extent: e@e;
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 6/6/2014 11:48'!
sliderGrabbedAt: handPositionRelativeToSlider

	grabPosition _ handPositionRelativeToSlider.
	sliderShadow
		morphPosition: slider morphPosition extent: slider morphExtent;
		show! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:49'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing box e |
	e _ self boxExtent.
	spacing _ e x + 2.

	box _ self createCloseBox.
	self addMorphFront: box.
	box morphPosition: 2@2 extent: e.

	box _ self createCollapseBox.
	self addMorphFront: box.
	box morphPosition: spacing+2@2 extent: e.

	box _ self createExpandBox.
	self addMorphFront: box.
	box morphPosition: spacing*2+2@2 extent: e.

	box _ self createMenuBox.
	self addMorphFront: box.
	box morphPosition: spacing*3+2@2 extent: e! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 6/6/2014 11:51'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft extent: extent.
	aWorld startSteppingSubmorphsOf: self.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 6/6/2014 11:50'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww b |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: 0@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		b _ self layoutBounds.
		layoutMorph
			morphPosition: b origin extent: b extent ].
	
	layoutNeeded _ false! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:41'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result morphPosition: 29@90 extent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:41'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result morphPosition: 149@90 extent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:41'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	self addMorph: result.
	result morphPosition: 30@7 extent: 239@15.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:55'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphPosition: 14@25 extent: extent-(28@62).
	^ result! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 12:17'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: HandleSize asPoint).
	(iconName _ handleSpec iconSymbol) ifNotNil: [
			| form |
			form _ Icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 11:42'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	nameMorph morphPosition: namePosition.
	nameBackground morphPosition: nameMorph morphPosition - 2 extent: nameMorph morphExtent + 4.
	^nameMorph! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 15:25'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	x _ aHand morphPosition x - 20.
	y _ aHand morphPosition y + 20.
	x + self morphWidth > aHand world morphWidth ifTrue: [
		x _ aHand world morphWidth - self morphWidth ].
	y + self morphHeight > aHand world morphHeight ifTrue: [
		y _ aHand morphPosition y - self morphHeight - 12 ].
	newPos _ x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 14:35'!
image: anImage 
	| newExtent |
	self redrawNeeded.
	image _ anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ]. 
	newExtent _ image extent.
	extent = newExtent ifFalse: [
		self redrawNeeded.
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 11:43'!
adjustExtent
	"And reposition submorphs"
	| w h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m morphPosition: 0@y extent: w@h.
		y _ y + h ].
	self morphExtent: w@y! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 14:34'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent |
	wrapFlag _ true.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		self redrawNeeded.
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self model: aTextModel! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 14:33'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self textComposition extent max: 9 @ AbstractFont default height) + (0 @ 2).
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.	"Too conservative: only text composition
									should cause invalidation."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/6/2014 11:43'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"
		
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed := submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) + xSep max: 0) 
			+  boundsForLayout left.
	usableHeight := boundsForLayout height - (2*ySep) max: 0.
	boundsTop    := boundsForLayout top.	
	boundsRight  := boundsForLayout right.
	
	submorphs size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphs at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := (usableHeight - ht) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: (wd rounded min: minWidth)@ ht rounded.
			
		wd > 0 ifTrue: [ theLeft := theLeft + wd + xSep min: boundsRight ]
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/6/2014 11:44'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight |

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. 	"Too small. Don't bother!!"

	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed := submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	heights := submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) + ySep max: 0) 
				+  boundsForLayout top.
	usableWidth     := boundsForLayout width - (2*xSep) max: 0.
	boundsLeft      := boundsForLayout left.	
	boundsBottom := boundsForLayout bottom.
	
	submorphs size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphs at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := (usableWidth - wd) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: wd rounded @ (ht rounded min: minHeight).
			
		ht > 0 ifTrue: [ theTop := theTop + ht + ySep min: boundsBottom ]
	]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/6/2014 11:57'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (StringMorph contents: '1').

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 120 @ 50 extent: 180 @ 100.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 320 @ 50 extent: 180 @ 100.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 520 @ 50 extent: 180 @ 100! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 6/6/2014 11:47'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphPosition: aWorld morphExtent - extent // 2 extent: w@h.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
	aWorld startSteppingSubmorphsOf: self! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 6/6/2014 11:51'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	self
		morphPosition: 0@ (self world morphExtent y - (self externalizeDistance: extent ) y)
		extent: (self internalize: self world morphExtent) x @ self defaultHeight! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 10:58'!
contents: newContents 
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			 newContents string]
		ifFalse: [
			contents = newContents ifTrue: [^self].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 11:01'!
fitContents

	self morphExtent: self measureContents! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 11:01'!
fitContents

	self morphExtent: self measureContents! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 11:01'!
fitContents

	self morphExtent: self measureContents! !

!StringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 10:58'!
font: aFont emphasis: emphasisCode
	font _ aFont.
	emphasis _ emphasisCode.
	self fitContents.
	self redrawNeeded
"
in inspector say,
	 self font: StrikeFont default emphasis: 1
"! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 15:27'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: 0@2! !


!WorldState class methodsFor: 'sample code' stamp: 'jmv 6/6/2014 11:52'!
experiment1
	"To play with and learn about morphic Display update logic.
	Remove the morphs when done!!"
	"
	WorldState experiment1
	"
	| dr morph1 morph2 s w |
	morph1 _ RectangleLikeMorph new openInWorld.
	morph2 _ RectangleLikeMorph new openInWorld.
	morph2 color: Color green.
	morph2 morphPosition: 200@800 extent: 50@40.
	morph1 morphPosition: 120@720 extent: 220@100.

	w _ self runningWorld.
	s _ w instVarNamed: 'worldState'.
	dr _ s instVarNamed: 'damageRecorder'.

	dr doFullRepaint.
	dr reset;
	"	recordInvalidRect: (100@700 corner: 400@900);"
		recordInvalidRect: (100@700 corner: 150@900);
		recordInvalidRect: (300@700 corner: 400@900).
	Display fillColor: Color gray.
	"Do not draw the area used by the small rectangle, unless it is also drawn!!"
	s drawInvalidAreasWorld: w submorphs: {morph1. morph2 }! !

!methodRemoval: TranscriptMorph #morphExtent:!
TranscriptMorph removeSelector: #morphExtent:!
!methodRemoval: StringMorph #morphExtent:!
StringMorph removeSelector: #morphExtent:!
!methodRemoval: InnerTextMorph #morphExtent:!
InnerTextMorph removeSelector: #morphExtent:!
!methodRemoval: SystemWindow #morphExtent:!
SystemWindow removeSelector: #morphExtent:!
!methodRemoval: ScrollBar #morphExtent:!
ScrollBar removeSelector: #morphExtent:!
!methodRemoval: LimitedHeightTextMorph #morphExtent:!
LimitedHeightTextMorph removeSelector: #morphExtent:!
!methodRemoval: PluggableScrollPane #morphExtent:!
PluggableScrollPane removeSelector: #morphExtent:!
!methodRemoval: PluggableButtonMorph #morphExtent:!
PluggableButtonMorph removeSelector: #morphExtent:!
!methodRemoval: PasteUpMorph #morphExtent:!
PasteUpMorph removeSelector: #morphExtent:!
!methodRemoval: MenuMorph #intoWorld:!
MenuMorph removeSelector: #intoWorld:!
!methodRemoval: MenuMorph #moveRight:!
MenuMorph removeSelector: #moveRight:!
!methodRemoval: MenuMorph #positionAt:forHand:!
MenuMorph removeSelector: #positionAt:forHand:!
!methodRemoval: MagnifierMorph #morphExtent:!
MagnifierMorph removeSelector: #morphExtent:!
!methodRemoval: RectangleLikeMorph #basicExtent:!
RectangleLikeMorph removeSelector: #basicExtent:!
