'From Cuis 4.2 of 25 July 2013 [latest update: #2463] on 27 August 2015 at 1:40:34.111856 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding cachedMinExtent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!Morph methodsFor: 'geometry' stamp: 'KenD 8/24/2015 19:36'!
extentBorder
	"This is the number of pixels to add to internal minimum to calculate
	my minimumExtent.  I don;t have to do anything here. 
	This is the default for my subclasses"
	
	^ 0
	
! !

!Morph methodsFor: 'geometry' stamp: 'KenD 8/26/2015 12:44'!
fontPreferenceChanged
	"Preferred fonts scale a number of window relations.
	Let morphs which rely on this updte themselves.
	
	Note that the fontPreferenceChanged message is typically
	sent to the current world.  As a PasteUpMorph iinherits from me
	the code below works fine for this."
	
	"I  do nothing myself but my submorphs may."
	
	self submorphsDo: [ :m | m fontPreferenceChanged. ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'KenD 8/27/2015 08:21'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self rescaleButtons.
	self model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.
! !

!SystemWindow methodsFor: 'geometry' stamp: 'KenD 8/26/2015 21:50'!
rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonExtent buttonPos buttonDelta |
	buttonExtent := self boxExtent.
	buttonPos := 2@2.
	buttonDelta := self boxExtent x + 2.
	self submorphsReverseDo: [ :aMorph |
		(aMorph isKindOf: PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos := (buttonPos x  + buttonDelta) @ 2.
		].
	].
! !


!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 8/26/2015 14:40'!
calculateMinimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width := height := 0.
	(self direction =  #vertical)
		ifTrue: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumExtent.
				"use maximum width across submorphs"
				width := width max: smMinExtent x. 
				"sum up submorph heights"
				height := height + smMinExtent y. 
			]
		]
		ifFalse: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumExtent.
				"sum up submorphs width"
				width := width + smMinExtent x. 
				"use maximum height across submorph"
				height := height max: smMinExtent y. 
			]
		].

	^ (width @ height) + self extentBorder.! !

!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 8/26/2015 12:52'!
fontPreferenceChanged
	"Something may have changed.  
	Update my cache with the current size"
	
	super fontPreferenceChanged.
	cachedMinExtent := self calculateMinimumExtent ! !

!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 8/26/2015 13:06'!
minimumExtent
	"Answer my cached minimumExtent.
	This may be expensive to calculate, so I only do that as required."
	
	cachedMinExtent 
		ifNil: [ cachedMinExtent := self calculateMinimumExtent ].
	^ cachedMinExtent ! !



!Theme class methodsFor: 'user interface' stamp: 'KenD 8/26/2015 15:03'!
changeFontSizes

	| menu result |
	menu _ SelectionMenu fromArray: {
		{ 'Huge Fonts'. #hugeFonts}.
		{ 'Very big Fonts'. #veryBigFonts}.
		{ 'Big Fonts'. #bigFonts}.
		{ 'Standard Fonts'. #standardFonts}.
		{ 'Small Fonts'. #smallFonts}.
		{ 'Very small Fonts'. #verySmallFonts}.
		{ 'Tiny Fonts'. #tinyFonts}}.
	result _ menu startUpWithCaption: 'Choose a size'.

	result ifNotNil: [
		Preferences perform: result.
		DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].
		self inform: 'Please close and reopen all windows' ]! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding cachedMinExtent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph reorganize!
('*morphic-misc1' addCustomMenuItems:hand: direction: editMe padding)
('accessing' adoptWidgetsColor: direction padding: separation: xSeparation ySeparation)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' calculateMinimumExtent fontPreferenceChanged minimumExtent)
('initialization' beColumn beRow defaultColor initialize)
('layout' layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn: submorphsToLayout)
('submorphs-add/remove' addAdjusterMorph addMorph: addMorph:layoutSpec:)
('testing' is:)
('private' minPaneHeightForReframe minPaneWidthForReframe proportionalHeightNormalizationFactor proportionalWidthNormalizationFactor)
!


!WorkspaceWindow reorganize!
('menu' allowsMorphDropWording offerWindowMenu)
('drag n drop' allowsMorphDrop wantsDroppedMorph:event:)
('access' toggleAcceptDroppedMorphs)
('initialization' initialize)
('geometry')
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' addPossiblyUncoveredAreasIn:to: drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minPaneHeightForReframe minPaneWidthForReframe minimumExtent rescaleButtons)
('initialization' boxExtent createCloseBox createCollapseBox createExpandBox createMenuBox defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: buildWindowMenu changeColor fullScreen makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent openInWorld:)
('panes' setUpdatablePanesFrom: updatablePanes widgetsColor widgetsColor:)
('printing' printOn:)
('resize/collapse' expandBoxHit)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing' submorphsDrawingOutsideReverseDo:)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is: isOpaqueMorph)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' okToChange)
!

