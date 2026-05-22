"
SystemWindow is the Morphic implementation of Window: a labelled container for rectangular views, with iconic facilities for close, collapse/expand, and resizing.

The attribute onlyActiveOnTop, if set to true (and any call to activate will set this), determines that only the top member of a collection of such windows on the screen shall be active.  To be not active means that a mouse click in any region will only result in bringing the window to the top and then making it active.
"
Class {
	#name : 'SystemWindow',
	#superclass : 'PluggableMorph',
	#instVars : [
		'labelString',
		'layoutMorph',
		'adjusters'
	],
	#classVars : [
		'TopWindow'
	],
	#classInstVars : [
		'menuNshortcutsProviders'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'menu and shortcuts' }
SystemWindow class >> at: aWidgetKey registerMenuAndShortcutsSpecProvider: aMessageSend [
	menuNshortcutsProviders ifNil: [ menuNshortcutsProviders := Dictionary new ].
	(menuNshortcutsProviders at: aWidgetKey ifAbsentPut: [ OrderedDictionary new ])
		atLast: aMessageSend put: aMessageSend.
]

{ #category : 'menu and shortcuts' }
SystemWindow class >> at: aWidgetKey unregisterMenuAndShortcutsSpecProvider: aMessageSend [

	menuNshortcutsProviders ifNotNil: [ 
		(menuNshortcutsProviders at: aWidgetKey ifAbsent: [ ^self ]) removeKey: aMessageSend ]
]

{ #category : 'top window' }
SystemWindow class >> closeTopWindow [
	"Try to close the top window.  It may of course decline"

	TopWindow ifNotNil:
		[TopWindow delete]
]

{ #category : 'instance creation' }
SystemWindow class >> editText: aTextModel label: labelString wrap: aBoolean [
	| window |
	window := self new model: aTextModel.
	window setLabel: labelString.
	window layoutMorph
		addMorph: ((TextModelMorph withModel: aTextModel)
			wrapFlag: aBoolean)
		proportionalHeight: 1.
	^ window openInWorld
]

{ #category : 'top window' }
SystemWindow class >> noteTopWindowIn: aWorld but: aWindow [
	| newTop |
	"TopWindow must be nil or point to the top window in this project."
	TopWindow := nil.
	aWorld ifNil: [^ nil].
	newTop := aWorld submorphs 
				detect: [:m | (m is: #SystemWindow) and: [m visible and: [m ~~ aWindow]]]
				ifNone: [^nil].
	newTop activate.
	^newTop
]

{ #category : 'instance creation' }
SystemWindow class >> open: model [

	^self open: model label: nil
]

{ #category : 'instance creation' }
SystemWindow class >> open: model label: aString [

	|  window |
	window := self new.
	window
		model: model;
		buildMorphicWindow.
	aString ifNotNil: [ window setLabel: aString ].
	window openInWorld.
	^window
]

{ #category : 'menu and shortcuts' }
SystemWindow class >> registerAll: specs [

	specs do: [ :aSpec | self at: aSpec first registerMenuAndShortcutsSpecProvider: aSpec second ].

]

{ #category : 'menu and shortcuts' }
SystemWindow class >> registeredMenuAndShortcutsSpecProviders: aWidgetKey [
	menuNshortcutsProviders ifNil: [ ^nil ].
	^menuNshortcutsProviders at: aWidgetKey ifAbsent: nil.
]

{ #category : 'top window' }
SystemWindow class >> releaseClassCachedState [
	TopWindow := nil
]

{ #category : 'top window' }
SystemWindow class >> sendTopWindowToBack [
	"Send the top window of the world to the back, activating the one just beneath it"

	TopWindow ifNotNil:
		[TopWindow sendToBack]
]

{ #category : 'top window' }
SystemWindow class >> topWindow [

	^TopWindow
]

{ #category : 'menu and shortcuts' }
SystemWindow class >> unregisterAll: specs [

	specs do: [ :aSpec | self at: aSpec first unregisterMenuAndShortcutsSpecProvider: aSpec second ].

]

{ #category : 'instance creation' }
SystemWindow class >> windowEdgeAdjustingMorphClass [
	"Answer the adjusting morph class."

	^ WindowEdgeAdjustingMorph
]

{ #category : 'top window' }
SystemWindow class >> windowsIn: aWorld satisfying: windowBlock [
	| windows |
	windows := OrderedCollection new.
	aWorld submorphs do: [ :m |
		((m is: #SystemWindow) and: [ windowBlock value: m ]) ifTrue: [ windows addLast: m ]].
	^ windows
]

{ #category : 'top window' }
SystemWindow >> activate [
	"Bring me to the front and make me able to respond to mouse and keyboard"

	self activateAndSendTopToBack: false
]

{ #category : 'top window' }
SystemWindow >> activateAndForceLabelToShow [
	self activate.
	self morphPosition y < 0 ifTrue: [
		self morphPosition: (self morphPosition x @ 0)]
]

{ #category : 'top window' }
SystemWindow >> activateAndSendTopToBack: aBoolean [
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	
	self isTopWindow ifTrue: [
		self comeToFront.
		^self ].

	oldTop := TopWindow.
	TopWindow := self.
	self needsRedraw: true.

	oldTop ifNotNil: [
		oldTop invalidateTitleArea.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow := oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self comeToFront.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		self submorphToFocusKeyboard
			ifNotNil: [ :fkm | w activeHand newKeyboardFocus: fkm ]
			ifNil: [ w activeHand dispatchMouseOverEvent ]]
]

{ #category : 'menu' }
SystemWindow >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
"template..."
	aCustomMenu addLine.
	aCustomMenu add: 'edit label...' action: #relabel.

]

{ #category : 'submorphs-add/remove' }
SystemWindow >> addMorph: aMorph  [
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph
]

{ #category : 'submorphs-add/remove' }
SystemWindow >> addMorph: aMorph layoutSpec: aLayoutSpec [
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph layoutSpec: aLayoutSpec
]

{ #category : 'menu' }
SystemWindow >> addTileResizerMenuTo: aMenu [
	"We can look at preferences here to decide what too do"
	(Preferences at: #tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 		action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 				action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 		action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 	action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph := TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg := ('resize', resize asPlainString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.
]

{ #category : 'menu' }
SystemWindow >> addWindowControlTo: aMenu [

	aMenu
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 		icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
										action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 					icon: #collapseIcon;
		add: 'expand / contract' 			action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize.
		
	^aMenu
]

{ #category : 'submorphs-menus' }
SystemWindow >> allRegisteredMenuAndShortcutsSpecProviders: widgetKey do: aBlock widget: aWidget [
	"Hierarchy order allows more specific classes to override choices made by superclasses."
	(self class withAllSuperclassesDownFrom: (self menuAndShortcutRegistryRoot: widgetKey)) do:
		[ :windowClass |
			(windowClass registeredMenuAndShortcutsSpecProviders: widgetKey) ifNotNil: [ :providers |
				providers do:
					[ :eachMessageSend |
						(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]]].
]

{ #category : 'resize/collapse' }
SystemWindow >> allowedArea [
	^(RealEstateAgent maximumUsableAreaInWorld: self world) insetBy: Theme current fullScreenDeskMargin
]

{ #category : 'label' }
SystemWindow >> balloonText [
	^labelString
]

{ #category : 'layout' }
SystemWindow >> beColumn [
	
	self useLinearLayout.
	layoutMorph beColumn.
]

{ #category : 'layout' }
SystemWindow >> beForm [

	self useFormLayout
]

{ #category : 'layout' }
SystemWindow >> beRow [
	
	self useLinearLayout.
	layoutMorph beRow.
]

{ #category : 'menu' }
SystemWindow >> buildWindowMenu [

	| aMenu |

	aMenu := MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 			action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon;
		add: 'close others...' 			action: #closeOtherWindows 			icon: #closeIcon.
	
	self hasSaveAs
		ifTrue: [ aMenu add: 'Save as ...' action: #saveContents icon: #saveAsIcon ].
		
	aMenu
		addLine.
		
	self addWindowControlTo: aMenu.
	self addTileResizerMenuTo: aMenu.

	^ aMenu
]

{ #category : 'GUI building' }
SystemWindow >> buttonColor [

	^Theme current buttonColorFrom: self windowColor
]

{ #category : 'user interface' }
SystemWindow >> canDiscardEditsOf: aMorphWithChanges  [

	| okToLooseChanges |
	
	aMorphWithChanges canDiscardEdits ifTrue: [ ^true ].
	
	okToLooseChanges := self isItOkToLooseChanges.
	okToLooseChanges ifTrue: [ aMorphWithChanges disregardUnacceptedEdits ].
	
	^okToLooseChanges

		
]

{ #category : 'menu' }
SystemWindow >> changeColor [
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'
]

{ #category : 'open/close' }
SystemWindow >> closeBoxHit [
	"The user clicked on the close-box control in the window title.  For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down."

	(Preferences at: #dismissAllOnOptionClose) ifTrue:
		[Sensor rawMacOptionKeyPressed ifTrue:
			[^ self world closeUnchangedWindows]].
	self delete

]

{ #category : 'open/close' }
SystemWindow >> closeOtherWindows [
	self world cleanUpWorldExceptFor: self.
]

{ #category : 'initialization' }
SystemWindow >> createCloseButton [
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		iconDrawSelector: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: Theme current titleBarButtonsExtent
]

{ #category : 'initialization' }
SystemWindow >> createCollapseButton [
	^(PluggableButtonMorph model: self action: #collapse)
		iconDrawSelector: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: Theme current titleBarButtonsExtent
]

{ #category : 'initialization' }
SystemWindow >> createExpandButton [
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		iconDrawSelector: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: Theme current titleBarButtonsExtent
]

{ #category : 'initialization' }
SystemWindow >> createMenuButton [
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		iconDrawSelector: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: Theme current titleBarButtonsExtent
]

{ #category : 'initialization' }
SystemWindow >> defaultBorderWidth [
	"Answer the default border width for the receiver."
	^ Theme current windowBorderWidth
]

{ #category : 'GUI building' }
SystemWindow >> defaultButtonPaneHeight [
	"Answer the user's preferred default height for new button panes."

	^ Theme current buttonPaneHeight
]

{ #category : 'initialization' }
SystemWindow >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ `Color white`
]

{ #category : 'open/close' }
SystemWindow >> delete [
	| thisWorld |
	(self canDiscardEditsOf: self) ifFalse: [^self].
	thisWorld := self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	self model: nil
]

{ #category : 'events' }
SystemWindow >> doubleClick: aMouseEvent localPosition: currentPosition [
	self fullScreen.
]

{ #category : 'drawing' }
SystemWindow >> drawClassicFrameOn: aCanvas color: windowFrameColor [
	"Window border encompasses title area. No round corners."

	aCanvas
		fillRectangle: (self localBounds insetBy: 1.5)
		color: color
		borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self borderColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas
		fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight)
		color: windowFrameColor.
]

{ #category : 'drawing' }
SystemWindow >> drawLabelOn: aCanvas [

	| x0 y0 f l availableW w |
	f := Preferences at: #windowTitleFont.
	x0 :=  self labelXPosition.
	y0 := borderWidth * 6 // 10.
	l := labelString.
	availableW := extent x - x0.
	w := f widthOfString: l.
	[ w > availableW ] whileTrue: [
		"l := l squeezedTo: (1.0 * l size * availableW / w) truncated."
		l := l truncateTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w := f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles
]

{ #category : 'drawing' }
SystemWindow >> drawOn: aCanvas [
	| windowFrameColor roundCorners |
	windowFrameColor := self windowFrameColor.
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			self drawRoundedFrameOn: aCanvas color: windowFrameColor ]
		ifFalse: [
			self drawClassicFrameOn: aCanvas color: windowFrameColor ].
	labelString ifNotNil: [ self drawLabelOn: aCanvas ].
]

{ #category : 'drawing' }
SystemWindow >> drawRoundedFrameOn: aCanvas color: windowFrameColor [

	aCanvas
		drawRoundedFrame: self localBounds
		border: borderWidth
		color: self borderColor
		topColor: windowFrameColor
		insideColor: color
		labelHeight: self labelHeight.
]

{ #category : 'resize/collapse' }
SystemWindow >> expandBoxHit [
	"The full screen expand box has been hit"

	self fullScreen
]

{ #category : 'geometry' }
SystemWindow >> extentChanged: oldExtent [
	"Our extent changed.
	- Reposition adjusters
	- Resize our layoutMorph"

	| h thickness w cornerExtent wh ww halfThickness |
	thickness := self borderWidth.
	halfThickness := thickness * 0.5.
	cornerExtent := thickness * 5.
	ww := extent x.
	wh := extent y.
	w := ww - cornerExtent - cornerExtent.
	h := wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) position: cornerExtent@halfThickness extent: w@0.
	(adjusters at: #bottomAdjuster) position: cornerExtent@(wh-halfThickness) extent: w@0.
	(adjusters at: #leftAdjuster) position: halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #rightAdjuster) position: ww-halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #topLeftAdjuster) position: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) position: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) position: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) position: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].
]

{ #category : 'geometry' }
SystemWindow >> fontPreferenceChanged [
	
	super fontPreferenceChanged.
	self rescaleButtons.
	self morphExtent: extent. "To honor #minimumExtent"
	layoutMorph ifNotNil: [ 	"Title and border may have changed. So #interiorOrigin and #interiorExtent may have changed too."
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].
	model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.
]

{ #category : 'user interface' }
SystemWindow >> forceSaveContents [
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	
	If none exists ask the user for a filename."
	
	self hasSaveAs ifFalse: [ ^ self ].
	self valueOfProperty: #lastFilenameUsedToSave
		ifPresentDo: [ :pathName | self forceSaveContentsTo: pathName asFullFileEntry ]
		ifAbsent: [
			self
				request: 'Enter file name'
				initialAnswer: ''
				verifying: [ :aString | aString notEmpty ]
				do: [ :fileName | | file |
					file := fileName asFileEntry.
					self saveContentsTo: file.
					self setLabel: file pathName ]].
]

{ #category : 'user interface' }
SystemWindow >> forceSaveContentsTo: aFileEntry [
	"Saves the contents to the given filename.
	
	Force overwrting the file if it exists."

	self hasSaveAs ifFalse: [^self ].
	aFileEntry forceWriteStreamDo: [ :stream |
		stream nextPutAll: model contentsForFileSaving ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName.
]

{ #category : 'geometry services' }
SystemWindow >> fullyCoveredRectangle [
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	This implementation is a good optimization for many common cases."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	b := self displayBounds.
	b isNil ifTrue: [
		^nil ].
	b := b insetBy: `2@2 corner: 1@1`.
	Theme current roundWindowCorners ifFalse: [
		^ b ].
	e := (self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling.
	^ b origin + e corner: b corner
]

{ #category : 'events' }
SystemWindow >> handlesMouseDown: aMouseEvent [
	^true
]

{ #category : 'user interface' }
SystemWindow >> hasSaveAs [
	"Returns true if the window has a model which can be saved to a file"
	^model is: #canSaveContents
]

{ #category : 'open/close' }
SystemWindow >> initialExtent [
	^ RealEstateAgent standardWindowExtent
]

{ #category : 'open/close' }
SystemWindow >> initialFrameIn: aWorld [
	^RealEstateAgent initialFrameFor: self world: aWorld
]

{ #category : 'initialization' }
SystemWindow >> initialize [
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString := 'Untitled Window'].
	
	self initializeLabelArea.
	extent := `300 @ 200`.

	self initializeAdjusters.

	"by default"
	self beColumn
]

{ #category : 'initialization' }
SystemWindow >> initializeAdjusters [

	| adjusterClass |
	adjusterClass := self windowEdgeAdjustingMorphClass.
	adjusters := Dictionary new.
	adjusters at: #topAdjuster put: adjusterClass forTop.
	adjusters at: #bottomAdjuster put: adjusterClass forBottom.
	adjusters at: #leftAdjuster put: adjusterClass forLeft.
	adjusters at: #rightAdjuster put: adjusterClass forRight.
	adjusters at: #topLeftAdjuster put: adjusterClass forTopLeft.
	adjusters at: #bottomLeftAdjuster put: adjusterClass forBottomLeft.
	adjusters at: #topRightAdjuster put: adjusterClass forTopRight.
	adjusters at: #bottomRightAdjuster put: adjusterClass forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ]
]

{ #category : 'initialization' }
SystemWindow >> initializeLabelArea [
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft := (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing := Theme current titleBarButtonsExtent x *13//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft
]

{ #category : 'geometry' }
SystemWindow >> interiorExtent [
	| p |
	p := super interiorExtent.
	^p x @ (p y - self labelHeight)
]

{ #category : 'geometry' }
SystemWindow >> interiorOrigin [
	| p |
	p := super interiorOrigin.
	^p x @ (p y + self labelHeight)
]

{ #category : 'initialization' }
SystemWindow >> intoWorld: aWorld [
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self activate.
]

{ #category : 'change reporting' }
SystemWindow >> invalidateTitleArea [

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (`0@0` extent: extent x @ (self labelHeight + borderWidth))
]

{ #category : 'testing' }
SystemWindow >> is: aSymbol [
	^ aSymbol == #SystemWindow or: [ super is: aSymbol ]
]

{ #category : 'drawing' }
SystemWindow >> isHighlighted [
	^super isHighlighted or: [self previewing]
]

{ #category : 'user interface' }
SystemWindow >> isItOkToLooseChanges [
	
	^ self confirm:
'Changes have not been saved.
Is it OK to cancel those changes?'.
]

{ #category : 'top window' }
SystemWindow >> isTopWindow [

	^ self == TopWindow
]

{ #category : 'label' }
SystemWindow >> label [
	^ labelString
]

{ #category : 'label' }
SystemWindow >> labelHeight [
	"Answer the height for the window label."
	^ (Preferences at: #windowTitleFont) lineSpacing+1
]

{ #category : 'geometry' }
SystemWindow >> labelXPosition [
	| f |
	f := Preferences at: #windowTitleFont.
	^ f lineSpacing * 5 + borderWidth
]

{ #category : 'layout' }
SystemWindow >> layoutMorph [
	^layoutMorph
]

{ #category : 'drawing' }
SystemWindow >> makeMeFullyVisible  [

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-extent)]) ifTrue: [
		^ self "OK -- visible"].
"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self extentInWorld world: self world) topLeft
"
	self fitInOwner

]

{ #category : 'drawing' }
SystemWindow >> makeMeVisible  [

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft
"
	self fitInOwner
]

{ #category : 'menu' }
SystemWindow >> makeSecondTopmost [
	| thisWorld |
	thisWorld := self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorph: self behind: nextWindow ]
]

{ #category : 'submorphs-menus' }
SystemWindow >> menuAndShortcutRegistryRoot: widgetKey [
	"Answer the topmost class whose menu and shortcut registry we honor.
	It is SystemWindow by default.
	But classes redefining menus may want to ignore providers registered assuming menus that they redefine.
	See inheritance."
	^SystemWindow
]

{ #category : 'submorphs-menus' }
SystemWindow >> menuNshortcutsSpecsDo: aBlock [
	"Iterate over our own menu and shortcut specs.
	Specific implementation in SystemWindows."

	self windowMenuSpec do: aBlock.
]

{ #category : 'submorphs-menus' }
SystemWindow >> menuNshortcutsSpecsFor: aWidget do: aBlock [
	"aWidget belongs in our submorphs tree.
	Some morphs (for instance PluggableMorphs) may have a #menuNshortcutsKey, provided by us on
	creation, that is used here to pick the proper menu-and-shortcut specs for it.
	SystemWindow also provides a class level registry that allows adding / modifying / removing shortcuts
	and menu entries by subclassing the Window class. See class ivar 'menuNshortcutsProviders'. "

	aWidget menuNshortcutsKey ifNotNil: [ :widgetKey |
		(widgetKey numArgs = 0
			ifTrue: [self perform: widgetKey]
			ifFalse: [self perform: widgetKey with: aWidget]) do: aBlock.
		self allRegisteredMenuAndShortcutsSpecProviders: widgetKey do: aBlock widget: aWidget ].
]

{ #category : 'geometry' }
SystemWindow >> minimumExtent [

	^layoutMorph minimumExtent + (borderWidth * 2) + (0@self labelHeight)
		max: Theme current titleBarButtonsExtent x * 6 @ 0
]

{ #category : 'events' }
SystemWindow >> mouseButton1Down: aMouseEvent localPosition: localEventPosition [
	aMouseEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2:  self
		event:  aMouseEvent 
		clkSel:  nil
		clkNHalf:  nil
		dblClkSel:  #doubleClick:localPosition:
		dblClkNHalfSel:  nil
		tripleClkSel:  nil
		dragSel: #dragEvent:localPosition:
	
]

{ #category : 'moving' }
SystemWindow >> moveBottom [
	"Move the window to the bottom edge of its owner/world/display."

	self morphPosition: self morphPosition x @ (self allowedArea bottom - self morphHeight)
]

{ #category : 'moving' }
SystemWindow >> moveBottomLeft [
	"Move the window to the bottom left corner of its owner/world/display."

	self morphPosition: self allowedArea left @ (self allowedArea bottom - self morphHeight)
]

{ #category : 'moving' }
SystemWindow >> moveBottomRight [
	"Move the window to the bottom right corner of its owner/world/display."

	self morphPosition: self allowedArea bottomRight - self morphExtent
]

{ #category : 'moving' }
SystemWindow >> moveLeft [
	"Move the window to the left edge of its owner/world/display."

	self morphPosition: self allowedArea left @ self morphPosition y
]

{ #category : 'moving' }
SystemWindow >> moveRight [
	"Move the window to the right edge of its owner/world/display."

	self morphPosition: (self allowedArea right - self morphWidth) @ self morphPosition y
]

{ #category : 'moving' }
SystemWindow >> moveTop [
	"Move the window to the top edge of its owner/world/display."

	self morphPosition: self morphPosition x @ self allowedArea top
]

{ #category : 'moving' }
SystemWindow >> moveTopLeft [
	"Move the window to the top left corner of its owner/world/display."

	self morphPosition: self allowedArea topLeft
]

{ #category : 'moving' }
SystemWindow >> moveTopRight [
	"Move the window to the tog right corner of its owner/world/display."

	self morphPosition: (self allowedArea right - self morphWidth) @ self allowedArea top
]

{ #category : 'user interface' }
SystemWindow >> notifyUserWith: message [
	"Notifies the user with a message and an 'ok' button"
	| morph |
	morph := MenuMorph new.
	morph addTitle: message.
	morph add: 'Ok' action: nil.
	morph openInWorld.
]

{ #category : 'menu' }
SystemWindow >> offerWindowMenu [

	self buildWindowMenu popUpInWorld: self world
]

{ #category : 'user interface' }
SystemWindow >> okToChange [

	^self canDiscardEditsOf: layoutMorph
]

{ #category : 'user interface' }
SystemWindow >> okToChangeDueTo: aMorph [
	
	^self okToChange
]

{ #category : 'initialization' }
SystemWindow >> openInWorld [
	"Ensure all widgets have proper colors before opening"
	Theme current widgetsColorFromToolColor ifTrue: [
		self widgetsColor: self windowColor ].
	super openInWorld.
	(Preferences at: #expandAllWindows) ifTrue: [
		self expandBoxHit ].
]

{ #category : 'open/close' }
SystemWindow >> openInWorld: aWorld [
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect := self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
]

{ #category : 'structure' }
SystemWindow >> owningWindow [
	"Return the first enclosing morph that is a kind of Window, or nil if none"

	^ self
]

{ #category : 'printing' }
SystemWindow >> printOn: aStream  [
	aStream nextPutAll: labelString asPlainString
]

{ #category : 'label' }
SystemWindow >> relabel [
	self request: 'New title for this window' initialAnswer: labelString verifying: [:aString| aString notEmpty] do: [:aString| self setLabel: aString]
]

{ #category : 'geometry' }
SystemWindow >> rescaleButtons [
	"boxExtent changed. Update my buttons."

	| buttonExtent buttonPos spacing |
	buttonExtent := Theme current titleBarButtonsExtent.
	buttonPos := (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing := Theme current titleBarButtonsExtent x *13//10.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos := buttonPos + (spacing@0).
		].
	]
]

{ #category : 'resize/collapse' }
SystemWindow >> resize [
	| resizeMorph |
	resizeMorph := ResizeMorph new morphExtent: `200@150`.
	resizeMorph action: [self resize: (resizeMorph selectionRectangle: Display extent)].
	resizeMorph morphPosition: self world activeHand morphPosition.
	resizeMorph openInWorld
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeBottom [
	self resize: (self allowedArea top: self allowedArea center y)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeBottomLeft [
	self resize: (self allowedArea leftCenter corner: self allowedArea bottomCenter)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeBottomRight [
	self resize: (self allowedArea center corner: self allowedArea corner)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeFull [
	self resize: self allowedArea
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeLeft [
	self resize: (self allowedArea right: self allowedArea center x)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeRight [
	self resize: (self allowedArea left: self allowedArea center x)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeTop [
	self resize: (self allowedArea bottom: self allowedArea center y)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeTopLeft [
	self resize: (self allowedArea origin corner: self allowedArea center)
]

{ #category : 'resize/collapse' }
SystemWindow >> resizeTopRight [
	self resize: (self allowedArea topCenter corner: self allowedArea rightCenter)
]

{ #category : 'user interface' }
SystemWindow >> saveContents [
	"Prompts the user for a file name and saves the contents to the file"

	self hasSaveAs ifFalse: [ ^ self ].
	self
		request: 'Enter file name'
		initialAnswer: (self valueOfProperty: #lastFilenameUsedToSave ifAbsent: [ '' ])
		verifying: [ :aString |
			aString notEmpty ]
		do: [ :fileName | | file |
			file := fileName asFileEntry.
			self saveContentsTo: file.
			self setLabel: file pathName ].
]

{ #category : 'user interface' }
SystemWindow >> saveContentsTo: aFileEntry [
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	aFileEntry writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream nextPutAll: model contentsForFileSaving.
			self 
				setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName;
				notifyUserWith: 'Contents saved to', String newLineString, aFileEntry pathName ]].
]

{ #category : 'menu' }
SystemWindow >> sendToBack [
	| thisWorld |
	thisWorld := self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorphBack: self ]
]

{ #category : 'label' }
SystemWindow >> setLabel: aString [

	labelString := aString.
	self invalidateTitleArea
]

{ #category : 'menu' }
SystemWindow >> setWindowColor [
	"Allow the user to select a new basic color for the window"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self widgetsColor;
		putUpFor: self near: self morphFullBoundsInWorld"
]

{ #category : 'menu' }
SystemWindow >> setWindowColor: incomingColor [
	| existingColor aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor := incomingColor asNontranslucentColor.
	aColor = `Color black` ifTrue: [^ self].
	existingColor := self widgetsColor.
	existingColor ifNil: [^ Smalltalk beep].
	self widgetsColor: aColor.
	self redrawNeeded
]

{ #category : 'stepping' }
SystemWindow >> stepAt: millisecondSinceLast [
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [ self wantsStepsWhenCollapsed ]) ifTrue: [
		model ifNotNil: [ model stepAt: millisecondSinceLast ].
		super stepAt: millisecondSinceLast ]
]

{ #category : 'stepping' }
SystemWindow >> stepTime [
	^ 200 "milliseconds"
]

{ #category : 'GUI building' }
SystemWindow >> textBackgroundColor [

	^Theme current paneBackgroundFrom: self widgetsColor
]

{ #category : 'label' }
SystemWindow >> update: aSymbol [
	super update: aSymbol.
	aSymbol == #relabel
		ifTrue: [ model ifNotNil: [ self setLabel: model labelString ]]
]

{ #category : 'layout' }
SystemWindow >> useFormLayout [

	(layoutMorph is: #FormLayoutMorph) ifFalse: [
		self useLayoutMorph:  LayoutMorph newForm ].
]

{ #category : 'layout' }
SystemWindow >> useLayoutMorph: aNewLayout [

	self validateLayoutMorphCanBeChanged.

	aNewLayout adoptWidgetsColor: self windowColor.
	
	layoutMorph
		ifNil: [ self addMorphFront: aNewLayout ]
		ifNotNil: [ self replaceSubmorph: layoutMorph by: aNewLayout ].
		
	layoutMorph :=  aNewLayout.
	
	"We need this so that the layout is within the interiorExtent"
	self extentChanged: self morphExtent.
	
	^ layoutMorph
]

{ #category : 'layout' }
SystemWindow >> useLinearLayout [

	(layoutMorph is: #LinearLayoutMorph) ifFalse: [
		self useLayoutMorph:  LayoutMorph newColumn ].
]

{ #category : 'layout' }
SystemWindow >> validateLayoutMorphCanBeChanged [

	(layoutMorph notNil and: [ layoutMorph hasSubmorphs ])
		ifTrue: [ self error: 'Cannot replace a LayoutMorph containing submorphs' ]
]

{ #category : 'drawing' }
SystemWindow >> visible: aBoolean [
	self visible == aBoolean
		ifTrue: [ ^ self ].
	super visible: aBoolean.
	aBoolean ifTrue: [
		self activate ]
]

{ #category : 'stepping' }
SystemWindow >> wantsSteps [
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	^ model wantsSteps
]

{ #category : 'stepping' }
SystemWindow >> wantsStepsWhenCollapsed [
	"Default is not to bother updating collapsed windows"

	^ false
]

{ #category : 'events' }
SystemWindow >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph"
	^aMorph isWorldMorph or:[Preferences at: #systemWindowEmbedOK]
]

{ #category : 'panes' }
SystemWindow >> widgetsColor [

	^borderColor
]

{ #category : 'panes' }
SystemWindow >> widgetsColor: aColor [
	"aColor will be used for titles, borders, etc.
	A variation of it, #paneColorFrom:, will be used for panes background"

	borderColor := aColor.
	self color: self textBackgroundColor.
	self adoptWidgetsColor: borderColor
]

{ #category : 'resizing' }
SystemWindow >> windowBottom: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	self morphHeight: (self internalize: aPointInOwner) y.
]

{ #category : 'resizing' }
SystemWindow >> windowBottomLeft: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta := (self internalize: aPointInOwner) - self morphBottomLeft.
	self morphExtent: extent + (delta x negated @ delta y).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphBottomLeft).
]

{ #category : 'resizing' }
SystemWindow >> windowBottomRight: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	self morphExtent: (self internalize: aPointInOwner).
]

{ #category : 'GUI building' }
SystemWindow >> windowColor [
	"Some default"
	^model class windowColor
]

{ #category : 'initialization' }
SystemWindow >> windowEdgeAdjustingMorphClass [
	"Answer the adjusting morph class."

	^ self class windowEdgeAdjustingMorphClass
]

{ #category : 'panes' }
SystemWindow >> windowFrameColor [

	| windowFrameColor |
	windowFrameColor := self borderColor.
	self isTopWindow
		ifTrue: [ windowFrameColor := windowFrameColor lighter ].
	^windowFrameColor
]

{ #category : 'resizing' }
SystemWindow >> windowLeft: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner newPositionInOwnCoordinates |
	cornerInOwner := self externalize: extent.
	newPositionInOwnCoordinates := (self internalize: aPointInOwner) x @ 0.
	self morphPosition: (self externalize: newPositionInOwnCoordinates).
	self morphExtent: (self internalize: cornerInOwner).
]

{ #category : 'submorphs-menus' }
SystemWindow >> windowMenuSpec [
	"Spec for the menu and shortcut keys for SystemWindows.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 			$w 	window 	delete)
	)
]

{ #category : 'resizing' }
SystemWindow >> windowRight: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	self morphWidth: (self internalize: aPointInOwner) x.
]

{ #category : 'resizing' }
SystemWindow >> windowTop: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner newPositionInOwnCoordinates |
	cornerInOwner := self externalize: extent.
	newPositionInOwnCoordinates := 0 @ (self internalize: aPointInOwner) y.
	self morphPosition: (self externalize: newPositionInOwnCoordinates).
	self morphExtent: (self internalize: cornerInOwner).
]

{ #category : 'resizing' }
SystemWindow >> windowTopLeft: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner |
	cornerInOwner := self externalize: extent.
	self morphPosition: aPointInOwner.
	self morphExtent: (self internalize: cornerInOwner).
]

{ #category : 'resizing' }
SystemWindow >> windowTopRight: aPointInOwner [
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta := (self internalize: aPointInOwner) - self morphTopRight.
	self morphExtent: extent + (delta x @ delta y negated).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphTopRight).
]
