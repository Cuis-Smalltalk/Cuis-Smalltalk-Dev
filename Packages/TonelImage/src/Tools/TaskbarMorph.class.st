"
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.
scale allows 1x 2x 4x taskbar height. [scale= 1,2,4]
"
Class {
	#name : 'TaskbarMorph',
	#superclass : 'LinearLayoutMorph',
	#instVars : [
		'viewBox',
		'scale',
		'clock',
		'showFPS'
	],
	#category : 'Tools-Taskbar',
	#package : 'Tools',
	#tag : 'Taskbar'
}

{ #category : 'new-morph participation' }
TaskbarMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu
	
	More than one taskbar confuses the running wolrd!"

	^ false
]

{ #category : 'services' }
TaskbarMorph >> aboutToCollapse: aMorph [
	"Add a button for aMorph if not already there (see #taskbarIncludesAllWindows)"

	(self buttonFor: aMorph) ifNil: [
		self addButtonFor: aMorph ]
]

{ #category : 'services' }
TaskbarMorph >> addButtonFor: aMorph [

	| button taskbarButtonIcon |
	aMorph == self ifFalse: [
		button := HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #showAndComeToFront
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		(aMorph is: #SystemWindow) ifTrue: [
			button
				secondaryActionSelector: #offerWindowMenu ].
		taskbarButtonIcon := aMorph
			valueOfProperty: #taskbarButtonIcon
			ifAbsentPut: [aMorph imageForm: self defaultHeight*5//4 @ self defaultHeight depth: 32].
		button
			color: color;
			icon: taskbarButtonIcon;
			setBalloonText: #label.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]
]

{ #category : 'geometry' }
TaskbarMorph >> basicDisplayBounds [

	self visible ifFalse: [ ^nil ].
	^self morphPosition extent: extent
]

{ #category : 'accessing' }
TaskbarMorph >> buttonFor: aMorph [
	
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model == aMorph
				ifTrue: [ ^button ]]
	].
	^nil
]

{ #category : 'accessing' }
TaskbarMorph >> clockString [
	| ms |
	^String streamContents: [ :strm |
		strm nextPutAll: '  '.
		DateAndTime now printHMSOn: strm.
		showFPS ifTrue: [
			strm nextPutAll: ' FPS: '.
			ms := self world millisecondsSinceLastCycle.
			ms > 1
				ifTrue: [(1000.0 / ms) printOn: strm fractionDigits: 1 ]
				ifFalse: [ strm nextPutAll: '>500' ]].
		strm nextPutAll: ' ' ].
]

{ #category : 'initialization' }
TaskbarMorph >> defaultColor [
	^ `Color lightGray`
]

{ #category : 'initialization' }
TaskbarMorph >> defaultHeight [

	^ ((Preferences at: #windowTitleFont) lineSpacing * 2 * self scale) asInteger
]

{ #category : 'initialization' }
TaskbarMorph >> defaultScale [

	 ^ 1
]

{ #category : 'submorphs-add/remove' }
TaskbarMorph >> delete [

	| w |
	self restoreAll.
	super delete.
	w := self world ifNil: [ self runningWorld ].
	w ifNotNil: [ w taskbarDeleted ]
]

{ #category : 'notifications' }
TaskbarMorph >> fontPreferenceChanged [
	clock font: nil.
	self scale: self scale.
]

{ #category : 'menu' }
TaskbarMorph >> getMenu [

	| menu |
	menu := MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Small Height' action: #scaleSmall;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4;
		add: 'Show FPS' action: #showFPS;
		add: 'Hide FPS' action: #hideFPS.
	^menu
]

{ #category : 'event handling testing' }
TaskbarMorph >> handlesMouseDown: aMouseButtonEvent [

	^ true
]

{ #category : 'accessing' }
TaskbarMorph >> hideFPS [
	 showFPS := false.
	clock step; morphExtent: `0@0`.
]

{ #category : 'initialization' }
TaskbarMorph >> initialize [
	super initialize.
	viewBox := LinearLayoutMorph newRow color: self defaultColor.
	showFPS := false.
	clock := UpdatingLabelMorph 
		contents: '' 
		font: (Preferences at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	clock
		target: self;
		getSelector: #clockString;
		stepTime: 1000;
		setBalloonText: [ Date today asString ].
	self
		addMorph: clock
		layoutSpec:  (LayoutSizeSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSizeSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ].
	clock
		setProperty: #handlesMouseDown: toValue: true;
		setProperty: #'mouseButton1Down:localPosition:' 
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self positionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu extentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]
]

{ #category : 'testing' }
TaskbarMorph >> is: aSymbol [
	^ aSymbol == #TaskbarMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
TaskbarMorph >> isSticky [
	"answer whether the receiver is Sticky"
	^true
]

{ #category : 'submorphs-add/remove' }
TaskbarMorph >> noteNewOwner: aMorph [
	"I have just been added as a submorph of aMorph"

	super noteNewOwner: aMorph.
	aMorph submorphsDo: [ :subMorph |
		self refreshTaskbarFor: subMorph ].
	self screenSizeChanged.
]

{ #category : 'services' }
TaskbarMorph >> refreshTaskbarFor: aMorph [

	aMorph isIncludedInTaskbar
		ifTrue: [
			(self buttonFor: aMorph) ifNil: [self addButtonFor: aMorph ]]
		ifFalse: [ self removeButtonFor: aMorph ].
]

{ #category : 'services' }
TaskbarMorph >> removeButtonFor: aMorph [

	(self buttonFor: aMorph) ifNotNil: [ :b |
		b delete ]
]

{ #category : 'services' }
TaskbarMorph >> restoreAll [
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model showAndComeToFront ]	]
]

{ #category : 'accessing' }
TaskbarMorph >> scale [

	 ^ scale ifNil: [ self defaultScale ] ifNotNil: [ scale ]
]

{ #category : 'resize' }
TaskbarMorph >> scale: anInteger [

	(anInteger between: 1/2 and: 4) ifFalse: [ self error: 'scale should be 1/2, 1, 2 or 4' ].
	scale := anInteger.
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]
]

{ #category : 'resize' }
TaskbarMorph >> scaleNormal [

	self scale: 1
]

{ #category : 'resize' }
TaskbarMorph >> scaleSmall [

	self scale: 1/2
]

{ #category : 'resize' }
TaskbarMorph >> scaleX2 [

	self scale: 2
]

{ #category : 'resize' }
TaskbarMorph >> scaleX4 [

	self scale: 4
]

{ #category : 'events' }
TaskbarMorph >> screenSizeChanged [
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	self world ifNotNil: [ :w |
		y := w morphExtent y - self defaultHeight.
		e := (self internalizeDistance: w morphExtent x @ self defaultHeight) asIntegerPoint.
		self position: 0@y extent: e ].
]

{ #category : 'accessing' }
TaskbarMorph >> showFPS [
	 showFPS := true.
]

{ #category : 'services' }
TaskbarMorph >> wasDeleted: aMorph [
	"aMorph was deleted. Remove button for aMorph"

	self removeButtonFor: aMorph
]

{ #category : 'services' }
TaskbarMorph >> wasOpened: aMorph [
	"aMorph was added to the world. Add button for aMorph if appropriate."

	aMorph isIncludedInTaskbar ifTrue: [
		self addButtonFor: aMorph ]
]
