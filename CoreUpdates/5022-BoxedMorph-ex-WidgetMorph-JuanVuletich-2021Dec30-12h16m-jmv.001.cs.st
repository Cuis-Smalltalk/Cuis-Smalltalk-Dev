'From Cuis 5.0 [latest update: #5019] on 30 December 2021 at 12:17:10 pm'!
Smalltalk renameClassNamed: #WidgetMorph as: #BoxedMorph!

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/30/2021 12:16:47'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ BoxedMorph new noBorder
		color: ((target is: #SystemWindow)
			ifTrue: [target windowColor]
			ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ LabelMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition _ haloBox bottom + Preferences haloHandleSize.
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !


!BoxedMorph methodsFor: 'testing' stamp: 'jmv 12/30/2021 12:16:47'!
is: aSymbol
	^ aSymbol == #BoxedMorph or: [ super is: aSymbol ]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example1
"
	self example1
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BoxedMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example10
"
	self example10 openInWorld
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example10.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
^ pane! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example11
"
	self example11
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BoxedMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example1b
"
Based on #example1, but using some ImageMorph instead of RectangleLikeMorph, so extent is not adjustable
	self example1b
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (ImageMorph new name: #B);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BoxedMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (ImageMorph new name: #G);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ImageMorph new name: #J);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example2
	"
	self example2
	"
	| pane row |
	pane _ (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rightOrBottom);
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example20
"
	self example20
"
| column |

column _ (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 30).
column morphPosition: 150@130 extent: 400@300.
column openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BoxedMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BoxedMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (LabelMorph contents: '1').

rect1 := BoxedMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := BoxedMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	morphPosition: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (LabelMorph contents: '2').

rect1 := BoxedMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := BoxedMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (LabelMorph contents: '3').

rect1 := BoxedMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := BoxedMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 12/30/2021 12:16:47'!
example7
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example7
	"
	"============================================"
	| c colorHexValue colorName r w |
	w := SystemWindow new.
	r := LayoutMorph newRow separation: 30 @ 10.
	c := LayoutMorph newColumn separation: 20 @ 10.
	colorHexValue := LabelMorph contents:  'F97306'.
	colorName := LabelMorph contents: 'cornSilk'.

	r addMorph: colorHexValue.
	r addMorph: colorName.
	c addMorph: BoxedMorph new.
	c addMorph: r.
	w addMorph: c .
	w openInWorld.
	"============================================"! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 12:16:47'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 12/30/2021 12:16:47'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ BoxedMorph new noBorder.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ DraggeableButtonMorph new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 12/30/2021 12:16:47'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames dirtyFlagsPane namesPane fileNamesPane 
	upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor;
			   hideScrollBarsIndefinitely.
	dirtyFlagsPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	namesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor;
			   alwaysHideVerticalScrollbar.
	fileNamesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlagsPane proportionalWidth: 0.13;
		addAdjusterAndMorph: namesPane proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
		
	names leftSibling: dirtyFlags rightSibling: fileNames; scrollSiblings: true.
	dirtyFlags rightSibling: names; scrollSiblings: true.
	fileNames leftSibling: names; scrollSiblings: true.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 12/30/2021 12:16:47'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList dirtyFlagsPane changeSetListPane classListPane
	  messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlagsPane _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlagsPane proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetListPane proportionalWidth: 0.47;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
			
	"Scroll Sibling Panes together."
	changeSetList leftSibling: dirtyFlags; scrollSiblings: true.
	dirtyFlags rightSibling: changeSetList; scrollSiblings: true.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.
		

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!ResizeMorph methodsFor: 'events' stamp: 'jmv 12/30/2021 12:16:47'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	from _ self toGridPoint: localEventPosition.
	outlineMorph _ BoxedMorph new
		borderColor: `Color black`;
		color: `Color transparent`;
		openInWorld;
		hide.
	self selectTo: localEventPosition! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 12/30/2021 12:16:47'!
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
	buttonHW _ Theme current titleBarButtonsExtent x.
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
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 12/30/2021 12:16:47'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph _ BoxedMorph new noBorder.
	newMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line | | font |
		font _ Preferences standardMenuFont.
		s _ LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph _ newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry services')
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
('geometry' morphLocalBounds)
!


!BoxedMorph reorganize!
('accessing' borderColor borderColor: borderWidth borderWidth: color color:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor initialize noBorder)
('drawing' drawOn:)
('geometry' displayBoundsUpdateFrom: ensureMinimimExtent extentBorder fitInto: fullBoundsInOwner morphBottomLeft morphBottomRight morphExtent morphExtent: morphExtentInOwner: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphHeightInOwner: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphTopLeft morphTopRight morphWidth morphWidth: morphWidthInOwner: privateExtent: rotationCenter topLeftInOwner)
('geometry services' coversLocalPoint: coversPixel: includesPixel:)
('geometry testing' hasVariableExtent knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('layout' minItemWidth)
('layout-properties' layoutSpec)
('testing' is:)
('halos and balloon help' haloShowsCoordinateSystem okayToResizeEasily okayToRotateEasily okayToScaleEasily)
!


!LayoutMorph class reorganize!
('instance creation' new newColumn newRow)
('examples' example1 example10 example11 example13 example1b example2 example20 example3 example6 example7 launcherExample)
('new-morph participation' categoryInNewMorphMenu)
!


!StringRequestMorph reorganize!
('accessing' emptyTextDisplayMessage: response response:)
('drawing' drawOn:)
('initialization' acceptBlock: addTextPane addTitle: cancelBlock: defaultBorderWidth defaultColor initialize setQuery:initialAnswer: validationBlock:)
('private' cancel getUserResponseOrCancel: selectionInterval textBox)
('testing' wantsToBeOnTop)
!


!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: scrollValue)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('geometry' buttonExtent computeSlider expandSlider extentChanged: freeSliderRoom minimumExtent totalSliderRoom)
('initialization' initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs updateDownButton: updateLeftButton: updateRightButton: updateUpButton:)
('model access' internalScrollValue: scrollValue:)
('scrolling' scrollByPage scrollDown scrollDown: scrollTo: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbedAt: sliderReleased)
('testing' isHorizontal)
!


!CodePackageListWindow reorganize!
('GUI building' buildButtonPane buildMorphicWindow buildRequirementsPane initialExtent windowColor)
('commands' addRequirement browse browseChanges createPackage deletePackage)
('geometry' fontPreferenceChanged)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu building' changeSetMenu classListMenu messageMenu)
('menu commands' browseMethodConflicts browseVersions)
('keyboard shortcuts' changeSetListKey:from: classListKey:from: messageListKey:from:)
!


!ResizeMorph reorganize!
('as yet unclassified' selectTo: selectionRectangle: toGridPoint: updateOutlineMorph)
('drawing' drawGridOn: drawOn:)
('event handling testing' handlesMouseDown:)
('initialization' defaultBorderWidth initialize)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('printing' printOn:)
('accessing' action: grid:)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar wantsToBeOnTop)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!

