'From Cuis 4.2 of 25 July 2013 [latest update: #2019] on 8 June 2014 at 10:07:16.130532 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/8/2014 20:27'!
addMorphBack: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: submorphs size+1 position: aPoint! !


!Morph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:34'!
inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."
	"
	(EllipseMorph new morphExtent: 200@170) inATwoWayScrollPane openInHand
	"

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget morphExtent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	widget color: self color darker darker.
	^widget! !


!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:25'!
setCompleter: anAutoCompleter position: aPoint 
	completer _ anAutoCompleter.
	self resetMenu.
	self morphPosition: aPoint.
	self openInWorld! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/8/2014 20:32'!
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
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 6/8/2014 20:32'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock position: (0@ -20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 6/8/2014 20:33'!
addToScroller: aMorph

	scroller addMorph: aMorph position: 0@0! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:11'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:13'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:18'!
createCloseBox
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		icon: Theme current closeIcon;
		setBalloonText: 'close this window';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:18'!
createCollapseBox
	^(PluggableButtonMorph model: self action: #collapse)
		icon: Theme current collapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:18'!
createExpandBox
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		icon: Theme current expandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:18'!
createMenuBox
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		icon: Theme current windowMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:23'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing |
	spacing _ self boxExtent x + 2.
	self addMorph: self createCloseBox position: 2@2.
	self addMorph: self createCollapseBox position: spacing+2@2.
	self addMorph: self createExpandBox position: spacing*2+2@2.
	self addMorph: self createMenuBox position: spacing*3+2@2! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 19:55'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result morphExtent: 93@27.
	self addMorph: result position: 29@90.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 19:56'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result morphExtent: 93@27.
	self addMorph: result position: 149@90.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 19:57'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	result morphExtent: 239@15.
	self addMorph: result position: 30@7.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 19:58'!
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
	result morphExtent: extent-(28@62).
	self addMorph: result position: 14@25.
	^ result! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 6/8/2014 19:56'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	FillInTheBlankMorph request: 'What is your favorite color?'
	"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 6/8/2014 20:01'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	self addMorph: nameBackground position: namePosition - 2.
	self addMorph: nameMorph position: namePosition.
	^nameMorph! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 6/8/2014 20:27'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	delta _ m morphExtent // 2.
	self addMorphBack: m position: delta negated! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 6/8/2014 20:28'!
grabMorph: aMorph from: formerOwner at: aPoint
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph position: aPoint.
	aMorph justGrabbedFrom: formerOwner.! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:28'!
example1
"
	self example1
"
| pane row |
pane _ LayoutMorph newColumn separation: 5.
pane color: Color red.

row _ LayoutMorph newRow.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:29'!
example11
"
	self example11
"
| pane row |
pane _ LayoutMorph newColumn separation: 5.
pane color: Color red.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@230.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:30'!
example13
	"
	self example13
	"
	| pane row innerRow |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ (LayoutMorph newRow separation: 5) color: Color red.
	innerRow
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:31'!
example2
	"
	self example2
	"
	| pane row |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:31'!
example3
	"
	self example3
	"
	| pane row innerRow |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ (LayoutMorph newRow separation: 5) color: Color red.
	innerRow
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@230.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:03'!
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
	morphPosition: 120 @ 50 extent: 180 @ 100;
	openInWorld.

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
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


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
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 6/8/2014 20:07'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphPosition: aWorld morphExtent - extent // 2 extent: w@h.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
	aWorld addMorph: self.
	aWorld startSteppingSubmorphsOf: self! !

