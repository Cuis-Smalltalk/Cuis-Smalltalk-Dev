'From Cuis 5.0 [latest update: #4396] on 12 October 2020 at 9:05:07 pm'!
Smalltalk renameClassNamed: #StringMorph as: #LabelMorph!

!LabelMorph commentStamp: 'jmv 10/12/2020 21:03:15' prior: 0!
LabelMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!


!IndentingListItemMorph commentStamp: 'jmv 10/12/2020 21:03:00' prior: 0!
An IndentingListItemMorph is a LabelMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph.

It will also display lines around the toggle if the #showLinesInHierarchyViews Preference is set.

Instance variables:

indentLevel <SmallInteger> 	the indent level, from 0 at the root and increasing by 1 at each level of the hierarchy.

isExpanded <Boolean>		true if this item is expanded (showing its children)

complexContents <ListItemWrapper>	an adapter wrapping my represented item that can answer its children, etc.
	
firstChild <IndentingListItemMorph|nil>	my first child, or nil if none
	
container <SimpleHierarchicalListMorph>	my container
	
nextSibling <IndentingListItemMorph|nil>	the next item in the linked list of siblings, or nil if none.

icon 										a 16 x 16 form or nil

Contributed by Bob Arning as part of the ObjectExplorer package.
Don't blame him if it's not perfect.  We wanted to get it out for people to play with.!

Smalltalk renameClassNamed: #UpdatingStringMorph as: #UpdatingLabelMorph!

!UpdatingLabelMorph commentStamp: '<historical>' prior: 0!
UpdatingLabelMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!


!LabelMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:06:00'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !

!LabelMorph class methodsFor: 'instance creation' stamp: 'jmv 10/12/2020 21:04:02'!
contents: aString

	^ self contents: aString font: nil! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 10/12/2020 21:02:19'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (LabelMorph contents: '1').

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
pane addMorph: (LabelMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (LabelMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 10/12/2020 21:02:19'!
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
	c addMorph: RectangleLikeMorph new.
	c addMorph: r.
	w addMorph: c .
	w openInWorld.
	"============================================"! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:02:19'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	subLabelMorph _ LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	progress _ ProgressBarMorph new.
	progress morphExtent: 200 @ FontFamily defaultLineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: FontFamily defaultLineSpacing! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:02:38'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingLabelMorph initializedInstance
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8
! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 10/12/2020 21:02:19'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
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


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 10/12/2020 21:02:19'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem sm |
	self listMorph highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.
	
	sm _ LabelMorph contents: listItem.
	dragged _ DraggingGuideMorph new.
	dragged addMorph: sm.
	dragged morphExtent: sm morphExtent.
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 10/12/2020 21:02:19'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 10/12/2020 21:02:19'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
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
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
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
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
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
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.47;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

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
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:02:19'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ LabelMorph new contents: queryString.
	result lock.
	result morphExtent: `24@2` * self sizeUnit.
	self addMorph: result position: `2@0.5` * self sizeUnit // 1.
	^ result! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:02:19'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ RectangleLikeMorph new.
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
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 10/12/2020 21:04:14'!
label
	"Answer a nice label.
	
	Pinned menus in the taskbar are easier to identify on big screens."

	titleMorph ifNil: [ ^ super label ].
	titleMorph submorphsDo: [ :labelMorph |
		"Be careful"
		[ ^ labelMorph contents ] onDNU: #contents do: [] ].
	^ super label ":] One never knows"! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 10/12/2020 21:02:19'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
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
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 10/12/2020 21:02:19'!
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
		feedbackMorph _ LabelMorph new color: `Color veryDarkGray`.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newRow)
('examples' example1 example10 example11 example13 example1b example2 example20 example3 example6 example7 launcherExample)
('new-morph participation' categoryInNewMorphMenu)
!


!ProgressMorph reorganize!
('accessing' done done: incrDone: label: label:subLabel: subLabel: updatePositionAndExtent)
('initialization' defaultColor initialize openInWorld:)
!


!TaskbarMorph reorganize!
('accessing' buttonFor: scale)
('events' notifyDisplayResize screenSizeChanged)
('initialization' defaultHeight defaultScale initialize)
('testing' is: isSticky)
('services' aboutToCollapse: addButtonFor: removeButtonFor: restoreAll wasDeleted: wasOpened:)
('submorphs-add/remove' delete noteNewOwner:)
('menus' handlesMouseDown: mouseButton2Activity)
('resize' scale: scaleNormal scaleSmall scaleX2 scaleX4)
!


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:event: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry testing' isOrthoRectangularMorph morphContainsPoint: submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addFontEmphHandle: addFontSizeHandle: addGrabHandle: addGrowHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addRotateHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultBorderWidth defaultColor initialize)
('testing' is: wantsHalo)
('private' addHandle: addHandles addNameString: basicBox doDebug:with: doDrag:with: doDup:with: doGrab:with: doGrow:with: doMenu:with: doRecolor:with: doRot:with: endInteraction maybeCollapse:with: maybeDismiss:with: maybeDoDup:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startGrow:with: startRot:with:)
('forward to target' chooseEmphasisOrAlignment chooseFont deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' acceptDropsFrom:performing:whenOutsideList: makeItemsDraggable makeItemsUndraggable rejectDrops)
('drawing' drawOn: flashRow:)
('dropping/grabbing' acceptDroppingMorph:atRow:withEvent: acceptDroppingMorph:event: acceptDroppingMorph:outsideListWithEvent: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' allowsMorphDrop handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: gainFocusFrom: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo: itemsAreDraggable listMorph)
('siblings' leftSibling: leftSibling:rightSibling: rightSibling:)
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


!FillInTheBlankMorph reorganize!
('accessing' emptyTextDisplayMessage: response response: selectionInterval)
('initialization' createAcceptButton createCancelButton createQueryTextMorph: createTextPaneAcceptOnCR: defaultColor delete initialize responseUponCancel: setQuery:initialAnswer:acceptOnCR: sizeUnit)
('invoking' getUserResponse)
('menu' acceptClicked cancelClicked)
('drawing' drawOn:)
!


!StringRequestMorph reorganize!
('accessing' emptyTextDisplayMessage: response response:)
('drawing' drawOn:)
('initialization' acceptBlock: addTextPane addTitle: cancelBlock: defaultBorderWidth defaultColor initialize intoWorld: setQuery:initialAnswer: validationBlock:)
('private' adjustSubmorphsLayout cancel getUserResponseOrCancel: selectionInterval)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items label lastItem stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
!

