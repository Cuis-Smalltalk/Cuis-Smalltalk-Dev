'From Cuis 5.0 of 7 November 2016 [latest update: #3035] on 16 January 2017 at 11:04:32 am'!
(Smalltalk classNamed: 'Taskbar') ifNotNil: [ :tbClass |
	PasteUpMorph allInstancesDo: [ :w | w hideTaskbar ].
	tbClass allInstancesDo: [ :each | each delete ]]!
!classDefinition: #UpdatingStringMorph category: #'Morphic-Widgets'!
StringMorph subclass: #UpdatingStringMorph
	instanceVariableNames: 'target getSelector stepTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!UpdatingStringMorph commentStamp: 'jmv 1/5/2013 23:49' prior: 0!
UpdatingStringMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!

!classDefinition: #TaskbarMorph category: #'Tools-Taskbar'!
LayoutMorph subclass: #TaskbarMorph
	instanceVariableNames: 'viewBox scale'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Taskbar'!

!TaskbarMorph commentStamp: '<historical>' prior: 0!
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.
scale allows 1x 2x 4x tarkbar height. [scale= 1,2,4]!


!Preferences class methodsFor: 'personalization' stamp: 'jmv 1/15/2017 18:51:02'!
taskbarIncludesAllWindows
	"
	true: All windows are included in Taskbar
	false: Only collapsed windows are included in Taskbar
	"
	^ self
		valueOfFlag: #taskbarIncludesAllWindows
		ifAbsent: [ true ].! !


!Morph methodsFor: 'accessing' stamp: 'jmv 1/15/2017 18:06:45'!
taskbar
	^self world ifNotNil: [ :w | w taskbar ]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 1/15/2017 14:52:58'!
showAndComeToFront

	self show; comeToFront! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 1/16/2017 09:56:14'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 1/16/2017 09:56:07'!
removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 1/15/2017 18:57:53'!
taskbarDeleted
	taskbar _ nil! !


!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
getSelector: aSymbol
	getSelector _ aSymbol! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:18'!
stepTime

	^stepTime! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
stepTime: aNumber
	stepTime _ aNumber! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
target: anObject
	target _ anObject! !

!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 09:23'!
initialize
	super initialize.
	target _ self.
	getSelector _ #contents.
	stepTime _ 50! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast

	self contents: (target perform: getSelector)! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/4/2013 13:18'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !

!UpdatingStringMorph methodsFor: 'geometry' stamp: 'jmv 1/15/2017 14:25:27'!
fitContents
	"Don't shrink each time contents change.
	Might shrink during layout"
	self morphExtent: (extent max: self measureContents)! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 4/15/2014 09:26'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (AbstractFont familyName: 'DejaVu' pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!TaskbarMorph methodsFor: 'accessing' stamp: 'jmv 1/15/2017 14:49:41'!
buttonFor: aMorph
	
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model == aMorph
				ifTrue: [ ^button ]]
	].
	^nil! !

!TaskbarMorph methodsFor: 'accessing' stamp: 'jmv 1/15/2017 14:49:30'!
scale

	 ^ scale ifNil: [ self defaultScale ] ifNotNil: [ scale ]! !

!TaskbarMorph methodsFor: 'events' stamp: 'jmv 1/8/2017 16:48:00'!
notifyDisplayResize
	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: self.
	self screenSizeChanged! !

!TaskbarMorph methodsFor: 'events' stamp: 'jmv 1/8/2017 16:48:14'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	WorldState addDeferredUIMessage: [
		self world ifNotNil: [ :w |
			y _ w morphExtent y - self defaultHeight.
			e _ self internalizeDistance: w morphExtent x @ self defaultHeight.
			self morphPosition: 0@y extent: e ]]! !

!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 1/15/2017 14:49:51'!
defaultHeight

	^ Preferences windowTitleFont height * 2 * self scale! !

!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 1/15/2017 14:49:19'!
defaultScale

	 ^ 1! !

!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 1/8/2017 16:57:33'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingStringMorph initializedInstance
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 1.0).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: 5
! !

!TaskbarMorph methodsFor: 'testing' stamp: 'jmv 1/8/2017 16:44:53'!
is: aSymbol
	^ aSymbol == #TaskbarMorph or: [ super is: aSymbol ]! !

!TaskbarMorph methodsFor: 'testing' stamp: 'jmv 1/8/2017 16:47:41'!
isSticky
	"answer whether the receiver is Sticky"
	^true! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:42:23'!
addButtonFor: aMorph

	| button |
	button _ PluggableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #showAndComeToFront.
	button
		color: self color;
		icon: (aMorph imageForm: 32);
		setBalloonText: aMorph label.
	viewBox
		addMorph: button
		fixedWidth: self defaultHeight! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:20:11'!
removeButtonFor: aMorph

	(self buttonFor: aMorph) ifNotNil: [ :b |
		b delete ]! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:49:21'!
restoreAll
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model showAndComeToFront ]	]! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:23:09'!
wasCollapsed: aMorph
	"Add a button for aMorph if not already there (see #taskbarIncludesAllWindows)"

	(self buttonFor: aMorph) ifNil: [
		self addButtonFor: aMorph ]! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:25:27'!
wasDeleted: aMorph
	"aMorph was deleted. Remove button for aMorph"

	self removeButtonFor: aMorph! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:19:22'!
wasMadeVisible: aMorph
	"aMorph is now visible. Remove button for aMorph if appropriate (see #taskbarIncludesAllWindows)"

	Preferences taskbarIncludesAllWindows ifFalse: [
		self removeButtonFor: aMorph ]! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/15/2017 18:21:15'!
wasOpened: aMorph
	"aMorph was added to the world. Add button for aMorph if appropriate (see #taskbarIncludesAllWindows)"
	
	Preferences taskbarIncludesAllWindows ifTrue: [
		self addButtonFor: aMorph ]! !

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 1/15/2017 18:57:58'!
delete

	| w |
	self restoreAll.
	super delete.
	w _ self world ifNil: [ self runningWorld ].
	Display removeActionsWithReceiver: self.
	w ifNotNil: [ w taskbarDeleted ]! !

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 1/16/2017 09:52:23'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.
	Preferences taskbarIncludesAllWindows ifTrue: [
		aMorph submorphsDo: [ :m |
			m == self ifFalse: [
				self addButtonFor: m ]]].
	self notifyDisplayResize! !

!TaskbarMorph methodsFor: 'menus' stamp: 'jmv 1/15/2017 19:14:11'!
handlesMouseDown: aMouseButtonEvent

	^ true! !

!TaskbarMorph methodsFor: 'menus' stamp: 'jmv 1/15/2017 19:10:57'!
mouseButton2Activity

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.
	menu popUpInWorld! !

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 1/15/2017 19:01:25'!
scale: anInteger

	(anInteger between: 1 and: 4) ifFalse: [ self error: 'scale should be 1 2 or 4' ].
	scale := anInteger.
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]! !

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 1/15/2017 19:00:49'!
scaleNormal

	self scale: 1! !

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 1/15/2017 19:00:52'!
scaleX2

	self scale: 2! !

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 1/15/2017 19:00:55'!
scaleX4

	self scale: 4! !


!TaskbarMorph class methodsFor: 'system startup' stamp: 'jmv 1/8/2017 16:47:17'!
initClassCachedState

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each notifyDisplayResize ]! !


!Morph methodsFor: 'drawing' stamp: 'jmv 1/15/2017 18:24:25'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	aBoolean ifFalse: [
		self redrawNeeded ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	aBoolean ifTrue: [
		self redrawNeeded.
		self taskbar ifNotNil: [ :tb |
			tb wasMadeVisible: self ]]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 1/15/2017 14:58:58'!
comeToFrontAndAddHalo
	self show.
	self comeToFront.
	self addHalo! !

!Morph methodsFor: 'menus' stamp: 'jmv 1/15/2017 18:24:40'!
collapse
	"If taskbar not visible, just hide."

	self hide.
	self taskbar
		ifNotNil: [ :tb | tb wasCollapsed: self ]! !

!Morph methodsFor: 'menus' stamp: 'jmv 1/15/2017 14:59:12'!
expand
	
	self show.
	self comeToFront! !

!Morph methodsFor: 'testing' stamp: 'jmv 1/15/2017 15:04:18'!
isCollapsed

	^ self visible not! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/8/2017 16:44:57'!
allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 1/15/2017 14:43:48'!
showTaskbar

	taskbar ifNil: [
		taskbar _ TaskbarMorph newRow.
		taskbar openInWorld: self ]! !


!StringMorph methodsFor: 'geometry' stamp: 'jmv 1/15/2017 14:22:53'!
measureContents
	| f |
	f _ self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f height! !

!methodRemoval: TaskbarMorph #intoWorld:!
TaskbarMorph removeSelector: #intoWorld:!
!methodRemoval: PasteUpMorph #isCollapsed:!
PasteUpMorph removeSelector: #isCollapsed:!
!methodRemoval: PasteUpMorph #minimize:!
PasteUpMorph removeSelector: #minimize:!
!methodRemoval: PasteUpMorph #restore:!
PasteUpMorph removeSelector: #restore:!
!methodRemoval: PasteUpMorph #taskbar:!
PasteUpMorph removeSelector: #taskbar:!

!StringMorph reorganize!
('accessing' contents contents: font font:emphasis: fontToUse)
('drawing' drawOn:)
('font' emphasis:)
('halos and balloon help' addOptionalHandlesTo:box:)
('initialization' defaultColor initWithContents:font:emphasis: initialize)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont)
('printing' font: printOn:)
('geometry' fitContents measureContents minimumExtent)
!
PasteUpMorph allInstancesDo: [ :w | w showTaskbar ]!
