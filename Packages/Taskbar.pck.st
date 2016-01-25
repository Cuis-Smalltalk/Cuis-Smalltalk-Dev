'From Cuis 4.2 of 25 July 2013 [latest update: #2667] on 24 January 2016 at 10:07:29.307531 pm'!
'Description Please enter a description for this package'!
!provides: 'Taskbar' 1 1!
!requires: 'Morphic-Widgets-Extras' 1 0 nil!
!classDefinition: #TaskbarTask category: #'Taskbar-Core'!
Switch subclass: #TaskbarTask
	instanceVariableNames: 'morph taskbar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Core'!
!classDefinition: 'TaskbarTask class' category: #'Taskbar-Core'!
TaskbarTask class
	instanceVariableNames: ''!

!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'viewBox scale'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Core'!
!classDefinition: 'Taskbar class' category: #'Taskbar-Core'!
Taskbar class
	instanceVariableNames: ''!


!TaskbarTask commentStamp: '<historical>' prior: 0!
I represent iconified windows on the Taskbar.!

!Taskbar commentStamp: '<historical>' prior: 0!
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.
scale allows 1x 2x 4x tarkbar height. [scale= 1,2,4]!

!TaskbarTask methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
minimize: aMorph

	| button imageForm |

	morph _ aMorph.

	self turnOn.

	button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: #switch.
	imageForm _ aMorph imageForm: 32.
	button
		color: taskbar color;
		icon: imageForm;
		setBalloonText: morph label.
	morph visible: false.
			
	^button! !

!TaskbarTask methodsFor: 'accessing' stamp: 'cbr 4/18/2011 22:16'!
morph

	^ morph! !

!TaskbarTask methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
restore

	morph visible: true.
	(taskbar buttonFor: morph) ifNotNil: [ :button |
		button delete ]! !

!TaskbarTask methodsFor: 'state' stamp: 'jmv 8/13/2013 09:04'!
switch

	self flag: #todo. "Does the model really need to be a switch? I don't think so..."
	super switch.
	self restore! !

!TaskbarTask methodsFor: 'accessing' stamp: 'jmv 5/3/2015 19:04'!
taskbar: aTaskbar
	taskbar _ aTaskbar! !

!TaskbarTask class methodsFor: 'instance creation' stamp: 'jmv 5/3/2015 19:04'!
forTaskbar: aTaskbar
	^self new taskbar: aTaskbar! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:49'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."

	aCustomMenu 
		addLine;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.! !

!Taskbar methodsFor: 'private' stamp: 'jmv 11/10/2013 23:14'!
buttonFor: aMorph
	
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model morph == aMorph
				ifTrue: [ ^button ]]
	].
	^nil! !

!Taskbar methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:56'!
defaultColor
	^ Theme current menu! !

!Taskbar methodsFor: 'initialization' stamp: 'KenD 12/13/2013 16:06'!
defaultHeight

	^ Preferences windowTitleFont height * 2 * self scale! !

!Taskbar methodsFor: 'initialization' stamp: 'KenD 12/13/2013 09:54'!
defaultScale

	 ^ 1! !

!Taskbar methodsFor: 'change reporting' stamp: 'jmv 5/27/2015 10:12'!
delete

	| w |
	self restoreAll.
	super delete.
	w _ self world ifNil: [ self runningWorld ].
	Display removeActionsWithReceiver: self.
	w ifNotNil: [ w taskbar: nil ]! !

!Taskbar methodsFor: 'initialization' stamp: 'jmv 1/2/2015 13:52'!
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

!Taskbar methodsFor: 'initialization' stamp: 'jmv 5/3/2015 11:33'!
intoWorld: aWorld
	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: self.
	self screenSizeChanged! !

!Taskbar methodsFor: 'testing' stamp: 'jmv 8/13/2013 08:14'!
is: aSymbol
	^ aSymbol == #Taskbar or: [ super is: aSymbol ]! !

!Taskbar methodsFor: 'testing' stamp: 'jmv 8/13/2013 09:02'!
isCollapsed: aMorph

	^(self buttonFor: aMorph) notNil! !

!Taskbar methodsFor: 'testing' stamp: 'jmv 12/17/2012 13:00'!
isSticky
	"answer whether the receiver is Sticky"
	^true! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
minimize: aMorph
	viewBox
		addMorph: ((TaskbarTask forTaskbar: self) minimize: aMorph)
		fixedWidth: self defaultHeight! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
normalScale

	self scale: 1.! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:39'!
resize
	"My scale has changed"
	
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]
! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:04'!
restore: aMorph
	
	(self buttonFor: aMorph) ifNotNil: [ :button | button model restore ]! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:10'!
restoreAll
	
	viewBox ifNotNil: [ viewBox submorphs reverseDo: [ :button |  button model restore ]]! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 09:56'!
scale

	 ^ scale ifNil: [ self defaultScale ] ifNotNil: [ scale ]! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:36'!
scale: anInteger

	(anInteger between: 1 and: 4) ifFalse: [ self error: 'scale should be 1 2 or 4' ].
	 scale := anInteger.
	 self resize.! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleNormal

	self scale: 1! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleX2

	self scale: 2! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleX4

	self scale: 4! !

!Taskbar methodsFor: 'stepping' stamp: 'jmv 5/3/2015 11:14'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	WorldState addDeferredUIMessage: [
		self world ifNotNil: [ :w |
			self
				morphPosition: 0@ (w morphExtent y - (self externalizeDistance: extent ) y)
				extent: (self internalize: self world morphExtent) x @ self defaultHeight ]]! !
