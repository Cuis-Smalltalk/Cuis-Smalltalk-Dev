'From Cuis 4.2 of 25 July 2013 [latest update: #1793] on 13 August 2013 at 9:12:08.101047 am'!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString isCollapsed updatablePanes widgetsColor layoutMorph adjusters '
	classVariableNames: 'TopWindow '
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!TaskbarTask methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:03'!
restore

	morph visible: true.
	
	Taskbar singleton submorphs do: [ :button |
		button model == self ifTrue: [ Taskbar singleton removeMorph: button ]
	]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/13/2013 09:11'!
visible: aBoolean
	super visible: aBoolean.
	self activate! !


!Taskbar methodsFor: 'testing' stamp: 'jmv 8/13/2013 09:02'!
isCollapsed: aMorph

	^(self buttonFor: aMorph) notNil! !

!Taskbar methodsFor: 'private' stamp: 'jmv 8/13/2013 09:01'!
buttonFor: aMorph
	
	self submorphs do: [ :button |
		button model morph == aMorph
			ifTrue: [ ^button ]].
	^nil! !


!Taskbar class methodsFor: 'testing' stamp: 'jmv 8/13/2013 09:00'!
isCollapsed: aWindow
	^self singleton isCollapsed: aWindow! !


!TaskbarTask methodsFor: 'state' stamp: 'jmv 8/13/2013 09:04'!
switch

	self flag: #todo. "Does the model really need to be a switch? I don't think so..."
	super switch.
	self restore! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 08:53'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse.  self displayWorld].
	self collapseNonWindows! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 08:53'!
expandAll
	"Expand all windows"
	Taskbar singleton restoreAll.
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ])
		reverseDo: [ :w | w expand.  self displayWorld ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 08:52'!
findWindow: evt 
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."

	| menu expanded collapsed nakedMorphs |
	menu := MenuMorph new.
	expanded := SystemWindow windowsIn: self
				satisfying: [:w | w isCollapsed not].
	collapsed := SystemWindow windowsIn: self
				satisfying: [:w | w isCollapsed].
	nakedMorphs := self submorphsSatisfying: [:m | (m is: #SystemWindow) not].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) 
		ifTrue: [^Smalltalk beep].
	(expanded 
		asArray sort: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) 
			do: 
				[:w | 
				menu 
					add: w label
					target: w
					action: #activateAndForceLabelToShow.
				w canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) 
		ifFalse: [menu addLine].
	(collapsed 
		asArray sort: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) 
			do: 
				[:w | 
				menu 
					add: w label
					target: w
					action: #expand.
				w canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	nakedMorphs isEmpty ifFalse: [menu addLine].
	(nakedMorphs asArray sort: 
			[:w1 :w2 | 
			w1 nameForFindWindowFeature 
				caseInsensitiveLessOrEqual: w2 nameForFindWindowFeature]) 
		do: 
			[:w | 
			menu 
				add: w nameForFindWindowFeature
				target: w
				action: #comeToFrontAndAddHalo].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/13/2013 09:07'!
justDroppedInto: newOwnerMorph event: anEvent

	TopWindow ~~ self ifTrue: [ self activate ].
	^super justDroppedInto: newOwnerMorph event: anEvent! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/13/2013 08:56'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Honor my minimumExtent."

	super morphExtent: (aPoint max: self minimumExtent)! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/13/2013 08:52'!
createCollapseBox
	^(PluggableButtonMorph model: self action: #collapse)
		icon: Theme current collapseIcon;
		setBalloonText: 'collapse this window'! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/13/2013 09:11'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	
	self initializeLabelArea.
	extent _ 300 @ 200.
	updatablePanes _ #().

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorph: m ].

	"by default"
	self beColumn! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/13/2013 09:08'!
collapse

	Taskbar visible ifTrue: [ Taskbar minimize: self ]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/13/2013 09:11'!
expand

	Taskbar visible ifTrue: [ Taskbar restore: self ]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/13/2013 08:52'!
expandBoxHit
	"The full screen expand box has been hit"

	self fullScreen! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/13/2013 09:06'!
isCollapsed

	^ Taskbar isCollapsed: self! !

!SystemWindow methodsFor: 'stepping' stamp: 'jmv 8/13/2013 09:06'!
stepAt: millisecondClockValue
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [self wantsStepsWhenCollapsed]) ifTrue: [
		model ifNotNil: [model stepAt: millisecondClockValue in: self].
		super stepAt: millisecondClockValue "let player, if any, step"]

"Since this method ends up calling step, the model-stepping logic should not be duplicated there."! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/13/2013 08:47'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	self owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
		aBoolean ifTrue: [
			oldTop owner addMorphBack: oldTop ]].

	self owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			self owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


!CodeWindow methodsFor: 'top window' stamp: 'jmv 8/13/2013 08:48'!
activateAndSendTopToBack: aBoolean
	super activateAndSendTopToBack: aBoolean.
	self decorateButtons! !


!MessageSetWindow methodsFor: 'top window' stamp: 'jmv 8/13/2013 08:51'!
activateAndSendTopToBack: aBoolean
	super activateAndSendTopToBack: aBoolean.
	(model messageListIndex = 0 and: [ model messageList notEmpty ])
		ifTrue: [	
			model messageListIndex: 1 ]! !


!Taskbar methodsFor: 'change reporting' stamp: 'jmv 8/13/2013 09:05'!
delete

	self restoreAll.
	super delete! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:04'!
restore: aMorph
	
	(self buttonFor: aMorph) ifNotNil: [ :button | button model restore ]! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:04'!
restoreAll
	
	self submorphs do: [ :button | button model restore ]! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/13/2013 08:54'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	Preferences reverseWindowStagger 
		ifTrue: 
			[^self 
				strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld].
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w isCollapsed not]) 
						collect: [:w | w morphBoundsInWorld].
	allOrigins := otherFrames collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld) do: 
			[:aPosition | 
			"First see if one of the standard positions is free"

			(allOrigins includes: aPosition) 
				ifFalse: 
					[^(aPosition extent: initialExtent) 
						translatedAndSquishedToBeWithin: allowedArea]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom 
		and: [putativeFrame right < screenRight]] 
			whileTrue: 
				[(allOrigins includes: putativeOrigin) 
					ifFalse: 
						[^(putativeOrigin extent: initialExtent) 
							translatedAndSquishedToBeWithin: allowedArea]].
	^(self scrollBarSetback @ self screenTopSetback extent: initialExtent) 
		translatedAndSquishedToBeWithin: allowedArea! !


!Taskbar reorganize!
('change reporting' delete)
('commands' minimize: restore: restoreAll)
('initialization' defaultColor)
('stepping' step stepTime wantsSteps)
('testing' is: isCollapsed: isSticky)
('private' buttonFor:)
!

!methodRemoval: SystemWindow #collapseOrExpand!
SystemWindow removeSelector: #collapseOrExpand!
!methodRemoval: SystemWindow #passivate!
SystemWindow removeSelector: #passivate!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString updatablePanes widgetsColor layoutMorph adjusters'
	classVariableNames: 'TopWindow'
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!methodRemoval: TaskbarTask #restore:!
TaskbarTask removeSelector: #restore:!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

PluggableButtonMorph allInstancesDo: [ :b |
	b actionSelector = #collapseOrExpand 
		ifTrue: [ b action: #collapse]]!
