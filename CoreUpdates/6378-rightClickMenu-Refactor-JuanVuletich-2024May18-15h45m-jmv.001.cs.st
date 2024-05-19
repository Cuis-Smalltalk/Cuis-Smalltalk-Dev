'From Cuis7.1 [latest update: #6377] on 18 May 2024 at 4:01:29 pm'!

!Morph methodsFor: 'menus' stamp: 'jmv 5/16/2024 09:53:46'!
getMenu
	^ nil! !


!WorldMorph methodsFor: 'world menu' stamp: 'jmv 5/16/2024 09:55:41'!
getMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed (by answering nil)"

	^(Preferences at: #worldMenu) getMenuIn: self at: self activeHand.! !


!TaskbarMorph methodsFor: 'menu' stamp: 'jmv 5/16/2024 09:53:01'!
getMenu

	| menu |
	menu := MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Small Height' action: #scaleSmall;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.
	^menu! !


!Morph methodsFor: 'event handling' stamp: 'jmv 5/16/2024 09:54:33'!
mouseButton2Activity
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]! !

!methodRemoval: TranscriptMorph #mouseButton2Activity stamp: 'jmv 5/18/2024 15:55:06'!
TranscriptMorph removeSelector: #mouseButton2Activity!
!methodRemoval: PluggableScrollPane #mouseButton2Activity stamp: 'jmv 5/18/2024 15:54:58'!
PluggableScrollPane removeSelector: #mouseButton2Activity!
!methodRemoval: PluggableScrollPane #getMenu stamp: 'jmv 5/18/2024 15:57:52'!
PluggableScrollPane removeSelector: #getMenu!
!methodRemoval: InnerTextMorph #mouseButton2Activity stamp: 'jmv 5/18/2024 15:54:54'!
InnerTextMorph removeSelector: #mouseButton2Activity!
!methodRemoval: TaskbarMorph #mouseButton2Activity stamp: 'jmv 5/18/2024 15:56:30'!
TaskbarMorph removeSelector: #mouseButton2Activity!
!methodRemoval: WorldMorph #mouseButton2Activity stamp: 'jmv 5/18/2024 15:57:38'!
WorldMorph removeSelector: #mouseButton2Activity!
!methodRemoval: WorldMorph #addCustomMenuItems:hand: stamp: 'jmv 5/18/2024 15:58:55'!
WorldMorph removeSelector: #addCustomMenuItems:hand:!
!methodRemoval: WorldMorph #invokeWorldMenu stamp: 'jmv 5/18/2024 15:58:59'!
WorldMorph removeSelector: #invokeWorldMenu!

!TaskbarMorph reorganize!
('accessing' buttonFor: scale)
('events' screenSizeChanged)
('initialization' defaultHeight defaultScale initialize)
('testing' is: isSticky)
('services' aboutToCollapse: addButtonFor: refreshTaskbarFor: removeButtonFor: restoreAll wasDeleted: wasOpened:)
('submorphs-add/remove' delete noteNewOwner:)
('menu' getMenu)
('resize' scale: scaleNormal scaleSmall scaleX2 scaleX4)
('notifications' fontPreferenceChanged)
('geometry' basicDisplayBounds)
('event handling testing' handlesMouseDown:)
!


!TranscriptMorph reorganize!
('accessing' color:)
('initialization' defaultBorderWidth initialize)
('drawing' adoptWidgetsColor: drawOn:)
('event handling testing' handlesMouseDown:)
('menu' getMenu)
('menu commands' clearAll clearFile clearInternal clearStdout doImmediateUpdates doRegularUpdates dontLogToFile dontLogToStdout editContents logToFile logToStdout)
('updating' updateWorkspace)
!

