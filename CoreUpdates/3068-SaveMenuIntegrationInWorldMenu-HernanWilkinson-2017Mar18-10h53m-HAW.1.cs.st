'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 18 March 2017 at 8:30:03 pm'!

!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 3/18/2017 20:26:59'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self colorForDebugging: menu.
	menu addStayUpIcons.
	self fillIn: menu
		from: {
				{ 'Open...'. { self. #openWindow}}.
				{ 'New morph...'. { self. #newMorph}.
					'Offers a variety of ways to create new objects'}.
				{ 'Preferences...'. { self. #preferencesDo}.
					'put up a menu offering many controls over appearance and system preferences.'}.
				{ 'Windows...'. { self. #windowsDo}}.
				{ 'Help...'. { self. #helpDo}.
					'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.
				nil.
				{ 'Changes...'. { self. #changesDo}}.
				{ 'Debug...'. { self. #debugDo}.
					'a menu of debugging items'}.
				{ 'Restore Display (r)'. { myWorld. #restoreMorphicDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name\and save it under that name on disk.\Clear all user preferences and user state (class vars).' withNewLines}.
				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the image and quit out of Cuis.'}.				
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !

!methodRemoval: TheWorldMenu #saveAndQuitSession!
TheWorldMenu removeSelector: #saveAndQuitSession!
!methodRemoval: TheWorldMenu #saveMenu!
TheWorldMenu removeSelector: #saveMenu!
!methodRemoval: TheWorldMenu #saveOptionsDo!
TheWorldMenu removeSelector: #saveOptionsDo!

!TheWorldMenu reorganize!
('action' doMenuItem:with: staggerPolicyString toggleWindowPolicy)
('commands' changeBackgroundColor cleanUpWorld fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: openChangeSorter1 quitSession saveAndQuit saveWorldInFile setDisplayDepth splitNewMorphList:depth: startMessageTally startThenBrowseMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu changesMenu colorForDebugging: debugMenu fillIn:from: fillIn:fromDictionaries: helpMenu newMorph openMenu preferencesMenu)
('mechanics' menu: world:hand:)
('popups' changesDo debugDo doPopUp: helpDo openWindow preferencesDo)
('windows & flaps menu' windowsDo windowsMenu)
('menu' openCommentGuide openTerseGuide)
!

