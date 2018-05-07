'From Cuis 5.0 of 7 November 2016 [latest update: #3306] on 2 May 2018 at 2:33:08 pm'!

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:47:43'!
openChangesMenu
	"Build the changes menu for the world."

	self doPopUp: self changesMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:48:18'!
openDebugMenu

	self doPopUp: self debugMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:45:28'!
openHelpMenu
	"Build and show the help menu for the world."

	self doPopUp: self helpMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:30:15'!
openOpenMenu

	self doPopUp: self openMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:43:44'!
openPreferencesMenu
	"Build and show the preferences menu for the world."

	self doPopUp: self preferencesMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 5/1/2018 21:45:06'!
openWindowsMenu
	"Build the windows menu for the world."

	self doPopUp: self windowsMenu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/2/2018 14:32:47'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu addStayUpIcons.
	self
		fillIn: menu
		fromDictionaries: `{
				{
					#label 			-> 		'Open...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openOpenMenu.
					#icon 			-> 		#openIcon.
					#balloonText 	-> 		'Submenu to open various system tools'.
				} asDictionary.
				{
					#label 			-> 		'New morph...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#newMorph.
					#icon 			-> 		#morphsIcon.
					#balloonText 	-> 		'Offers a variety of ways to create new objects'.
				} asDictionary.
				{
					#label 			-> 		'Preferences...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openPreferencesMenu.
					#icon 			-> 		#preferencesIcon.
					#balloonText 	-> 		'Opens a menu offering many controls over appearance and system preferences.'.
				} asDictionary.
				{
					#label 			-> 		'Windows...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openWindowsMenu.
					#icon 			-> 		#windowIcon.
					#balloonText 	-> 		'Manage open windows'.
				} asDictionary.
				{
					#label 			-> 		'Help...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openHelpMenu.
					#icon 			-> 		#helpIcon.
					#balloonText 	-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
				} asDictionary.
				nil.
				{
					#label 			-> 		'Changes...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openChangesMenu.
					#icon 			-> 		#changesIcon.
					#balloonText 	-> 		'Opens a menu of useful tools for dealing with changes'.
				} asDictionary.
				{
					#label 			-> 		'Debug...'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#openDebugMenu.
					#icon 			-> 		#debugIcon.
					#balloonText 	-> 		'a menu of debugging items'.
				} asDictionary.
				{
					#label 			-> 		'Restore Display (r)'.
					#object 			-> 		#myWorld.
					#selector 		-> 		#restoreDisplay.
					#icon 			-> 		#displayIcon.
					#balloonText 	-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
				} asDictionary.
				nil.
				{
					#label 			-> 		'Save'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveSession.
					#icon 			-> 		#saveIcon.
					#balloonText 	-> 		'Save the current version of the image on disk'.
				} asDictionary.
				{
					#label 			-> 		'Save as...'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveAs.
					#icon 			-> 		#saveAsIcon.
					#balloonText 	-> 		'Save the current version of the image on disk under a new name.'.
				} asDictionary.
				{
					#label 			-> 		'Save as New Version'.
					#object 			-> 		Smalltalk.
					#selector 		-> 		#saveAsNewVersion.
					#icon 			-> 		#saveAsNewVersionIcon.
					#balloonText 	-> 		'Save the current image a new version-stamped name\and save it under that name on disk.\Clear all user preferences and user state (class vars).' withNewLines.
				} asDictionary.
				{
					#label 			-> 		'Save and Quit'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#saveAndQuit.
					#icon 			-> 		#saveAndQuitIcon.
					#balloonText 	-> 		'Save the image and quit out of Cuis.'.
				} asDictionary.
				{
					#label 			-> 		'Quit'.
					#object 			-> 		#theWorldMenu.
					#selector 		-> 		#quitSession.
					#icon 			-> 		#quitIcon.
					#balloonText 	-> 		'Quit out of Cuis.'.
				} asDictionary.
			}`.
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 21:38:18'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item |
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object.
				realTarget == #myWorld ifTrue: [realTarget _ myWorld].
				realTarget == #theWorldMenu ifTrue: [realTarget _ self].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						aMenu
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						aMenu
							add: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				dict
					at: #icon
					ifPresent: [ :symbolOrFormOrNil |
						item setIcon: symbolOrFormOrNil ].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]].
	^ aMenu! !

!TheWorldMenu methodsFor: 'mechanics' stamp: 'jmv 5/1/2018 21:24:40'!
menu: titleString
	"Create a menu with the given title, ready for filling"

	| menu |
	(menu _ MenuMorph entitled: titleString) 
		defaultTarget: self; 
		addStayUpIcons.
	^ menu
! !

!methodRemoval: TheWorldMenu #changesDo!
TheWorldMenu removeSelector: #changesDo!
!methodRemoval: TheWorldMenu #colorForDebugging:!
TheWorldMenu removeSelector: #colorForDebugging:!
!methodRemoval: TheWorldMenu #debugDo!
TheWorldMenu removeSelector: #debugDo!
!methodRemoval: TheWorldMenu #helpDo!
TheWorldMenu removeSelector: #helpDo!
!methodRemoval: TheWorldMenu #openWindow!
TheWorldMenu removeSelector: #openWindow!
!methodRemoval: TheWorldMenu #preferencesDo!
TheWorldMenu removeSelector: #preferencesDo!
!methodRemoval: TheWorldMenu #windowsDo!
TheWorldMenu removeSelector: #windowsDo!
