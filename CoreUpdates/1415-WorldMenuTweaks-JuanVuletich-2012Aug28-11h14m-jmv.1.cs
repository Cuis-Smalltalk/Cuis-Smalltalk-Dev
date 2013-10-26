'From Cuis 4.0 of 21 April 2012 [latest update: #1414] on 28 August 2012 at 11:17:42 am'!

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/28/2012 11:16'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self addGestureHelpItemsTo: menu.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openStandardWorkspace}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/28/2012 11:17'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		{'System fonts...' . { self . #standardFontDo} . 'Choose the standard fonts to use for code, lists, menus, window titles, etc.'}.
		nil.
		{'Show taskbar' . {Taskbar . #show} . 'show the taskbar'}.
		{'Hide taskbar' . {Taskbar . #hide} . 'hide the taskbar'}.
		nil.
		{'Full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'Full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'Set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'Set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
		nil.
		{'Set Code Author...'. {Utilities. #setAuthor}. 'supply initials to be used to identify the author of code and other content.'}.
		{'All preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
	}! !

