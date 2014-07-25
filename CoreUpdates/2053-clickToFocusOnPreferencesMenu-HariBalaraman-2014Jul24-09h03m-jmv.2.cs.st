'From Cuis 4.2 of 25 July 2013 [latest update: #2049] on 24 July 2014 at 9:14:33.732187 am'!

!Preferences class methodsFor: 'get/set' stamp: 'HB 7/23/2014 16:08'!
disableFocusFollowsMouse

	"disable the window selection following the position of the mouse and base it on explicit mouse click"

	
	Preferences disable: #focusFollowsMouse.! !

!Preferences class methodsFor: 'get/set' stamp: 'HB 7/23/2014 16:08'!
enableFocusFollowsMouse

	"base the window selection on the present location of the mouse"
	
	Preferences enable: #focusFollowsMouse.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'HB 7/23/2014 16:11'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Focus follows mouse'. {Preferences. #enableFocusFollowsMouse}. 'At all times, make the active window and widget the one on which the mouse is located.'}.
		{'Click to focus'. {Preferences. #disableFocusFollowsMouse}. 'At all times, make the active window and widget the one where the mouse was clicked.'}.
		{'Font Sizes...' . {Theme . #changeFontSizes} . 'use larger or smaller text and widgets'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
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

