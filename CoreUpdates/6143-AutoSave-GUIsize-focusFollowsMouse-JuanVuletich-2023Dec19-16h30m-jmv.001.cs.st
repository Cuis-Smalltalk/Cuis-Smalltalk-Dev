'From Cuis6.0 [latest update: #6140] on 19 December 2023 at 4:32:46 pm'!

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/19/2023 15:46:31'!
disableFocusFollowsMouse
	Preferences name: #focusFollowsMouse category: #gui value: false.
	Preferences saveToDisk: #focusFollowsMouse.! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/19/2023 15:46:41'!
enableFocusFollowsMouse
	Preferences name: #focusFollowsMouse category: #gui value: true.
	Preferences saveToDisk: #focusFollowsMouse.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 12/19/2023 16:10:50'!
setDefaultFontSize: pointSize
	"To help user experiment until happy, reopen menu."

	Preferences at: #defaultFontSize put: pointSize.
	Preferences saveToDisk: #defaultFontSize.
	self changeFontSizes.! !

