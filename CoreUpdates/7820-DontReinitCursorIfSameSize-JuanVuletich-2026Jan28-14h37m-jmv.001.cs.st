'From Cuis7.7 [latest update: #7819] on 29 January 2026 at 10:16:15 am'!

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 29/Jan/2026 10:12:12'!
defaultFontSize: aPreference
	"
	Preferences at: #defaultFontSize put: '12'
	Preferences at: #defaultFontSize put: 14
	Preferences at: #defaultFontSize put: 21
	"
	| pointSize font titleFont |
	pointSize := aPreference value.
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	(Preferences at: #biggerCursors) == (pointSize > 13) ifFalse: [
		Preferences at: #biggerCursors put: (pointSize > 13). Cursor initialize.
		Cursor defaultCursor activateCursor ].
	MenuItemMorph initializeSubMenuMarker.
	^ font! !

