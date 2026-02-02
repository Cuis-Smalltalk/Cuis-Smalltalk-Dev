'From Cuis7.7 [latest update: #7825] on 2 February 2026 at 3:12:48 pm'!

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 2/Feb/2026 15:12:35'!
defaultFontSize: aPreference
	"
	Preferences at: #defaultFontSize put: '12'
	Preferences at: #defaultFontSize put: 14
	Preferences at: #defaultFontSize put: 21
	"
	| pointSize pointSizeForBigCursors font titleFont |
	pointSize := aPreference value.
	pointSizeForBigCursors := 12 * DisplayScreen hostOSScaleFactor.
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
	(Preferences at: #biggerCursors) == (pointSize > pointSizeForBigCursors) ifFalse: [
		Preferences at: #biggerCursors put: (pointSize > pointSizeForBigCursors). Cursor initialize.
		Cursor defaultCursor activateCursor ].
	MenuItemMorph initializeSubMenuMarker.
	^ font! !
	
PreferenceSet defaultFontSize: (Preferences at: #defaultFontSize).!