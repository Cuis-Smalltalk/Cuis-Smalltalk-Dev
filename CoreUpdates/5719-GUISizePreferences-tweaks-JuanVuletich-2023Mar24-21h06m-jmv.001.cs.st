'From Cuis 6.0 [latest update: #5718] on 24 March 2023 at 9:21:35 pm'!

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'jmv 3/24/2023 21:14:52'!
fontSizes
	^ `{
		#tinyFonts -> 7.
		#verySmallFonts -> 10.
		#smallFonts ->14.
		#standardFonts -> 20.
		#largeFonts -> 28.
		#veryLargeFonts -> 40.
		#hugeFonts -> 56} asDictionary`! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 3/24/2023 21:20:11'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize := self fontSizes at: guiElementSizePref value.
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
	Preferences at: #biggerCursors put: (pointSize > 30).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !


!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/24/2023 21:06:54'!
bottomLeftCorner: r height: height
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (width - 1 - x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/24/2023 21:07:04'!
bottomRightCorner: r height: height
	"Create and answer a Form for the bottomRight corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/24/2023 21:07:13'!
topLeftCorner: r height: height
	"Create and answer a Form for the topLeft corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (w - 1 - x @ (w - 1 - y)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/24/2023 21:07:23'!
topRightCorner: r height: height
	"Create and answer a Form for the topRight corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (x @ (w - y - 1)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet defaultFontSize: #standardFonts.!

