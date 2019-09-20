'From Cuis 5.0 [latest update: #3863] on 5 September 2019 at 6:18:36 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/5/2019 18:12:41'!
restoreDefaultFonts
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles.
	Preferences restoreDefaultFonts
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)
			(setSystemFontTo: 9))! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:08:38'!
defaultFont05
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont05
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 5)
			(setMenuFontTo: 5)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)
			(setSystemFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:09:16'!
defaultFont06
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont06
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 6)
			(setMenuFontTo: 6)
			(setWindowTitleFontTo: 8)
			(setCodeFontTo: 6)
			(setButtonFontTo: 6)
			(setSystemFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:09:35'!
defaultFont07
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont07
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)
			(setSystemFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:10:10'!
defaultFont08
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont08
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)
			(setSystemFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:10:36'!
defaultFont09
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont09
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)
			(setSystemFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:10:45'!
defaultFont10
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont10
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 10)
			(setMenuFontTo: 11)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 10)
			(setButtonFontTo: 10)
			(setSystemFontTo: 10)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:10:56'!
defaultFont11
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont11
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)
			(setSystemFontTo: 11)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:03'!
defaultFont12
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont12
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 12)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 12)
			(setButtonFontTo: 12)
			(setSystemFontTo: 12)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:11'!
defaultFont14
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont14
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)
			(setSystemFontTo: 14)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:20'!
defaultFont17
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont17
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)
			(setSystemFontTo: 17)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:26'!
defaultFont22
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont22
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 22)
			(setMenuFontTo: 22)
			(setWindowTitleFontTo: 28)
			(setCodeFontTo: 22)
			(setButtonFontTo: 22)
			(setSystemFontTo: 22)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:46'!
defaultFont28
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont28
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 28)
			(setMenuFontTo: 28)
			(setWindowTitleFontTo: 36)
			(setCodeFontTo: 28)
			(setButtonFontTo: 28)
			(setSystemFontTo: 28)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:11:56'!
defaultFont36
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont36
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 36)
			(setMenuFontTo: 36)
			(setWindowTitleFontTo: 46)
			(setCodeFontTo: 36)
			(setButtonFontTo: 36)
			(setSystemFontTo: 36)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:12:11'!
defaultFont46
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont46
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 46)
			(setMenuFontTo: 46)
			(setWindowTitleFontTo: 60)
			(setCodeFontTo: 46)
			(setButtonFontTo: 46)
			(setSystemFontTo: 46)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:12:19'!
defaultFont60
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont60
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 60)
			(setMenuFontTo: 60)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 60)
			(setButtonFontTo: 60)
			(setSystemFontTo: 60)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/5/2019 18:12:31'!
defaultFont80
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont80
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 80)
			(setMenuFontTo: 80)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 80)
			(setButtonFontTo: 80)
			(setSystemFontTo: 80)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/5/2019 17:51:19'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: Preferences windowTitleFont 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 9/5/2019 17:59:48'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingStringMorph initializedInstance
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: self defaultHeight // 8
! !


!FontChanger class methodsFor: 'changing font' stamp: 'jmv 9/5/2019 18:08:23'!
changeTo: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	Preferences
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#setListFontTo:. Preferences standardListFont pointSize.}.
				{#setMenuFontTo:. Preferences standardMenuFont pointSize.}.
				{#setWindowTitleFontTo:. Preferences windowTitleFont pointSize.}.
				{#setCodeFontTo:. Preferences standardCodeFont pointSize.}.
				{#setButtonFontTo:. Preferences standardButtonFont pointSize.}.
				{#setSystemFontTo:. FontFamily defaultPointSize}.
			}.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].

! !

