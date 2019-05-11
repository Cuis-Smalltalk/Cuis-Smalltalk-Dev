'From Cuis 5.0 of 7 November 2016 [latest update: #3728] on 5 May 2019 at 4:11:41 pm'!

!FontFamily class methodsFor: 'accessing' stamp: 'jmv 5/5/2019 14:58:07'!
defaultFamilyName
	^ DefaultFamilyName! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 14:46:11'!
addFamily: aFontFamily
	AvailableFamilies at: aFontFamily familyName put: aFontFamily! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/5/2019 15:04:20'!
restoreDefaultFonts
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles.
	Preferences restoreDefaultFonts
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9))! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/5/2019 15:05:14'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [
			StrikeFont install: FontFamily defaultFamilyName.
			font _ FontFamily familyName: fontFamilyName pointSize: triplet second ].
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:07'!
defaultFont05
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont05
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 5)
			(setMenuFontTo: 5)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:11'!
defaultFont06
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont06
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 6)
			(setMenuFontTo: 6)
			(setWindowTitleFontTo: 8)
			(setCodeFontTo: 6)
			(setButtonFontTo: 6)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:14'!
defaultFont07
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont07
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:18'!
defaultFont08
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont08
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:22'!
defaultFont09
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont09
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:28'!
defaultFont10
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont10
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 10)
			(setListFontTo: 10)
			(setMenuFontTo: 11)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 10)
			(setButtonFontTo: 10)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:34'!
defaultFont11
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont11
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 11)
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:37'!
defaultFont12
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont12
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 12)
			(setListFontTo: 12)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 12)
			(setButtonFontTo: 12)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:42'!
defaultFont14
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont14
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 14)
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:48'!
defaultFont17
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont17
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 17)
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:51'!
defaultFont22
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont22
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 22)
			(setListFontTo: 22)
			(setMenuFontTo: 22)
			(setWindowTitleFontTo: 28)
			(setCodeFontTo: 22)
			(setButtonFontTo: 22)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:03:56'!
defaultFont28
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont28
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 28)
			(setListFontTo: 28)
			(setMenuFontTo: 28)
			(setWindowTitleFontTo: 36)
			(setCodeFontTo: 28)
			(setButtonFontTo: 28)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:04:00'!
defaultFont36
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont36
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 36)
			(setListFontTo: 36)
			(setMenuFontTo: 36)
			(setWindowTitleFontTo: 46)
			(setCodeFontTo: 36)
			(setButtonFontTo: 36)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:04:06'!
defaultFont46
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont46
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 46)
			(setListFontTo: 46)
			(setMenuFontTo: 46)
			(setWindowTitleFontTo: 60)
			(setCodeFontTo: 46)
			(setButtonFontTo: 46)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:04:10'!
defaultFont60
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont60
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 60)
			(setListFontTo: 60)
			(setMenuFontTo: 60)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 60)
			(setButtonFontTo: 60)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/5/2019 15:04:14'!
defaultFont80
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont80
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setSystemFontTo: 80)
			(setListFontTo: 80)
			(setMenuFontTo: 80)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 80)
			(setButtonFontTo: 80)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !


!AbstractFont methodsFor: 'displaying' stamp: 'jmv 5/5/2019 14:58:54'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| char fallbackFont x y answer |
	fallbackFont _ (FontFamily
		familyName: FontFamily defaultFamilyName
		aroundPointSize: self pointSize*0.9)
			emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/5/2019 15:05:35'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((FontFamily familyName: FontFamily defaultFamilyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.

que todos, menos estos, tengan superscript y subscript en cero. Y en estos, apropiado. y en 'aca' usarlo. y listo
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/5/2019 15:05:40'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((FontFamily familyName: FontFamily defaultFamilyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 5/5/2019 16:01:49'!
install: aString
"
StrikeFont install: 'DejaVu'.
"
"
StrikeFont install: 'DejaVu Sans Mono'.
FontFamily defaultFamilyName: 'DejaVu Sans Mono'.
Preferences bigFonts.
Character initialize.
"
"
StrikeFont install: 'DejaVu Sans'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans'.
FontFamily defaultFamilyName: 'DejaVu Sans'.
Preferences standardFonts.
Character initialize.
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| family |
	family _ nil.
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			font print.
			family ifNil: [
				family _ StrikeFontFamily new.
				family familyName: aString.].
			family atPointSize: s put: font ]].
	family ifNotNil: [
		FontFamily addFamily: family ]! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 15:13:29'!
migrate
	"
	FontFamily migrate
	"
	| family def strikeFont |
	AvailableFamilies _ Dictionary new.
	def _ AbstractFont default.

	AbstractFont familyNames do: [ :familyName |
		family _ StrikeFontFamily new.
		family familyName: familyName.
		(AbstractFont pointSizesFor: familyName) do: [ :ps |
			strikeFont _ AbstractFont familyName: familyName pointSize: ps.
			family atPointSize: ps put: strikeFont.
			def == strikeFont ifTrue: [
				DefaultFamilyName _ family familyName.
				DefaultPointSize _ ps ]].
		AvailableFamilies at: family familyName put: family ].! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 5/5/2019 15:05:46'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (AbstractFont familyName: FontFamily defaultFamilyName pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !

!methodRemoval: Preferences class #defaultFontFamily!
Preferences class removeSelector: #defaultFontFamily!
!methodRemoval: Preferences class #setDefaultFontFamilyTo:!
Preferences class removeSelector: #setDefaultFontFamilyTo:!
