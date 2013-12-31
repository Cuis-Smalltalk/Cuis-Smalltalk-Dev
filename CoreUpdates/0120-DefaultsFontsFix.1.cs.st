'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 26 February 2009 at 2:00:39 pm'!!OldSystemWindow methodsFor: 'initialization' stamp: 'jmv 2/26/2009 13:40'!initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	label := (OldStringMorph new)
				contents: labelString;
				font: Preferences windowTitleFont.
	"Add collapse box so #labelHeight will work"
	collapseBox := self createCollapseBox.
	stripes := Array with: (OldRectangleMorph newBounds: bounds)
				with: (OldRectangleMorph newBounds: bounds).
	"see extent:"
	self addLabelArea.
	labelArea addMorph: (stripes first borderWidth: 1).
	labelArea addMorph: (stripes second borderWidth: 2).
	self setLabelWidgetAllowance.
	self addCloseBox.
	self addMenuControl.
	labelArea addMorph: label.
	self wantsExpandBox ifTrue: [self addExpandBox].
	labelArea addMorph: collapseBox.
	self setFramesForLabelArea.
	Preferences noviceMode 
		ifTrue: 
			[closeBox ifNotNil: [closeBox setBalloonText: 'close window' translated].
			menuBox ifNotNil: [menuBox setBalloonText: 'window menu' translated].
			collapseBox 
				ifNotNil: [collapseBox setBalloonText: 'collapse/expand window' translated]]! !!OldSystemWindow methodsFor: 'label' stamp: 'jmv 2/26/2009 13:39'!widthOfFullLabelText	^Preferences windowTitleFont widthOfString: labelString! !!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/26/2009 13:44'!fontConfigurationMenu
	| aMenu |
	aMenu := OldMenuMorph new defaultTarget: Preferences.
	aMenu addTitle: 'Standard System Fonts' translated.
	aMenu addStayUpIcons.
	aMenu add: 'default text font...' translated action: #chooseSystemFont.
	aMenu 
		balloonTextForLastItem: 'Choose the default font to be used for code and  in workspaces, transcripts, etc.' 
				translated.
	aMenu lastItem font: Preferences standardDefaultTextFont.
	aMenu add: 'list font...' translated action: #chooseListFont.
	aMenu lastItem font: Preferences standardListFont.
	aMenu balloonTextForLastItem: 'Choose the font to be used in list panes' 
				translated.
	aMenu add: 'menu font...' translated action: #chooseMenuFont.
	aMenu lastItem font: Preferences standardMenuFont.
	aMenu 
		balloonTextForLastItem: 'Choose the font to be used in menus' translated.
	aMenu add: 'window-title font...' translated action: #chooseWindowTitleFont.
	aMenu lastItem font: Preferences windowTitleFont.
	aMenu 
		balloonTextForLastItem: 'Choose the font to be used in window titles.' 
				translated.
	aMenu add: 'balloon-help font...' translated action: #chooseBalloonHelpFont.
	aMenu lastItem font: Preferences standardBalloonHelpFont.
	aMenu 
		balloonTextForLastItem: 'choose the font to be used when presenting balloon help.' 
				translated.
	aMenu add: 'code font...' translated action: #chooseCodeFont.
	aMenu lastItem font: Preferences standardCodeFont.
	aMenu balloonTextForLastItem: 'Choose the font to be used in code panes.' 
				translated.
	aMenu addLine.
	aMenu add: 'restore default font choices' translated
		action: #restoreDefaultFonts.
	aMenu 
		balloonTextForLastItem: 'Use the standard system font defaults' translated.
	aMenu add: 'print default font choices' translated
		action: #printStandardSystemFonts.
	aMenu 
		balloonTextForLastItem: 'Print the standard system font defaults to the Transcript' 
				translated.
	^aMenu! !!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/26/2009 13:46'!printStandardSystemFonts
	"self printStandardSystemFonts"

	#(standardDefaultTextFont standardListFont 
	standardMenuFont windowTitleFont 
	standardBalloonHelpFont standardCodeFont standardButtonFont) do: [:selector |
		| font |
		font _ Preferences perform: selector.
		Transcript
			cr; show: selector;
			space; show: font printString]! !!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/26/2009 13:58'!restoreDefaultFonts
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."
	"Preferences restoreDefaultFonts"

	self setDefaultFonts: #(
		(setSystemFontTo:		Vernada				10)
		(setListFontTo:			Vernada				10)
		(setMenuFontTo:		Vernada				12)
		(setWindowTitleFontTo:	Vera					12		1)
		(setBalloonHelpFontTo:	Vernada				7)
		(setCodeFontTo:		Vernada				10)
		(setButtonFontTo:		Accuny					12)
	)! !!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/26/2009 13:53'!setDefaultFonts: defaultFontsSpec	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."	| fontNames map font |	fontNames _ defaultFontsSpec collect: [:array | array second].	map _ IdentityDictionary new.	fontNames do: [:originalName |		| style response |		style _ map at: originalName put: (TextStyle named: originalName).		style ifNil: [			response _ TextStyle modalStyleSelectorWithTitle: 'Choose replacement for text style ', originalName.			map at: originalName put: (response ifNil: [TextStyle default])]].	defaultFontsSpec do: [:triplet |		font _ (map at: triplet second) fontOfPointSize: triplet third.		triplet size > 3 ifTrue: [			font _ font emphasized: triplet fourth ].		self			perform: triplet first			with: font]! !