"
To set a theme:
"Note that some parts of the UI will not update. You may need to reopen your browsers, etc."
ThemeName beCurrent.
"Actually now you can do this from the appearance menu too!"

Themes are collections of methods that affect the way the system presents itself. They can be used to change the color scheme, as well as to modify other look and feel properties (e.g., round versus square window corners, embossed versus solid window labels.) Each of these methods answers a value like true or false, or a Color. Various parts of the system relating primarily to tools and menus query Theme class>>current to determine their configuration.

Theme serves as base theme. You can make new color schemes by subclassing it, and you can see what's possible by browsing its methods.

Shout's colors are configured using #shout. The method should be fairly self explanatory. The data structure answered by this method is used in #generateShoutConfig to create a large nested array, which is in turn requested by Shout to configure it's color scheme. The default scheme on systems that the present author has encountered thus far use only a subset
of shout's full customizability, as does this facade for it's configuration. For more granular control of Shout, you can override #generateShoutConfig to answer the data structure directly. To get a sense of what it looks like,
print:

Theme new generateShoutConfig.
*Be Warned:* If something goes wrong with this method, or the data isn't shaped correctly, weird and/or bad things can happen to Morphic. Be careful. I've supplied a test for it if you do decide to tinker with it. Send your theme #beCurrent in order to initialize Shout after modifying #shout.

Icons can now be swapped in and out of the system using ContentPack. The actual system icons are kept in a ContentPack instance that Theme hangs onto in a class variable called Content. Access the data structure with:

Theme content.

...and see the comment on ContentPack for more details. You can choose from three icon "moods;" in one almost every menu item has an icon, in another, the usual menu items have icons, and a few have been added, and in a third, icons are not shown, which is nice if you've done a theme using colors that don't work well with the built in icons and don't have a suitable set handy.

MenuIcons has been removed from the system. It's duties have been absorbed by Theme and ContentPack.

	- cbr
"
Class {
	#name : 'Theme',
	#superclass : 'Object',
	#classVars : [
		'Content',
		'CurrentTheme'
	],
	#category : 'Graphics-Themes',
	#package : 'Graphics',
	#tag : 'Themes'
}

{ #category : 'instance creation' }
Theme class >> beCurrent [
	^ self currentTheme: self
]

{ #category : 'importing/exporting' }
Theme class >> bootstrap [

	"Destructive. Loads up initial content."
	self flag: #todo. "Consider removal. --cbr"
	
	Content := ContentPack import
]

{ #category : 'user interface' }
Theme class >> changeFontSizes [
	| options menu |
	options := #(
		'40 - very large GUI elements'
		'32 - large GUI elements'
		'28 - large GUI elements'
		'24 - very high dpi laptops'
		'21 - very high dpi laptops'
		'18 - high dpi laptops'
		'16 - high dpi laptops'
		'14 - large desktop displays'
		'13 - large desktop displays'
		'12 - standard dpi displays'
		'11 - standard dpi displays'
		'10 - small GUI elements'
		'  9 - small GUI elements'
		'  6 - very small GUI elements'
		).
	menu := MenuMorph new.
	menu addTitle: 'Point size of default Font'; addStayUpIcons; removeStayUpBox.
	options do: [ :option | | pt addedItem |
		pt := option findPositiveInteger.
		addedItem := menu add: option target: self action: #chooseDefaultFontSize: argument: pt.
		(pt = FontFamily defaultPointSize) ifTrue: [ menu selectItem: addedItem ] ].
	menu popUpInWorld: self runningWorld.
]

{ #category : 'user interface' }
Theme class >> changeIcons [

	(MenuMorph new defaultTarget: self)
		addTitle: 'Menu Icons';
		addStayUpIcons;
		add: 'Use icons for menu entries' action: #useMenuIcons;
		add: 'Don''t use icons for menu entries' action: #useNoMenuIcons;
		popUpInWorld: self runningWorld
]

{ #category : 'user interface' }
Theme class >> changeTheme [

	| menu |
	menu := MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	
	(FeatureRequirement name: 'Theme-Themes') isAlreadySatisfied
		ifTrue: [
			menu addLine.
			Smalltalk at: #PersonalizedTheme ifPresent: [ :ptClass |
				ptClass savedThemes keysDo: [ :themeName |
					menu 
						add: themeName
						target: ptClass
						action: #loadThemeNamed:
						argument: themeName ]]]
		ifFalse: [
			menu addLine.
			menu add: '* Load Additional Themes *' target: self action: #loadAdditionalThemes ].

	menu popUpInWorld: self runningWorld.
]

{ #category : 'user interface' }
Theme class >> chooseDefaultFontSize: pointSize [
	"To help user experiment until happy, reopen menu."

	self setDefaultFontSize: pointSize; changeFontSizes.
]

{ #category : 'accessing' }
Theme class >> content [

	^ Content ifNil: [ Content := ContentPack new ]
]

{ #category : 'accessing' }
Theme class >> content: aContentPack [

	Content := aContentPack
]

{ #category : 'accessing' }
Theme class >> current [
	^ CurrentTheme
]

{ #category : 'class initialization' }
Theme class >> currentTheme: aTheme [

	CurrentTheme := aTheme new.
	SHTextStylerST80 releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		CurrentTheme useTaskbar
			ifTrue: [w showTaskbar]
			ifFalse: [w hideTaskbar].
		w restoreDisplay ].
	
	^ CurrentTheme
]

{ #category : 'user interface' }
Theme class >> largerDefaultFont [
	self setDefaultFontSize: (Preferences at: #defaultFontSize) + 1
]

{ #category : 'user interface' }
Theme class >> loadAdditionalThemes [
	"Load additional themes and reopen menu."
	Feature requireInformFaiure: #'Theme-Themes'.
	self changeTheme
]

{ #category : 'user interface' }
Theme class >> setDefaultFontSize: pointSize [

	Preferences at: #defaultFontSize put: pointSize.
	Preferences saveToDisk: #defaultFontSize
]

{ #category : 'user interface' }
Theme class >> setDefaultSizeAllWindows [

    Preferences name: #expandAllWindows category: #gui value: false.
    Preferences saveToDisk: #expandAllWindows.
]

{ #category : 'user interface' }
Theme class >> setExpandAllWindows [

    Preferences name: #expandAllWindows category: #gui value: true.
    Preferences saveToDisk: #expandAllWindows.
]

{ #category : 'user interface' }
Theme class >> smallerDefaultFont [
	self setDefaultFontSize: (Preferences at: #defaultFontSize) - 1
]

{ #category : 'user interface' }
Theme class >> useMenuIcons [
	Preferences at: #wantsMenuIcons put: true
]

{ #category : 'user interface' }
Theme class >> useNoMenuIcons [
	Preferences at: #wantsMenuIcons put: false
]

{ #category : 'consistency verification' }
Theme class >> verifyShoutConfig [

	"
	self verifyShoutConfig
	"
	^self allSubclasses
		inject: OrderedCollection new into: [ :errors :themeClass |
			[ themeClass new generateShoutConfig ] on: Error do: [:anError | | atIfAbsentContext key |
				"I have to do this because error is not resumable - Hernan"
				atIfAbsentContext := thisContext.
				10 timesRepeat: [ atIfAbsentContext := atIfAbsentContext sender ].
				key := atIfAbsentContext tempAt: 1.
				errors add: themeClass -> key.
				atIfAbsentContext receiver at: key put: Color black.
				atIfAbsentContext restart ].
			errors ]
]

{ #category : 'widget colors' }
Theme >> acceptButton [

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ `Color r: 0.2 g: 0.6 b: 0.1` ])
]

{ #category : 'icons' }
Theme >> acceptIcon [

			"	#( 'resolution' 'context' 'filename' )	"
	^ self fetch: #( '16x16' 'smalltalk' 'accept' )
]

{ #category : 'icons' }
Theme >> addressBookIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-address-book' )

]

{ #category : 'private - shout mappings' }
Theme >> ansiAssignment [
	"#bold"

	^ #(
		ansiAssignment
	)
]

{ #category : 'icons' }
Theme >> appearanceIcon [

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-theme' )

]

{ #category : 'icon lookup' }
Theme >> appendExtensionToContentSpec: anArray [

	^ ((anArray asOrderedCollection 
			copyReplaceFrom: 3 
			to: 3 
			with: 
				{ (anArray at: 3) , '.png' } ))
]

{ #category : 'private - shout mappings' }
Theme >> argumentTypes [

	^ #(
		patternArg
		methodArg
		blockPatternArg
		blockArg
		argument
	)
]

{ #category : 'private - shout mappings' }
Theme >> assignment [
	"#bold"

	^ #(
		assignment
	)
]

{ #category : 'tool colors' }
Theme >> autoCompleter [
	"Answer the autocompleter's color for  entries."

	^ self text
]

{ #category : 'tool colors' }
Theme >> autoCompleterDefaultBorderColor [
	"Answer the auto comleters default border color."

	^ `Color gray`
]

{ #category : 'tool colors' }
Theme >> autoCompleterMaybeInvalid [
	"Answer the autocompleter's color for possible invalid entries."

	^ `Color blue`
]

{ #category : 'colors' }
Theme >> background [

	"^ `Color r: 0.7 g: 0.72 b: 0.83`."
	^ `Color r: 0.167 g: 0.344 b: 0.629`
]

{ #category : 'icons' }
Theme >> blankIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'blank' )

]

{ #category : 'private - shout mappings' }
Theme >> blockLevelFive [

	^ #(
		leftParenthesis5
		rightParenthesis5
		blockStart5
		blockEnd5
	)
]

{ #category : 'private - shout mappings' }
Theme >> blockLevelFour [

	^ #(
		blockStart4 
		blockEnd4
		leftParenthesis4
		rightParenthesis4
	)

]

{ #category : 'private - shout mappings' }
Theme >> blockLevelOne [

	^ #(
		blockStart1
		blockEnd1
		leftParenthesis1
		rightParenthesis1
	)
]

{ #category : 'private - shout mappings' }
Theme >> blockLevelSeven [

	^ #(
		leftParenthesis7
		rightParenthesis7
		blockStart7
		blockEnd7
	)
]

{ #category : 'private - shout mappings' }
Theme >> blockLevelSix [

	^ #(
		leftParenthesis6
		rightParenthesis6
		blockStart6
		blockEnd6
	)

]

{ #category : 'private - shout mappings' }
Theme >> blockLevelThree [

	^ #(
		blockStart3
		blockEnd3
		leftParenthesis3
		rightParenthesis3
	)

]

{ #category : 'private - shout mappings' }
Theme >> blockLevelTwo [

	^ #(
		blockStart2
		blockEnd2
		leftParenthesis2
		rightParenthesis2
	)

]

{ #category : 'private - shout mappings' }
Theme >> blockLevelZero [

	^ #(
		blockStart
		blockEnd
		leftParenthesis
		rightParenthesis
	)
]

{ #category : 'private - shout mappings' }
Theme >> blockTemps [

	^ #(
		blockTempVar
		blockPatternTempVar
	)
]

{ #category : 'tool colors' }
Theme >> browser [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.5 g: 0.7 b: 0.4`]
]

{ #category : 'other options' }
Theme >> buttonBorderWidth [
	^ 1
]

{ #category : 'colors' }
Theme >> buttonColorFrom: aColor [
	^ Display depth <= 8
			ifTrue: [ `Color transparent` ]
			ifFalse: [ aColor paler ]
]

{ #category : 'colors' }
Theme >> buttonLabel [
	^ `Color gray: 0.18`
]

{ #category : 'other options' }
Theme >> buttonPaneHeight [
	"Answer the user's preferred default height for button panes."

	^(Preferences at: #standardButtonFont) lineSpacing * 14 // 8
]

{ #category : 'widget colors' }
Theme >> cancelButton [

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ `Color r: 0.8 g: 0.2 b: 0.2` ])
]

{ #category : 'icons' }
Theme >> cancelIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'cancel' )

]

{ #category : 'tool colors' }
Theme >> changeList [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.65 g: 0.6 b: 0.85` ]
]

{ #category : 'tool colors' }
Theme >> changeSorter [
	^ self packageList
]

{ #category : 'icons' }
Theme >> changesIcon [

	^ self fetch: #( '16x16' 'actions' 'view-refresh')

]

{ #category : 'icons' }
Theme >> chatIcon [

	^ self fetch: #( '16x16' 'apps' 'internet-group-chat' )

]

{ #category : 'icons' }
Theme >> classIcon [

	^ self fetch: #( '16x16' 'categories' 'applications-other' )

]

{ #category : 'icons' }
Theme >> clockIcon [

	^ self fetch: #( '16x16' 'actions' 'appointment-new' )

]

{ #category : 'icons' }
Theme >> closeIcon [

	^ self fetch: #( '14x14' 'buttons' 'close' )

]

{ #category : 'icons' }
Theme >> collapseIcon [

	^ self fetch: #( '14x14' 'buttons' 'collapse' )

]

{ #category : 'icons' }
Theme >> copyIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-copy' )

]

{ #category : 'icons' }
Theme >> cutIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-cut' )

]

{ #category : 'icons' }
Theme >> dateIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-calendar'  )

]

{ #category : 'icons' }
Theme >> debugIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'debug' )

]

{ #category : 'tool colors' }
Theme >> debugger [
	^ `Color h: 0.0 s: 0.6 v: 0.7`
]

{ #category : 'colors' }
Theme >> defaultButtonColor [

	^ self defaultWindowColor darker
]

{ #category : 'colors' }
Theme >> defaultListColor [

	^ self defaultWindowColor lighter
]

{ #category : 'tool colors' }
Theme >> defaultWindowColor [
	^ `Color veryVeryLightGray`
]

{ #category : 'private - shout mappings' }
Theme >> defaults [

	^ #(
		default
		arrayStart
		arrayEnd
		arrayStart1
		arrayEnd1
		leftBrace
		rightBrace
		cascadeSeparator
		chainSeparator
		statementSeparator
		externalCallType
		externalCallTypePointerIndicator
		blockArgColon
	)
]

{ #category : 'icons' }
Theme >> deleteIcon [

	^ self fetch: #( '16x16' 'places' 'user-trash' )

]

{ #category : 'icons' }
Theme >> developmentIcon [
	
	^ self fetch: #( '16x16' 'categories' 'applications-development' )

]

{ #category : 'icons' }
Theme >> displayIcon [

	^ self fetch: #( '16x16' 'devices' 'video-display' )

]

{ #category : 'icons' }
Theme >> doItIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'do-it' )
]

{ #category : 'icons' }
Theme >> editFindReplaceIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-find-replace' )

]

{ #category : 'icons' }
Theme >> emblemImportantIcon [

	^ self fetch: #( '16x16' 'emblems' 'emblem-important' )
]

{ #category : 'other options' }
Theme >> embossedButtonLabels [
	"Currently only apply to rounded buttons!"

	^true
]

{ #category : 'other options' }
Theme >> embossedTitles [
	^true
]

{ #category : 'colors' }
Theme >> errorColor [
	^ `Color red lighter`
]

{ #category : 'icons' }
Theme >> exitFullscreenIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'exit-fullscreen' )

]

{ #category : 'icons' }
Theme >> expandIcon [

	^ self fetch: #( '14x14' 'buttons' 'expand' )

]

{ #category : 'icons' }
Theme >> exploreIcon [

	^ self fetch: #( '16x16' 'actions' 'document-properties' )

]

{ #category : 'colors' }
Theme >> failureColor [
	^ `Color yellow lighter`
]

{ #category : 'icon lookup' }
Theme >> fetch: aTuple "	#( 'resolution' 'context' 'filename' )	" [

	"Get an icon from Content. See icons protocol."

	| contentSpecifier icon themeGuess |
	
	icon := nil.
	themeGuess := self class.
	contentSpecifier := self appendExtensionToContentSpec: aTuple.

	[ icon isNil ] 
		whileTrue: [
			icon := self class content
				from: themeGuess name
				get: contentSpecifier.
	
			icon ifNotNil: [ ^ icon ].
	
			themeGuess = Theme
				ifTrue: [ ^ nil "See comment in ContentPack>>from:get: --cbr" ].
	
			themeGuess := themeGuess superclass
		]
]

{ #category : 'tool colors' }
Theme >> fileContentsBrowser [
	^ `Color tan duller`
]

{ #category : 'tool colors' }
Theme >> fileList [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.7 g: 0.55 b: 0.7` ]
]

{ #category : 'icons' }
Theme >> fileOutIcon [

	^ self fetch: #( '16x16' 'actions' 'document-print' )

]

{ #category : 'icons' }
Theme >> findIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-find' )

]

{ #category : 'colors' }
Theme >> focusIndicator [
	^ self textHighlight
]

{ #category : 'icons' }
Theme >> fontIcon [
	^(Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'font-x-generic.png'
]

{ #category : 'icons' }
Theme >> fontXGenericIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'font-x-generic' )

]

{ #category : 'icons' }
Theme >> formatJustifyCenterIcon [

	^ self fetch: #( '16x16' 'actions' 'format-justify-center' )

]

{ #category : 'icons' }
Theme >> formatJustifyFillIcon [

	^ self fetch: #( '16x16' 'actions' 'format-justify-fill' )

]

{ #category : 'icons' }
Theme >> formatJustifyLeftIcon [

	^ self fetch: #( '16x16' 'actions' 'format-justify-left' )

]

{ #category : 'icons' }
Theme >> formatJustifyRightIcon [

	^ self fetch: #( '16x16' 'actions' 'format-justify-right' )

]

{ #category : 'other options' }
Theme >> fullScreenDeskMargin [
	^ (Preferences at: #fullScreenLeavesDeskMargins) ifTrue: [48] ifFalse: [0]
]

{ #category : 'private - shout mappings' }
Theme >> generateShoutConfig [

	| styles colors |
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self literals . colors at: #literals}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
		{self blockLevelZero . colors at: #blockLevelZero}.
		{self blockLevelOne . colors at: #blockLevelOne}.
		{self blockLevelTwo . colors at: #blockLevelTwo}.
		{self blockLevelThree . colors at: #blockLevelThree}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self incompleteMessages . colors at: #incompleteMessages . #underlined}.
		{self argumentTypes . colors at: #arguments . self italic}.
		{self symbols . colors at: #messages . #bold}.
		{self pattern . colors at: #selector . #bold}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #bold}.
		{self return . nil . #bold}.
		{self tempVars . colors at: #tempVars . self italic}.
		{self blockTemps . colors at: #tempBar . self italic}
	} do: [ :style |
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements := style asOrderedCollection.
					elements at: 1 put: category.
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined .#bold}.
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #comment . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles
]

{ #category : 'icons' }
Theme >> genericTextIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'text-x-generic' )

]

{ #category : 'private - shout mappings' }
Theme >> globals [

	^ #(
		primitiveOrExternalCallStart
		primitiveOrExternalCallEnd
		globalVar
		workspaceVar
	)
]

{ #category : 'icons' }
Theme >> globeIcon [

	^ self fetch: #( '16x16' 'apps' 'internet-web-browser' )

]

{ #category : 'icons' }
Theme >> goBottomIcon [

	^ self fetch: #( '16x16' 'actions' 'go-bottom' )

]

{ #category : 'icons' }
Theme >> goDownIcon [
	
	^ self fetch: #( '16x16' 'actions' 'go-down' )

]

{ #category : 'icons' }
Theme >> goTopIcon [

	^ self fetch: #( '16x16' 'actions' 'go-top' )

]

{ #category : 'icons' }
Theme >> goUpIcon [

	^ self fetch: #( '16x16' 'actions' 'go-up' )

]

{ #category : 'icons' }
Theme >> graphicsIcon [

	^ self fetch: #( '16x16' 'categories' 'applications-graphics' )

]

{ #category : 'icons' }
Theme >> halfRefreshIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'half-refresh' )

]

{ #category : 'icons' }
Theme >> haloCollapseIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-collapse' )

]

{ #category : 'icons' }
Theme >> haloColorIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-color' )

]

{ #category : 'icons' }
Theme >> haloDebugIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-debug' )

]

{ #category : 'icons' }
Theme >> haloDismissIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-dismiss' )

]

{ #category : 'icons' }
Theme >> haloDragIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-drag' )

]

{ #category : 'icons' }
Theme >> haloDuplicateIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-duplicate' )

]

{ #category : 'icons' }
Theme >> haloFontEmphasisIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-font-emphasis' )

]

{ #category : 'icons' }
Theme >> haloFontSizeIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-font-size' )

]

{ #category : 'icons' }
Theme >> haloGrabIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-grab' )

]

{ #category : 'icons' }
Theme >> haloHelpIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-help' )

]

{ #category : 'icons' }
Theme >> haloMenuIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-menu' )

]

{ #category : 'icons' }
Theme >> haloResizeIcon [

	^ Form
		small1BitExtent: 16@16
		pixelBits: #(
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000111111010000
			2r0000100001010000
			2r0000100001010000
			2r0000100001010000
			2r0000100001010000
			2r0000111111010000
			2r0000000000010000
			2r0000111111110000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000)
		offset: 0@0
]

{ #category : 'icons' }
Theme >> haloRotateIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'halo-rotate' )

]

{ #category : 'icons' }
Theme >> haloScaleIcon [

	^ Form
		small1BitExtent: 16@16
		pixelBits: #(
			2r0000000000000000
			2r0000000000000000
			2r0000111110000000
			2r0001000001000000
			2r0010001000100000
			2r0010001000100000
			2r0010111110100000
			2r0010001000100000
			2r0010001000100000
			2r0001000001100000
			2r0000111110010000
			2r0000000000001000
			2r0000000000000100
			2r0000000000000010
			2r0000000000000000
			2r0000000000000000)
		offset: 0@0
]

{ #category : 'icons' }
Theme >> helpIcon [

	^ self fetch: #( '16x16' 'apps' 'help-browser' )

]

{ #category : 'widget colors' }
Theme >> hoverHelp [
	"Answer the hover help morph's background color."

	^ `Color r: 1.0 g: 1.0 b: 0.7`
]

{ #category : 'icons' }
Theme >> imageIcon [
	^((Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'image-x-generic.png')
]

{ #category : 'private - shout mappings' }
Theme >> incompleteMessages [

	^ #(
		incompleteKeyword
		incompleteBinary
		incompleteUnary
	)
]

{ #category : 'icons' }
Theme >> inspectIcon [
	
	^ self fetch: #( '16x16' 'actions' 'system-search' )

]

{ #category : 'private - shout mappings' }
Theme >> instVar [
	^ #(
		instVar
	)
]

{ #category : 'icons' }
Theme >> instanceIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'application-x-executable' )

]

{ #category : 'private - shout mappings' }
Theme >> italic [
	^(Preferences at: #italicsInShout)
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]
]

{ #category : 'icons' }
Theme >> junkIcon [

	^ self fetch: #( '16x16' 'actions' 'mail-mark-junk' )

]

{ #category : 'icons' }
Theme >> keyboardShortcutsIcon [

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-keyboard-shortcuts' )

]

{ #category : 'other options' }
Theme >> layoutAdjusterThickness [

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ (Preferences at: #standardListFont) pointSize // 3
]

{ #category : 'colors' }
Theme >> line [
	^ self textHighlight darker
]

{ #category : 'icons' }
Theme >> listAddIcon [

	^ self fetch: #( '16x16' 'actions' 'list-add' )

]

{ #category : 'widget colors' }
Theme >> listHighlightFocused: aBoolean [
	^ self textHighlightFocused: aBoolean
]

{ #category : 'widget colors' }
Theme >> listMultiHighlightFocused: aBoolean [
	^ (self listHighlightFocused: aBoolean) 
		adjustSaturation: 0.0 brightness: -0.08
]

{ #category : 'icons' }
Theme >> listRemoveIcon [

	^ self fetch: #( '16x16' 'actions' 'list-remove' )

]

{ #category : 'private - shout mappings' }
Theme >> literals [

	^ #(
			character
			integer
			number
			-
	)
]

{ #category : 'icons' }
Theme >> mailForwardIcon [
	
	^ self fetch: #( '16x16' 'actions' 'mail-forward' )

]

{ #category : 'icons' }
Theme >> mailMarkJunkIcon [

	^ self fetch: #( '16x16' 'actions' 'mail-mark-junk' )

]

{ #category : 'icons' }
Theme >> mediaPlaybackStartIcon [

	^ self fetch: #( '16x16' 'actions' 'media-playback-start' )

]

{ #category : 'menu colors' }
Theme >> menu [
	Display depth <= 2 ifTrue: [^ `Color white` ].
	^ `Color r: 0.75 g: 0.75 b: 0.75 alpha: 0.93`
]

{ #category : 'other options' }
Theme >> menuBorderWidth [
	^ self roundWindowCorners ifTrue: [0] ifFalse: [1]
]

{ #category : 'menu colors' }
Theme >> menuHighlight [
	^ Display depth < 8
		ifTrue: [ `Color veryLightGray` ]
		ifFalse: [ self textHighlight ]
]

{ #category : 'menu colors' }
Theme >> menuText [
	^ `Color black`
]

{ #category : 'menu colors' }
Theme >> menuTitleBar [
	Display depth = 1 ifTrue: [^ `Color white`].
	Display depth = 2 ifTrue: [^ `Color gray: 0.5`].
	^ self menu darker
]

{ #category : 'tool colors' }
Theme >> messageNames [

	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.53 g: 0.77 b: 0.382` ]
]

{ #category : 'tool colors' }
Theme >> messageSet [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.45 g: 0.6 b: 0.85` ]
]

{ #category : 'private - shout mappings' }
Theme >> messages [

	^ #(
		keyword
		binary
		unary
	)
]

{ #category : 'private - shout mappings' }
Theme >> methodTags [

	^ #(
		primitive
		pragmaKeyword
		pragmaUnary
		pragmaBinary
		externalFunctionCallingConvention
		module
	)
]

{ #category : 'icons' }
Theme >> morphsIcon [
	
	^ self fetch: #( '16x16' 'smalltalk' 'morphs' )

]

{ #category : 'icons' }
Theme >> newFolderIcon [

	^ self fetch: #( '16x16' 'actions' 'folder-new' )

]

{ #category : 'icons' }
Theme >> newIcon [
	
	^ self fetch: #( '16x16' 'actions' 'document-new' )

]

{ #category : 'icons' }
Theme >> newWindowIcon [

	^ self fetch: #( '16x16' 'actions' 'window-new' )

]

{ #category : 'icons' }
Theme >> openIcon [

	^ self fetch: #( '16x16' 'actions' 'document-open' )

]

{ #category : 'tool colors' }
Theme >> packageDownloader [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color fromHexString: '#d0e973'` ]
]

{ #category : 'icons' }
Theme >> packageIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'package-x-generic' )
]

{ #category : 'tool colors' }
Theme >> packageInstaller [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color fromHexString: '#88e6d8'` ]
]

{ #category : 'tool colors' }
Theme >> packageList [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.63 g: 0.47 b: 0.08` ]
]

{ #category : 'colors' }
Theme >> paneBackgroundFrom: aColor [
	^ aColor veryMuchLighter
]

{ #category : 'icons' }
Theme >> pasteIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-paste' )

]

{ #category : 'private - shout mappings' }
Theme >> pattern [
	"#bold"

	^ #(
		patternKeyword
		patternBinary
		patternUnary
	)
]

{ #category : 'icons' }
Theme >> playIcon [
	^ self fetch: #( '16x16' 'actions' 'media-playback-start' )
]

{ #category : 'icons' }
Theme >> preferencesDesktopFontIcon [

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-font' )

]

{ #category : 'icons' }
Theme >> preferencesIcon [

	^ self fetch: #( '16x16' 'categories' 'preferences-system' )

]

{ #category : 'icon lookup' }
Theme >> prepend: aTheme toContentSpec: anArray [

	^ { aTheme printString}, anArray
]

{ #category : 'icons' }
Theme >> printIcon [

	^ self fetch: #( '16x16' 'actions' 'document-print-preview' )

]

{ #category : 'icons' }
Theme >> printerIcon [

	^ self fetch: #( '16x16' 'devices' 'printer' )

]

{ #category : 'private - shout mappings' }
Theme >> pseudoVariables [

	^ ClassBuilder reservedNames 
]

{ #category : 'icons' }
Theme >> pushPinIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'push-pin' )

]

{ #category : 'icons' }
Theme >> quitIcon [

	^ self fetch: #( '16x16' 'actions' 'system-log-out' )

]

{ #category : 'icons' }
Theme >> redoIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-redo' )

]

{ #category : 'icons' }
Theme >> refreshIcon [
	^ self fetch: #( '16x16' 'actions' 'view-refresh')
]

{ #category : 'icons' }
Theme >> removableMediaIcon [

	^ self fetch: #( '16x16' 'devices' 'drive-removable-media' )

]

{ #category : 'icons' }
Theme >> resizeBottomIcon [
	"
	Theme current resizeBottomIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom' )
]

{ #category : 'icons' }
Theme >> resizeBottomLeftIcon [
	"
	Theme current resizeBottomLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-left' )
]

{ #category : 'icons' }
Theme >> resizeBottomRightIcon [
	"
	Theme current resizeBottomRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-right' )
]

{ #category : 'icons' }
Theme >> resizeFullIcon [
	"
	Theme current resizeFullIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize' )
]

{ #category : 'icons' }
Theme >> resizeLeftIcon [
	"
	Theme current resizeLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-left' )
]

{ #category : 'icons' }
Theme >> resizeRightIcon [
	"
	Theme current resizeRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-right' )
]

{ #category : 'icons' }
Theme >> resizeTopIcon [
	"
	Theme current resizeTopIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top' )
]

{ #category : 'icons' }
Theme >> resizeTopLeftIcon [
	"
	Theme current resizeTopLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-left' )
]

{ #category : 'icons' }
Theme >> resizeTopRightIcon [
	"
	Theme current resizeTopRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-right' )
]

{ #category : 'private - shout mappings' }
Theme >> return [

	^ #(
		#return
	)
]

{ #category : 'other options' }
Theme >> roundButtons [
	^true
]

{ #category : 'other options' }
Theme >> roundWindowCorners [
	^true
]

{ #category : 'other options' }
Theme >> roundedButtonRadius [
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ Preferences at: #roundedButtonRadius
]

{ #category : 'other options' }
Theme >> roundedWindowRadius [
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^Preferences at: #roundedWindowRadius
]

{ #category : 'icons' }
Theme >> saveAndQuitIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'save-and-quit' )

]

{ #category : 'icons' }
Theme >> saveAsIcon [

	^ self fetch: #( '16x16' 'actions' 'document-save-as' )

]

{ #category : 'icons' }
Theme >> saveAsNewVersionIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'save-as-new-version' )

]

{ #category : 'icons' }
Theme >> saveIcon [

	^ self fetch: #( '16x16' 'actions' 'document-save' )

]

{ #category : 'icons' }
Theme >> scriptIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'text-x-script' )

]

{ #category : 'colors' }
Theme >> scrollbarButtonColor [
	^ `Color gray: 0.95`
]

{ #category : 'colors' }
Theme >> scrollbarColor [
	^ `Color white`
]

{ #category : 'other options' }
Theme >> scrollbarShowButtons [
	^ true
]

{ #category : 'colors' }
Theme >> scrollbarSliderShadowColor [
	^ `Color white`
]

{ #category : 'other options' }
Theme >> scrollbarThickness [
	^ Preferences at: #scrollbarThickness
]

{ #category : 'icons' }
Theme >> selectAllIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-select-all' )

]

{ #category : 'icons' }
Theme >> sendReceiveIcon [

	^ self fetch: #( '16x16' 'actions' 'mail-send-receive' )

]

{ #category : 'shout' }
Theme >> shout [
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  						-> nil.
		#arguments  					-> #(cyan muchDarker).
		#comment 						-> #(green muchDarker).
		#tempBar 						-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 						-> #(magenta muchDarker).
		#pseudoVariables 				-> #(red muchDarker).
		#literals 						-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 			-> #(gray veryMuchDarker).
		#blockLevelZero 				-> #black.
		#blockLevelOne 				-> #brown.
		#blockLevelTwo 				-> #magenta.
		#blockLevelThree 				-> #red.
		#blockLevelFour 				-> #(orange darker).
		#blockLevelFive 				-> #(orange muchDarker).
		#blockLevelSix 				-> #(green muchDarker).
		#blockLevelSeven 				-> #blue.
		#defaults 						-> #black.
		#undefined 					-> #red.
		#methodTags 					-> #(green muchDarker).
	}
]

{ #category : 'icons' }
Theme >> speadsheetTemplateIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-spreadsheet-template' )

]

{ #category : 'icons' }
Theme >> spreadsheetIcon [

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-spreadsheet' )

]

{ #category : 'icons' }
Theme >> stillCameraIcon [

	^ self fetch: #( '16x16' 'devices' 'camera-photo' )

]

{ #category : 'icons' }
Theme >> stopIcon [
	^ self fetch: #( '16x16' 'actions' 'media-playback-stop' )
]

{ #category : 'colors' }
Theme >> successColor [
	^ `Color green lighter`
]

{ #category : 'icons' }
Theme >> switchIcon [

	^ self fetch: #( '16x16' 'actions' 'system-shutdown' )

]

{ #category : 'private - shout mappings' }
Theme >> symbols [
	^ #(
		symbol
		stringSymbol
		literalArray
	)
]

{ #category : 'icons' }
Theme >> systemFileManagerIcon [

	^ self fetch: #( '16x16' 'apps' 'system-file-manager' )

]

{ #category : 'icons' }
Theme >> systemIcon [

	^ self fetch: #( '16x16' 'categories' 'applications-system' )

]

{ #category : 'icons' }
Theme >> systemMonitorIcon [

	^ self fetch: #( '16x16' 'apps' 'utilities-system-monitor' )

]

{ #category : 'private - shout mappings' }
Theme >> tempBar [
	
	^ #(
		methodTempBar
		blockTempBar
		blockArgsBar
	)
]

{ #category : 'private - shout mappings' }
Theme >> tempVars [

	^ #(
		tempVar
		patternTempVar
		poolConstant
	)
]

{ #category : 'icons' }
Theme >> terminalIcon [

	^ self fetch: #( '16x16' 'apps' 'utilities-terminal' )
]

{ #category : 'tool colors' }
Theme >> testRunner [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `(Color r: 0.650 g: 0.753 b: 0.976) duller` ]
]

{ #category : 'colors' }
Theme >> text [
	^ `Color black`
]

{ #category : 'colors' }
Theme >> textCursor [
	^ Display depth <= 2
		ifTrue: [ `Color black` ]
		ifFalse: [ self text ]
]

{ #category : 'tool colors' }
Theme >> textEditor [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color gray: 0.6` ]
]

{ #category : 'icons' }
Theme >> textEditorIcon [

	^ self fetch: #( '16x16' 'apps' 'accessories-text-editor' )

]

{ #category : 'colors' }
Theme >> textEmptyDisplayMessage [
	^ `Color veryLightGray`
]

{ #category : 'colors' }
Theme >> textHighlight [
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^ `Color hue: 204 chroma: 0.29 luminance: 0.77`
]

{ #category : 'colors' }
Theme >> textHighlightFocused: focused [
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ `Color veryLightGray` ].
	Display depth = 2 ifTrue: [^ `Color gray: 0.87` ].
	textHighlight := self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]
]

{ #category : 'widget colors' }
Theme >> textPane [
	^ `Color white`
]

{ #category : 'other options' }
Theme >> titleBarButtonsExtent [
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e := (Preferences at: #windowTitleFont) pointSize * 13 // 10.
	^e@e
]

{ #category : 'tool colors' }
Theme >> transcript [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.8 g: 0.6 b: 0.3` ]
]

{ #category : 'private - shout mappings' }
Theme >> undefined [

	^ #(
		invalid
		excessCode
		#'$'
		undefinedKeyword
		undefinedBinary
		undefinedUnary
	)
]

{ #category : 'icons' }
Theme >> undoIcon [

	^ self fetch: #( '16x16' 'actions' 'edit-undo' )

]

{ #category : 'colors' }
Theme >> unfocusedTextHighlightFrom: aColor [
	^aColor adjustSaturation: -0.15 brightness: -0.13
]

{ #category : 'icons' }
Theme >> updateIcon [

	^ self fetch: #( '16x16' 'status' 'software-update-available' )

]

{ #category : 'other options' }
Theme >> useTaskbar [
	^true
]

{ #category : 'other options' }
Theme >> useUniformColors [
	^false
]

{ #category : 'icons' }
Theme >> usersIcon [

	^ self fetch: #( '16x16' 'apps' 'system-users' )

]

{ #category : 'tool colors' }
Theme >> versionsBrowser [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `(Color r: 0.869 g: 0.753 b: 1.0) duller` ]
]

{ #category : 'icons' }
Theme >> viewFullscreenIcon [

	^ self fetch: #( '16x16' 'actions' 'view-fullscreen' )

]

{ #category : 'icons' }
Theme >> wallpaperIcon [

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-wallpaper' )

]

{ #category : 'icons' }
Theme >> warningIcon [

	^ self fetch: #( '16x16' 'status' 'dialog-warning' )
]

{ #category : 'icons' }
Theme >> weatherFewCloudsIcon [

	^ self fetch: #( '16x16' 'status' 'weather-few-clouds' )

]

{ #category : 'widget colors' }
Theme >> widgetsColorFromToolColor [
	"Answer false for subclasses that want to control color of widgets independently of the SystemWindow owning them"
	^true
]

{ #category : 'other options' }
Theme >> windowBorderWidth [
	| w |
	w := (Preferences at: #standardListFont) pointSize / 11.
	w := w * (self roundWindowCorners ifTrue: [4] ifFalse: [2]).
	^ w rounded max: 1
]

{ #category : 'icons' }
Theme >> windowIcon [

	^ self fetch: #( '16x16' 'apps' 'preferences-system-windows'  )

]

{ #category : 'colors' }
Theme >> windowLabel [
	^ `Color gray: 0.3`
]

{ #category : 'icons' }
Theme >> windowMenuIcon [

	^ self fetch: #( '14x14' 'buttons' 'window-menu' )

]

{ #category : 'tool colors' }
Theme >> workspace [
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color h: 60.0 s: 0.73 v: 0.72` ]
]

{ #category : 'icons' }
Theme >> worldIcon [

	^ self fetch: #( '16x16' 'smalltalk' 'world' )

]
