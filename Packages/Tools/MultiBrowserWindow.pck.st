'From Cuis7.3 [latest update: #6895] on 6 December 2024 at 4:46:34 pm'!
'Description '!
!provides: 'MultiBrowserWindow' 1 3!
SystemOrganization addCategory: #MultiBrowserWindow!


!classDefinition: #MultiBrowserWindow category: #MultiBrowserWindow!
BrowserWindow subclass: #MultiBrowserWindow
	instanceVariableNames: 'browserWindowsState'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MultiBrowserWindow'!
!classDefinition: 'MultiBrowserWindow class' category: #MultiBrowserWindow!
MultiBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #BrowserWindowState category: #MultiBrowserWindow!
Object subclass: #BrowserWindowState
	instanceVariableNames: 'model mainLayout codePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MultiBrowserWindow'!
!classDefinition: 'BrowserWindowState class' category: #MultiBrowserWindow!
BrowserWindowState class
	instanceVariableNames: ''!


!MultiBrowserWindow commentStamp: 'jmv 12/6/2024 16:37:14' prior: 0!
BrowserWindow new openInWorld
MultiBrowserWindow new openInWorld

BrowserWindow findAndBrowseClass
BrowserWindow fullOnClass: Float selector: #abs
BrowserWindow hierarchyOnClass: Float selector: #abs

MultiBrowserWindow findAndBrowseClass
MultiBrowserWindow fullOnClass: Float selector: #abs
MultiBrowserWindow openOnClasses: (Magnitude withAllSubclasses select: [ :c | c hasSubclasses ])
MultiBrowserWindow openOnClasses: {Number. Float. Point} selectors: { #abs. #abs. #x}!

!MultiBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 12/3/2024 11:00:16'!
buildMorphicWindow

	super buildMorphicWindow.
	browserWindowsState add:
		(BrowserWindowState model: model mainLayout: layoutMorph codePane: codePane).
	self browserModelsDo: [ :m |
		m addDependent: self ].! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/3/2024 09:44:12'!
addNew
	| browser |
	browser := Browser new.
	self model: browser.
	layoutMorph := nil.
	self beColumn.
	self buildMorphicWindow.
	self adoptWidgetsColor: borderColor.
	self update: #relabel.! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/3/2024 11:06:04'!
closeTab
	| i |
	browserWindowsState size > 1 ifTrue: [
		i := self indexOfModel.
		browserWindowsState removeAt: i.
		self showTab: 1 ].! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/2/2024 16:46:44'!
findClass

	self addNew.
	super findClass! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/3/2024 10:53:41'!
nextTab
	| i |
	i := self indexOfModel.
	i := i-1+1 \\ browserWindowsState size + 1.
	self showTab: i.! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/3/2024 10:54:02'!
prevTab
	| i |
	i := self indexOfModel.
	i := i-1-1 \\ browserWindowsState size + 1.
	self showTab: i.! !

!MultiBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/3/2024 11:00:02'!
showTab: i
	model := self browserModelAt: i.
	layoutMorph := self mainLayoutAt: i.
	codePane := self codePaneAt: i.
	self mainLayoutsDo: [ :each | each visible: each = layoutMorph ].
	self update: #relabel.! !

!MultiBrowserWindow methodsFor: 'menu' stamp: 'jmv 12/3/2024 11:02:09'!
offerMultiBrowserMenu
	"
	1. window title
	2. other window title
	new window
	copy this window
	remove this window
	"

	| aMenu |

	aMenu := MenuMorph new defaultTarget: self.
	browserWindowsState withIndexDo: [ :each :i | | menuItem |
		menuItem := aMenu add: i printString, '-', each browserModel labelString action: #showTab: argument: i.
		each mainLayout canDiscardEdits ifFalse: [
			menuItem color: Color red ]].
	aMenu add: 'Add new Browser' action: #addNew.
	browserWindowsState size > 1 ifTrue: [
		aMenu add: 'Close current' action: #closeTab ].

	aMenu popUpInWorld: self world! !

!MultiBrowserWindow methodsFor: 'geometry' stamp: 'jmv 12/6/2024 16:04:30'!
labelXPosition
	| f |
	f := Preferences at: #windowTitleFont.
	^ f lineSpacing * 9 + borderWidth! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/6/2024 16:28:50'!
createMultiBrowserCloseButton
	^ (PluggableButtonMorph model: self action: #closeTab)
		iconDrawSelector: #drawSquareCloseIcon;
		setBalloonText: 'close this browser tab';
		morphExtent: Theme current titleBarButtonsExtent! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/6/2024 16:36:29'!
createMultiBrowserMenuButton
	^ (PluggableButtonMorph model: self action: #offerMultiBrowserMenu)
		iconDrawSelector: #drawSquareExpandIcon;
		setBalloonText: 'multi browser menu';
		morphExtent: Theme current titleBarButtonsExtent! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/2/2024 16:02:17'!
createMultiBrowserNextButton
	^ (PluggableButtonMorph model: self action: #nextTab)
		iconDrawSelector: #drawRightIcon;
		setBalloonText: 'next browser';
		morphExtent: Theme current titleBarButtonsExtent! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/2/2024 16:02:00'!
createMultiBrowserPrevButton
	^ (PluggableButtonMorph model: self action: #prevTab)
		iconDrawSelector: #drawLeftIcon;
		setBalloonText: 'prev browser';
		morphExtent: Theme current titleBarButtonsExtent! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/3/2024 10:58:02'!
initialize
	super initialize.
	browserWindowsState := OrderedCollection new.
! !

!MultiBrowserWindow methodsFor: 'initialization' stamp: 'jmv 12/6/2024 16:02:51'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	super initializeLabelArea.
	topLeft := (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing := Theme current titleBarButtonsExtent x *13//10.
	self addMorph: self createMultiBrowserPrevButton position: spacing*4@0 + topLeft.
	self addMorph: self createMultiBrowserMenuButton position: spacing*5@0 + topLeft.
	self addMorph: self createMultiBrowserCloseButton position: spacing*6@0 + topLeft.
	self addMorph: self createMultiBrowserNextButton position: spacing*7@0 + topLeft.! !

!MultiBrowserWindow methodsFor: 'label' stamp: 'jmv 12/3/2024 10:48:32'!
setLabel: aString
	super setLabel: self indexOfModel printString, '. ', aString.! !

!MultiBrowserWindow methodsFor: 'layout' stamp: 'jmv 12/3/2024 10:57:19'!
layoutSubmorphs

	| pos |
	super layoutSubmorphs.
	pos := borderWidth @ (borderWidth + self labelHeight).
	self mainLayoutsDo: [ :lm |
		lm position: pos extent: extent - pos - borderWidth ].! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 11:01:58'!
browserModelAt: i
	^(browserWindowsState at: i) browserModel! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 11:02:03'!
browserModelsDo: aBlock
	browserWindowsState do: [ :m | aBlock value: m browserModel ].! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 10:49:12'!
codePaneAt: i
	^(browserWindowsState at: i) codePane! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 11:01:14'!
indexOfModel
	^browserWindowsState findFirst: [ :bws | bws browserModel = model ]! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 10:49:02'!
mainLayoutAt: i
	^(browserWindowsState at: i) mainLayout! !

!MultiBrowserWindow methodsFor: 'private' stamp: 'jmv 12/3/2024 10:50:55'!
mainLayoutsDo: aBlock
	browserWindowsState do: [ :bws | aBlock value: bws mainLayout ].! !

!MultiBrowserWindow class methodsFor: 'utilities' stamp: 'eem 12/5/2024 16:30:28'!
beDefaultBrowserWindowClass
	PreferenceSet sysPreferences
		name: #browserWindowClass
		category: #programming
		value: self! !

!MultiBrowserWindow class methodsFor: 'instance creation' stamp: 'jmv 12/2/2024 16:59:58'!
openOnClasses: aCollection
	"
	MultiBrowserWindow openOnClasses: (Magnitude withAllSubclasses select: [ :c | c hasSubclasses ])
	"
	| w |
	w := self fullOnClass: nil selector: nil.
	aCollection
		do: 	[ :c |
			w model 
				setSelectedSystemCategory: c category;
				selectClass: c ]
		separatedBy:
			[ w addNew ].
	w showTab: 1.
! !

!MultiBrowserWindow class methodsFor: 'instance creation' stamp: 'jmv 12/3/2024 10:21:14'!
openOnClasses: aCollection selectors: another
	"
	MultiBrowserWindow openOnClasses: {Number. Float. Point} selectors: { #abs. #abs. #x}
	"
	| w |
	w := self fullOnClass: nil selector: nil.
	aCollection with: another
		do: 	[ :c :s |
			w model 
				setSelectedSystemCategory: c category;
				selectClass: c;
				selectedMessageName: s;
				showHomeCategory ]
		separatedBy:
			[ w addNew ].
	w showTab: 1.
! !

!BrowserWindowState methodsFor: 'accessing' stamp: 'jmv 12/3/2024 10:58:46'!
browserModel
	^model! !

!BrowserWindowState methodsFor: 'accessing' stamp: 'jmv 12/3/2024 10:42:32'!
codePane
	^codePane! !

!BrowserWindowState methodsFor: 'accessing' stamp: 'jmv 12/3/2024 10:42:27'!
mainLayout
	^mainLayout! !

!BrowserWindowState methodsFor: 'initialization' stamp: 'jmv 12/3/2024 10:39:33'!
model: aModel mainLayout: aLayout codePane: aCodePane
	model := aModel.
	mainLayout := aLayout.
	codePane := aCodePane.! !

!BrowserWindowState class methodsFor: 'instance creation' stamp: 'jmv 12/3/2024 10:39:48'!
model: aModel mainLayout: aLayout codePane: aCodePane
	^self new model: aModel mainLayout: aLayout codePane: aCodePane! !
