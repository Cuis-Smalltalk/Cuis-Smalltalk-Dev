'From Cuis 4.2 of 25 July 2013 [latest update: #2706] on 25 March 2016 at 1:42:42.410219 pm'!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString updatablePanes layoutMorph adjusters '
	classVariableNames: 'TopWindow '
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!Object methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:49'!
stepAt: millisecondSinceLast
	"See comment at #wantsSteps"! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 3/25/2016 12:49'!
stepAt: millisecondSinceLast

	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes _ true ]]].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]! !


!ProcessBrowser methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:49'!
stepAt: millisecondSinceLast
	self updateProcessList! !


!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 3/25/2016 13:28'!
updateIfNeeded
	self didCodeChangeElsewhere
		ifTrue: [
			self acceptedContentsChanged]! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 3/25/2016 13:28'!
updateIfNeeded
	| aMethod |
	aMethod _ classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 3/25/2016 13:28'!
updateIfNeeded
	| aComment |
	aComment _ classOfMethod organization commentRemoteStr.
	aComment == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aComment last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/25/2016 13:29'!
updateIfNeeded
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList ].

	newList _ self changeSetDirtyFlags.
	(priorDirtyFlags == nil or: [priorDirtyFlags ~= newList])
		ifTrue: [
			priorDirtyFlags _ newList.
			self changed: #changeSetDirtyFlags ]! !


!Inspector methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:49'!
stepAt: millisecondSinceLast
	| newText |
	newText := self contentsIsString
		ifTrue: [self selection]
		ifFalse: ["keep it short to reduce time to compute it"
			self selectionPrintString ].
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/25/2016 13:29'!
update
	"Recompute all of my lists."

	self updateIfNeeded! !


!MorphicTranslation methodsFor: 'comparing' stamp: 'jmv 3/25/2016 13:41'!
= aMorphicTranslation
	self == aMorphicTranslation ifTrue: [ ^ true ].
	(aMorphicTranslation is: #MorphicLocation) ifFalse: [ ^false ].
	aMorphicTranslation isPureTranslation ifFalse: [ ^false ].
	^self translation = aMorphicTranslation translation! !


!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 3/24/2016 22:54'!
verifyContents
	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes."

	| newList oldList |
	oldList _ list ifNil: [ #() ].
	newList _ self getList.
	oldList = newList ifFalse: [
		self updateList ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 3/24/2016 23:25'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self rescaleButtons.
	self model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.
! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/25/2016 13:35'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	
	self initializeLabelArea.
	extent _ 300 @ 200.

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ].

	"by default"
	self beColumn! !

!SystemWindow methodsFor: 'stepping' stamp: 'jmv 3/25/2016 12:50'!
stepAt: millisecondSinceLast
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [ self wantsStepsWhenCollapsed ]) ifTrue: [
		model ifNotNil: [ model stepAt: millisecondSinceLast ].
		super stepAt: millisecondSinceLast ]! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 13:37'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updateIfNeeded ]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/24/2016 23:25'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes |
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicSystemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: self buildMorphicClassColumn proportionalWidth: 0.2;
		addAdjusterAndMorph: self buildMorphicMessageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/24/2016 23:26'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicClassColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont height + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 3/24/2016 23:26'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection! !


!VersionsBrowserWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 13:31'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded! !


!ChangeSorterWindow methodsFor: 'updating' stamp: 'jmv 3/25/2016 13:30'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 3/24/2016 23:26'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |
	"Build widgets. We'll assemble them below."
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	self setLabel: ([model object printStringLimitedTo: 64] on: Error do: [ model object class name, ' (printing failed)']).! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/24/2016 23:26'!
buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 _ PluggableListMorph
				model: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:
				mainView: self
				menuGetter: #processListMenu
				keystrokeAction: #processListKey:from:.
	list2 _ PluggableListMorph
				model: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:
				mainView: self
				menuGetter: #stackListMenu
				keystrokeAction: #stackListKey:from:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph _ TextModelMorph
				textProvider: model
				textGetter: #selectedMethod.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 3/24/2016 23:10'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)

		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useAllIcons
	".
	Theme beCurrent.
	Taskbar showTaskbar
	"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 3/24/2016 23:10'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	self runningWorld backgroundImageData: nil.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 3/24/2016 23:10'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

!methodRemoval: Preferences class #smartUpdating!
Preferences class removeSelector: #smartUpdating!
!methodRemoval: SystemWindow #setUpdatablePanesFrom:!
SystemWindow removeSelector: #setUpdatablePanesFrom:!
!methodRemoval: SystemWindow #updatablePanes!
SystemWindow removeSelector: #updatablePanes!
!methodRemoval: Behavior #removeSelectorSilently:!
Behavior removeSelector: #removeSelectorSilently:!
!methodRemoval: Inspector #stepAt:in:!
Inspector removeSelector: #stepAt:in:!
!methodRemoval: Inspector #updateListsAndCodeIn:!
Inspector removeSelector: #updateListsAndCodeIn:!
!methodRemoval: ChangeSorter #stepAt:in:!
ChangeSorter removeSelector: #stepAt:in:!
!methodRemoval: ChangeSorter #updateIfNecessary!
ChangeSorter removeSelector: #updateIfNecessary!
!methodRemoval: ClassCommentVersionsBrowser #updateListsAndCodeIn:!
ClassCommentVersionsBrowser removeSelector: #updateListsAndCodeIn:!
!methodRemoval: ClassCommentVersionsBrowser #updatePaneIfNeeded!
ClassCommentVersionsBrowser removeSelector: #updatePaneIfNeeded!
!methodRemoval: VersionsBrowser #updateListsAndCodeIn:!
VersionsBrowser removeSelector: #updateListsAndCodeIn:!
!methodRemoval: VersionsBrowser #updatePaneIfNeeded!
VersionsBrowser removeSelector: #updatePaneIfNeeded!
!methodRemoval: CodeProvider #stepAt:in:!
CodeProvider removeSelector: #stepAt:in:!
!methodRemoval: CodeProvider #updateListsAndCodeIn:!
CodeProvider removeSelector: #updateListsAndCodeIn:!
!methodRemoval: CodeProvider #updatePaneIfNeeded!
CodeProvider removeSelector: #updatePaneIfNeeded!
!methodRemoval: ProcessBrowser #stepAt:in:!
ProcessBrowser removeSelector: #stepAt:in:!
!methodRemoval: ObjectExplorer #stepAt:in:!
ObjectExplorer removeSelector: #stepAt:in:!
!methodRemoval: Object #stepAt:in:!
Object removeSelector: #stepAt:in:!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString layoutMorph adjusters'
	classVariableNames: 'TopWindow'
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
