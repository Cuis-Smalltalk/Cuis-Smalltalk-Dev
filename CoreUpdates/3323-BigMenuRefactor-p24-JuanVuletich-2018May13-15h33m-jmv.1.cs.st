'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 13 May 2018 at 4:26:25 pm'!

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 15:58:47'!
codeFileListMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Code File'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileIn'.
				#object 			-> 		#model.
				#selector 		-> 		#fileIn
			} asDictionary.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOut
			} asDictionary.
			{
				#label 			-> 		'remove existing'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUnmodifiedClasses
			} asDictionary.
		}`.
	^ aMenu! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 15:53:28'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Change List'.
	aMenu addStayUpIcons.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn selections'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInSelections.
				#balloonText 	-> 		'import the selected items into the image'.
			} asDictionary.
			{
				#label 			-> 		'fileOut selections...	'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSelections.
				#balloonText 	-> 		'create a new file containing the selected items'.
			} asDictionary.
			{
				#label 			-> 		'fileOut current version of selections...'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutCurrentVersionsOfSelections.
				#balloonText 	-> 		'create a new file containing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'compare to current'.
				#object 			-> 		#model.
				#selector 		-> 		#compareToCurrentVersion.
				#balloonText 	-> 		'open a separate window which shows the text differences between the on-file version and the in-image version.'.
			} asDictionary.
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#balloonText 	-> 		'start or stop showing diffs in the code pane.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select new methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectNewMethods.
				#balloonText 	-> 		'select methods in the file that do not currently exist in the image'.
			} asDictionary.
			{
				#label 			-> 		'select changes for absent classes'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForAbsentClasses.
				#balloonText 	-> 		'select methods in the file for classes that are not defined in the image'.
			} asDictionary.
			{
				#label 			-> 		'select all changes for this class'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForThisClass.
				#balloonText 	-> 		'select all methods in the file that belong to the currently-selected class'.
			} asDictionary.
			{
				#label 			-> 		'select unchanged methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectUnchangedMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions are the same as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods equivalent to current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectEquivalentMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods older than current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectMethodsOlderThanCurrent.
				#balloonText 	-> 		'select methods in the file that are older than the one currently in the image'.
			} asDictionary.
			{
				#label 			-> 		'select removals of sent methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectRemovalsOfSent.
				#balloonText 	-> 		'select all method removals of methods that have some sender in the image'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select all (a)'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAll.
				#balloonText 	-> 		'select all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'deselect all'.
				#object 			-> 		#model.
				#selector 		-> 		#deselectAll.
				#balloonText 	-> 		'deselect all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'invert selections'.
				#object 			-> 		#model.
				#selector 		-> 		#invertSelections.
				#balloonText 	-> 		'select every item that is not currently selected, and deselect every item that *is* currently selected'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse class and method'.
				#selector 		-> 		#browseMethodFull.
				#balloonText 	-> 		'open a full browser showing the selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse all versions of single selection'.
				#selector 		-> 		#browseVersions.
				#balloonText 	-> 		'open a version browser showing the versions of the currently selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse current versions of selections'.
				#selector 		-> 		#browseCurrentVersionsOfSelections.
				#balloonText 	-> 		'open a message-list browser showing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			{
				#label 			-> 		'destroy current methods of selections'.
				#object 			-> 		#model.
				#selector 		-> 		#destroyCurrentCodeOfSelections.
				#balloonText 	-> 		'remove (*destroy*) the in-image counterparts of all selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove doIts'.
				#object 			-> 		#model.
				#selector 		-> 		#removeDoIts.
				#balloonText 	-> 		'remove all items that are doIts rather than definitions'.
			} asDictionary.
			{
				#label 			-> 		'remove older versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeOlderMethodVersions.
				#balloonText 	-> 		'remove all but the most recent versions of methods in the list'.
			} asDictionary.
			{
				#label 			-> 		'remove up-to-date versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUpToDate.
				#balloonText 	-> 		'remove all items whose code is the same as the counterpart in-image code'.
			} asDictionary.
			{
				#label 			-> 		'remove empty class comments'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyClassComments.
				#balloonText 	-> 		'remove all empty class comments'.
			} asDictionary.
			{
				#label 			-> 		'remove selected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelections.
				#balloonText 	-> 		'remove the selected items from the change-list'.
			} asDictionary.
			{
				#label 			-> 		'remove unselected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeNonSelections.
				#balloonText 	-> 		'remove all the items not currently selected from the change-list'.
			} asDictionary.
		}`.
	^ aMenu! !


!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 16:02:25'!
contextFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreContextSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#contextClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 16:05:17'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fullStack (f)'.
				#selector 		-> 		#fullStack
			} asDictionary.
			{
				#label 			-> 		'restart (r)'.
				#selector 		-> 		#restarl
			} asDictionary.
			{
				#label 			-> 		'proceed (p)'.
				#selector 		-> 		#proceed
			} asDictionary.
			{
				#label 			-> 		'step (t)'.
				#selector 		-> 		#doStep
			} asDictionary.
			{
				#label 			-> 		'step through (T)'.
				#selector 		-> 		#stepIntoBlock
			} asDictionary.
			{
				#label 			-> 		'send (e)'.
				#selector 		-> 		#send
			} asDictionary.
			{
				#label 			-> 		'where (w)'.
				#selector 		-> 		#where
			} asDictionary.
			{
				#label 			-> 		'peel to first like this'.
				#selector 		-> 		#peelToFirst
			} asDictionary.
			nil.
			{
				#label 			-> 		'return entered value'.
				#selector 		-> 		#returnValue
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry
			} asDictionary.
		}`.
	^aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 16:01:41'!
receiverFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreReceiverSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#receiverClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 16:07:20'!
volumeMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory.
	^ aMenu! !


!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 16:19:22'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'inspect (i)'.
				#selector 		-> 		#inspectSelection.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreSelection.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#label 			-> 		'copy to clipboard (c)'.
				#selector 		-> 		#copySelectionToClipboard.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#label 			-> 		'basic inspect'.
				#selector 		-> 		#inspectBasic.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#label 			-> 		'references finder'.
				#selector 		-> 		#openReferencesFinder.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#label 			-> 		'weight explorer'.
				#selector 		-> 		#openWeightExplorer.
				#icon 			-> 		#exploreIcon
			} asDictionary.
		}`.

	object _ model object.
	(object is: #Dictionary) ifTrue: [
		aMenu addItemsFromDictionaries: `{
			nil.
			{
				#label 			-> 		'senders of this key'.
				#selector 		-> 		#sendersOfSelectedKey.
			} asDictionary.
			{
				#label 			-> 		'add key'.
				#selector 		-> 		#addEntry.
			} asDictionary.
			{
				#label 			-> 		'rename key'.
				#selector 		-> 		#renameEntry.
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelection.
			} asDictionary.
		}` ]
	
	ifFalse: [ (object is: #Set) ifTrue: [
		aMenu addItemsFromDictionaries: `{
			nil.
			{
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelection.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
		}` ]].

	aMenu
		addItemsFromDictionaries: `{
		nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
		}`.
	^ aMenu! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 5/13/2018 16:25:51'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				action: #yourself]
		ifNotNil: [
			aMenu
				addItemsFromDictionaries: `{
					{
						#label 			-> 		'inspect (i)'.
						#selector 		-> 		#inspectSelection.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'explore (I)'.
						#selector 		-> 		#exploreSelection.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'copy to clipboard (c)'.
						#selector 		-> 		#copySelectionToClipboard.
						#icon 			-> 		#copyIcon
					} asDictionary.
					{
						#label 			-> 		'basic inspect'.
						#selector 		-> 		#inspectBasic.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'references finder'.
						#selector 		-> 		#openReferencesFinder.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'weight explorer'.
						#selector 		-> 		#openWeightExplorer.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					nil.
					{
						#label 			-> 		'browse full (b)'.
						#selector 		-> 		#browseMethodFull.
						#icon 			-> 		#editFindReplaceIcon
					} asDictionary.
					{
						#label 			-> 		'browse hierarchy (h)'.
						#selector 		-> 		#browseHierarchy.
						#icon 			-> 		#goTopIcon
					} asDictionary.
					{
						#label 			-> 		'browse protocol (p)'.
						#selector 		-> 		#browseFullProtocol.
						#icon 			-> 		#spreadsheetIcon
					} asDictionary.
				}`.
			aMenu addLine;
				add: 'monitor changes'
				target: self
				action: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					action: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				action: #stopMonitoring ].
	^ aMenu! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel errorColor failureColor focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor successColor text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness minimalWindows roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarThickness steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useTaskbar useUniformColors useWindowTitleGradient)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelSeven blockLevelSix blockTemps defaults firstBlockLevel generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('menus - private' iconDefinersFor: iconsDefinitionFor:)
('menus' allIcons basicIcons menuDecorations miscellaneousIcons)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('accessing' decorateMenu:)
('shout' shout)
('initialization' initialize)
('keyboard shortcuts' keyStroke:morph:)
('as yet unclassified' windowClosed: windowOpen:)
!

