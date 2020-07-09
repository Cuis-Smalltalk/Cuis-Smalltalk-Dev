'From Cuis 5.0 [latest update: #4249] on 6 July 2020 at 10:16:58 am'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SmalltalkCompleterMorph category: #'Tools-Autocompletion'!
AutoCompleterMorph subclass: #SmalltalkCompleterMorph
	instanceVariableNames: 'selectorDocumentation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleterMorph commentStamp: '<historical>' prior: 0!
Specific for Smalltalk code.!


!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:28:19'!
crPressedOnSelectorDocumentation

	self hideSelectorDocumentation.
	self activeHand newKeyboardFocus: completer textMorph ! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:26:11'!
hideSelectorDocumentation

	selectorDocumentation ifNotNil: [ selectorDocumentation hide ].
	self stillActive ! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:26:19'!
initializeSelectorDocumentation

	selectorDocumentation := TextModelMorph withText: ''.
	selectorDocumentation textMorph
		acceptOnCR: false;
		crAction: [ self crPressedOnSelectorDocumentation ].

	selectorDocumentation
		wrapFlag: true;
		borderColor: Color black;
		borderWidth: 1;
		disableEditing;
		openInWorld
	! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:26:24'!
isShowingSelectorDocumentation

	^selectorDocumentation notNil and: [ selectorDocumentation visible ]! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:58'!
methodDocumentationSeparator

	^ Text
		string: String newLineString, '------------------------------------------------------------------------------------------------', String newLineString
		attribute: TextColor black.! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:17'!
selectorDefaultDocumentationLocation

	| relativeSelected |

	relativeSelected := (self selected - self firstVisible) min: self itemsPerPage - 1 max: 0.

	^location externalizePosition: extent x@(relativeSelected * self itemHeight + 1).

	! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:26:58'!
selectorDocumentation

	selectorDocumentation ifNil: [ self initializeSelectorDocumentation ].
	^selectorDocumentation ! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:08'!
selectorDocumentationExtent

	^`600@250`! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:22'!
selectorDocumentationText

	| selectedEntry selectorsClasses |

	selectedEntry := completer selectedEntry.
	(completer isCategoryEntry: selectedEntry) ifTrue: [ ^'' ].
	selectedEntry := selectedEntry asSymbol.
	selectorsClasses := completer selectorsClasses.

	^ selectorsClasses isEmpty
		ifTrue: [ self selectorDocumentationTextForAllImplementorsOf: selectedEntry ]
		ifFalse: [ self selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses ].
	! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:27'!
selectorDocumentationTextForAllI: aMethodsCollection

	| selectorDocumentationText implementors methodDocumentationSeparator |

	selectorDocumentationText := Text new.
	methodDocumentationSeparator := self methodDocumentationSeparator.
	implementors := aMethodsCollection asSortedCollection: [ :leftMethod :rightMethod | leftMethod methodClass classDepth <  rightMethod methodClass classDepth ].

	implementors
		do: [ :implementor | selectorDocumentationText := selectorDocumentationText append: (completer documentationOf: implementor)]
		separatedBy: [ selectorDocumentationText := selectorDocumentationText append: methodDocumentationSeparator ].

	^ selectorDocumentationText! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:34'!
selectorDocumentationTextForAllImplementorsOf: selectedEntry

	^ self selectorDocumentationTextForAllI: (Smalltalk allImplementorsOf: selectedEntry).

	! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:38'!
selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses

	| methodsToShow |

	methodsToShow := selectorsClasses
		inject: IdentitySet new
		into: [ :methods :aClass |
			(aClass lookupSelector: selectedEntry) ifNotNil: [ :method | methods add: method ].
			methods ].

	^self selectorDocumentationTextForAllI: methodsToShow ! !

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'jmv 7/6/2020 09:27:44'!
showSelectorDocumentation

	| selectorDocumentationLocation selectorDocumentationExtent |

	selectorDocumentationLocation := self selectorDefaultDocumentationLocation.
	selectorDocumentationExtent := self selectorDocumentationExtent.
	selectorDocumentationLocation := self adjust: selectorDocumentationLocation ifOutOfScreenWith: selectorDocumentationExtent xOffset: extent x yOffset: self itemHeight negated.

	self setDefaultColors.

	self selectorDocumentation
		model: (TextModel withText: self selectorDocumentationText);
		morphPosition: selectorDocumentationLocation extent: selectorDocumentationExtent;
		wrapFlag: false;
		show.

	! !

!SmalltalkCompleterMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/6/2020 09:28:43'!
delete

	selectorDocumentation ifNotNil: [
		selectorDocumentation delete.
		selectorDocumentation := nil ].

	^super delete ! !

!SmalltalkCompleterMorph methodsFor: 'actions' stamp: 'jmv 7/6/2020 10:12:40'!
resetMenu

	self hideSelectorDocumentation.
	super resetMenu! !

!SmalltalkCompleterMorph methodsFor: 'accessing' stamp: 'jmv 7/6/2020 10:15:33'!
selected: aNumber
	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [
			selected _ aNumber.
			self isShowingSelectorDocumentation ifTrue: [ self showSelectorDocumentation ]]! !

!SmalltalkCompleterMorph methodsFor: 'stepping' stamp: 'jmv 7/6/2020 10:16:05'!
stepAt: millisecondSinceLast

	self isShowingSelectorDocumentation ifTrue: [ ^self ].
	super stepAt: millisecondSinceLast! !


!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 7/6/2020 09:23:02'!
morphClass
	^AutoCompleterMorph! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 7/6/2020 10:05:22'!
handleLeftArrowKeystrokeBefore: kbEvent
	"Subclasses might do something"
	^true! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 7/6/2020 10:05:28'!
handleRightArrowKeystrokeBefore: kbEvent
	"Subclasses might do something"
	^true! !


!ClassNameCompleter methodsFor: 'menu morph' stamp: 'jmv 7/6/2020 09:23:38'!
morphClass
	^SmalltalkCompleterMorph! !


!SmalltalkCompleter methodsFor: 'menu morph' stamp: 'jmv 7/6/2020 09:23:50'!
morphClass
	^SmalltalkCompleterMorph! !

!SmalltalkCompleter methodsFor: 'keyboard' stamp: 'jmv 7/6/2020 10:06:22'!
handleLeftArrowKeystrokeBefore: kbEvent
	canShowSelectorDocumentation ifTrue: [
		menuMorph hideSelectorDocumentation ].
	^ true! !

!SmalltalkCompleter methodsFor: 'keyboard' stamp: 'jmv 7/6/2020 10:06:11'!
handleRightArrowKeystrokeBefore: kbEvent
	canShowSelectorDocumentation ifTrue: [
		menuMorph showSelectorDocumentation ].
	^ true! !


!AutoCompleterMorph methodsFor: 'accessing' stamp: 'jmv 7/6/2020 10:15:09'!
selected: aNumber
	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [
			selected _ aNumber ]! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 7/6/2020 10:12:50'!
resetMenu
	| width newExtent |

	self calculateItemsPerPage.

	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.

	width _ self calculateWidth.
	newExtent _ width + 4 @ (self itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 7/6/2020 10:16:14'!
stepAt: millisecondSinceLast

	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !


!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'jmv 7/6/2020 09:24:40'!
initializedInstance
	| completer m |
	completer _ SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	m _ completer morphClass
		completer: completer
		position: `200 @ 200`.
	completer instVarNamed: 'menuMorph' put: m.
	^m! !


!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 7/6/2020 09:24:31'!
openCompletionMenu

	| theEditor |

	theEditor _ textMorph editor.
	position _ theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1) = Character space
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: (cursorIndex >  theEditor text size ifTrue: [ cursorIndex-1 ] ifFalse: [ cursorIndex ])].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph _  self morphClass
				completer: self
				position: characterBlock bottomLeft + textMorph morphPositionInWorld ].
! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 7/6/2020 10:10:44'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."

	| shouldOpenMorph |
	shouldOpenMorph _ self shouldOpenMorph.

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [ ^ self openCompletionMenuFor: kbEvent if: shouldOpenMorph ].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.

	kbEvent isEsc ifTrue: [ self closeMenu. ^ true].
	kbEvent isBackspace ifTrue: [ shouldOpenMorph ifFalse: [ self closeMenu ]. ^ false].
	kbEvent isHome ifTrue: [ menuMorph goHome. ^ true ].
	kbEvent isEnd ifTrue: [ menuMorph goToEnd. ^ true].
	kbEvent isQuestionMark ifTrue: [ menuMorph help. ^true].
	kbEvent isArrowUp ifTrue: [ menuMorph goUp. ^ true].
	kbEvent isArrowDown ifTrue: [ menuMorph goDown. ^ true].
	kbEvent isPageUp ifTrue: [ menuMorph goPageUp. ^ true].
	kbEvent isPageDown ifTrue: [ menuMorph goPageDown. ^ true].

	kbEvent isArrowRight ifTrue: [ ^self handleRightArrowKeystrokeBefore: kbEvent ].
	kbEvent isArrowLeft ifTrue: [ ^self handleLeftArrowKeystrokeBefore: kbEvent ].

	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ].

	^false! !

!methodRemoval: SmalltalkCompleter #handleKeystrokeBefore: stamp: 'jmv 7/6/2020 10:03:09'!
SmalltalkCompleter removeSelector: #handleKeystrokeBefore:!
!methodRemoval: SmalltalkCompleter #canShowSelectorDocumentation stamp: 'jmv 7/6/2020 09:59:38'!
SmalltalkCompleter removeSelector: #canShowSelectorDocumentation!
!methodRemoval: ClassNameCompleter #canShowSelectorDocumentation stamp: 'jmv 7/6/2020 09:59:38'!
ClassNameCompleter removeSelector: #canShowSelectorDocumentation!
!methodRemoval: AutoCompleter #canShowSelectorDocumentation stamp: 'jmv 7/6/2020 09:59:36'!
AutoCompleter removeSelector: #canShowSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #isShowingSelectorDocumentation stamp: 'jmv 7/6/2020 09:26:24'!
AutoCompleterMorph removeSelector: #isShowingSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #selectorDefaultDocumentationLocation stamp: 'jmv 7/6/2020 09:27:17'!
AutoCompleterMorph removeSelector: #selectorDefaultDocumentationLocation!
!methodRemoval: AutoCompleterMorph #methodDocumentationSeparator stamp: 'jmv 7/6/2020 09:27:58'!
AutoCompleterMorph removeSelector: #methodDocumentationSeparator!
!methodRemoval: AutoCompleterMorph #initializeSelectorDocumentation stamp: 'jmv 7/6/2020 09:26:19'!
AutoCompleterMorph removeSelector: #initializeSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #selectorDocumentationExtent stamp: 'jmv 7/6/2020 09:27:08'!
AutoCompleterMorph removeSelector: #selectorDocumentationExtent!
!methodRemoval: AutoCompleterMorph #selectorDocumentationTextOf:forAll: stamp: 'jmv 7/6/2020 09:27:38'!
AutoCompleterMorph removeSelector: #selectorDocumentationTextOf:forAll:!
!methodRemoval: AutoCompleterMorph #showSelectorDocumentation stamp: 'jmv 7/6/2020 09:27:44'!
AutoCompleterMorph removeSelector: #showSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #crPressedOnSelectorDocumentation stamp: 'jmv 7/6/2020 09:28:19'!
AutoCompleterMorph removeSelector: #crPressedOnSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #selectorDocumentation stamp: 'jmv 7/6/2020 09:26:58'!
AutoCompleterMorph removeSelector: #selectorDocumentation!
!methodRemoval: AutoCompleterMorph #selectorDocumentationTextForAllI: stamp: 'jmv 7/6/2020 09:27:27'!
AutoCompleterMorph removeSelector: #selectorDocumentationTextForAllI:!
!methodRemoval: AutoCompleterMorph #selectorDocumentationTextForAllImplementorsOf: stamp: 'jmv 7/6/2020 09:27:34'!
AutoCompleterMorph removeSelector: #selectorDocumentationTextForAllImplementorsOf:!
!methodRemoval: AutoCompleterMorph #hideSelectorDocumentation stamp: 'jmv 7/6/2020 09:26:11'!
AutoCompleterMorph removeSelector: #hideSelectorDocumentation!
!methodRemoval: AutoCompleterMorph #selectorDocumentationText stamp: 'jmv 7/6/2020 09:27:22'!
AutoCompleterMorph removeSelector: #selectorDocumentationText!
!methodRemoval: AutoCompleterMorph #delete stamp: 'jmv 7/6/2020 09:28:43'!
AutoCompleterMorph removeSelector: #delete!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleterMorph reorganize!
('selector documentation' crPressedOnSelectorDocumentation hideSelectorDocumentation initializeSelectorDocumentation isShowingSelectorDocumentation methodDocumentationSeparator selectorDefaultDocumentationLocation selectorDocumentation selectorDocumentationExtent selectorDocumentationText selectorDocumentationTextForAllI: selectorDocumentationTextForAllImplementorsOf: selectorDocumentationTextOf:forAll: showSelectorDocumentation)
('submorphs-add/remove' delete)
('actions' resetMenu)
('accessing' selected:)
('stepping' stepAt:)
!


!ClassNameCompleter reorganize!
('testing' opensWithTab)
('entries' computeEntries)
('menu morph' morphClass)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:at:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary lookForBinarySelectorAfter:startingAt:with: lookForBinarySelectorAfterArrayStartStartingAt:with: lookForBinarySendRange lookForNoUnaryMessageSend selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' changePositionTo: possibleInvalidSelectors)
('menu morph' morphClass)
('keyboard' handleLeftArrowKeystrokeBefore: handleRightArrowKeystrokeBefore:)
!
