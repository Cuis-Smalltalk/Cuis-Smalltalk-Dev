'From Cuis 5.0 of 7 November 2016 [latest update: #3590] on 19 February 2019 at 2:11:35 pm'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity selectorDocumentation originalPosition showUp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser possibleInvalidSelectors selectorsClasses '
	classVariableNames: 'AccessLock EntriesLimit Selectors '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #DynamicTypingSmalltalkCompleter category: #'Tools-Autocompletion'!
SmalltalkCompleter subclass: #DynamicTypingSmalltalkCompleter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #AutoCompleterSelectorsCollector category: #'Tools-Autocompletion'!
Object subclass: #AutoCompleterSelectorsCollector
	instanceVariableNames: 'categoriesWithSelectors addedSelectorsFastSet prefix entriesToShow possibleInvalidSelectors otherClasses selectorsLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes allSource '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!TextModel methodsFor: 'shout styling' stamp: 'HAW 1/19/2019 20:23:45'!
shouldStyle: aText with: aSHTextStylerST80 
	
	^true! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:03'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:09'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:14'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:20'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !


!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 20:21:41'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName
! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 20:28:38'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 19:56:26'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	textProvider computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName 
! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 20:11:16'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	textProvider computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName
! !


!Workspace methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:46:35'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	anAutocompleter computeMessageEntriesForClassOrNil: (self classOfBindingOf: aName)! !


!TextProvider methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:29'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:35'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:44'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:30:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !


!Debugger methodsFor: 'auto complete' stamp: 'HAW 2/18/2019 18:18:51'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

	self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName
! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 20:28:21'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

	^self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:35:07'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	contextStackIndex = 0 
		ifTrue: [ anAutocompleter computeMessageEntriesForUnknowClass ]
		ifFalse: [ anAutocompleter computeMessageEntriesForClass: (self receiver instVarNamed: aName) class]! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:35:07'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ anAutocompleter computeMessageEntriesForUnknowClass ].
	
	anAutocompleter computeMessageEntriesForClass: (self debuggerMap namedTempAt: tempIndex in: context) class

	! !


!Inspector methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:35:07'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	anAutocompleter computeMessageEntriesForClass: (object instVarNamed: aName) class ! !


!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/10/2019 17:37:57'!
autoCompleterDocumentationAppendingToParameter: aParameterAppendBlock toReturn: aReturnAppendBlock
	
	"This message is sent either by the dynamic typing or live typing auto complete. If you do not have 
	live typing installed you will see one sender, do not refactor it!! - Hernan"
	
	| methodNode text |
 
	text := self receiverTextAutoCompleterDocumentation.
	
	methodNode := self methodNode.
	text := self selectorAutoCompleterDocumentationAppendingTo: text using: methodNode appendingToParameter: aParameterAppendBlock.
	text := text append: aReturnAppendBlock value.
	text := self commentAutoCompleterDocumentationAppendigTo: text using: methodNode.
			
	^text! !

!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:52:30'!
commentAutoCompleterDocumentationAppendigTo: text using: methodNode
	
	| comment |

	comment := methodNode comment.
	^ comment
		ifNil: [ text ] 
		ifNotNil: [ text append: (self commentAutoCompleterDocumentationOf: comment)].
		
	! !

!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:53:01'!
commentAutoCompleterDocumentationOf: comment
 	
	^ Text 
		string: (String streamContents: [ :stream |				
			stream
				newLine; newLine;
				nextPutAll: comment first ])
		attributes: (SHTextStylerST80 attributesFor: #comment)! !

!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:22:02'!
dynamicTypingAutoCompleterDocumentation
	
	^ self 
		autoCompleterDocumentationAppendingToParameter: [ :parameterName | '' ]
		toReturn: [ '' ]! !

!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:37:27'!
receiverTextAutoCompleterDocumentation
	
	| receiverString |
	
	receiverString := String streamContents: [ :stream |
		stream 
			print: self methodClass;
			nextPutAll: '>>' ].
	
	^Text string: receiverString attributes: (SHTextStylerST80 attributesFor: #patternKeyword).
		
	! !

!CompiledMethod methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:45:23'!
selectorAutoCompleterDocumentationAppendingTo: sourceText using: methodNode appendingToParameter: aParameterAppendBlock

	| selector text |
	
	selector := methodNode selectorNode key.
	selector isUnary 
		ifTrue: [ text := sourceText append: (Text string: selector attributes: (SHTextStylerST80 attributesFor: #patternKeyword)) ]
		ifFalse: [
			text := sourceText.
			selector keywords 
				with: methodNode argumentNames 
				do: [ :keyword :argumentName |
					text := text append: (Text string: keyword attributes: (SHTextStylerST80 attributesFor: #patternKeyword)).
					text := text append: (Text string: ' ', argumentName, ' ' attributes: (SHTextStylerST80 attributesFor: #methodArg)).
					text := text append: (aParameterAppendBlock value: argumentName) ] 
				separatedBy: [ text := text append: String newLineString, String tab ]].

	^ text
! !


!TextEditor methodsFor: 'as yet unclassified' stamp: 'HAW 2/12/2019 17:14:49'!
characterBlockForIndex: anIndex

	^ textComposition characterBlockForIndex: anIndex ! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/12/2019 18:22:48'!
resetMenu
	
	| width newExtent adjustedY |
	
	self hideSelectorDocumentation.
	firstVisible _ 1.
	self selected: 1.

	width _ self calculateWidth.
	newExtent _ width + 4 @ (self visibleItemsCount * self itemHeight+2).
	
	adjustedY := (self isYOutOfScreen: originalPosition with: newExtent) 
		ifTrue: [ originalPosition y - newExtent y - self itemHeight ]
		ifFalse: [ originalPosition y ].

	self morphPosition: originalPosition x @ adjustedY extent: newExtent.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/11/2019 20:13:14'!
adjust: aLocation ifOutOfScreenWith: anExtent xOffset: xOffset yOffset: yOffset  
	
	| adjustedLocationX adjustedLocationY |
	
	adjustedLocationX := (self isXOutOfScreen: aLocation with: anExtent)
		ifTrue: [ aLocation x - anExtent x - xOffset ]
		ifFalse: [ aLocation x ].
	
	adjustedLocationX < 0 ifTrue: [ adjustedLocationX := aLocation x ].
		
	adjustedLocationY := (self isYOutOfScreen: aLocation with: anExtent) 
		ifTrue: [ aLocation y - anExtent y - yOffset ]
		ifFalse: [ aLocation y ].
		
	^adjustedLocationX @ adjustedLocationY 
	! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/11/2019 18:22:59'!
calculateWidth

	| width font |
	
	width _ 120.
	font _ self class listFont.

	1
		to: completer entryCount
		do: [ :index | width _ width max: (font widthOfString: (completer entries at: index) asString)].
	
	completer entryCount > self class itemsPerPage ifTrue: [ width _ width + ScrollBar scrollbarThickness ].
	
	^ width ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/14/2019 17:10:34'!
colorOf: entry

	^(completer isPossibleInvalidEntry: entry) 
		ifTrue: [ `Color blue` ] 
		ifFalse: [ Theme current text ]
	! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:23:08'!
drawContainingRectangle: aCanvas
	
	aCanvas frameAndFillRectangle: (`0@0` extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:55:46'!
drawDownArrowOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
		at: self downButtonPosition.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:46:59'!
drawItemOf: index on: aCanvas width: width top: itemTop

	| rectangle entry |
	
	rectangle _ 1@itemTop extent: width@self itemHeight.
	index = self selected ifTrue: [ aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].

	entry _ completer entries at: index.
	aCanvas
		drawString: entry asString
		at: rectangle topLeft
		font: self class listFont
		color: (self colorOf: entry).
	! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:51:54'!
drawItemsOn: aCanvas width: width

	| itemTop |
	
	itemTop _ 1.
	self firstVisible
		to: self lastVisible
		do: [ :index | 
			self drawItemOf: index on: aCanvas width: width top: itemTop.
			itemTop _ itemTop + self itemHeight ].! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/11/2019 00:03:34'!
drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height _ extent y - (2 * scrollbarThickness).
	top _ (1.0 * self firstVisible-1 / completer entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom _ (1.0 * self lastVisible / completer entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray` ! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/11/2019 00:02:04'!
drawScrollBarOn: aCanvas
	
	| scrollbarThickness width |

	width _ extent x-2.
	completer entryCount > self class itemsPerPage  ifTrue: [
		scrollbarThickness _ ScrollBar scrollbarThickness.
		width _ width - scrollbarThickness.
		
		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].
		
	^width 
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:57:30'!
drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		frameRectangle: (extent x - scrollbarThickness@0 extent: scrollbarThickness @ extent y)
		borderWidth: 1
		color: borderColor.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:54:49'!
drawUpArrowOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
		at: self upButtonPosition.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/14/2019 18:26:25'!
setDefaultColors

	self color: self defaultColor.
	self borderColor: self defaultBorderColor ! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:15:51'!
crPressedOnSelectorDocumentation
	
	self hideSelectorDocumentation. 
	self activeHand newKeyboardFocus: completer textMorph ! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:13:41'!
hideSelectorDocumentation

	selectorDocumentation ifNotNil: [ selectorDocumentation hide ].
	self stillActive ! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:15:38'!
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

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:14:01'!
isShowingSelectorDocumentation

	^selectorDocumentation notNil and: [ selectorDocumentation visible ]! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:40:55'!
methodDocumentationSeparator
	
	^ Text 
		string: String newLineString, '------------------------------------------------------------------------------------------------', String newLineString 
		attribute: TextColor black.! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/11/2019 16:27:17'!
selectorDefaultDocumentationLocation

	| relativeSelected |
	
	relativeSelected := (self selected - self firstVisible) min: self class itemsPerPage - 1 max: 0.
	 
	^location externalizePosition: extent x@(relativeSelected * self itemHeight + 1).

	! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 17:12:11'!
selectorDocumentation

	selectorDocumentation ifNil: [ self initializeSelectorDocumentation ].
	^selectorDocumentation ! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/11/2019 16:18:05'!
selectorDocumentationExtent

	^`600@250`! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 18:11:20'!
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

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/18/2019 18:31:56'!
selectorDocumentationTextForAllI: aMethodsCollection

	| selectorDocumentationText implementors methodDocumentationSeparator |
	
	selectorDocumentationText := Text new.
	methodDocumentationSeparator := self methodDocumentationSeparator.		
	implementors := aMethodsCollection asSortedCollection: [ :leftMethod :rightMethod | leftMethod methodClass classDepth <  rightMethod methodClass classDepth ].
	
	implementors 
		do: [ :implementor | selectorDocumentationText := selectorDocumentationText append: (completer documentationOf: implementor)]
		separatedBy: [ selectorDocumentationText := selectorDocumentationText append: methodDocumentationSeparator ].
		
	^ selectorDocumentationText! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/18/2019 18:16:04'!
selectorDocumentationTextForAllImplementorsOf: selectedEntry

	^ self selectorDocumentationTextForAllI: (Smalltalk allImplementorsOf: selectedEntry).

	! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/18/2019 18:25:34'!
selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses
	
	| methodsToShow |
	
	methodsToShow := selectorsClasses 
		inject: IdentitySet new
		into: [ :methods :aClass | 
			(aClass lookupSelector: selectedEntry) ifNotNil: [ :method | methods add: method ].
			methods ].
			
	^self selectorDocumentationTextForAllI: methodsToShow ! !

!AutoCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 2/14/2019 18:27:16'!
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

!AutoCompleterMorph methodsFor: 'submorphs-add/remove' stamp: 'HAW 2/8/2019 17:11:23'!
delete

	selectorDocumentation ifNotNil: [ 
		selectorDocumentation delete.
		selectorDocumentation := nil ].
	
	^super delete ! !

!AutoCompleterMorph methodsFor: 'testing' stamp: 'HAW 2/11/2019 19:45:39'!
isXOutOfScreen: aLocation with: anExtent
	
	^aLocation x + anExtent x > DisplayScreen actualScreenSize x! !

!AutoCompleterMorph methodsFor: 'testing' stamp: 'HAW 2/11/2019 19:43:44'!
isYOutOfScreen: aLocation with: anExtent
	
	^aLocation y + anExtent y > DisplayScreen actualScreenSize y! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'HAW 1/21/2019 00:53:00'!
crAction: aBlock
	"Sets the action to perform when user presses <Return> key"
	^self setProperty: #crAction toValue: aBlock ! !


!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:21:43'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:21:54'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:22:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 12:22:05'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

	anAutocompleter computeMessageEntriesForUnknowClass! !


!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 1/19/2019 07:07:02'!
isArrowLeft
	
	^keyValue = 28! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 1/18/2019 19:48:13'!
isArrowRight
	
	^keyValue = 29 ! !


!MethodReference methodsFor: 'auto complete' stamp: 'HAW 2/8/2019 18:28:00'!
dynamicTypingAutoCompleterDocumentation

	^self compiledMethod dynamicTypingAutoCompleterDocumentation ! !

!MethodReference methodsFor: 'auto complete' stamp: 'HAW 2/18/2019 18:31:40'!
methodClass

	^self actualClass ! !


!AutoCompleter methodsFor: 'accessing' stamp: 'HAW 1/21/2019 01:02:40'!
textMorph

	^textMorph! !

!AutoCompleter methodsFor: 'entries' stamp: 'HAW 1/19/2019 23:55:50'!
selectedEntry

	^self entries at: menuMorph selected! !

!AutoCompleter methodsFor: 'entries' stamp: 'HAW 2/6/2019 16:22:43'!
selectedEntryFormatted

	^(self entries at: menuMorph selected), ' '! !

!AutoCompleter methodsFor: 'testing' stamp: 'HAW 2/8/2019 17:23:31'!
canSelect: anEntry

	^true! !

!AutoCompleter methodsFor: 'testing' stamp: 'HAW 2/10/2019 21:35:19'!
isPossibleInvalidEntry: anEntry

	^false! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 1/19/2019 23:55:29'!
selectedEntryFormatted

	^(self entries at: menuMorph selected) separateKeywords! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 2/8/2019 17:09:54'!
selectorsClasses

	^selectorsClasses ! !

!SmalltalkCompleter methodsFor: 'testing' stamp: 'HAW 2/8/2019 17:29:12'!
canSelect: anEntry

	^ (self isCategoryEntry: anEntry) not! !

!SmalltalkCompleter methodsFor: 'testing' stamp: 'HAW 2/8/2019 17:29:29'!
isCategoryEntry: anEntry

	^anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader! !

!SmalltalkCompleter methodsFor: 'testing' stamp: 'HAW 2/10/2019 21:35:32'!
isPossibleInvalidEntry: anEntry

	^possibleInvalidSelectors includes: anEntry ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 16:07:20'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange 
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 17:09:54'!
computeMessageEntriesForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addSelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 12:43:44'!
computeMessageEntriesForClassOrNil: aClassOrNil

	aClassOrNil 
		ifNil: [ self computeMessageEntriesForUnknowClass ]
		ifNotNil: [ self computeMessageEntriesForClass: aClassOrNil ].
	
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 16:10:27'!
computeMessageEntriesOfCascadeReceiverAt: aRange 
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/11/2019 00:21:12'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange 

	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'HAW 2/15/2019 15:40:47'!
computeMessageEntriesForUnknowClass

	| selectorsToShow |
	
	selectorsClasses _ #().
	possibleInvalidSelectors _ IdentitySet new.
	selectorsToShow _ OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ possibleInvalidSelectors add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow _  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries _ selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	entries _ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	
! !

!SmalltalkCompleter methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 18:12:00'!
documentationOf: aMethod

	self subclassResponsibility ! !


!SmalltalkCompleter class methodsFor: 'instance creation' stamp: 'HAW 2/10/2019 17:44:10'!
withModel: aStringHolder

	"Looks it its subclasses the right autocompleter depending on the Type System. If you do not have
	Live Typing installed you will see only one subclass but if Live Typing is intalled LiveTypeingSmalltalkCompleter
	subclass is added - Hernan"
	
	| smalltalkCompleterClass |
	
	smalltalkCompleterClass := self subclasses detect: [ :aSubclass | aSubclass isForCurrentTypeSystem ].
	
	^smalltalkCompleterClass new setModel: aStringHolder! !

!SmalltalkCompleter class methodsFor: 'testing' stamp: 'HAW 2/8/2019 15:57:29'!
isForCurrentTypeSystem

	self subclassResponsibility ! !

!SmalltalkCompleter class methodsFor: 'accessing' stamp: 'HAW 2/15/2019 15:27:22'!
entriesLimit
	
	^EntriesLimit ! !


!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 16:09:35'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange 
	
	self computeMessageEntriesForUnknowClass 
	! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 16:10:08'!
computeMessageEntriesOfCascadeReceiverAt: aRange 
	
	self computeMessageEntriesForUnknowClass
	
! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/11/2019 00:21:13'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange 

	self computeMessageEntriesForUnknowClass 
	
! !

!DynamicTypingSmalltalkCompleter methodsFor: 'selector documentation' stamp: 'HAW 2/8/2019 18:13:08'!
documentationOf: aMethod

	^aMethod dynamicTypingAutoCompleterDocumentation! !


!DynamicTypingSmalltalkCompleter class methodsFor: 'testing' stamp: 'HAW 2/8/2019 18:35:09'!
isForCurrentTypeSystem

	^ Smalltalk isLiveTypingInstalled not! !


!AutoCompleterSelectorsCollector methodsFor: 'initialization' stamp: 'HAW 2/15/2019 15:32:49'!
initializeFor: aPrefix withSelectorsLimitedTo: aLimit 

	prefix := aPrefix.
	categoriesWithSelectors := OrderedDictionary new.
	addedSelectorsFastSet := IdentitySet new.
	possibleInvalidSelectors := IdentitySet new.
	selectorsLimit := aLimit ! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 2/15/2019 15:03:00'!
addSelectorsOf: aClass

	self addSelectorsOf: aClass upTo: nil! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 2/15/2019 15:04:08'!
addSelectorsOf: aClass upTo: aSuperclassToExclude

	self addSelectorsOfAll: (Array with: aClass) upTo: aSuperclassToExclude ! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 2/15/2019 15:04:18'!
addSelectorsOfAll: classes upTo: aSuperclass

	classes do: [ :aClass | 
		otherClasses := classes copyWithout: aClass.
		self addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclass ]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 2/10/2019 21:54:59'!
addedSelectors: selectors

	addedSelectorsFastSet addAll: selectors ! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 2/10/2019 21:32:17'!
possibleInvalidSelectors
	
	^possibleInvalidSelectors! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 2/15/2019 15:34:27'!
addCategoriesAndSelectorsOf: aClass

	| classOrganization |
	
	classOrganization := aClass organization.
	
	classOrganization categories do: [ :aCategory | | categorySelectors selectedSelectors | 
		self hasReachSelectorsLimit ifTrue: [ ^ self ].
		selectedSelectors := self prefixedSelectorsOf: aCategory in: classOrganization.
		selectedSelectors isEmpty ifFalse: [ 
			categorySelectors := categoriesWithSelectors at: aCategory ifAbsentPut: [ OrderedCollection new ].
			categorySelectors addAll: selectedSelectors.
			self addedSelectors: selectedSelectors.
			self addToPossibleInvalidIfCorrespond: selectedSelectors ]]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 2/15/2019 15:35:41'!
addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclassToExclude

	| currentClass |
	
	currentClass := aClass.
	
	[ currentClass ~= aSuperclassToExclude and: [ currentClass notNil ] and: [ self hasReachSelectorsLimit not ] ] whileTrue: [ 
		self addCategoriesAndSelectorsOf: currentClass.
		currentClass := currentClass superclass].
	 
! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 2/15/2019 15:19:35'!
addToPossibleInvalidIfCorrespond: selectedSelectors

	selectedSelectors do: [ :aSelector |
		(otherClasses allSatisfy: [ :otherClass | otherClass canUnderstand: aSelector ]) ifFalse: [ possibleInvalidSelectors add: aSelector ]]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 2/7/2019 18:40:03'!
prefixedSelectorsOf: aCategory in: aClassOrganization
	
	^ (aClassOrganization listAtCategoryNamed: aCategory) 
		select: [ :aSelector |  (aSelector beginsWith: prefix) and: [ (addedSelectorsFastSet includes: aSelector) not ] ].
! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show' stamp: 'HAW 2/8/2019 16:25:25'!
entriesToShow
	
	self hasCollectedOnlyOneSelector
		ifTrue: [ entriesToShow := categoriesWithSelectors anyOne ]
		ifFalse: [ 
			entriesToShow := OrderedCollection new.
			self 
				addPrioritizedCategories;
				addLeftCategories ].
						  			
	^entriesToShow 
	
	   ! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/18/2019 18:26:36'!
addCategory: aCategory with: selectors 

	entriesToShow 
		add: (self categoryEntryFor: aCategory); 
		addAll: selectors
	   ! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/7/2019 18:26:18'!
addLeftCategories
	
	categoriesWithSelectors keysAndValuesDo: [ :aCategory :selectors | self addCategory: aCategory with: selectors ].
	! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/7/2019 18:27:10'!
addPrioritizedCategories

	self prioritizedCategories do: [ :aCategory | self addPrioritizedCategory: aCategory ]! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/7/2019 18:27:10'!
addPrioritizedCategory: aCategory

	categoriesWithSelectors 
		at: aCategory 
		ifPresent: [ :selectors | 
			self addCategory: aCategory with: selectors.
			categoriesWithSelectors removeKey: aCategory ].
! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/8/2019 17:24:26'!
categoryEntryFor: aCategory
	
	^self class categoryEntryHeader, aCategory ! !

!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 2/7/2019 18:19:23'!
prioritizedCategories 
	
	^#('instance creation')! !

!AutoCompleterSelectorsCollector methodsFor: 'testing' stamp: 'HAW 2/8/2019 16:26:00'!
hasCollectedOnlyOneSelector
	
	^ categoriesWithSelectors size = 1 and: [ categoriesWithSelectors anyOne size = 1 ]! !

!AutoCompleterSelectorsCollector methodsFor: 'testing' stamp: 'HAW 2/15/2019 15:34:27'!
hasReachSelectorsLimit
	
	^addedSelectorsFastSet size >= selectorsLimit! !


!AutoCompleterSelectorsCollector class methodsFor: 'instance creation' stamp: 'HAW 2/15/2019 15:33:47'!
for: aPrefix

	^self for: aPrefix withSelectorsLimitedTo: SmalltalkCompleter entriesLimit! !

!AutoCompleterSelectorsCollector class methodsFor: 'instance creation' stamp: 'HAW 2/15/2019 15:33:36'!
for: aPrefix withSelectorsLimitedTo: aLimit

	^self new initializeFor: aPrefix withSelectorsLimitedTo: aLimit! !

!AutoCompleterSelectorsCollector class methodsFor: 'category entry' stamp: 'HAW 2/8/2019 17:24:43'!
categoryEntryHeader

	^ '-- '! !


!SHParserST80 methodsFor: 'accessing' stamp: 'HAW 2/10/2019 18:35:50'!
allSource

	^allSource! !

!SHParserST80 methodsFor: 'accessing' stamp: 'HAW 2/10/2019 18:36:28'!
allSource: aSourceCode

	allSource _ aSourceCode! !


!TextModel methodsFor: 'auto complete' stamp: 'HAW 1/20/2019 18:47:22'!
classOfThisContext

	^ MethodContext ! !


!TextProvider methodsFor: 'auto complete' stamp: 'HAW 1/20/2019 18:47:47'!
classOfThisContext

	^ MethodContext ! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 2/19/2019 14:09:50'!
with: otherCollection do: twoArgBlock separatedBy: separatorBlock

	| beforeFirst | 
	
	beforeFirst := true.
	self with: otherCollection do: [ :selfElement :otherCollectionElement |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		twoArgBlock value: selfElement value: otherCollectionElement ].
		
	
! !


!AutoCompleterMorph methodsFor: 'accessing' stamp: 'HAW 1/19/2019 09:05:56'!
selected
	"Answer the value of selected"
	selected ifNil: [ self selected: self firstVisible ].
	^ selected! !

!AutoCompleterMorph methodsFor: 'accessing' stamp: 'HAW 2/8/2019 17:14:24'!
selected: aNumber 

	"Set the value of selected"

	((aNumber between: 1 and: completer entryCount) and: [ aNumber ~= selected ])
		ifTrue: [ 
			selected _ aNumber.
			self isShowingSelectorDocumentation ifTrue: [ self showSelectorDocumentation ]]! !

!AutoCompleterMorph methodsFor: 'activity tracking' stamp: 'HAW 2/14/2019 18:26:11'!
updateColor

	| remaining alpha |

	remaining := (self timeout - self timeOfLastActivity).
	remaining < 1000 
		ifTrue: [
			alpha _ remaining / 1000.0.
			self color: (self color alpha: alpha).
			self borderColor: (borderColor alpha: alpha) ]
		ifFalse: [ self setDefaultColors ]
		
			! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:52:19'!
downButtonPosition

	^ `0@0` + (extent - ScrollBar scrollbarThickness)! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:39:30'!
drawOn: aCanvas
	
	| width |
	
	self drawContainingRectangle: aCanvas.
	width _ self drawScrollBarOn: aCanvas.
	self drawItemsOn: aCanvas width: width 
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'HAW 2/10/2019 23:52:14'!
upButtonPosition

	^extent x - ScrollBar scrollbarThickness@0! !

!AutoCompleterMorph methodsFor: 'events' stamp: 'HAW 1/19/2019 06:12:38'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

!AutoCompleterMorph methodsFor: 'initialization' stamp: 'HAW 2/12/2019 18:22:48'!
setCompleter: anAutoCompleter position: aPoint 

	completer _ anAutoCompleter.
	
	originalPosition := aPoint.
	
	self resetMenu.
	self openInWorld! !

!AutoCompleterMorph methodsFor: 'stepping' stamp: 'HAW 2/8/2019 17:14:01'!
stepAt: millisecondSinceLast

	self isShowingSelectorDocumentation ifTrue: [ ^self ].
	
	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !


!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'HAW 2/12/2019 17:56:00'!
completer: anAutoCompleter position: aPoint 

	| newObject |

	newObject _ self new.
	newObject setCompleter: anAutoCompleter position: aPoint.

	^ newObject! !


!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 2/6/2019 20:30:26'!
classOfThisContext

	^ MethodContext! !


!AutoCompleter methodsFor: 'menu morph' stamp: 'HAW 2/16/2019 08:33:04'!
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
			menuMorph _  AutoCompleterMorph 
				completer: self
				position: characterBlock bottomLeft + textMorph morphPositionInWorld ].
! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'HAW 2/12/2019 18:22:55'!
handleKeystrokeAfter: aKeyboardEvent
	| newPos |
	menuMorph ifNil: [^self].
	newPos _ textMorph editor startIndex-1.
	newPos = position ifTrue: [^self].
	newPos < position
		ifTrue: [
			prefix _ prefix copyFrom: 1 to: prefix size+(newPos-position).
			position _ newPos ]
		ifFalse: [
			position _ position + 1.
			prefix _ prefix copyWith: (model actualContents at: position) ].
	self computeEntries.
	entries notEmpty
		ifTrue: [ menuMorph resetMenu ]
		ifFalse: [ self closeMenu ]! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'HAW 2/8/2019 17:14:24'!
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
	kbEvent isQuesitonMark ifTrue: [ menuMorph help. ^true].
	kbEvent isArrowUp ifTrue: [ menuMorph goUp. ^ true].
	kbEvent isArrowDown ifTrue: [ menuMorph goDown. ^ true].
	kbEvent isPageUp ifTrue: [ menuMorph goPageUp. ^ true].
	kbEvent isPageDown ifTrue: [ menuMorph goPageDown. ^ true].
	kbEvent isArrowRight ifTrue: [ menuMorph showSelectorDocumentation. ^ true ].
	kbEvent isArrowLeft ifTrue: [ menuMorph hideSelectorDocumentation. ^ true ].	
	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ]. 
	 
	^false! !

!AutoCompleter methodsFor: 'entries' stamp: 'HAW 2/8/2019 17:27:05'!
insertSelected
	
	| entry editor selEnd str |
	
	textMorph ifNil: [ ^false ].
	
	entry _ self selectedEntryFormatted.
	(self canSelect: entry) ifFalse: [ ^true ].
	
	editor _ textMorph editor.
	str _ model actualContents string.
	selEnd _ position.
	[selEnd < str size and: [ (str at: selEnd+1) tokenish ]] whileTrue: [ selEnd _ selEnd + 1 ].
	(selEnd < str size and: [ (str at: selEnd+1) = $ ]) ifTrue: [ selEnd _ selEnd + 1].
	editor selectFrom: position-prefix size+1 to: selEnd.
	editor
		replaceSelectionWith: entry;
		deselectAndPlaceCursorAt: position - prefix size + 1 + (self newCursorPosition: entry).
	textMorph redrawNeeded.
	menuMorph delete.
	menuMorph _ nil.
	
	^ true! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 2/10/2019 21:38:01'!
computeEntries

	| allSource contextClass specificModel last3Ranges range |

	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	last3Ranges _ self parse: allSource in: contextClass and: specificModel.
	range _ last3Ranges third ifNil: [ ^entries _ #() ].
	possibleInvalidSelectors _ #().
	
	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource using: last3Ranges at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 2/10/2019 18:35:24'!
parse: allSource in: contextClass and: specificModel
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position);
		allSource: allSource.
	parser parse: (specificModel is: #CodeProvider).
	
	^ parser last3Ranges.
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/18/2019 18:13:38'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id rangeType |

	id _ allSource copyFrom: range start to: range end.
	rangeType _ range rangeType.

	rangeType == #globalVar  			ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self  					ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	rangeType == #super  				ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true  					ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false  				ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil  					ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character  			ifTrue: [ ^self computeMessageEntriesForClass: id first class ].
	rangeType == #number  				ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #string  				ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #symbol  				ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #stringSymbol  		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #instVar  				ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id ].
	rangeType == #methodArg  			ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #tempVar  				ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #blockArg  			ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id ].
	rangeType == #blockTempVar  		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id ].
	rangeType == #workspaceVar  		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) ].
	rangeType == #thisContext  		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) ]. 
	rangeType == #classVar  			ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #poolConstant  		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #blockEnd  			ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure ].
	rangeType == #arrayEnd  			ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #rightBrace  			ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #unary  				ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id at: range ].
	rangeType == #rightParenthesis  	ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturnAt: range ].
	rangeType == #cascadeSeparator  	ifTrue: [ ^self computeMessageEntriesOfCascadeReceiverAt: range ]. 

	self computeMessageEntriesForUnknowClass 
	
	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/8/2019 12:18:59'!
computeMessageEntriesWhenSendinMessageFor: allSource using: last3Ranges in: contextClass and: specificModel 
	
	| prevPrevRange prevRange |
	
	"If previous range is a constant or a well known identifier, we might filter messages"
	prevRange _ last3Ranges second.
	prevPrevRange _ last3Ranges first.
	
	"3 if -> ifNil: but not ifTrue:
	3=4 -> ifNil: or ifTrue:"
	^ (self canComputeMessageEntriesFor: prevRange and: prevPrevRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: prevRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'HAW 2/15/2019 15:36:56'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	
	EntriesLimit _ 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		allImplemented _ Smalltalk allImplementedMessages.
		Selectors _ Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method allImplemented: allImplemented]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue _ SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue _ SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i |
				Selectors at: (Smalltalk specialSelectorAt: i) put: maxSortValue ]]! !

!methodRemoval: AutoCompleterSelectorsCollector class #for:limitingNumberOfSelectorsTo:!
AutoCompleterSelectorsCollector class removeSelector: #for:limitingNumberOfSelectorsTo:!
!methodRemoval: AutoCompleterSelectorsCollector class #for:withALimitOf:!
AutoCompleterSelectorsCollector class removeSelector: #for:withALimitOf:!
!methodRemoval: AutoCompleterSelectorsCollector #addEntriesToShowTo:!
AutoCompleterSelectorsCollector removeSelector: #addEntriesToShowTo:!
!methodRemoval: AutoCompleterSelectorsCollector #addPrioritzedCategory:!
AutoCompleterSelectorsCollector removeSelector: #addPrioritzedCategory:!
!methodRemoval: AutoCompleterSelectorsCollector #addPrioritzedCategory:with:!
AutoCompleterSelectorsCollector removeSelector: #addPrioritzedCategory:with:!
!methodRemoval: AutoCompleterSelectorsCollector #hasReachNumberOfSelectorsLimit!
AutoCompleterSelectorsCollector removeSelector: #hasReachNumberOfSelectorsLimit!
!methodRemoval: AutoCompleterSelectorsCollector #initialize!
AutoCompleterSelectorsCollector removeSelector: #initialize!
!methodRemoval: AutoCompleterSelectorsCollector #initializeFor:!
AutoCompleterSelectorsCollector removeSelector: #initializeFor:!
!methodRemoval: AutoCompleterSelectorsCollector #initializeFor:limitingNumberOfSelectorsTo:!
AutoCompleterSelectorsCollector removeSelector: #initializeFor:limitingNumberOfSelectorsTo:!
!methodRemoval: AutoCompleterSelectorsCollector #markAddedSelectorsAsPossibleInvalid!
AutoCompleterSelectorsCollector removeSelector: #markAddedSelectorsAsPossibleInvalid!
!methodRemoval: AutoCompleterSelectorsCollector #priorizeCategories!
AutoCompleterSelectorsCollector removeSelector: #priorizeCategories!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfOfEnclosedExpressionReturnAt:!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfOfEnclosedExpressionReturnAt:!
!methodRemoval: SmalltalkCompleter #computeMessageEntries:!
SmalltalkCompleter removeSelector: #computeMessageEntries:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesForSelectors:!
SmalltalkCompleter removeSelector: #computeMessageEntriesForSelectors:!
!methodRemoval: SmalltalkCompleter #returnTypeOfEnclosedExpressionAt:!
SmalltalkCompleter removeSelector: #returnTypeOfEnclosedExpressionAt:!
!methodRemoval: SmalltalkCompleter #returnTypeOfUnaryMessage:at:!
SmalltalkCompleter removeSelector: #returnTypeOfUnaryMessage:at:!
!methodRemoval: SmalltalkCompleter #selectedEntry!
SmalltalkCompleter removeSelector: #selectedEntry!
!methodRemoval: SmalltalkCompleter #typeOfCascadeReceiverAt:!
SmalltalkCompleter removeSelector: #typeOfCascadeReceiverAt:!
!methodMoveToSomePackage: SmalltalkCompleter #withMethodNodeOf:do:ifError:!
SmalltalkCompleter removeSelectorIfInBaseSystem: #withMethodNodeOf:do:ifError:!
!methodMoveToSomePackage: SmalltalkCompleter #withMethodNodeOfAllSourceCodeDo:ifError:!
SmalltalkCompleter removeSelectorIfInBaseSystem: #withMethodNodeOfAllSourceCodeDo:ifError:!
!methodRemoval: AutoCompleter #keyStroke:!
AutoCompleter removeSelector: #keyStroke:!
!methodRemoval: ClassNameRequestMorph #classOfBlockArgNamed:!
ClassNameRequestMorph removeSelector: #classOfBlockArgNamed:!
!methodRemoval: ClassNameRequestMorph #classOfBlockTempVarNamed:!
ClassNameRequestMorph removeSelector: #classOfBlockTempVarNamed:!
!methodRemoval: ClassNameRequestMorph #classOfInstVarNamed:!
ClassNameRequestMorph removeSelector: #classOfInstVarNamed:!
!methodRemoval: ClassNameRequestMorph #classOfTempVarNamed:!
ClassNameRequestMorph removeSelector: #classOfTempVarNamed:!
!methodRemoval: AutoCompleterMorph #adjust:ifOutOfScreenWith:!
AutoCompleterMorph removeSelector: #adjust:ifOutOfScreenWith:!
!methodRemoval: AutoCompleterMorph #adjust:ifOutOfScreenWith:xOffset:!
AutoCompleterMorph removeSelector: #adjust:ifOutOfScreenWith:xOffset:!
!methodRemoval: AutoCompleterMorph #adjustPositionIfOutOfScreen!
AutoCompleterMorph removeSelector: #adjustPositionIfOutOfScreen!
!methodRemoval: AutoCompleterMorph #adjustPositionIfOutOfScreen:!
AutoCompleterMorph removeSelector: #adjustPositionIfOutOfScreen:!
!methodRemoval: AutoCompleterMorph #adjustedIfOutOfScreen:!
AutoCompleterMorph removeSelector: #adjustedIfOutOfScreen:!
!methodRemoval: AutoCompleterMorph #drawItemOn:width:!
AutoCompleterMorph removeSelector: #drawItemOn:width:!
!methodRemoval: AutoCompleterMorph #isOutOfScreen:with:!
AutoCompleterMorph removeSelector: #isOutOfScreen:with:!
!methodRemoval: AutoCompleterMorph #privateExtent:!
AutoCompleterMorph removeSelector: #privateExtent:!
!methodRemoval: AutoCompleterMorph #resetMenu:!
AutoCompleterMorph removeSelector: #resetMenu:!
!methodRemoval: AutoCompleterMorph #selectorDocumentationLocation!
AutoCompleterMorph removeSelector: #selectorDocumentationLocation!
!methodRemoval: AutoCompleterMorph #testxxx!
AutoCompleterMorph removeSelector: #testxxx!
!methodRemoval: AutoCompleterMorph #testxxxxx!
AutoCompleterMorph removeSelector: #testxxxxx!
!methodRemoval: Inspector #classOfInstVarNamed:!
Inspector removeSelector: #classOfInstVarNamed:!
!methodRemoval: Debugger #classOfBlockArgNamed:!
Debugger removeSelector: #classOfBlockArgNamed:!
!methodRemoval: Debugger #classOfBlockTempVarNamed:!
Debugger removeSelector: #classOfBlockTempVarNamed:!
!methodRemoval: Debugger #classOfInstVarNamed:!
Debugger removeSelector: #classOfInstVarNamed:!
!methodRemoval: Debugger #classOfTempVarNamed:!
Debugger removeSelector: #classOfTempVarNamed:!
!methodRemoval: TextProvider #classOfBlockArgNamed:!
TextProvider removeSelector: #classOfBlockArgNamed:!
!methodRemoval: TextProvider #classOfBlockTempVarNamed:!
TextProvider removeSelector: #classOfBlockTempVarNamed:!
!methodRemoval: TextProvider #classOfInstVarNamed:!
TextProvider removeSelector: #classOfInstVarNamed:!
!methodRemoval: TextProvider #classOfTempVarNamed:!
TextProvider removeSelector: #classOfTempVarNamed:!
!methodRemoval: Workspace #classOfTempVarNamed:!
Workspace removeSelector: #classOfTempVarNamed:!
!methodRemoval: PluggableTextModel #classOfBlockArgNamed:!
PluggableTextModel removeSelector: #classOfBlockArgNamed:!
!methodRemoval: PluggableTextModel #classOfBlockTempVarNamed:!
PluggableTextModel removeSelector: #classOfBlockTempVarNamed:!
!methodRemoval: PluggableTextModel #classOfInstVarNamed:!
PluggableTextModel removeSelector: #classOfInstVarNamed:!
!methodRemoval: PluggableTextModel #classOfTempVarNamed:!
PluggableTextModel removeSelector: #classOfTempVarNamed:!
!methodRemoval: TextModel #classOfBlockArgNamed:!
TextModel removeSelector: #classOfBlockArgNamed:!
!methodRemoval: TextModel #classOfBlockTempVarNamed:!
TextModel removeSelector: #classOfBlockTempVarNamed:!
!methodRemoval: TextModel #classOfInstVarNamed:!
TextModel removeSelector: #classOfInstVarNamed:!
!methodRemoval: TextModel #classOfTempVarNamed:!
TextModel removeSelector: #classOfTempVarNamed:!

!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('misc' getSelection refetch)
('testing' canBindVariables isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom:)
('shout support' privateAddBoldAt: privateRemoveBoldAt:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: commandForReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttributes:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
('copying' postCopy)
('user interface support' autoCompleterClass editorClass formatAndStyleIfNeededWith: textStylerClass)
('shout styling' shouldStyle:with:)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
!


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfBindingOf: classOfWorkspaceVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('testing' canBindVariables is:)
('accessing' methodNodeOf:ifErrorsParsing:)
('*LiveTyping' selectedClassOrMetaClass)
!


!Browser reorganize!
('accessing' acceptedStringOrText contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('*Refactoring' insertSuperclass isEditingMethod removeClass renameClass safelyRemoveClass)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
('*LiveTyping' classDefinitionText computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed: contents:notifying: selectedMessage)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('testing' is:)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('*Refactoring' isEditingMethod)
('*Cuis-University-Model' askForSuperclassOf:toImplement:ifCancel:)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is:)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth fillWith: first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' asByteArray asColorArray asNewArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream @)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withNextDo: withPreviousDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -= derivative incrementFraction integral)
('assertions' assertIsInBounds:)
('*Refactoring' intervalOfSubCollection:)
('as yet unclassified' allButLastDo:)
!


!CompiledMethod reorganize!
('accessing' bytecodeSetName clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' hasNewPropertyFormat hasReportableSlip is: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isTestMethod usesClosureBytecodes)
('printing' abstractSymbolic decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreambleFrom:at: getSource getSourceFor:in: getSourceFromFile linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceClass sourceFileStream sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile methodNode parserClass)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('*Refactoring' classAndSelector hasArgumentOrTemporary: printClassAndSelectorOn: printOn: readsInstanceVariable: referencesParameterAt: sendsOrRefersTo: sourceCode writesInstanceVariable:)
('*DenotativeObject' printTheNonMetaClassAndSelectorOn:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('*LiveTyping' caretAutoCompleterDocumentation doesOnlySurelySend: initializeTypeInformation isQuickReturnSelf liveTypingAutoCompleterDocumentation methodVariablesTypes parameterTypeAutoCompleterDocumentationOf:using: printTypeInfoOn: returnRawTypes returnTypeAutoCompleterDocumentation returnTypeInfo returnTypes typeInfoOfVariableNamed: typeInfoOfVariableNamed:ifAbsent: typedMessageSendersOf: typesOfInstanceVariableNamed: typesOfInstanceVariableNamed:ifAbsent: typesOfVariableNamed: typesOfVariableNamed:ifAbsent: variablesRawTypes)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' parseNodeIncluding:ifAbsent: rangesForInstanceVariable:ifAbsent: rangesForTemporaryVariable:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordsRanges selectorLastPosition)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('*Refactoring' addPositionTo:of:inside: classAndSelector hasArgumentOrTemporary: hasLocalNamed: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLasPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: selectorKeywordPositionAt: selectorKeywordsPositions tempNodes withParseNodeIncluding:do:ifAbsent:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
('*LiveTyping' identifierPrintString nodeTypePrintString topParseNodeIncluding:ifAbsent: typedMessageSendSelectorKeywordPositionsOf:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
!

!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity selectorDocumentation originalPosition showUp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterMorph reorganize!
('accessing' itemHeight selected selected:)
('actions' goDown goHome goPageDown goPageUp goToEnd goUp help resetMenu)
('activity tracking' lastActivity stillActive timeOfLastActivity timeout updateColor)
('drawing' adjust:ifOutOfScreenWith:xOffset:yOffset: calculateWidth colorOf: downButtonPosition drawContainingRectangle: drawDownArrowOn:thickness: drawItemOf:on:width:top: drawItemsOn:width: drawOn: drawScrollBarMovementBarOn:thickness: drawScrollBarOn: drawScrollBarRectangleOn:thickness: drawUpArrowOn:thickness: setDefaultColors upButtonPosition)
('events' mouseButton1Up:localPosition:)
('event handling testing' handlesMouseDown:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('paging' currentPage gotoPage: pageCount)
('stepping' stepAt: stepTime wantsSteps)
('private' firstVisible lastVisible visibleItemsCount)
('selector documentation' crPressedOnSelectorDocumentation hideSelectorDocumentation initializeSelectorDocumentation isShowingSelectorDocumentation methodDocumentationSeparator selectorDefaultDocumentationLocation selectorDocumentation selectorDocumentationExtent selectorDocumentationText selectorDocumentationTextForAllI: selectorDocumentationTextForAllImplementorsOf: selectorDocumentationTextOf:forAll: showSelectorDocumentation)
('submorphs-add/remove' delete)
('testing' isXOutOfScreen:with: isYOutOfScreen:with:)
!


!MethodReference reorganize!
('queries' actualClass category classIsMeta classSymbol compiledMethod isValid methodSymbol selector sourceCode)
('setting' setClass:methodSymbol:stringVersion: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
('printing' displayStringOrText)
('*DenotativeObject' recalculateStringVersionForDenotativeObject)
('auto complete' dynamicTypingAutoCompleterDocumentation methodClass)
('*LiveTyping' classAndSelector doesOnlySurelySend: liveTypingAutoCompleterDocumentation prefixStringVersionWith: printClassAndSelectorOn: printOn: typedMessageSendersOf:)
!


!AutoCompleter reorganize!
('accessing' setModel: textMorph textMorph:)
('menu morph' closeMenu menuClosed openCompletionMenu)
('keyboard' autoCompletionAround:keyStroke: handleKeystrokeAfter: handleKeystrokeBefore:)
('keyboard - private' isTab:and: openCompletionMenuFor:if: shouldCloseMenu: shouldInsertSelected: shouldOpenMorph shouldOpenMorphWhenNoPrefixAt: shouldOpenMorphWhenNoPrefixFor: shouldOpenMorphWhenPrefixAt:and:)
('entries' computeEntries entries entryCount insertSelected newCursorPosition: selectedEntry selectedEntryFormatted)
('testing' canSelect: isPossibleInvalidEntry: opensWithTab)
!

!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor:and: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and:)
('identifier entries - private' computeIdentifierEntries)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
!

SmalltalkCompleter initialize!

!SmalltalkCompleter class reorganize!
('class initialization' addSelector:method:allImplemented: initialize)
('services' isThereAnImplementorOf: thatStartsCaseSensitive:)
('synchronization' protected:)
('system change notifications' classRemoved:fromCategory: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodRemoved:selector:inProtocol:class:)
('instance creation' withModel:)
('testing' isForCurrentTypeSystem)
('accessing' entriesLimit)
!


!DynamicTypingSmalltalkCompleter reorganize!
('message entries - private' computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt:)
('selector documentation' documentationOf:)
!


!DynamicTypingSmalltalkCompleter class reorganize!
('testing' isForCurrentTypeSystem)
!


!AutoCompleterSelectorsCollector reorganize!
('initialization' initializeFor:withSelectorsLimitedTo:)
('collecting' addSelectorsOf: addSelectorsOf:upTo: addSelectorsOfAll:upTo: addedSelectors: possibleInvalidSelectors)
('collecting - private' addCategoriesAndSelectorsOf: addSelectorsMarkingPossibleInvalidOnesOf:upTo: addToPossibleInvalidIfCorrespond: prefixedSelectorsOf:in:)
('entries to show' entriesToShow)
('entries to show - private' addCategory:with: addLeftCategories addPrioritizedCategories addPrioritizedCategory: categoryEntryFor: prioritizedCategories)
('testing' hasCollectedOnlyOneSelector hasReachSelectorsLimit)
!


!AutoCompleterSelectorsCollector class reorganize!
('instance creation' for: for:withSelectorsLimitedTo:)
('category entry' categoryEntryHeader)
!

!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes allSource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
