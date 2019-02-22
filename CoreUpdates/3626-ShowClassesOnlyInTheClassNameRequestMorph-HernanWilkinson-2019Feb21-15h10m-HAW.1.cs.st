'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 5:09:48 pm'!
!classDefinition: #ClassNameCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #ClassNameCompleter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation '
	classVariableNames: 'AccessLock EntriesLimit Selectors '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 16:00:50'!
canShowSelectorDocumentation

	self subclassResponsibility! !


!ClassNameCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 16:01:01'!
canShowSelectorDocumentation

	^false! !

!ClassNameCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 15:54:56'!
opensWithTab

	^true! !

!ClassNameCompleter methodsFor: 'entries' stamp: 'HAW 2/21/2019 15:53:24'!
computeEntries

	prefix _ model actualContents string.
	entries _ (Smalltalk classNames select: [ :aClassName | aClassName beginsWith: prefix ]) sort.
	! !


!SmalltalkCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 16:06:32'!
canShowSelectorDocumentation

	^canShowSelectorDocumentation ! !


!ClassNameRequestMorph methodsFor: 'user interface support' stamp: 'HAW 2/21/2019 15:53:49'!
autoCompleterClassFor: textGetter
	
	^ClassNameCompleter ! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'HAW 2/21/2019 16:10:52'!
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
	
	self canShowSelectorDocumentation 
		ifTrue: [ 
			kbEvent isArrowRight ifTrue: [ menuMorph showSelectorDocumentation. ^ true ].
			kbEvent isArrowLeft ifTrue: [ menuMorph hideSelectorDocumentation. ^ true ]]
		ifFalse: [
			"If it is showing identifiers I eat the right arrow key because the user is used to it when
			showing selectors,  so to avoid an unexpected behavior I do nothing with it -Hernan"
			kbEvent isArrowRight ifTrue: [ ^ true ]].
					
	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ]. 
	 
	^false! !

!AutoCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 15:55:35'!
opensWithTab
	
	"Returns wheter should open the auto completer when pressing Tab or not"
	
	^false! !


!SmalltalkCompleter methodsFor: 'testing' stamp: 'HAW 2/21/2019 15:55:07'!
opensWithTab

	^true! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/21/2019 16:05:59'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id rangeType |

	canShowSelectorDocumentation _ true.
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

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'HAW 2/21/2019 16:03:28'!
computeIdentifierEntries
	"Use an aux Set to avoid duplicates, but keep the order given."

	| entriesSet lastTitle |
	entriesSet _ Set new.
	lastTitle _ nil.
	canShowSelectorDocumentation _ false.
	
	entries _ Array streamContents: [ :strm |
		parser namesBeginningWith: prefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle _ kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		entries _ Array with: entriesSet anyOne ]! !

!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'HAW 2/21/2019 16:06:15'!
computeMessageEntriesForUnknowClass

	| selectorsToShow |
	
	selectorsClasses _ #().
	possibleInvalidSelectors _ IdentitySet new.
	canShowSelectorDocumentation _ true.
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


!ClassNameCompleter reorganize!
('testing' canShowSelectorDocumentation opensWithTab)
('entries' computeEntries)
!

!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor:and: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and:)
('identifier entries - private' computeIdentifierEntries)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
!

