'From Cuis 5.0 of 7 November 2016 [latest update: #3451] on 23 September 2018 at 10:09:08 pm'!

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:03:43'!
isAlphaNumeric

	^ self keyCharacter isAlphaNumeric! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:29:31'!
isArrowDown

	^ keyValue = 31! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:29:05'!
isArrowUp

	^ keyValue = 30! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:26:20'!
isBackspace

	^ keyValue = 8! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:04:09'!
isColon

	^ self keyCharacter = $:.! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:34:21'!
isCtrlSpace
	
	^ (self controlKeyPressed or: [ self rawMacOptionKeyPressed ]) and: [ self isSpace ]! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:27:43'!
isEnd
	
	^ keyValue = 4! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:25:30'!
isEsc

	^ keyValue = 27! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:27:07'!
isHome
	
	^ keyValue = 1! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:30:29'!
isPageDown

	^ keyValue = 12! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:30:07'!
isPageUp

	^ keyValue = 11! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 21:28:26'!
isQuesitonMark
	
	^ self keyCharacter = $? ! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 20:55:17'!
isSpace

	^ #(0 32 160) includes: keyValue.! !

!KeyboardEvent methodsFor: 'testing' stamp: 'HAW 9/23/2018 20:55:13'!
isTab

	^self keyCharacter = Character tab.! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 21:16:16'!
isTab: kbEvent and: shouldOpenMoprh
	
	^ self opensWithTab 
		and: [ kbEvent isTab 
		and: [ shouldOpenMoprh ]]! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 21:35:54'!
openCompletionMenuFor: kbEvent if: shouldOpenMorph

	"Ctrl-Space or Tab for open"
	"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"
	(kbEvent isCtrlSpace or: [self isTab: kbEvent and: shouldOpenMorph]) ifTrue: [ self openCompletionMenu. ^ true].
		
	"Auto-open - currently deactivated"
"	(ctrl not & cmd not & alphanum) ifTrue: [ self openCompletionMenu ]."
	
	^ false! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 21:06:20'!
shouldCloseMenu: kbEvent

	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"

	^ ((kbEvent controlKeyPressed not and: [ kbEvent commandAltKeyPressed not ]) and: [ kbEvent isAlphaNumeric or: [ kbEvent isColon ]]) not
! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 20:58:51'!
shouldInsertSelected: kbEvent
	
	^ kbEvent isReturnKey 
		or: [ (kbEvent isSpace and: [ kbEvent controlKeyPressed or: [ kbEvent rawMacOptionKeyPressed ]]) 
		or: [ kbEvent isTab]]! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 19:46:17'!
shouldOpenMorph

	| currentPos currentChar   |
	
	currentPos _ textMorph editor startIndex-1.
	currentPos <= 0 ifTrue: [ ^ false ].
	currentChar _ model actualContents at: currentPos.
	
	^ currentChar = Character space
		ifTrue: [ self shouldOpenMorphWhenNoPrefixAt: currentPos-1 ]
		ifFalse: [ self shouldOpenMorphWhenPrefixAt: currentPos and: currentChar ].
	


	! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 21:23:06'!
shouldOpenMorphWhenNoPrefixAt: currentPos 
	
	^  model textSize >= currentPos 
		and: [ currentPos > 0
		and: [ (model actualContents at: currentPos) isAlphaNumeric ]] ! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 9/23/2018 19:48:46'!
shouldOpenMorphWhenPrefixAt: currentPos and: currentChar 
	
	^ model textSize >= currentPos and: [ currentChar isAlphaNumeric or: [ currentChar == $: ]] 


	! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:30:07'!
canComputeMessageEntriesFor: prevRange and: prevPrevRange

	^ prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:17:21'!
computeEntriesOfMessageOrIdentifiersFor: allSource using: last3Ranges at: range in: contextClass and: specificModel
	
	prefix _ allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ ^self computeMessageEntriesWhenSendinMessageFor: allSource using: last3Ranges in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #() ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:26:07'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClass |
	
	id _ allSource copyFrom: range start to: range end.
	receiverClass _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ self classOfInstVarNamed: id in: specificModel ].
			[ #tempVar ] 			-> [ self classOfTempVarNamed: id in: specificModel ].
			[ #workspaceVar ]		-> [ self classOfWorkspaceVarNamed: id in: specificModel ].
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #thisContext ] 		-> [ self classOfThisContextIn: specificModel ]. } 
		otherwise: [ nil ].
		
	self computeMessageEntries: receiverClass! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:29:16'!
computeMessageEntriesWhenSendinMessageFor: allSource using: last3Ranges in: contextClass and: specificModel 
	
	| prevPrevRange prevRange |
	
	"If previous range is a constant or a well known identifier, we might filter messages"
	prevRange _ last3Ranges second.
	prevPrevRange _ last3Ranges first.
	
	"3 if -> ifNil: but not ifTrue:
	3=4 -> ifNil: or ifTrue:"
	^ (self canComputeMessageEntriesFor: prevRange and: prevPrevRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: prevRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntries: nil ]
! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:18:09'!
computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel 
	
	prefix _ ''.
	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:03:49'!
parse: sourceToParse in: contextClass and: specificModel
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: sourceToParse.
	parser parse.
	
	^ parser last3Ranges.
! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:01:52'!
selectedClassOrMetaClassIn: specificModel

	^ (specificModel is: #CodeProvider) ifTrue: [ specificModel selectedClassOrMetaClass ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 19:30:43'!
textProviderOrModel

	^ (model is: #hasTextProvider) ifTrue: [ model textProvider ] ifFalse: [ model ].! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 22:03:28'!
classOfInstVarNamed: aName in: specificModel
	
	(specificModel isKindOf: Debugger) ifTrue: [ ^specificModel contextStackIndex ~= 0 ifTrue: [ (specificModel receiver instVarNamed: aName) class] ].
	
	^nil
	! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 16:52:20'!
classOfLiteral: aLiteral in: aClass 
	
	| compilerClass |
	
	compilerClass := aClass ifNil: [ Compiler ] ifNotNil: [ aClass compilerClass ].
	
	^ (compilerClass evaluate: aLiteral) class ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 16:52:36'!
classOfLocalBindingNamed: aName in: aClass 
	
	^  (aClass localBindingOf: aName) ifNotNil: [ :aBinding | aBinding value class ]! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 22:04:08'!
classOfTempVarNamed: aName in: specificModel 
		
	(specificModel isKindOf: Workspace) ifTrue: [ ^ self classOfTempVarNamed: aName inWorkspace: specificModel ].
	(specificModel isKindOf: Debugger)    ifTrue: [ ^ self classOfTempVarNamed: aName inDebugger: specificModel ]. 
	(specificModel isKindOf: Inspector)    ifTrue: [ ^ self classOfTempVarNamed: aName inInspector: specificModel ]. 
	
	^ nil! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 19:31:39'!
classOfTempVarNamed: aName inDebugger: aDebugger
		
	| context tempIndex |
							
	context := aDebugger selectedContext.
	tempIndex := context tempNames indexOf: aName.
	
	^ tempIndex ~= 0 ifTrue: [(context tempAt: tempIndex) class]! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 21:50:35'!
classOfTempVarNamed: aName inInspector: anInspector
 
	^ (anInspector bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 16:38:27'!
classOfTempVarNamed: aName inWorkspace: aWorkspace

	^ (aWorkspace bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 19:31:57'!
classOfThisContextIn: specificModel
	
	"thisContext could mean ContextPart or BlockClosure..."
	^ (specificModel isKindOf: Debugger) ifTrue: [ self classOfThisContextInDebugger: specificModel ]
		! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 17:06:19'!
classOfThisContextInDebugger: aDebugger
		
	^ aDebugger selectedContext class! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 22:05:20'!
classOfWorkspaceVarNamed: id in: specificModel
	
	(specificModel isKindOf: Workspace) ifTrue: [ ^ self classOfWorkspaceVarNamed: id inWorkspace: specificModel ].
	(specificModel isKindOf: Inspector) ifTrue: [ ^self classOfWorkspaceVarNamed: id inInspector: specificModel ].
	
	^nil! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 16:44:50'!
classOfWorkspaceVarNamed: aName inInspector: anInspector 

	^  (anInspector object instVarNamed: aName) class ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 16:43:48'!
classOfWorkspaceVarNamed: aName inWorkspace: aWorkspace
												
	^  (aWorkspace bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] ! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'HAW 9/23/2018 21:30:54'!
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
	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ]. 
	
	^false! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 9/23/2018 19:18:00'!
computeEntries

	| allSource contextClass specificModel last3Ranges range |
	
	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	last3Ranges _ self parse: (allSource copyFrom: 1 to: position) in: contextClass and: specificModel.
	range _ last3Ranges third ifNil: [ ^entries _ #() ].

	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource using: last3Ranges at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 9/23/2018 19:23:35'!
newCursorPosition: anEntry

	^anEntry indexOf: $ ! !

!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 9/23/2018 19:23:40'!
selectedEntry

	^(self entries at: menuMorph selected) separateKeywords! !

!methodRemoval: SmalltalkCompleter #canDetectTypeOf:in:!
SmalltalkCompleter removeSelector: #canDetectTypeOf:in:!
!methodRemoval: SmalltalkCompleter #classForInstVarNamed:in:!
SmalltalkCompleter removeSelector: #classForInstVarNamed:in:!
!methodRemoval: SmalltalkCompleter #classOfLiteral:!
SmalltalkCompleter removeSelector: #classOfLiteral:!
!methodRemoval: SmalltalkCompleter #classOfLocalBindingNamed:of:!
SmalltalkCompleter removeSelector: #classOfLocalBindingNamed:of:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:inBindingsProvider:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:inBindingsProvider:!
!methodRemoval: SmalltalkCompleter #computeEntriesOf:in:!
SmalltalkCompleter removeSelector: #computeEntriesOf:in:!
!methodRemoval: AutoCompleter #isArrowDown:!
AutoCompleter removeSelector: #isArrowDown:!
!methodRemoval: AutoCompleter #isArrowUp:!
AutoCompleter removeSelector: #isArrowUp:!
!methodRemoval: AutoCompleter #isCtrlSpace:!
AutoCompleter removeSelector: #isCtrlSpace:!
!methodRemoval: AutoCompleter #isEnd:!
AutoCompleter removeSelector: #isEnd:!
!methodRemoval: AutoCompleter #isHome:!
AutoCompleter removeSelector: #isHome:!
!methodRemoval: AutoCompleter #isPageDown:!
AutoCompleter removeSelector: #isPageDown:!
!methodRemoval: AutoCompleter #isPageUp:!
AutoCompleter removeSelector: #isPageUp:!
!methodRemoval: AutoCompleter #isQuesitonMark:!
AutoCompleter removeSelector: #isQuesitonMark:!

!AutoCompleter reorganize!
('accessing' setModel: textMorph:)
('menu morph' closeMenu menuClosed openCompletionMenu)
('keyboard' autoCompletionAround:keyStroke: handleKeystrokeAfter: handleKeystrokeBefore:)
('keyboard - private' isTab:and: openCompletionMenuFor:if: shouldCloseMenu: shouldInsertSelected: shouldOpenMorph shouldOpenMorphWhenNoPrefixAt: shouldOpenMorphWhenPrefixAt:and:)
('entries' computeEntries entries entryCount insertSelected newCursorPosition: selectedEntry)
('testing' opensWithTab)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' canComputeMessageEntriesFor:and: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: parse:in:and: selectedClassOrMetaClassIn: textProviderOrModel)
('class detection' classOfInstVarNamed:in: classOfLiteral:in: classOfLocalBindingNamed:in: classOfTempVarNamed:in: classOfTempVarNamed:inDebugger: classOfTempVarNamed:inInspector: classOfTempVarNamed:inWorkspace: classOfThisContextIn: classOfThisContextInDebugger: classOfWorkspaceVarNamed:in: classOfWorkspaceVarNamed:inInspector: classOfWorkspaceVarNamed:inWorkspace:)
('testing' opensWithTab)
!

