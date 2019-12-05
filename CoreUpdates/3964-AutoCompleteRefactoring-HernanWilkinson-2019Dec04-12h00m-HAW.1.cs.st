'From Cuis 5.0 [latest update: #3963] on 4 December 2019 at 4:17:22 pm'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange '
	classVariableNames: 'AccessLock EntriesLimit Selectors '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!TextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:16:53'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:17:33'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:11:59'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:15:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !


!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:16:53'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:17:33'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:11:59'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:15:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName ! !


!Workspace methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:15:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForClassOrNil: (self classOfBindingOf: aName) ! !


!TextProvider methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:16:53'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:17:33'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:11:59'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:15:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !


!Debugger methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:16:53'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:17:33'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	^self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:52:45'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	contextStackIndex = 0 
		ifTrue: [ anAutocompleter computeMessageEntriesForUnknowClass ]
		ifFalse: [ anAutocompleter computeMessageEntriesForClass: (self receiver instVarNamed: aName) class ]! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:37:54'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ anAutocompleter computeMessageEntriesForUnknowClass ].
	
	anAutocompleter computeMessageEntriesForClass: 
		(self debuggerMap namedTempAt: tempIndex in: context) class ! !


!Inspector methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:37:55'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForClass: (object instVarNamed: aName) class ! !


!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:16:53'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:17:33'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:11:59'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 12/4/2019 15:15:51'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:18:56'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange   
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:37:55'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel  
	
	| id rangeType |

	canShowSelectorDocumentation _ true.
	id _ allSource copyFrom: range start to: range end.
	rangeType _ range rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	rangeType == #super
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: id first class ].
	rangeType == #number
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #string
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #symbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #stringSymbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #instVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id ].
	rangeType == #methodArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #tempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #blockArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id ].
	rangeType == #blockTempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id ].
	rangeType == #workspaceVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) ].
	rangeType == #thisContext
		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) ]. 
	rangeType == #classVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #poolConstant
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #blockEnd
		ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure ].
	rangeType == #arrayEnd
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #rightBrace
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #unary
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id at: range ].
	rangeType == #rightParenthesis
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturnAt: range ].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiverAt: range ]. 

	self computeMessageEntriesForUnknowClass 
	
	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:37:54'!
computeMessageEntriesForClass: aClass  

	self isPreviousMessageSendBinary
		ifTrue: [ self computeMessageEntriesWithBinaryMessageForClass: aClass ]
		ifFalse: [ self computeMessageEntriesWithoutBinaryMessageForClass: aClass ].
		
	self ifEmptyEntriesShowAllPrefixedSelectors! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:37:55'!
computeMessageEntriesForClassOrNil: aClassOrNil  

	aClassOrNil 
		ifNil: [ self computeMessageEntriesForUnknowClass ]
		ifNotNil: [ self computeMessageEntriesForClass: aClassOrNil ].
	
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:20:04'!
computeMessageEntriesOfCascadeReceiverAt: aRange   
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:19:35'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange   

	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:37:14'!
computeMessageEntriesWithBinaryMessageForClass: aClass 

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addUnaryAndBinarySelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:36:49'!
computeMessageEntriesWithoutBinaryMessageForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addSelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:26:55'!
isPreviousMessageSendBinary

	^possibleBinarySendRange notNil and: [ possibleBinarySendRange rangeType = #binary ].

	! !


!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:18:56'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange   
	
	self computeMessageEntriesForUnknowClass 
	! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:20:04'!
computeMessageEntriesOfCascadeReceiverAt: aRange   
	
	self computeMessageEntriesForUnknowClass
	
! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:19:35'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange   

	self computeMessageEntriesForUnknowClass 
	
! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:25:10'!
computeMessageEntriesWhenSendinMessageFor: allSource using: last3Ranges in: contextClass and: specificModel 
	
	| prevRange |
	
	"If previous range is a constant or a well known identifier, we might filter messages"
	prevRange _ last3Ranges second.
	possibleBinarySendRange _ last3Ranges first.
	
	^ (self canComputeMessageEntriesFor: prevRange ) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: prevRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 12/4/2019 15:25:10'!
computeMessageEntriesWithEmptyPrefixFor: allSource using: last3Ranges at: range in: contextClass and: specificModel 
	
	prefix _ ''.
	possibleBinarySendRange _ last3Ranges second.

	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel .! !

!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt:with: stamp: 'HAW 12/4/2019 15:19:35'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:with:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt:with: stamp: 'HAW 12/4/2019 15:20:04'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:with:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at:with: stamp: 'HAW 12/4/2019 15:18:56'!
DynamicTypingSmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesForClass:with: stamp: 'HAW 12/4/2019 14:55:27'!
SmalltalkCompleter removeSelector: #computeMessageEntriesForClass:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithPossibleBinaryMessageSendForClass: stamp: 'HAW 12/4/2019 15:37:55'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithPossibleBinaryMessageSendForClass:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt:with: stamp: 'HAW 12/4/2019 15:20:04'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithBinaryMessageSendForClass:withPreviousBinaryMessageSend: stamp: 'HAW 12/4/2019 14:49:16'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithBinaryMessageSendForClass:withPreviousBinaryMessageSend:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesForClassOrNil:with: stamp: 'HAW 12/4/2019 15:14:37'!
SmalltalkCompleter removeSelector: #computeMessageEntriesForClassOrNil:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt:with: stamp: 'HAW 12/4/2019 15:19:35'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesFor:at:in:and:with: stamp: 'HAW 12/4/2019 15:21:22'!
SmalltalkCompleter removeSelector: #computeMessageEntriesFor:at:in:and:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithPossibleBinaryMessageSendForClass:with: stamp: 'HAW 12/4/2019 14:57:15'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithPossibleBinaryMessageSendForClass:with:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesForClass:withPreviousBinaryMessageSend: stamp: 'HAW 12/4/2019 14:48:51'!
SmalltalkCompleter removeSelector: #computeMessageEntriesForClass:withPreviousBinaryMessageSend:!
!methodRemoval: SmalltalkCompleter #isBinaryMessageSend: stamp: 'HAW 12/4/2019 15:26:27'!
SmalltalkCompleter removeSelector: #isBinaryMessageSend:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithBinaryMessageSendForClass: stamp: 'HAW 12/4/2019 15:37:14'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithBinaryMessageSendForClass:!
!methodRemoval: SmalltalkCompleter #isPreviousMessageSendBinary: stamp: 'HAW 12/4/2019 15:26:55'!
SmalltalkCompleter removeSelector: #isPreviousMessageSendBinary:!
!methodRemoval: SmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at:with: stamp: 'HAW 12/4/2019 15:18:56'!
SmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:with:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofBlockArgNamed:with: stamp: 'HAW 12/4/2019 15:16:53'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:with:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofBlockTempVarNamed:with: stamp: 'HAW 12/4/2019 15:17:33'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:with:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!
!methodRemoval: Inspector #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
Inspector removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofBlockArgNamed:with: stamp: 'HAW 12/4/2019 15:16:53'!
Debugger removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:with:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
Debugger removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofBlockTempVarNamed:with: stamp: 'HAW 12/4/2019 15:17:33'!
Debugger removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:with:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
Debugger removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofBlockArgNamed:with: stamp: 'HAW 12/4/2019 15:16:53'!
TextProvider removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:with:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
TextProvider removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofBlockTempVarNamed:with: stamp: 'HAW 12/4/2019 15:17:33'!
TextProvider removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:with:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
TextProvider removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!
!methodRemoval: Workspace #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
Workspace removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofBlockArgNamed:with: stamp: 'HAW 12/4/2019 15:16:53'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:with:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofBlockTempVarNamed:with: stamp: 'HAW 12/4/2019 15:17:33'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:with:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofBlockArgNamed:with: stamp: 'HAW 12/4/2019 15:16:53'!
TextModel removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:with:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofTempVarNamed:with: stamp: 'HAW 12/4/2019 15:15:51'!
TextModel removeSelector: #computeMessageEntriesIn:ofTempVarNamed:with:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofBlockTempVarNamed:with: stamp: 'HAW 12/4/2019 15:17:33'!
TextModel removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:with:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofInstVarNamed:with: stamp: 'HAW 12/4/2019 15:11:59'!
TextModel removeSelector: #computeMessageEntriesIn:ofInstVarNamed:with:!

!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
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
('accessing' methodNodeOf:ifErrorsParsing: selectedClassOrMetaClass)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context: resetToSelectedContextWith:)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('testing' is: isEditingMethod)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('contents' instanceVariableRenamed)
('*Cuis-University-Model' argumentNamesOf:sentIn: compileInitilizationMessage:in:fromClass:context: compileInstanceCreationMessage:in:fromClass:context: compileMessage:inCategory:fromClass:context: implement:inCategory:fromClass:context:)
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

!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:using:at:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary)
('identifier entries - private' computeIdentifierEntries)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
!

