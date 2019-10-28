'From Cuis 5.0 [latest update: #3935] on 28 October 2019 at 1:00:31 am'!
!classDefinition: #AutoCompleterSelectorsCollector category: #'Tools-Autocompletion' stamp: 'HAW 10/28/2019 01:00:31'!
Object subclass: #AutoCompleterSelectorsCollector
	instanceVariableNames: 'categoriesWithSelectors addedSelectorsFastSet prefix entriesToShow possibleInvalidSelectors otherClasses selectorsLimit selectorSelectionCondition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:46:37'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:48:30'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:45:40'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !


!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:46:37'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange 

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange
! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:48:30'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange 

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:01'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	textProvider computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 
! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:45:40'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	textProvider computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange
! !


!Workspace methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:47:59'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForClassOrNil: (self classOfBindingOf: aName) with: possibleBinaryMessageSendRange! !


!TextProvider methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:46:37'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:48:30'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:45:40'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !


!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:46:37'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange 

	self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange
! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:48:30'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange 

	^self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	contextStackIndex = 0 
		ifTrue: [ anAutocompleter computeMessageEntriesForUnknowClass ]
		ifFalse: [ anAutocompleter computeMessageEntriesForClass: (self receiver instVarNamed: aName) class with: possibleBinaryMessageSendRange]! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:45:40'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ anAutocompleter computeMessageEntriesForUnknowClass ].
	
	anAutocompleter computeMessageEntriesForClass: (self debuggerMap namedTempAt: tempIndex in: context) class with: possibleBinaryMessageSendRange

	! !


!Inspector methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForClass: (object instVarNamed: aName) class with: possibleBinaryMessageSendRange ! !


!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:46:37'!
computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:48:30'!
computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:43:00'!
computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/27/2019 21:45:40'!
computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName with: possibleBinaryMessageSendRange 

	anAutocompleter computeMessageEntriesForUnknowClass! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 10/27/2019 20:09:37'!
canComputeMessageEntriesFor: prevRange 

	^ prevRange notNil ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 18:38:46'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange with: possibleBinaryMessageSendRange  
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/28/2019 00:24:58'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel with: possibleBinarySendRange 
	
	| id rangeType |

	canShowSelectorDocumentation _ true.
	id _ allSource copyFrom: range start to: range end.
	rangeType _ range rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class with: possibleBinarySendRange ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass with: possibleBinarySendRange ].
	rangeType == #super
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass with: possibleBinarySendRange ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True with: possibleBinarySendRange ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False with: possibleBinarySendRange ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject with: possibleBinarySendRange ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: id first class with: possibleBinarySendRange ].
	rangeType == #number
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #string
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #symbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #stringSymbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #instVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id with: possibleBinarySendRange ].
	rangeType == #methodArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id with: possibleBinarySendRange ].
	rangeType == #tempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id with: possibleBinarySendRange ].
	rangeType == #blockArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id with: possibleBinarySendRange ].
	rangeType == #blockTempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id with: possibleBinarySendRange ].
	rangeType == #workspaceVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) with: possibleBinarySendRange ].
	rangeType == #thisContext
		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) with: possibleBinarySendRange ]. 
	rangeType == #classVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #poolConstant
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) with: possibleBinarySendRange ].
	rangeType == #blockEnd
		ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure with: possibleBinarySendRange ].
	rangeType == #arrayEnd
		ifTrue: [ ^self computeMessageEntriesForClass: Array with: possibleBinarySendRange ].
	rangeType == #rightBrace
		ifTrue: [ ^self computeMessageEntriesForClass: Array with: possibleBinarySendRange ].
	rangeType == #unary
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id at: range with: possibleBinarySendRange ].
	rangeType == #rightParenthesis
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturnAt: range with: possibleBinarySendRange ].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiverAt: range with: possibleBinarySendRange ]. 

	self computeMessageEntriesForUnknowClass 
	
	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 21:55:05'!
computeMessageEntriesForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addSelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 21:54:40'!
computeMessageEntriesForClass: aClass with: possibleBinaryMessageSendRange 

	(self isBinaryMessageSend: possibleBinaryMessageSendRange)
		ifTrue: [ self computeMessageEntriesForClass: aClass withPreviousBinaryMessageSend: possibleBinaryMessageSendRange ]
		ifFalse: [ self computeMessageEntriesForClass: aClass ].
		
	self ifEmptyEntriesShowAllPrefixedSelectors! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 21:55:51'!
computeMessageEntriesForClass: aClass withPreviousBinaryMessageSend: possibleBinaryMessageSendRange

	self computeMessageEntriesForClass: aClass
	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 21:47:59'!
computeMessageEntriesForClassOrNil: aClassOrNil with: possibleBinaryMessageSendRange 

	aClassOrNil 
		ifNil: [ self computeMessageEntriesForUnknowClass ]
		ifNotNil: [ self computeMessageEntriesForClass: aClassOrNil with: possibleBinaryMessageSendRange ].
	
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 23:31:06'!
computeMessageEntriesOfCascadeReceiverAt: aRange with: possibleBinaryMessageSendRange  
	
	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 23:30:13'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange with: possibleBinaryMessageSendRange  

	self subclassResponsibility ! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 19:56:52'!
computeMessageEntriesWithEmptyPrefixFor: allSource using: last3Ranges at: range in: contextClass and: specificModel 
	
	prefix _ ''.
	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel with: last3Ranges second.! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 18:40:11'!
isBinaryMessageSend: possibleBinaryMessageSendRange

	^possibleBinaryMessageSendRange notNil and: [ possibleBinaryMessageSendRange rangeType = #binary ].

	! !


!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 18:38:46'!
computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange with: possibleBinaryMessageSendRange  
	
	self computeMessageEntriesForUnknowClass 
	! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 23:31:06'!
computeMessageEntriesOfCascadeReceiverAt: aRange with: possibleBinaryMessageSendRange  
	
	self computeMessageEntriesForUnknowClass
	
! !

!DynamicTypingSmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 23:30:13'!
computeMessageEntriesOfEnclosedExpressionReturnAt: aRange with: possibleBinaryMessageSendRange  

	self computeMessageEntriesForUnknowClass 
	
! !


!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/28/2019 00:38:01'!
addKeywordSelectorsOf: aClass 
	
	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass ]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/28/2019 00:38:01'!
addKeywordSelectorsOf: aClass upTo: aSuperclassToExclude

	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/28/2019 00:38:01'!
addKeywordSelectorsOfAll: classes upTo: aSuperclass

	self selectKeywordSelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclass ]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/27/2019 19:38:13'!
addUnaryAndBinarySelectorsOf: aClass 
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass ].
	! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/27/2019 23:54:56'!
addUnaryAndBinarySelectorsOf: aClass upTo: aSuperclassToExclude
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ].
	! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 10/27/2019 23:55:50'!
addUnaryAndBinarySelectorsOfAll: classes upTo: aSuperclassToExclude
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclassToExclude ].
	! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 10/28/2019 00:38:01'!
selectKeywordSelectorsWhile: aClosure 

	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword ] while: aClosure 
! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 10/27/2019 19:36:18'!
selectSelectorsThatSatisfy: aSelectorsSelectionCondition while: aClosure
	
	| currentSelectorsSelectionCondition |
	
	currentSelectorsSelectionCondition := selectorSelectionCondition.
	[ selectorSelectionCondition := aSelectorsSelectionCondition.
	aClosure value ] ensure: [ selectorSelectionCondition := currentSelectorsSelectionCondition ].! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 10/27/2019 19:38:54'!
selectUnaryAndBinarySelectorsWhile: aClosure
	
	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword not ] while: aClosure 
! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 10/27/2019 12:40:42'!
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
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource using: last3Ranges at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/27/2019 20:09:37'!
computeMessageEntriesWhenSendinMessageFor: allSource using: last3Ranges in: contextClass and: specificModel 
	
	| prevPrevRange prevRange |
	
	"If previous range is a constant or a well known identifier, we might filter messages"
	prevRange _ last3Ranges second.
	prevPrevRange _ last3Ranges first.
	
	"3 if -> ifNil: but not ifTrue:
	3=4 -> ifNil: or ifTrue:"
	
	^ (self canComputeMessageEntriesFor: prevRange ) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: prevRange in: contextClass and: specificModel with: prevPrevRange ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !


!AutoCompleterSelectorsCollector methodsFor: 'initialization' stamp: 'HAW 10/27/2019 19:20:29'!
initializeFor: aPrefix withSelectorsLimitedTo: aLimit 

	prefix := aPrefix.
	categoriesWithSelectors := OrderedDictionary new.
	addedSelectorsFastSet := IdentitySet new.
	possibleInvalidSelectors := IdentitySet new.
	selectorsLimit := aLimit.
	selectorSelectionCondition := [ :aSelector | true ]! !

!AutoCompleterSelectorsCollector methodsFor: 'collecting - private' stamp: 'HAW 10/27/2019 19:22:52'!
prefixedSelectorsOf: aCategory in: aClassOrganization
	
	^ (aClassOrganization listAtCategoryNamed: aCategory) 
		select: [ :aSelector |  
			(aSelector beginsWith: prefix) 
				and: [ (selectorSelectionCondition value: aSelector) 
				and: [ (addedSelectorsFastSet includes: aSelector) not ]]].
! !

!methodRemoval: AutoCompleterSelectorsCollector #addBinarySelectorsOfAll:upTo: stamp: 'HAW 10/28/2019 00:37:34'!
AutoCompleterSelectorsCollector removeSelector: #addBinarySelectorsOfAll:upTo:!
!methodRemoval: AutoCompleterSelectorsCollector #selectBinaryKeywordWhile: stamp: 'HAW 10/27/2019 19:39:19'!
AutoCompleterSelectorsCollector removeSelector: #selectBinaryKeywordWhile:!
!methodRemoval: AutoCompleterSelectorsCollector #addBinarySelectorsOf:upTo: stamp: 'HAW 10/28/2019 00:36:27'!
AutoCompleterSelectorsCollector removeSelector: #addBinarySelectorsOf:upTo:!
!methodRemoval: AutoCompleterSelectorsCollector #addBinarySelectorsOf: stamp: 'HAW 10/28/2019 00:35:46'!
AutoCompleterSelectorsCollector removeSelector: #addBinarySelectorsOf:!
!methodRemoval: AutoCompleterSelectorsCollector #selectBinarySelectorsWhile: stamp: 'HAW 10/28/2019 00:38:01'!
AutoCompleterSelectorsCollector removeSelector: #selectBinarySelectorsWhile:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt: stamp: 'HAW 10/27/2019 23:31:06'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at: stamp: 'HAW 10/27/2019 18:38:46'!
DynamicTypingSmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:!
!methodRemoval: DynamicTypingSmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt: stamp: 'HAW 10/27/2019 23:30:13'!
DynamicTypingSmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfEnclosedExpressionReturnAt: stamp: 'HAW 10/27/2019 23:30:13'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfEnclosedExpressionReturnAt:!
!methodRemoval: SmalltalkCompleter #computeEntriesOfUnaryMessageReturnNamed:at: stamp: 'HAW 10/27/2019 18:38:46'!
SmalltalkCompleter removeSelector: #computeEntriesOfUnaryMessageReturnNamed:at:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesOfCascadeReceiverAt: stamp: 'HAW 10/27/2019 23:31:06'!
SmalltalkCompleter removeSelector: #computeMessageEntriesOfCascadeReceiverAt:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesForClassOrNil: stamp: 'HAW 10/27/2019 21:47:59'!
SmalltalkCompleter removeSelector: #computeMessageEntriesForClassOrNil:!
!methodRemoval: SmalltalkCompleter #canComputeMessageEntriesFor:and: stamp: 'HAW 10/27/2019 20:09:37'!
SmalltalkCompleter removeSelector: #canComputeMessageEntriesFor:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesWithEmptyPrefixFor:at:in:and: stamp: 'HAW 10/27/2019 12:40:59'!
SmalltalkCompleter removeSelector: #computeMessageEntriesWithEmptyPrefixFor:at:in:and:!
!methodRemoval: SmalltalkCompleter #computeMessageEntriesFor:at:in:and: stamp: 'HAW 10/27/2019 18:30:27'!
SmalltalkCompleter removeSelector: #computeMessageEntriesFor:at:in:and:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofBlockArgNamed: stamp: 'HAW 10/27/2019 21:46:37'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:!
!methodRemoval: ClassNameRequestMorph #computeMessageEntriesIn:ofBlockTempVarNamed: stamp: 'HAW 10/27/2019 21:48:30'!
ClassNameRequestMorph removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:!
!methodRemoval: Inspector #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
Inspector removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
Debugger removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
Debugger removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofBlockArgNamed: stamp: 'HAW 10/27/2019 21:46:37'!
Debugger removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:!
!methodRemoval: Debugger #computeMessageEntriesIn:ofBlockTempVarNamed: stamp: 'HAW 10/27/2019 21:48:30'!
Debugger removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
TextProvider removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
TextProvider removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofBlockArgNamed: stamp: 'HAW 10/27/2019 21:46:37'!
TextProvider removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:!
!methodRemoval: TextProvider #computeMessageEntriesIn:ofBlockTempVarNamed: stamp: 'HAW 10/27/2019 21:48:30'!
TextProvider removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:!
!methodRemoval: Workspace #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
Workspace removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofBlockArgNamed: stamp: 'HAW 10/27/2019 21:46:37'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:!
!methodRemoval: PluggableTextModel #computeMessageEntriesIn:ofBlockTempVarNamed: stamp: 'HAW 10/27/2019 21:48:30'!
PluggableTextModel removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofInstVarNamed: stamp: 'HAW 10/27/2019 21:43:01'!
TextModel removeSelector: #computeMessageEntriesIn:ofInstVarNamed:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofTempVarNamed: stamp: 'HAW 10/27/2019 21:45:40'!
TextModel removeSelector: #computeMessageEntriesIn:ofTempVarNamed:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofBlockArgNamed: stamp: 'HAW 10/27/2019 21:46:37'!
TextModel removeSelector: #computeMessageEntriesIn:ofBlockArgNamed:!
!methodRemoval: TextModel #computeMessageEntriesIn:ofBlockTempVarNamed: stamp: 'HAW 10/27/2019 21:48:30'!
TextModel removeSelector: #computeMessageEntriesIn:ofBlockTempVarNamed:!

!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed:with: computeMessageEntriesIn:ofBlockTempVarNamed:with: computeMessageEntriesIn:ofInstVarNamed:with: computeMessageEntriesIn:ofTempVarNamed:with:)
!


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfBindingOf: classOfWorkspaceVarNamed: computeMessageEntriesIn:ofTempVarNamed:with:)
('testing' canBindVariables is:)
('accessing' methodNodeOf:ifErrorsParsing:)
('*LiveTyping' selectedClassOrMetaClass)
!


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' insertSuperclass removeClass renameClass renameGlobal safelyRemoveClass)
('testing' isEditingMethod)
('*LiveTyping' browseActualSendersOf: computeMessageEntriesIn:ofBlockArgNamed:with: computeMessageEntriesIn:ofBlockTempVarNamed:with: computeMessageEntriesIn:ofInstVarNamed:with: computeMessageEntriesIn:ofTempVarNamed:with: confirmActualRemovalOf: confirmRemovalWhenHasSenders confirmRemovalWhenNoSenders hasNoActualSendersOrIsItself: removeMessageCheckingActualSenders removeMethod: resetCategoryTypes resetClassType typeCheckCategory typeCheckClass)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
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
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed:with: computeMessageEntriesIn:ofBlockTempVarNamed:with: computeMessageEntriesIn:ofInstVarNamed:with: computeMessageEntriesIn:ofTempVarNamed:with:)
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
('auto complete' computeMessageEntriesIn:ofInstVarNamed:with:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is:)
!


!PseudoClass reorganize!
('class' classComment: classPool comment comment: definition definition: metaClass renameTo: sharedPools)
('accessing' allCallsOn allInstVarNames allSuperclasses compilerClass fullName instVarNames name name: organization realClass superclassName theNonMetaClass variablesAndOffsetsDo:)
('removing' removeAllUnmodified removeUnmodifiedMethods:)
('private' allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: confirmRemovalOf: evaluate: makeSureClassExists: makeSureSuperClassExists: parserClass)
('testing' exists hasChanges hasComment hasDefinition hasMetaclass is: isMeta lastUnderscoreMeansSubscript nameExists needsInitialize)
('fileIn/fileOut' fileIn fileInCategory: fileInDefinition fileInDefinitionAndMetaclass fileInMethod: fileInMethods fileInMethods: fileOut fileOutCategory: fileOutDefinitionOn: fileOutMethod: fileOutMethods:on: fileOutMethodsOn: fileOutOn:)
('errors' classNotDefined)
('categories' removeCategory: removedCategoryName whichCategoryIncludesSelector:)
('methods' addMethodChange: closuresInfoAt: compiledMethodAt:ifAbsent: methodChange: removeMethod: removeSelector: removeSelectorIfInBaseSystem: selectors sourceCode sourceCodeAt: sourceCodeAt:put: sourceCodeTemplate stampAt:)
('printing' literalScannedAs:notifying: printOn:)
('testing method dictionary' bindingOf: includesSelector:)
('compatibility' browseClassVarRefs category chooseInstVarThenDo:)
('*LiveTyping' typeInfoOfInstanceVariableNamed:ifAbsent:)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at:with: computeMessageEntriesFor:at:in:and:with: computeMessageEntriesForClass: computeMessageEntriesForClass:with: computeMessageEntriesForClass:withPreviousBinaryMessageSend: computeMessageEntriesForClassOrNil:with: computeMessageEntriesOfCascadeReceiverAt:with: computeMessageEntriesOfEnclosedExpressionReturnAt:with: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:using:at:in:and: ifEmptyEntriesShowAllPrefixedSelectors isBinaryMessageSend:)
('identifier entries - private' computeIdentifierEntries)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
!

!classDefinition: #AutoCompleterSelectorsCollector category: #'Tools-Autocompletion' stamp: 'HAW 10/28/2019 01:00:31'!
Object subclass: #AutoCompleterSelectorsCollector
	instanceVariableNames: 'categoriesWithSelectors addedSelectorsFastSet prefix entriesToShow possibleInvalidSelectors otherClasses selectorsLimit selectorSelectionCondition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterSelectorsCollector reorganize!
('initialization' initializeFor:withSelectorsLimitedTo:)
('collecting' addKeywordSelectorsOf: addKeywordSelectorsOf:upTo: addKeywordSelectorsOfAll:upTo: addSelectorsOf: addSelectorsOf:upTo: addSelectorsOfAll:upTo: addUnaryAndBinarySelectorsOf: addUnaryAndBinarySelectorsOf:upTo: addUnaryAndBinarySelectorsOfAll:upTo: addedSelectors: possibleInvalidSelectors)
('collecting - private' addCategoriesAndSelectorsOf: addCategory:of: addSelectorsMarkingPossibleInvalidOnesOf:upTo: addToPossibleInvalidIfCorrespond: prefixedSelectorsOf:in: selectKeywordSelectorsWhile: selectSelectorsThatSatisfy:while: selectUnaryAndBinarySelectorsWhile:)
('entries to show' entriesToShow)
('entries to show - private' addCategory:with: addLeftCategories addPrioritizedCategories addPrioritizedCategory: categoryEntryFor: prioritizedCategories)
('testing' hasCollectedOnlyOneSelector hasReachSelectorsLimit)
!

