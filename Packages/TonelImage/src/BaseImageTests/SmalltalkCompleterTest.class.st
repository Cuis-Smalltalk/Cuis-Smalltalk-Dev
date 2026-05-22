Class {
	#name : 'SmalltalkCompleterTest',
	#superclass : 'TestCase',
	#instVars : [
		'instanceVariable1',
		'classToBrowse',
		'selectorToBrowse'
	],
	#classVars : [
		'ClassVariableForTesting'
	],
	#pools : [
		'poolDictionaryForTesting'
	],
	#category : 'BaseImageTests-Tools-Autocompletion',
	#package : 'BaseImageTests',
	#tag : 'Tools-Autocompletion'
}

{ #category : 'testing' }
SmalltalkCompleterTest class >> isAbstract [

	^true
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assert: aCompleter analizedSelectorsFrom: classes canShowDocumentation: aBoolean detectedPossibleInvalidSelector: selectors suggested: entries  [
	
	classes 
		ifNil: [ self assert: aCompleter selectorsClasses isNil ]
		ifNotNil: [ self assert: aCompleter selectorsClasses asSet = classes asSet ].
	self assert: aCompleter canShowSelectorDocumentation equals: aBoolean.
	self assert: aCompleter possibleInvalidSelectors asSet = selectors asSet.
	entries 
		ifNil: [ self assert: aCompleter entries isNil ]
		ifNotNil: [ self assert: (self entriesToCompareFrom: aCompleter entries) = (self entriesToCompareFrom: entries) ]
		
		
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesAreSelectorsForUnknownClasses: aCompleter  [
	
	| expectedEntries expectedPossibleInvalidSelectors |
	expectedPossibleInvalidSelectors _ Set new.
	expectedEntries _ aCompleter computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: expectedPossibleInvalidSelectors.
	
	self
		assert: aCompleter
		analizedSelectorsFrom: #()
		canShowDocumentation: true
		detectedPossibleInvalidSelector: expectedPossibleInvalidSelectors
		suggested: expectedEntries
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: sourceCode binding: aVariableName to: aValue [
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: sourceCode  [
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: aCompiledMethod  [
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: aCompiledMethod.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.
	
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areSelectorsOf: aClass [
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areAllSelectorsOf: aClass.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areUnaryAndBinarySelectorsOf: aClass  [
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areUnaryAndBinarySelectorsOf: aClass.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesOf: aCompleter areAllSelectorsOf: aClass [

	self
		assertEntriesOf: aCompleter
		areAllSelectorsOf: aClass
		beginningWith: ''
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesOf: aCompleter areAllSelectorsOf: aClass beginningWith: aPrefix [

	self assertEntriesOf: aCompleter areAllSelectorsOfAll: { aClass } beginningWith: aPrefix

]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesOf: aCompleter areAllSelectorsOfAll: classes beginningWith: aPrefix [

	| suggested |

	suggested := classes inject: Set new into: [ :suggestedCollector :aClass |
		suggestedCollector 
			addAll: (aCompleter selectorsOf: aClass beginningWith: aPrefix);
			yourself ].
	
	self
		assert: aCompleter
		analizedSelectorsFrom: classes
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: suggested.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesOf: aCompleter areIdentifiersBeginningWith: aPrefix [

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (aCompleter computeIdentifierEntriesBeginningWith: aPrefix)
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesOf: aCompleter areUnaryAndBinarySelectorsOf: aClass [

	self
		assert: aCompleter
		analizedSelectorsFrom: {aClass}
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: (self using: aCompleter addUnaryAndBinarySelectorsOf: aClass).
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areIdentifiersBeginningWith: aPrefix [
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: aPrefix.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass [
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOf: aClass
		beginningWith: ''
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass beginningWith: aPrefix [
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes [
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOfAll: classes
		beginningWith: ''
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes beginningWith: aPrefix [
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOfAll: classes
		beginningWith: aPrefix 
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsing: sourceCode areUnaryAndBinarySelectorsOf: aClass [
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areUnaryAndBinarySelectorsOf: aClass.
	
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass [
	
	self
		assertEntriesWhenBrowsingMethod: aMethod
		areSelectorsOf: aClass
		beginningWith: ''
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass beginningWith: aPrefix [
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> assertEntriesWhenBrowsingMethod: aMethod areUnaryAndBinarySelectorsOf: aClass [
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areUnaryAndBinarySelectorsOf: aClass.
	
	
]

{ #category : 'test support' }
SmalltalkCompleterTest >> autocompleteEntriesBrowsing: sourceCode [
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	completer computeEntries.
	
	^ completer
]

{ #category : 'test support' }
SmalltalkCompleterTest >> autocompleteEntriesBrowsingMethod: aCompiledMethod  [
	
	| completer selector |
	
	selector := aCompiledMethod selector.
	completer := self createCompleterForBrowsing: aCompiledMethod sourceCode.
	"Not nice, but does it work - Hernan"
	completer textProviderOrModel instVarNamed: 'currentCompiledMethod' put: aCompiledMethod.
	completer textProviderOrModel selectedMessageName: selector.
	
	completer computeEntries.
	
	^ completer
]

{ #category : 'test support' }
SmalltalkCompleterTest >> autocompleteEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue  [
	
	| completer |
	completer := self
		createCompleterForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
		
	completer computeEntries.
	
	^ completer
]

{ #category : 'test support' }
SmalltalkCompleterTest >> browseClass: aClass  [
	
	classToBrowse _ aClass.
]

{ #category : 'test objects' }
SmalltalkCompleterTest >> createCompleterForBrowsing: sourceCode  [
	
	| browser model |
	browser := Browser new.
	browser setSelectedSystemCategoryTreeItem: (SystemCategoryWrapper with: classToBrowse category name: classToBrowse category model: browser).
	browser classListIndex: (browser classList indexOf: classToBrowse).
	
	model := (PluggableTextModel on: browser) actualContents: sourceCode.
	browser editSelection: #newMessage.

	^ self createCompleterWith: model.
]

{ #category : 'test objects' }
SmalltalkCompleterTest >> createCompleterForWorkspaceWith: sourceCode binding: aVariableName to: aValue [
	
	| model |
	model := Workspace withText: sourceCode.
	(model bindingOf: aVariableName) value: aValue.
	
	^ self createCompleterWith: model
]

{ #category : 'test objects' }
SmalltalkCompleterTest >> createCompleterWith: aModel [
	
	^ (SmalltalkCompleter withModel: aModel)
		changePositionTo: aModel actualContents size;
		yourself
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> denyComputingEntriesIsSupportedWhenBrowsing: sourceCode  [
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	self
		should: [ completer computeEntries ]
		raise: Error.
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: nil.
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> denyHasEntries: aCompleter   [

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: #()
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> entriesToCompareFrom: aCollectionOfEntries [

	^(aCollectionOfEntries reject: [ :anEntry | 
		anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader ]) asSet
]

{ #category : 'test objects' }
SmalltalkCompleterTest >> firstKeyword: a secondKeyword: b [

]

{ #category : 'test objects' }
SmalltalkCompleterTest >> m1: arg1 arg1  [

]

{ #category : 'test objects' }
SmalltalkCompleterTest >> m2: arg1 1 + arg1  [

]

{ #category : 'test objects' }
SmalltalkCompleterTest >> m3: arg1 arg1 [

]

{ #category : 'running' }
SmalltalkCompleterTest >> performTest [

	^SmalltalkCompleter changeEntriesLimitTo: SmallInteger maxVal during: [ super performTest ]
]

{ #category : 'setUp/tearDown' }
SmalltalkCompleterTest >> setUp [

	super setUp.
	
	self browseClass: SmalltalkCompleterTest.
	

]

{ #category : 'tests' }
SmalltalkCompleterTest >> test000_AutocompletesMessagesFor_GlobalVariables_WithSelectorsFromTheirClasses [
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTest '
		areSelectorsOf: SmalltalkCompleterTest class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < SmalltalkCompleterTest '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest class.
	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test001_AutocompletesMessagesFor_Self_WithSelectorsFromTheBrowsedClass [
	
	self
		assertEntriesWhenBrowsing: 'm1 self '
		areSelectorsOf: SmalltalkCompleterTest.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < self '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test002_AutocompletesMessagesFor_Super_WithSelectorsFromTheBrowsedClass [
	
	self
		assertEntriesWhenBrowsing: 'm1 super '
		areSelectorsOf: TestCase.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < super '
		areUnaryAndBinarySelectorsOf: TestCase.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test003_AutocompletesMessagesFor_Super_WithSelectorsForUnknownClassesWhenTheBrowsedClassDoesNotHaveASuperclass [
	
	self browseClass: ProtoObject.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 super '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < super '.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test004_AutocompletesMessagesFor_True_WithSelectorsFromTrue [
	
	self
		assertEntriesWhenBrowsing: 'm1 true '
		areSelectorsOf: True.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < true '
		areUnaryAndBinarySelectorsOf: True.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test005_AutocompletesMessagesFor_False_WithSelectorsFromFalse [
	
	self
		assertEntriesWhenBrowsing: 'm1 false '
		areSelectorsOf: False.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < false '
		areUnaryAndBinarySelectorsOf: False.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test006_AutocompletesMessagesFor_Nil_WithSelectorsFromUndefinedObject [
	
	self
		assertEntriesWhenBrowsing: 'm1 nil '
		areSelectorsOf: UndefinedObject.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < nil '
		areUnaryAndBinarySelectorsOf: UndefinedObject.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test007_AutocompletesMessagesFor_Characters_WithSelectorsFromCharacter [
	
	self
		assertEntriesWhenBrowsing: 'm1 $a '
		areSelectorsOf: $m class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < $a '
		areUnaryAndBinarySelectorsOf: $m class.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test008_AutocompletesMessagesFor_Numbers_WithSelectorsFromTheirClass [
	
	self
		assertEntriesWhenBrowsing: 'm1 1 '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < 1 '
		areUnaryAndBinarySelectorsOf: SmallInteger.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test009_AutocompletesMessagesFor_Strings_WithSelectorsFromString [
	
	self
		assertEntriesWhenBrowsing: 'm1 ''a'' '
		areSelectorsOf: 'm' class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ''a'' '
		areUnaryAndBinarySelectorsOf: 'm' class	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test010_AutocompletesMessagesFor_Symbols_WithSelectorsFromSymbol [
	
	self
		assertEntriesWhenBrowsing: 'm1 #a '
		areSelectorsOf: Symbol.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #a '
		areUnaryAndBinarySelectorsOf: Symbol.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test011_AutocompletesMessagesFor_StringSymbol_WithSelectorsFromSymbol [
	
	self
		assertEntriesWhenBrowsing: 'm1 #''a'' '
		areSelectorsOf: Symbol.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < #''a'' '
		areUnaryAndBinarySelectorsOf: Symbol.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test012_AutocompletesMessagesFor_InstanceVariables [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 instanceVariable1 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + instanceVariable1 '.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test013_AutocompletesMessagesFor_MethodArguments_WithSelectorsForUnknownClasses [
	
	"I reference to SmalltalkCompleterTest directly and not thru 'self class' because this test has subclasses
	the #m1: and #m2: are defined in SmalltalkCompleterTest - Hernan"
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m1:.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m2: 
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test014_AutocompletesMessagesFor_TemporaryVariables_WithSelectorsForUnknownClasses [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| 1 + a '.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test015_AutocompletesMessagesFor_BlockArguments_WithSelectorsForUnknownClasses [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | 1 + a '.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test016_AutocompletesMessagesFor_BlockTemporaryVariables_WithSelectorsForUnknownClasses [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | a '.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | 1 + a '.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test017_AutocompletesMessagesFor_NilWorkspaceVariables_WithSelectorsForUnknownClasses [
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: 'x '
		binding: 'x'
		to: nil.
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: '1 + x '
		binding: 'x'
		to: nil.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test017_AutocompletesMessagesFor_NonNilWorkspaceVariables_WithSelectorsFromTheirClasses [
	
	self
		assertEntriesForWorkspaceWith: 'x '
		binding: 'x'
		to: 1
		areSelectorsOf: SmallInteger.
		
	self
		assertEntriesForWorkspaceWith: '1 < x '
		binding: 'x'
		to: 1
		areUnaryAndBinarySelectorsOf: SmallInteger.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test018_AutocompletesMessagesFor_ThisContext_WithSelectorsFromMethodContext [
	
	self
		assertEntriesWhenBrowsing: 'm1 thisContext '
		areSelectorsOf: MethodContext.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < thisContext '
		areUnaryAndBinarySelectorsOf: MethodContext.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test019_AutocompletesMessagesFor_ClassVariables_WithSelectorsFromTheirClasses [
	
	ClassVariableForTesting _ 1.
		
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTesting '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ClassVariableForTesting '
		areUnaryAndBinarySelectorsOf: SmallInteger.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test020_AutocompletesMessagesFor_PoolConstants_WithSelectorsFromTheirClasses [
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < X '
		areUnaryAndBinarySelectorsOf: SmallInteger.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test021_AutocompletesMessagesFor_BlockEndings_WithSelectorsFromBlockClosure [
	
	self
		assertEntriesWhenBrowsing: 'm1 [] '
		areSelectorsOf: BlockClosure.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < [] '
		areUnaryAndBinarySelectorsOf: BlockClosure.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test022_AutocompletesMessagesFor_ArrayEnds_WithSelectorsFromArray [
	
	self
		assertEntriesWhenBrowsing: 'm1 #() '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #() '
		areUnaryAndBinarySelectorsOf: Array.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test023_AutocompletesMessagesFor_RightBraces_WithSelectorsFromArray [
	
	self
		assertEntriesWhenBrowsing: 'm1 {} '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < {} '
		areUnaryAndBinarySelectorsOf: Array.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test023_AutocompletingMessagesFor_UnaryMessages [
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < self class '.	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test024_AutocompletingMessagesFor_RightParenthesis [
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 (1) '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < (1) '.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test025_AutocompletesMessagesFor_UnknownIdentifiers_WithSelectorsForUnknownClasses [
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 Foo1234 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 foo1234 '.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test026_AutocompletingMessagesFor_Cascade [
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class; '.
	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test027_AutocompletesEntriesFor_GlobalVariables_WithIdentifiersBegginingWithThem [
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTes'
		areIdentifiersBeginningWith: 'SmalltalkCompleterTest'.
	
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test028_AutocompletesEntriesFor_PoolConstants_WithIdentifiersBegginingWithThem [
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X'
		areIdentifiersBeginningWith: 'X'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test029_AutocompletesEntriesFor_WorkspaceVariables_WithIdentifiersBegginingWithThem [
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: 'x'
		binding: 'x'
		to: 1. 
		
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: 'x'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test030_AutocompletesEntriesFor_ClassVariables_WithIdentifiersBegginingWithThem [
	
	ClassVariableForTesting _ 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTestin'
		areIdentifiersBeginningWith: 'ClassVariableForTesting'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test031_AutocompletesEntriesFor_InstanceVariables_WithIdentifiersBegginingWithThem [
	
	self
		assertEntriesWhenBrowsing: 'm1 instanceVariable'
		areIdentifiersBeginningWith: 'instanceVariable1'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test032_AutocompletesEntriesFor_MethodArguments_WithIdentifiersBegginingWithThem [
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: SmalltalkCompleterTest >> #m3:.
	
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (completer computeIdentifierEntriesBeginningWith: 'arg1').
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test033_AutocompletesEntriesFor_TemporaryVariables_WithIdentifiersBegginingWithThem [
	
	self
		assertEntriesWhenBrowsing: 'm1 |xx| x'
		areIdentifiersBeginningWith: 'xx'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test034_AutocompletesEntriesFor_BlockArguments_WithIdentifiersBegginingWithThem [
	
	self
		assertEntriesWhenBrowsing: 'm1 [ :xx | x'
		areIdentifiersBeginningWith: 'xx'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test035_AutocompletesEntriesFor_BlockTemporaryVariables_WithIdentifiersBegginingWithThem [
	
	self
		assertEntriesWhenBrowsing: 'm1 [ | xx | x'
		areIdentifiersBeginningWith: 'xx'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test036_AutocompletesEntriesFor_IncompleteIdentifiers_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 Obj'
		areIdentifiersBeginningWith: 'Obj'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test037_AutocompletesEntriesFor_ReservedNames_WithIdentifiersBegginingWithThem [

	self assertEntriesWhenBrowsing: 'm1 self' areIdentifiersBeginningWith: 'self'.
	self assertEntriesWhenBrowsing: 'm1 supe' areIdentifiersBeginningWith: 'super'.
	self assertEntriesWhenBrowsing: 'm1 tru' areIdentifiersBeginningWith: 'true'.
	self assertEntriesWhenBrowsing: 'm1 fals' areIdentifiersBeginningWith: 'false'.
	self assertEntriesWhenBrowsing: 'm1 ni' areIdentifiersBeginningWith: 'nil'.
	self assertEntriesWhenBrowsing: 'm1 thisContex' areIdentifiersBeginningWith: 'thisContext'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test038_DoesNotAutocompleteEntriesFor_SameUndefinedIdentifier [

	| completer |
	completer := self createCompleterForBrowsing: 'asddas: kdkk ads'.
	
	completer computeEntries.
	
	self denyHasEntries: completer 
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test038b_AutocompleteEntriesFor_UndefinedIdentifiers [

	| completer undefinedIdentifierEntries |
	
	completer := self createCompleterForBrowsing: 'm1 
	| otherIdentifier |
	identifier1 + identifier2 + otherIdentifier.
	ident'.
	
	completer computeEntries.
	undefinedIdentifierEntries := self entriesToCompareFrom: completer entries.
	self assert: undefinedIdentifierEntries size equals: 2.
	self assert: (undefinedIdentifierEntries includes: 'identifier1').
	self assert: (undefinedIdentifierEntries includes: 'identifier2').
	self deny: completer canShowSelectorDocumentation
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test039_AutocompletesEntriesFor_BinaryMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 1 ~='
		areSelectorsOf: SmallInteger
		beginningWith: '~='
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test040_AutocompletesEntriesFor_IncompleteBinaryMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 1 ~'
		areSelectorsOf: SmallInteger
		beginningWith: '~'
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test041_AutocompletesEntriesFor_UnaryMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 1 not'
		areSelectorsOf: SmallInteger
		beginningWith: 'not'
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test042_AutocompletesEntriesFor_IncompleteUnaryMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 1 no'
		areSelectorsOf: SmallInteger
		beginningWith: 'no'
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test043_AutocompletesEntriesFor_KeywordMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 1 at:'
		areSelectorsOf: SmallInteger
		beginningWith: 'at:'
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test044_AutocompletesEntriesFor_IncompleteKeywordMessages_WithIdentifiersBegginingWithThem [

	self
		assertEntriesWhenBrowsing: 'm1 self firstKeyword:'
		areSelectorsOf: SmalltalkCompleterTest
		beginningWith: 'firstKeyword:'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test045_AutocompletesEntriesFor_patternUnary_containsExactlyTwoMethods [

	| completer |
	completer := self createCompleterForBrowsing: 'fl'.
	
	completer computeEntries.

	self assert: completer entries size equals: 2.
	self assert: completer entries includes: 'flag: aSymbol'.
	self assert: completer entries includes: 'flash'.
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test046_AutocompletesEntriesFor_patternUnary_notContainsAnyMethod [

	| completer |
	completer := self createCompleterForBrowsing: 'flagggg'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test047_AutocompletesEntriesFor_patternUnary_doesNotAutocompleteMethodOfSameClass [

	| completer |
	completer := self createCompleterForBrowsing: 'm1'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test048_AutocompletesEntriesFor_patternKeyword_containsExactlyOneMethod [

	| completer |
	completer := self createCompleterForBrowsing: 'flag:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 1.
	self assert: completer entries includes: 'flag: aSymbol'

]

{ #category : 'tests' }
SmalltalkCompleterTest >> test049_AutocompletesEntriesFor_patternKeyword_notContainsAnyMethod [

	| completer |
	completer := self createCompleterForBrowsing: 'flagg:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test050_AutocompletesEntriesFor_patternKeyword_doesNotAutocompleteMethodOfSameClass [

	| completer |
	completer := self createCompleterForBrowsing: 'm1:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test051_AutocompletesEntriesFor_patternBinary_containsExactlyOneMethod [

	| completer |
	completer := self createCompleterForBrowsing: '->'.
	
	completer computeEntries.

	self assert: completer entries size equals: 1.
	self assert: completer entries includes: '-> anObject'
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test052_AutocompletesEntriesFor_patternBinary_notContainsAnyMethod [

	| completer |
	completer := self createCompleterForBrowsing: '*'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'tests' }
SmalltalkCompleterTest >> test053_AutocompletesEntriesFor_patternBinary_doestNotAutocompleteMethodOfSameClass [

	| completer |
	completer := self createCompleterForBrowsing: '+'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0
]

{ #category : 'testing' }
SmalltalkCompleterTest >> testMessages [
	"
	SmalltalkCompleterTest new testMessages
	"
	| fromSmalltalk fromUCompleter |
	fromSmalltalk := Smalltalk allImplementedMessages.
	fromUCompleter := Symbol allSymbols select: [ :s |
		SmalltalkCompleter isThereAnImplementorOf: s].

	"Some tests use #doSilently: when removing classes.
	This means that selectors may be garbage collected and not removed from SmalltalkCompleter."
	"self assert: fromSmalltalk = fromUCompleter asSet."
	self assert: (fromUCompleter includesAllOf: fromSmalltalk).
]

{ #category : 'assertions' }
SmalltalkCompleterTest >> using: aCompleter addUnaryAndBinarySelectorsOf: aClass [

	^ aCompleter unaryAndBinarySelectorsOf: aClass beginningWith: ''
]
