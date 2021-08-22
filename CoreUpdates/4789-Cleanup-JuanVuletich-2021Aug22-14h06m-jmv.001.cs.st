'From Cuis 5.0 [latest update: #4788] on 22 August 2021 at 2:12:35 pm'!

!TextModel methodsFor: 'object serialization' stamp: 'jmv 8/22/2021 13:55:14'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	"Maybe old instances won't have this variable set."
	undoRedoCommands ifNil: [
		undoRedoCommands _ ReadWriteStream on: #() ]! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 8/22/2021 13:55:23'!
flushUndoRedoCommands

	undoRedoCommands _ ReadWriteStream on: #().
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 8/22/2021 13:55:29'!
initialize
	"Initialize the state of the receiver with its default contents."

	actualContents _ '' asText.
	undoRedoCommands _ ReadWriteStream on: #().
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'copying' stamp: 'jmv 8/22/2021 13:55:34'!
postCopy
	super postCopy.
	actualContents _ actualContents copy.
	undoRedoCommands _ ReadWriteStream on: #().
	lastEditTimeStamp _ nil! !


!PluggableTextModel methodsFor: 'initialization' stamp: 'jmv 8/22/2021 13:55:38'!
initWith: aTextProvider
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."

	textProvider _ aTextProvider.
	undoRedoCommands _ ReadWriteStream on: #().
	lastEditTimeStamp _ nil! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 8/22/2021 13:56:25'!
runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: #().
	talliesStream _ WriteStream on: #().
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self triggerEvent: #decorateButtons.
	^result! !


!UndefinedObject methodsFor: 'class hierarchy' stamp: 'jmv 8/22/2021 14:07:11'!
subclasses
	"Return all the subclasses of nil"

	^Array streamContents: [ :classList |
		self subclassesDo: [ :class | classList nextPut: class ]].! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 8/22/2021 13:57:47'!
removeCategory: cat 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index _ categoryArray indexOf: cat ifAbsent: [^self].
	lastStop _ 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category'].
	categoryArray _ categoryArray copyReplaceFrom: index to: index with: #().
	categoryStops _ categoryStops copyReplaceFrom: index to: index with: #().
	categoryArray size = 0
		ifTrue:
			[categoryArray _ Array with: Default.
			categoryStops _ Array with: 0]
! !


!WordArray class methodsFor: 'as yet unclassified' stamp: 'jmv 8/22/2021 13:58:10'!
bobsTest
	| wa s1 s2 wa2 answer rawData |
"
WordArray bobsTest
"
	answer _ OrderedCollection new.
	wa _ WordArray with: 16r01020304 with: 16r05060708.
	{false. true} do: [ :pad |
		0 to: 3 do: [ :skip |
			s1 _ ReadWriteStream on: #[].

			s1 next: skip put: 0.		"start at varying positions"
			wa writeOn: s1.
			pad ifTrue: [s1 next: 4-skip put: 0].	"force length to be multiple of 4"

			rawData _ s1 contents.
			s2 _ ReadWriteStream with: rawData.
			s2 reset.
			s2 skip: skip.			"get to beginning of object"
			wa2 _ WordArray newFromStream: s2.
			answer add: {
				rawData size. 
				skip. 
				wa2 = wa. 
				wa2 asArray collect: [ :each | each printStringBase: 16]
			}
		].
	].
	^answer explore! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/22/2021 14:01:34'!
obsoleteMethodReferences
	"
	Smalltalk obsoleteMethodReferences
	Smalltalk browseObsoleteMethodReferences
	Open a browser on all referenced behaviors that are obsolete"

	| obsClasses obsRefs |
	^Array streamContents: [ :references |
		obsClasses _ self obsoleteBehaviors.
		'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
			from: 1 to: obsClasses size during: [ :barBlock |
		obsClasses keysAndValuesDo: [ :index :each |
			barBlock value: index.
			obsRefs _ self pointersTo: each except: obsClasses.
			obsRefs do: [ :ref |
				"Figure out if it may be a global"
				(ref isVariableBinding and: [ ref key isString "or Symbol" ]) ifTrue: [
					(self pointersTo: ref) do: [ :meth |
						(meth is: #CompiledMethod) ifTrue: [
							meth methodReference ifNotNil: [ :mref |
								(mref isValid and: [ mref compiledMethod == meth]) ifTrue: [
									references nextPut: mref ]]]]]]]]].! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/22/2021 13:56:35'!
listBuiltinModules
	"Smalltalk listBuiltinModules"
	"Return a list of all builtin modules (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library. The list will include all builtin plugins regardless of whether they are currently loaded or not. Note that the list returned is not sorted!!"
	| modules index name |
	modules _ WriteStream on: #().
	index _ 1.
	[true] whileTrue:[
		name _ self listBuiltinModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/22/2021 13:56:40'!
listLoadedModules
	"Smalltalk listLoadedModules"
	"Return a list of all currently loaded modules (e.g., plugins). Loaded modules are those that currently in use (e.g., active). The list returned will contain all currently active modules regardless of whether they're builtin (that is compiled with the VM) or external (e.g., residing in some external shared library). Note that the returned list is not sorted!!"
	| modules index name |
	modules _ WriteStream on: #().
	index _ 1.
	[true] whileTrue:[
		name _ self listLoadedModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'jmv 8/22/2021 13:55:58'!
truncateAtPosition
	"Truncate the receiver at current position.
	For example, this should evaluate to true:
		| s |
		s _ ReadWriteStream on: #().
		s nextPutAll: 'abcdefg'.
		s reset.
		s next; next.
		s nextPut: $z.
		s truncateAtPosition.
		s atEnd
	"
	readLimit _ position! !


!Parser methodsFor: 'primitives' stamp: 'jmv 8/22/2021 14:01:58'!
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library."
	| descriptorClass callType modifier retType externalName args argType module fn |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil ifTrue: [^self expected:'return type'].
	"Parse function name or index"
	externalName := here.
	(self match: #string) 
		ifTrue:[externalName := externalName asSymbol]
		ifFalse:[(self match:#number) ifFalse:[^self expected:'function name or index']].
	(self match: #leftParenthesis) ifFalse:[^self expected:'argument list'].
	args := WriteStream on: #().
	[self match: #rightParenthesis] whileFalse:[
		argType := self externalType: descriptorClass.
		argType == nil ifTrue:[^self expected:'argument'].
		argType isVoid & argType isPointerType not ifFalse:[args nextPut: argType]].
	(self matchToken: 'module:') ifTrue:[
		module := here.
		(self match: #string) ifFalse:[^self expected: 'String'].
		module := module asSymbol].
	Smalltalk at: #ExternalLibraryFunction ifPresent:[:xfn|
		fn := xfn name: externalName 
				module: module 
				callType: callType
				returnType: retType
				argumentTypes: args contents.
		self allocateLiteral: fn].
	(self matchToken: 'error:')
		ifTrue:
			[| errorCodeVariable |
			 errorCodeVariable := here.
			(hereType == #string
			 or: [hereType == #word]) ifFalse:[^self expected: 'error code (a variable or string)'].
			 self advance.
			 self addPragma: (Pragma keyword: #primitive:error: arguments: (Array with: 120 with: errorCodeVariable)).
			 fn ifNotNil: [fn setErrorCodeName: errorCodeVariable]]
		ifFalse:
			[self addPragma: (Pragma keyword: #primitive: arguments: #(120))].
	^true! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 8/22/2021 14:10:26'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes _ Array streamContents: [ :strm |
				Smalltalk
					allBehaviorsDo: [:each | (each sharedPools
							detect: 
								[:pool | 
								pool == reply]
							ifNone: nil)
							notNil ifTrue: [strm nextPut: each]]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/22/2021 14:12:10'!
splitNewMorphList: list depth: d
	| middle c prev next |
	d <= 0 ifTrue: [ ^ Array with: list ].
	middle := list size // 2 + 1.
	c := (list at: middle) name first: 3.
	prev := middle - 1.
	[
	prev > 0 and: [ ((list at: prev) name first: 3) = c ]] whileTrue: [ prev := prev - 1 ].
	next := middle + 1.
	[
	next <= list size and: [ ((list at: next) name first: 3) = c ]] whileTrue: [ next := next + 1 ].
	"Choose the better cluster"
	middle := middle - prev < (next - middle)
		ifTrue: [ prev + 1 ]
		ifFalse: [ next ].
	middle = 1 ifTrue: [ middle := next ].
	middle >= list size ifTrue: [ middle := prev + 1 ].
	(middle = 1 or: [ middle >= list size ]) ifTrue: [ ^ Array with: list ].
	^ Array streamContents: [ :out |
		out nextPutAll:
			(self
				splitNewMorphList:
					(list
						copyFrom: 1
						to: middle - 1)
				depth: d - 1).
		out nextPutAll:
			(self
				splitNewMorphList:
					(list
						copyFrom: middle
						to: list size)
				depth: d - 1) ].! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 8/22/2021 13:58:32'!
organization
	^ SystemOrganizer defaultList: #().! !


!WeightTracer methodsFor: 'weight' stamp: 'jmv 8/22/2021 13:58:40'!
scanClosureSkipping: anArray

	self prepareToScanClosure.
	self skipInternalNodesAnd: #().
	self basicScanClosure.
	self prepareToWeighClosure.
	self skipInternalNodesAnd: anArray.
	self basicScanClosure.
	self cleanUpAfterScan! !

