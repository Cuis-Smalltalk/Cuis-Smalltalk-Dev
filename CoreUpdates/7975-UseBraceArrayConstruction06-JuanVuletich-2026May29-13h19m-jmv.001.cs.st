'From Cuis7.7 [latest update: #7970] on 29 May 2026 at 1:34:56 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 12/Aug/2025 13:08:14'!
binaryPattern

	^ self advanceWithRangeDo: [ :selectorAsString :selectorRange | | selectorsymbol arguments |
		selectorsymbol := selectorAsString asSymbol.
		self selector: selectorsymbol.
		self argumentNameWithRangeDo: [ :argumentName :argumentRange |
		arguments := {encoder bindArg: argumentName range: argumentRange}.
		{selectorsymbol. arguments. 2. {selectorRange}}]]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 26/Jul/2023 18:02:25'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |

	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: [
			start := self startOfNextToken.
			selector := source collectionSpecies writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue:
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [

			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue:
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := {parseNode}.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue:
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
				optimizeSpecialSends: optimizeSpecialSends;
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 11/Nov/2018 14:26:13'!
withRangesOf: nodeAtRange including: aPosition do: aBlock

	| currentNodeRange ranges |

	currentNodeRange := nodeAtRange value.
	ranges := currentNodeRange isInterval ifTrue: [ {currentNodeRange} ] ifFalse: [ currentNodeRange ].

	ranges do: [ :aRange | (aRange includes: aPosition) ifTrue: [ aBlock value: aRange ]].

	! !


!BlockNode methodsFor: 'initialization' stamp: 'jmv 13/May/2014 07:04:00'!
arguments: argNodes statements: statementsCollection returns: returnBool from: encoder
	"Compile."
	arguments := argNodes.
	statements := statementsCollection size > 0
		ifTrue: [ statementsCollection ]
		ifFalse: [ {NodeNil} ].
	optimized := false.
	returns := returnBool! !

!BlockNode methodsFor: 'initialization' stamp: 'eem 20/May/2008 13:40:00'!
statements: statementsCollection returns: returnBool 
	"Decompile."

	| returnLast |
	returnLast := returnBool.
	returns := false.
	statements := 
		(statementsCollection size > 1 
			and: [(statementsCollection at: statementsCollection size - 1) 
					isReturningIf])
				ifTrue: 
					[returnLast := false.
					statementsCollection allButLast]
				ifFalse: [statementsCollection size = 0
						ifTrue: [{NodeNil}]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]! !


!MessageNode methodsFor: 'code generation' stamp: 'eem 5/Aug/2014 18:25:00'!
sizeCodeForRepeat: encoder value: forValue 
	"L1: ... Jmp(L1) nil (nil for value only);"
	| loopSize |
	"We assume long backward branches are always maximal size branches."
	loopSize := (receiver sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	sizes := {loopSize}.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 20/Aug/2016 20:16:06'!
transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := SelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := {(BlockNode withJust: NodeNil) noteOptimizedIn: self}.
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 28/Mar/2011 19:04:00'!
areasOutside: aRectangle
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle."

	| areas |
	"Make sure the intersection is non-empty"
     (self intersects: aRectangle)
		ifFalse: [^{self}].
	areas := OrderedCollection new.
	self areasOutside: aRectangle do: [ :r |  areas add: r ].
	^areas! !


!ClassDescription methodsFor: 'accessing class hierarchy' stamp: 'di 17/Feb/2000 22:36:00'!
classesThatImplementAllOf: selectorSet
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	found := OrderedCollection new.
	selectorSet do:
		[:sel | (self methodDict includesKey: sel) ifTrue: [found add: sel]].
	found isEmpty
		ifTrue: [^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: selectorSet)]]
		ifFalse: [remaining := selectorSet copyWithoutAll: found.
				remaining isEmpty ifTrue: [^ {self}].
				^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: remaining)]]! !


!Class methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 12/May/2025 17:16:17'!
addSubclass: aSubclass
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."

	aSubclass superclass ~~ self
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		subclasses := {aSubclass}.
		^self].
	subclasses do: [ :cl | cl == aSubclass ifTrue: [ ^self ]]. "Already my subclass"
	subclasses := Array withAll: subclasses with: aSubclass.! !

!Class methodsFor: 'pool variables' stamp: 'jmv 13/Sep/2023 18:07:13'!
addSharedPool: aSharedPool 
	"Add the argument, aSharedPool, as one of the receiver's shared pools. 
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"

	(self sharedPools includes: aSharedPool)
		ifTrue: [^self error: 'This is already in my shared pool list'].
	sharedPools := sharedPools
		ifNil: [{aSharedPool}]
		ifNotNil: [sharedPools copyWith: aSharedPool]! !


!Metaclass methodsFor: 'accessing' stamp: 'ar 11/Jul/1999 08:14:00'!
allInstances
	thisClass class == self ifTrue:[^{thisClass}].
	^super allInstances! !


!RunArray methodsFor: 'copying' stamp: 'jmv 4/Dec/2012 23:41:00'!
copyFrom: start to: stop
	| newRuns run1 run2 offset1 offset2 answer | 
	stop < start ifTrue: [
		answer := RunArray new.
		answer canJoinMessage: canJoinMessage.
		^answer ].
	self at: start setRunOffsetAndValue: [ :r :o :value1 |
		run1 := r.
		offset1 := o. 
		value1 ].
	self at: stop setRunOffsetAndValue: [ :r :o :value2 |
		run2 := r.
		offset2 := o.
		value2].
	run1 = run2
		ifTrue: [
			newRuns := {offset2 - offset1 + 1}]
		ifFalse: [
			newRuns := runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1 ].
	answer := RunArray runs: newRuns values: (values copyFrom: run1 to: run2).
	answer canJoinMessage: canJoinMessage.
	^answer! !


!CharacterSequence methodsFor: 'accessing' stamp: 'tk 10/Aug/2000 09:14:00'!
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens := OrderedCollection new.
	separators := delimiters isCharacter
		ifTrue: [{delimiters}]
		ifFalse: [delimiters].
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: separators startingAt: keyStop.
		keyStop := self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !


!Browser methodsFor: 'class functions' stamp: 'jmv 21/May/2022 22:29:19'!
explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits := Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole := lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply := '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes := Smalltalk allClassesImplementing: whole.
			classes := 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil! !

!Browser methodsFor: 'system category list' stamp: 'jmv 8/Jun/2011 08:15:00'!
systemCategorySingleton

	^ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [{selectedSystemCategory}]! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'di 26/Apr/2000 20:20:00'!
systemCategorySingleton

	| cls |
	cls := self selectedClass.
	^ cls ifNil: [#()]
		ifNotNil: [{cls category}]! !


!RepeatedCodeMessageSet methodsFor: 'source code ranges' stamp: 'HAW 1/Dec/2025 16:11:00'!
messageSendsRangesOf: aSelector

	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^{replacement asSourceCodeInterval}! !


!ContextVariablesInspector methodsFor: 'accessing' stamp: 'jmv 1/Mar/2010 09:51:00'!
fieldList 
	"Refer to the comment in Inspector|fieldList."

	object
		ifNil: [^ {'thisContext'}].
	^fieldList ifNil:[fieldList := {'thisContext'. 'stack top'. 'all temp vars'} , object tempNames]! !


!ObjectExplorer methodsFor: 'accessing' stamp: 'nk 24/Jul/2003 09:16:00'!
getList

	^{ObjectExplorerWrapper with: rootObject name: 'root' model: self parent: nil}! !


!ReferencesExplorer methodsFor: 'references' stamp: 'jmv 21/Apr/2022 12:40:50'!
getList

	| wrapper |
	wrapper := ReferencesExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^{wrapper}! !


!WeightExplorer methodsFor: 'references' stamp: 'jmv 21/Apr/2022 12:40:54'!
getList

	| wrapper |
	wrapper := WeightExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^{wrapper}! !


!TestSuite class methodsFor: 'instance creation - private' stamp: 'HAW 7/Feb/2017 11:28:55'!
forNoTestMethod: aCompiledMethod 
	
	| allTestSenders testCaseClassesReferencingClass reducedTestSenders suite |
	
	allTestSenders := self allTestsSending: aCompiledMethod selector.
	testCaseClassesReferencingClass := aCompiledMethod methodClass 
		withTestCaseClassDo:  [:aTestCaseClass | {aTestCaseClass} ]
		ifNone: [ self allTestCaseClassesReferencing: aCompiledMethod methodClass ].
		
	reducedTestSenders := allTestSenders select: [ :aMethodReference | testCaseClassesReferencingClass includes: aMethodReference actualClass ].
	reducedTestSenders isEmpty 
		ifTrue: [ suite := self forClass: aCompiledMethod methodClass ]
		ifFalse: [ 
			suite := self named: 'Tests senders of ', aCompiledMethod selector.
			reducedTestSenders do: [ :aMethodReference | suite addTest: (aMethodReference actualClass selector: aMethodReference selector)]].
	
	^suite! !


!Color methodsFor: 'groups of shades' stamp: 'tk 18/Jun/1996 00:00:00'!
mix: color2 shades: thisMany
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red mix: Color green shades: 12)"

	| redInc greenInc blueInc rr gg bb c out |
	thisMany = 1 ifTrue: [^ {color2}].
	redInc := color2 red - self red / (thisMany-1).
	greenInc := color2 green - self green / (thisMany-1).
	blueInc := color2 blue - self blue / (thisMany-1).
	rr := self red.  gg := self green.  bb := self blue.
	out := (1 to: thisMany) collect: [:num |
		c := Color r: rr g: gg b: bb.
		rr := rr + redInc.
		gg := gg + greenInc.
		bb := bb + blueInc.
		c].
	out at: out size put: color2.	"hide roundoff errors"
	^ out
! !


!DropFilesAction methodsFor: 'evaluating - private' stamp: 'jmv 16/Sep/2019 22:52:28'!
createMenuFor: options

	| icons lines labels |

	"options is a small collection, no problem to collect twice - Hernan"	
	labels := options collect: [ :option | option label ].
	icons := options collect: [ :option | option iconSpec ].

	shouldAskToStop 
		ifTrue: [
			lines := {labels size}.
			labels add: 'stop here'.
			icons add: #cancelIcon ]
		ifFalse: [ lines := #() ].
		
	^PopUpMenu labelArray: labels lines: lines icons: icons! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 22/Aug/2021 14:12:10'!
splitNewMorphList: list depth: d
	| middle c prev next |
	d <= 0 ifTrue: [ ^ {list} ].
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
	(middle = 1 or: [ middle >= list size ]) ifTrue: [ ^ {list} ].
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


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 25/Sep/2011 15:46:00'!
rectanglesAt: y
	(y+1) > extentForComposing y ifTrue: [^ #()].
	^ {0 @ y corner: extentForComposing x @ (y+1)}! !


!ObjectFinalizer methodsFor: 'initialization' stamp: 'ar 19/May/2003 20:12:00'!
receiver: aReceiver selector: aSelector argument: anObject
	receiver := aReceiver.
	selector := aSelector.
	arguments := {anObject}! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 26/Nov/2024 10:25:33'!
explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last == $: ifFalse: [^ nil].
	"Name of this method"
	lits := {msg}.
	(whole := lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply := ', which is the selector of this very method!!'.
			s := self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole := lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply := '.'.
			s := self class plateB].
		classes := Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes := 'many classes']
			ifFalse: [classes := 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 18/Mar/2020 19:50:06'!
computeMessageEntriesWithBinaryMessageForClass: aClass 

	selectorsClasses := {aClass}.
	
	entries := self unaryAndBinarySelectorsOf: aClass beginningWith: prefix.
		! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 18/Mar/2020 19:51:12'!
computeMessageEntriesWithoutBinaryMessageForClass: aClass

	selectorsClasses := {aClass}.
	
	entries := self selectorsOf: aClass beginningWith: prefix.! !

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'NPM 17/Mar/2020 17:53:08'!
computeIdentifierEntriesBeginningWith: aPrefix
	"Use an aux Set to avoid duplicates, but keep the order given."

	| entriesSet lastTitle candidates |
	entriesSet := Set new.
	lastTitle := nil.
	
	candidates := Array streamContents: [ :strm |
		parser namesBeginningWith: aPrefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle := kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		^ {entriesSet anyOne} ]
		ifFalse: [ ^ candidates ]! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 12/Mar/2018 15:48:12'!
maxLengthPoints
	| max points |
	max := self maxLength.
	max = 0 ifTrue: [^{`0 @ 0`}].
	points := OrderedCollection new.
	tally withIndexesDo: [:i :j :t | t = max ifTrue: [points add: i @ j]].
	^ points! !


!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 28/Aug/2022 21:49:46'!
messageSendsRangesOf: aSelector

	"To use polimorphysm with MethodReference - Fernando"

	^self isMessageListTitle ifTrue: [#()] ifFalse: [{completeSourceRange}]! !


!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/Jan/2019 08:46:41'!
createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor 
		
	implementors := {anImplementor}.
	senders := #().
	shouldShowChanges := false.
	
	self createAndApplyRefactoring ! !


!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 19/Mar/2025 15:34:26'!
createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor

	implementors := {anImplementor}.
	messageSends := #().
	shouldShowChanges := false.

	self createAndApplyRefactoring ! !

