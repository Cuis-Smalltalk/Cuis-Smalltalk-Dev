'From Cuis6.3 [latest update: #6208] on 6 February 2024 at 9:30:16 am'!

!Bag class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:07:32'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^ self new
		addAll: aCollection;
		yourself! !


!FileList methodsFor: 'file list menu' stamp: 'jmv 2/5/2024 17:58:33'!
itemsForNoFile

	| services |
	services := OrderedCollection new.
	services add: self serviceSortByName.
	services add: self serviceSortBySize.
	services add: (self serviceSortByDate useLineAfter: true).
	services add: self serviceAddNewFile.
	services add: self serviceAddNewDirectory.
	^ services

		! !


!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 2/5/2024 17:58:38'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| delay |
	self stopMonitoring.
	watcher := [
		delay := Delay forMilliseconds: msecs truncated.
		[
			| thisTally |
			thisTally := IdentityBag new.
			secs * 1000 // msecs timesRepeat: [
				"Could happen if process is halted and resumed."
				delay beingWaitedOn ifTrue: [ delay := Delay forMilliseconds: delay delayDuration ].
				delay wait.
				thisTally add: Processor nextReadyProcess ].
			tally := thisTally.
			aBoolean ifTrue: [ self findThePig ]
		] repeat
	] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 2/5/2024 17:58:44'!
leavesPrintExactOn: aStream

	| dict |
	dict := IdentityDictionary new.
	self leavesInto: dict fromSender: nil.
	dict asArray sort
		do: [ :node |
			node printOn: aStream.
			node printSenderCountsOn: aStream ]! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 2/5/2024 17:58:49'!
elementCategoryDict
	| dict firstIndex lastIndex |
	elementArray ifNil: [^ nil].
	dict := Dictionary new.
	1to: categoryStops size do: [:cat |
		firstIndex := self firstIndexOfCategoryNumber: cat.
		lastIndex := self lastIndexOfCategoryNumber: cat.
		firstIndex to: lastIndex do: [:el |
			dict at: (elementArray at: el) put: (categoryArray at: cat)].
	].
	^ dict.! !


!ClassBuilder methodsFor: 'validation' stamp: 'jmv 2/5/2024 17:58:56'!
validateClassvars: classVarArray from: oldClass forSuper: newSuper
	"Check if any of the classVars of oldClass conflict with the new superclass"
	| usedNames classVars temp |
	classVarArray isEmpty ifTrue:[^true]. "Okay"

	"Validate the class var names"
	usedNames := classVarArray asSet.
	usedNames size = classVarArray size 
		ifFalse:[	classVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp := var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp := var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames := newSuper allClassVarNames asSet.
		classVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl classVarNames includes: iv) ifTrue:[temp := cl]].
				self error: iv, ' is already defined in ', temp name.
				^false]]].

	oldClass ifNotNil: [
		usedNames := Set new.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl classVarNames].
		classVars := classVarArray.
		newSuper ifNotNil: [ classVars := classVars, newSuper allClassVarNames asArray].
		classVars do: [ :iv |
			(usedNames includes: iv) ifTrue: [
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true! !

!ClassBuilder methodsFor: 'validation' stamp: 'jmv 2/5/2024 17:59:04'!
validateInstvars: instVarArray from: oldClass forSuper: newSuper
	"Check if any of the instVars of oldClass conflict with the new superclass"
	| instVars usedNames temp |
	instVarArray isEmpty ifTrue:[^true]. "Okay"
	newSuper allowsSubInstVars ifFalse: [
		self error: newSuper printString, ' does not allow subclass inst vars. See allowsSubInstVars.'. ^ false].

	"Validate the inst var names"
	usedNames := instVarArray asSet.
	usedNames size = instVarArray size 
		ifFalse:[	instVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp := var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp := var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames := newSuper allInstVarNames asSet.
		instVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl instVarNames includes: iv) ifTrue:[temp := cl]].
				self error: iv,' is already defined in ', temp name.
				^false]]].
	oldClass ifNotNil: [
		usedNames := Set new.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl instVarNames].
		instVars := instVarArray.
		newSuper ifNotNil: [instVars := instVars, newSuper allInstVarNames].
		instVars do:[:iv|
			(usedNames includes: iv) ifTrue:[
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true! !


!Decompiler methodsFor: 'public access' stamp: 'jmv 2/5/2024 17:59:21'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method primitive ~= 0 and: [self skipCallPrimitive; willStore]) ifTrue:
				[pc := pc + (method encoderClass bytecodeSize: self firstByte).
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method primitive > 0 ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen! !


!ProcessorScheduler methodsFor: 'CPU usage tally' stamp: 'jmv 2/5/2024 17:59:27'!
tallyCPUUsageFor: seconds every: msec
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	| tally sem delay endDelay |
	tally := IdentityBag new.
	delay := Delay forMilliseconds: msec truncated.
	endDelay := Delay forSeconds: seconds.
	endDelay schedule.
	sem := Semaphore new.
	[
		[ endDelay isExpired ] whileFalse: [
			delay wait.
			tally add: Processor nextReadyProcess
		].
		sem signal.
	] forkAt: self highestPriority named: 'Processor CPU Usage Tallier'.

	^[ sem wait. tally ]! !


!Collection methodsFor: 'converting' stamp: 'jmv 2/5/2024 19:13:56'!
asSortedCollection: aSortBlock 
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection := SortedCollection sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection! !


!Symbol class methodsFor: 'services' stamp: 'jmv 2/5/2024 17:59:33'!
possibleSelectorsFor: misspelled 
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first ss |
	lookupString := misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs := lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new].
	first := lookupString first.
	short := lookupString size - (lookupString size // 4 max: 3) max: 2.
	long := lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates := OrderedCollection new.
	self allSymbolTablesDo: [:s | (((ss := s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best := lookupString correctAgainst: candidates.
	((misspelled last ~= $:) and: [misspelled size > 1]) ifTrue: [
		binary := misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best! !


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 2/5/2024 17:59:39'!
literalStrings
	| litStrs |
	litStrs := OrderedCollection new.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs! !


!Heap class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:04:24'!
new
	^self basicNew setCollection: (Array new: 10)! !


!OrderedCollection methodsFor: 'copying' stamp: 'jmv 2/5/2024 18:00:25'!
copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	| targetCollection |
	endIndex < startIndex ifTrue: [^self species new ].
	targetCollection := self species new.
	startIndex to: endIndex do: [:index | targetCollection addLast: (self at: index)].
	^ targetCollection! !

!OrderedCollection methodsFor: 'copying' stamp: 'jmv 2/5/2024 18:01:25'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	startIndex := start.
	stopIndex := stop.
	start < 1
		ifTrue: [startIndex := stopIndex := 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex := stopIndex := self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [ self errorOutOfBounds ]]].
	newOrderedCollection := self species new.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 2/5/2024 18:00:09'!
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 2/5/2024 18:00:18'!
collect: aBlock from: fromIndex to: toIndex
	"Override superclass in order to use addLast:, not at:put:."
	| result |
	(fromIndex < 1 or:[toIndex + firstIndex - 1 > lastIndex])
		ifTrue: [^self errorNoSuchElement].
	result := self species new.
	firstIndex + fromIndex - 1 to: firstIndex + toIndex - 1 do:
		[:index | result addLast: (aBlock value: (array at: index))].
	^ result
! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 2/5/2024 18:05:33'!
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self species new.
	1 to: self size do:
		[:index | result addLast: (twoArgBlock value: (self at: index)
									value: (otherCollection at: index))].
	^ result! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 2/5/2024 18:05:44'!
with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self species new.
	1 to: self size do: [ :index |
		result addLast:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 2/5/2024 18:05:51'!
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (elementAndIndexBlock
			value: (array at: index)
			value: index - firstIndex + 1)].
	^ newCollection! !


!OrderedCollection class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:02:39'!
new
	^self basicNew setCollection: (Array new: 10)! !

!OrderedCollection class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:05:23'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^self new
		resetTo: 1;
		addAll: aCollection;
		yourself! !


!SortedCollection class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:06:11'!
sortBlock: aBlock 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^ self new sortBlock: aBlock! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 2/5/2024 18:09:12'!
keysSortedSafely
	"Answer a sorted Collection containing the receiver's keys."
	| sortedKeys |
	sortedKeys := OrderedCollection new.
	self keysDo: [:each | sortedKeys addLast: each].
	sortedKeys sort:
		[ :x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 2/5/2024 18:11:02'!
fillCaches
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames := IdentitySet new.
	nonClassNames := IdentitySet new.
	self keysAndValuesDo: [ :key :value |
		value isInMemory ifTrue: [
			"The key == value name test below addresses two separate issues:
				1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
				2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
			(value class isMeta and: [ key == value name ])
				ifTrue: [ classNames add: key ]
				ifFalse: [ nonClassNames add: key ] ] ].
	classNames := classNames asArray sort.
	nonClassNames := nonClassNames asArray sort.
	cachedClassNames := classNames.
	cachedNonClassNames := nonClassNames.
	^{ classNames. nonClassNames }! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:10:08'!
allGlobalRefsWithout: classesAndMessagesPair
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					(m isSymbol and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:10:14'!
allImplementedMessagesWithout: classesAndMessagesPair
	"Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages.  Note this reports messages that are in the absent selectors set."
	| messages absentClasses |
	messages := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
			ifTrue: [#()]
			ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
				messages addAll: cl selectors]].
	^ messages! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:10:20'!
allPrimitiveMessages
	"Answer an OrderedCollection of all the methods that are implemented by 
	primitives."

	| aColl method | 
	aColl := OrderedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel | 
			method := class compiledMethodAt: sel.
			method primitive ~= 0 ifTrue: [
				aColl addLast: class name , ' ' , sel 
					, ' ' , method primitive printString]]].
	^aColl! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:10:25'!
allPrimitiveMethodsInCategories: aList
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl := OrderedCollection new.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asPlainString) asPlainString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method := aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:10:31'!
allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
		"The following may be sent without being in any literal frame"
		1 to: self specialSelectorSize do: [ :index | 
			sent add: (self specialSelectorAt: index)].
	self presumedSentMessages do: [ :sel | sent add: sel].
	^ sent! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:11:09'!
unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all := IdentitySet new.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented := IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [
					entry := unimplemented at: m ifAbsent: [Array new].
					entry := entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented! !


!OrderedDictionary methodsFor: 'private' stamp: 'jmv 2/5/2024 18:11:20'!
init: n

	super init: n.
	orderedKeys := OrderedCollection new! !


!SharedQueue class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:15:07'!
new
	"Answer a new instance of SharedQueue."

	^self basicNew initialize: 10! !


!Encoder methodsFor: 'initialization' stamp: 'jmv 2/5/2024 18:16:43'!
init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: Encoder doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= Encoder doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new.
	globalSourceRanges := OrderedCollection new.! !

!Encoder methodsFor: 'initialization' stamp: 'jmv 2/5/2024 18:16:50'!
initScopeAndLiteralTables

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new! !

!Encoder methodsFor: 'initialization' stamp: 'jmv 2/5/2024 18:16:59'!
temps: tempVars literals: lits class: cl 
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	literalStream := ReadStream on: lits.
	literalStream position: lits size.
	sourceRanges := Dictionary new.
	globalSourceRanges := OrderedCollection new.
! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 2/5/2024 18:18:21'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory lookIn |
	entries := OrderedCollection new.
	index := 1.
	done := false.
	lookIn := parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray := self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory := entryArray at: 4.
				entry := isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				entry name: (entryArray at: 1) parent: parentEntryOrNil.
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !


!Form class methodsFor: 'examples' stamp: 'jmv 2/5/2024 18:17:15'!
toothpaste: diam	
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors := `Color red wheel: 12`.
	color := 8.
	[ true ] whileTrue: [
		queue := OrderedCollection new.
		16 timesRepeat: [queue addLast: `-40@ -40`].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point := Sensor mousePoint.
		colr := colors atWrap: (color := color + 5).  "choose increment relatively prime to colors size"
		colr2 := colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point := point * 4 + Sensor mousePoint // 5.
			point := point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q := queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'jmv 2/5/2024 18:17:27'!
beFullyExpandedAsPer: aBlock

	| allChildren |
	allChildren := OrderedCollection new.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			| shouldExpandEach |
			shouldExpandEach := each canExpand and: [aBlock value: each].
			shouldExpandEach ~= each isExpanded ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpandedAsPer: aBlock
				]
		]! !


!TextComposition methodsFor: 'composition' stamp: 'jmv 2/5/2024 18:17:33'!
recomposeFrom: startArg to: stopArg delta: delta
	"Recompose this text.  The altered portion is between start and stop.
	Recomposition may continue to the end of the text, due to a ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed.
	Expand the requested interval to include whole paragraphs because there could be paragraph attributes."
	| intervalToFix start stop startLine newLines partialMaxRightX |
	"Have to recompose line above in case a word-break was affected."
	intervalToFix := model actualContents encompassParagraph: (startArg to: stopArg).
	start := intervalToFix first.
	stop := intervalToFix last.
	startLine := (self lineIndexFor: start) - 1 max: 1.
	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]
		whileTrue: [startLine := startLine - 1].  "Find leftmost of line pieces"
	newLines := OrderedCollection new.
	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].
	partialMaxRightX := self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top.
	"Partial recomposition computes actual right border only of recompsed text,
	so, it could make maxRightX larger but not smaller.
	This means that if the longest line gets shortened, we won't know, and maxRightX will be erroneously large"
	maxRightX := maxRightX max: partialMaxRightX! !


!TestResult methodsFor: 'accessing' stamp: 'jmv 2/5/2024 18:17:44'!
tests

	^ OrderedCollection new
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself
			! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 2/5/2024 18:18:27'!
printSpaceAnalysis: threshold on: aStream
	"Warning!!
	This is most likely wrong. It was written for 32 bit V3 images.
	Please take a look at #spaceForInstancesOf:
	Review both methods, and any other one that might be related.
	"
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace := cl spaceUsed.
				barBlock value: (n := n+1).
				Smalltalk garbageCollectMost.
				instCount := cl instanceCount.
				instSpace := 8 * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize := cl isBytes ifTrue: [1] ifFalse: [4].
						cl allInstancesDo: [ :x |
							instSpace := instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace := instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace := totalCodeSpace + codeSpace.
				totalInstCount := totalInstCount + instCount.
				totalInstSpace := totalInstSpace + instSpace]].
	totalPercent := 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results := (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent := s spaceForInstances*100.0/totalInstSpace.
		totalPercent := totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !

!SpaceTally methodsFor: 'class analysis' stamp: 'jmv 2/5/2024 18:17:48'!
preAllocateResultsFor: classes

	results := OrderedCollection new.
	classes do: [:cl | results add: (SpaceTallyItem analyzedClassName: cl name)].
	results := results asArray.
! !


!SHParserST80 methodsFor: 'parse support' stamp: 'jmv 2/5/2024 18:19:49'!
pushArgument: aString 
	(arguments at: blockDepth ifAbsentPut: [OrderedCollection new]) 
		add: aString! !

!SHParserST80 methodsFor: 'parse support' stamp: 'jmv 2/5/2024 18:19:57'!
pushTemporary: aString 
	(temporaries at: blockDepth ifAbsentPut: [OrderedCollection new]) 
		add: aString! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 2/5/2024 18:19:42'!
parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	| continue prevSourcePosition |
	self initializeInstanceVariables.
	sourcePosition := 1.
	arguments := Dictionary new.
	temporaries := Dictionary new.
	blockDepth := bracketDepth := braceDepth := 0.
	blockDepths := OrderedCollection with: blockDepth.
	blockDepthsStartIndexes := OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new] ifNotNil: [ ranges reset].
	errorBlock := [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		"Iterate once for methods, but pontentially several times for workspaces
		(to recover after errors, for possible good next lines or chunks)"
		continue := true.
		[ continue ] whileTrue: [
			prevSourcePosition := sourcePosition.
			self parseStatementList.
			continue := sourcePosition > prevSourcePosition.
			isAMethod
				ifTrue: [
					"Only if we are parsing a method, consider everything after this point as error."
					currentToken ifNotNil: [ self error ].
					continue := false]
				ifFalse: [
					sourcePosition > source size ifTrue: [continue := false]]].
	] ensure: [errorBlock := nil].
	^true! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'jmv 2/5/2024 18:20:40'!
renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod

	"If a temporary to declare already exists in the scope, a new fresh name
	will be found for it. The method returns an OrderedCollection of (variableNode -> newVariableName)
	replacements. If the variable does not need a rename, it will still be added to the collection.
	This method also adds the new name to the map that tracks temporaries
	names in scope for the requested method or block node across the whole refactoring - Fernando"

	| alreadyDeclaredTemporaries newTemporaries |

	newTemporaries := OrderedCollection new.
	alreadyDeclaredTemporaries := self declaredTemporariesFor: aMethodOrBlockNode ofSender: aSenderMethod.
	temporariesToDeclare do: [:temporary |
		| finalTemporaryName index |
		finalTemporaryName := temporary name.
		index := 1.
		[alreadyDeclaredTemporaries includes: finalTemporaryName] whileTrue: [
			finalTemporaryName := temporary name, index asString.
			index := index + 1.
		].
		alreadyDeclaredTemporaries add: finalTemporaryName.
		newTemporaries add: (temporary -> finalTemporaryName).
	].

	^newTemporaries.! !

!InlineMethod methodsFor: 'initialization' stamp: 'jmv 2/5/2024 18:20:33'!
methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	updatedSendersCode := Dictionary new.
	methodNodeToInline := methodToInline notOptimizedMethodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.! !

!methodRemoval: SortedCollection class #new:sortBlock: stamp: 'jmv 2/6/2024 09:23:49'!
SortedCollection class removeSelector: #new:sortBlock:!
