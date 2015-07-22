'From Cuis 4.2 of 25 July 2013 [latest update: #2409] on 10 July 2015 at 11:17:02.994625 pm'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector block tally talliesList '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!TimeProfileBrowser methodsFor: 'message list' stamp: 'jmv 7/9/2015 19:47'!
messageListIndex
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^talliesList indexOf: selectedMessage! !

!TimeProfileBrowser methodsFor: 'message list' stamp: 'jmv 7/9/2015 19:39'!
messageListIndex: anInteger
	"Set the index of the selected item to be anInteger."
	selectedMessage _ anInteger = 0 ifFalse: [ talliesList at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 23:16'!
reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows line |
	upTime := totalTime.
	oldSpaceEnd		:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**Memory**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	old			', oldSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '		young		', (youngSpaceEnd - oldSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  '		used		', youngSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  '		free		', (memoryEnd - youngSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**GCs**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ '	full			', fullGCs printString,' totalling ', fullGCTime printStringWithCommas, 'ms (', (100.0 * fullGCTime / upTime) rounded printString, '% uptime)'.
	fullGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * fullGCTime / fullGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ '	incr		', incrGCs printString, ' totalling ', incrGCTime printStringWithCommas, 'ms (', (100.0 * incrGCTime / upTime) rounded printString, '% uptime)'.
	incrGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * incrGCTime / incrGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ '	tenures		', tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		line _ line, ' (avg ', (1.0 * incrGCs / tenureCount) asInteger printString, ' GCs/tenure)'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ '	root table	', rootOverflows printStringWithCommas, ' overflows'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 23:05'!
reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream
	| totalSwitches pageOverflows pageDivorces actualSwitches line |
	vmStats size >= 61 ifFalse:[^self]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.
	
	line _ '**Processes**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Total process switches: ', totalSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Without Profiler: ', actualSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Stack page overflows: ', pageOverflows printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Stack page divorces: ', pageDivorces printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/9/2015 15:03'!
reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: 1! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 22:44'!
reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: threshold
	| line |
	tallyRoot tally = 0
		ifTrue: [
			line _ ' - no tallies obtained'.
			textStream nextPutAll: line; newLine.
			linesStream nextPut: line.
			talliesStream nextPut: nil ]
		ifFalse: [
			line _ 'Reporting - ' , totalTally printStringWithCommas, ' tallies, ' , totalTime printStringWithCommas , ' msec.'.
			textStream
				nextPutAll: line; newLine;
				newLine.
			linesStream nextPut: line; nextPut: ''.
			talliesStream nextPut: nil; nextPut: nil.
			tallyRoot
				fullPrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream 
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream.
		self reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream ]! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/9/2015 15:14'!
fullPrintOn: textStream linesOn: linesStream talliesOn: talliesStream threshold: perCent time: totalTime reportOnly: aProcessOrNil
	| threshold line |  
	threshold _ (perCent asFloat / 100 * tally) rounded.
	line _ '**Tree**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	self
		rootPrintOn: textStream
		linesOn: linesStream
		talliesOn: talliesStream 
		total: tally
		totalTime: totalTime
		threshold: threshold
		reportOnly: aProcessOrNil! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/9/2015 15:20'!
printOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime
	self subclassResponsibility! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/9/2015 15:19'!
rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups dashes line |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g | | sons |
		sons _ self sortCollectionFrom: g.
		dashes _ '--------------------------------'.
		line _ 'Process: ', (g anyOne process browserPrintStringFull: false).
		textStream
			nextPutAll: dashes; newLine;
			nextPutAll: line; newLine;
			nextPutAll: dashes; newLine.
		linesStream
			nextPut: dashes;
			nextPut: line;
			nextPut: dashes.
		talliesStream
			nextPut: nil; nextPut: nil; nextPut: nil.
		sons do: [ :son |
			son
				treePrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream
				tabs: OrderedCollection new
				thisTab: '  '
				total: total
				totalTime: totalTime
				threshold: threshold].
		textStream newLine ]! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/9/2015 15:21'!
treePrintOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabs thisTab: myTab total: total totalTime: totalTime threshold: threshold
	| sons sonTab |
	tabs do: [:tab | textStream nextPutAll: tab].
	self printOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime.
	sons := self sonsOver: threshold.
	sons isEmpty ifFalse:[
		tabs addLast: myTab.
		sons := self sortCollectionFrom: sons.
		1 to: sons size do:[:i | 
			sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
			(sons at: i)
				treePrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream
				tabs: tabs
				thisTab: sonTab
				total: total
				totalTime: totalTime
				threshold: threshold].
			tabs removeLast ]! !


!QFakeTally methodsFor: 'report' stamp: 'jmv 7/10/2015 23:00'!
printOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime

	| percentage line |
	line _ String streamContents: [ :lineStream |
		percentage := tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 1.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		lineStream
			nextPutAll: '-----Primitives (called at an unknown point below here in the call tree)------' ].

	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: self! !

!QFakeTally methodsFor: 'MethodReference compatibility' stamp: 'jmv 7/9/2015 19:48'!
actualClass
	^nil! !

!QFakeTally methodsFor: 'MethodReference compatibility' stamp: 'jmv 7/10/2015 22:40'!
methodSymbol
	^nil! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 7/9/2015 15:34'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams total: total totalTime: totalTime

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 1.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector ].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !

!QSystemTally methodsFor: 'MethodReference compatibility' stamp: 'jmv 7/9/2015 20:11'!
actualClass
	^method methodClass! !

!QSystemTally methodsFor: 'MethodReference compatibility' stamp: 'jmv 7/9/2015 19:42'!
methodSymbol
	^method selector! !


!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 7/9/2015 15:40'!
contents
	^nil! !

!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 7/10/2015 22:56'!
tab! !


!MessageSet methodsFor: 'contents' stamp: 'jmv 7/9/2015 19:59'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector source |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment].
		selector == #Definition ifTrue: [
			^ class definition].
		selector == #Hierarchy ifTrue: [^ class printHierarchy]].
	source _ class sourceMethodAt: selector ifAbsent: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	source _ self sourceStringPrettifiedAndDiffed.
	^ source asText makeSelectorBoldIn: class! !

!MessageSet methodsFor: 'private' stamp: 'jmv 7/9/2015 19:57'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !

!MessageSet methodsFor: 'private' stamp: 'jmv 7/9/2015 20:09'!
initializeMessageList: anArray
	messageList _ anArray.
	selectedMessage _ messageList isEmpty ifFalse: [ messageList first ]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 7/9/2015 20:07'!
filterFrom: aBlock
	"Filter the receiver's list down to only those items that satisfy aBlock, which takes a class an a selector as its arguments."
	
	self setFilteredList: (messageList select: [ :methodReference |
		aBlock
			value: methodReference actualClass
			value: methodReference methodSymbol ])! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 7/9/2015 19:38'!
runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| list result linesStream talliesStream textStream |

self assert: AndreasSystemProfiler canWork.

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: Array new.
	talliesStream _ WriteStream on: Array new.
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	
	"
	stream := ReadWriteStream 
				with: (String streamContents: [ :s | 
					tally report: s]).
	stream reset.
	list := OrderedCollection new.
	[stream atEnd] whileFalse: [list add: stream nextLine].
	"
	list _ linesStream contents.
	list _ talliesStream contents.
{textStream contents. linesStream contents. talliesStream contents } halt.
"	self initializeMessageList: list."

	messageList _ linesStream contents.
	talliesList _ talliesStream contents.
	selectedMessage _ talliesList isEmpty ifFalse: [ talliesList first ].
	
	self changed: #messageList.
	self changed: #messageListIndex.
	self changed: #decorateButtons.
	^result! !


!TimeProfileBrowser class methodsFor: 'instance creation' stamp: 'jmv 7/9/2015 14:50'!
spyOn: block
	"Open a profile browser on the given block, thereby running the block and 
	 collecting the message tally."
	"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
	"

	^self onBlock: block! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/9/2015 15:04'!
report
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :textStream |
		self reportTextOn: textStream linesOn: DummyStream new talliesOn: DummyStream new ]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 7/10/2015 22:35'!
methodHierarchy
	"Create and schedule a method browser on the hierarchy of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass _ model selectedClassOrMetaClass.
	sel _ model selectedMessageName.
	aClass ifNil: [ ^ self ].
	sel ifNil: [ ^ self ].
	aClassNonMeta _ aClass theNonMetaClass.
	isMeta _ aClassNonMeta ~~ aClass.
	list _ OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta _ isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	Smalltalk
		browseMessageList: list
		name: 'Inheritance of ' , sel.! !


!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 7/10/2015 22:56'!
newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."! !

!methodRemoval: MethodReference #setClassAndSelectorIn:!
MethodReference removeSelector: #setClassAndSelectorIn:!
!methodRemoval: AndreasSystemProfiler #report:!
AndreasSystemProfiler removeSelector: #report:!
!methodRemoval: TimeProfileBrowser #initializeMessageList:!
TimeProfileBrowser removeSelector: #initializeMessageList:!
!methodRemoval: TimeProfileBrowser #runBlock:pollingEvery:!
TimeProfileBrowser removeSelector: #runBlock:pollingEvery:!
!methodRemoval: TimeProfileBrowser #selectedMessage!
TimeProfileBrowser removeSelector: #selectedMessage!
!methodRemoval: TimeProfileBrowser #setClassAndSelectorIn:!
TimeProfileBrowser removeSelector: #setClassAndSelectorIn:!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector tally talliesList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!methodRemoval: ProtocolBrowser #setClassAndSelectorIn:!
ProtocolBrowser removeSelector: #setClassAndSelectorIn:!
!methodRemoval: MessageSet class #parse:toClassAndSelector:!
MessageSet class removeSelector: #parse:toClassAndSelector:!
!methodRemoval: MessageSet #setClassAndSelectorIn:!
MessageSet removeSelector: #setClassAndSelectorIn:!
!methodRemoval: CodeProvider #setClassAndSelectorIn:!
CodeProvider removeSelector: #setClassAndSelectorIn:!
