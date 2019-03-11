'From Cuis 5.0 of 7 November 2016 [latest update: #3651] on 11 March 2019 at 9:12:47 am'!

!CompiledMethod methodsFor: 'decompiling' stamp: 'HAW 3/11/2019 08:53:25'!
createMethodNode

	"Creates the parse tree that represents self"
	
	| aClass source |
	
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [self defaultSelector])
		in: aClass.
				
	^ aClass methodNodeFor: source
	! !

!CompiledMethod methodsFor: 'as yet unclassified' stamp: 'HAW 3/9/2019 02:52:10'!
methodNode: aMethodNode

	self propertyValueAt: #methodNode put: aMethodNode! !


!Compiler methodsFor: 'public access' stamp: 'HAW 3/11/2019 06:35:05'!
evaluateMethod: method to: receiver logged: doLog profiled: doProfile
	
	"See evaluate:in:to:notifying:ifFail:logged:profiled:
	It does the same but without compiling because it recevies the result of the compilation 
	as the parameter method. 
	self should have compile method"

	| value toLog itsSelection itsSelectionString |
	
	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	"Evaluate now."
	doProfile
		ifTrue: [
			AndreasSystemProfiler spyOn: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/11/2019 08:34:11'!
compileSelectionFor: anObject in: evalContext ifFail: failBlock

	| methodNode method sourceCode compiler |

	sourceCode := self selectionDoItSourceCodeIn: evalContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler
			sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"		
			compileDoIt: sourceCode
			in: anObject class
			context: evalContext
			notifying: self
			ifFail: [ ^ failBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method _ methodNode generate.
	method methodNode: methodNode.

	^{ #method -> method. #compiler -> compiler } asDictionary 
! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/11/2019 08:57:46'!
selectionDoItSourceCodeIn: evalContext

	^String streamContents: [ :stream |
		"I use previousContext and not ThisContext as in the parser to avoid
		name collision. Also, previousContext is more intention revealing - Hernan"
		stream
			nextPutAll: (evalContext ifNil: [ Scanner doItSelector ] ifNotNil: [ Scanner doItInSelector, ' previousContext' ]);
			newLine;
			newLine;
			nextPutAll: self selectionAsStream upToEnd ]! !


!CompiledMethod methodsFor: 'accessing' stamp: 'HAW 3/11/2019 08:35:51'!
defaultSelector 
	
	"Invent and answer an appropriate message selector (a Symbol) for me, 
	that is, one that will parse with the correct number of arguments."
	
	^Scanner doItSelector numArgs: self numArgs! !

!CompiledMethod methodsFor: 'source code management' stamp: 'HAW 3/11/2019 08:47:47'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."

	| flagByte source |

	flagByte := self last.
	
	"If no source pointer..."
	source := flagByte < 252 
		ifTrue: [ nil ]
		ifFalse: [ 
			"Situation normal; read the sourceCode from the file
			An error can happen here if, for example, the changes file has been truncated by an aborted download.  
			The present solution is to ignore the error and fall back on the decompiler.  
			A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  
			Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling 
			into this error case, random source code will get returned."
			[self getSourceFromFile]
				on: Error
				do: [ :ex | ex return: nil]].
		
	"If source code not available, use DoIt source code or if absent decompile blind (no temps)"
	^source ifNil: [ (class decompilerClass new decompile: selector in: class method: self) decompileString ]! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'HAW 3/11/2019 08:55:04'!
methodNode

	"Return the parse tree that represents self"
	
	"I do not save the method node in the #methodNode property if it does not
	exist to avoid keeping the method node in memory. 
	The methodNode is saved in the property #methodNode to avoid loosing the source
	code when debugging - Hernan"
	^self propertyValueAt: #methodNode ifAbsent: [ self createMethodNode ]! !


!Compiler methodsFor: 'public access' stamp: 'HAW 3/9/2019 00:01:13'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock

	^ self evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: false profiled: false! !

!Compiler methodsFor: 'public access' stamp: 'HAW 3/11/2019 08:45:50'!
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method |
	
	class _ (aContext ifNil: [ aReceiver ] ifNotNil: [ aContext receiver ]) class.
	methodNode _ self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method _ methodNode generate.
	"I'm not keeping the source nor the methodNode for back compabibility. 
	The SmalltalkEditor sends the message #evaluateMethod:... which already keep the method node
	 for the debugger to show the right source code - Hernan"

	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile! !

!Compiler methodsFor: 'private' stamp: 'HAW 3/11/2019 07:07:06'!
translate: aStream noPattern: noPattern ifFail: failBlock

	^self translate: aStream noPattern: noPattern doIt: noPattern ifFail: failBlock ! !


!Compiler class methodsFor: 'evaluating' stamp: 'HAW 3/9/2019 00:01:04'!
evaluate: textOrString for: anObject notifying: aRequestor logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aRequestor. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
		evaluate: textOrString
		in: nil
		to: anObject
		notifying: aRequestor
		ifFail: [^nil]
		logged: logFlag
		profiled: false! !


!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 3/11/2019 08:36:29'!
compileBacktickCode

	| compiler method methodNode |
	
	compiler _ Compiler new.
	methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
	method _ methodNode generate.
	
	"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
	sentInLiterals addAll: method messages.
	method literalsDo: [ :literal | literal isSymbol ifTrue: [ literal = self class doItSelector ifFalse: [sentInLiterals add: literal ]]].
		
	"Evaluate now."
	token _ nil withArgs: #() executeMethod: method ! !


!Parser methodsFor: 'expression types' stamp: 'HAW 3/11/2019 06:42:49'!
doitPatternInContext: context

	^context 
		ifNil: [{self class doItSelector. {}. 1. nil }]
		ifNotNil: [{self class doItInSelector. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]! !

!Parser methodsFor: 'public access' stamp: 'HAW 3/11/2019 08:31:06'!
parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock 

	^self parse: sourceStream class: class category: nil noPattern: noPattern doIt: noPattern context: aContext notifying: aRequestor ifFail: aBlock ! !


!AssignmentNode methodsFor: 'initialization' stamp: 'HAW 3/10/2019 12:37:09'!
variable: aVariable value: expression from: encoder

	"Case of remote temp vars"
	(aVariable isMemberOf: MessageAsTempNode) ifTrue: [ ^aVariable store: expression from: encoder].
	
	variable := aVariable.
	value := expression.
	
	^self
	
! !

!AssignmentNode methodsFor: 'initialization' stamp: 'HAW 3/9/2019 20:03:46'!
variable: aVariable value: expression from: encoder sourceRange: range

	| realNode |
	
	realNode := self variable: aVariable value: expression from: encoder.
	encoder noteSourceRange: range forNode: realNode.
	
	^realNode! !


!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'HAW 3/11/2019 08:56:15'!
store: expr from: encoder 
	"ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment).
	For assigning into temps of a context being debugged."

	selector key ~= #namedTempAt: ifTrue: [^self error: 'cant transform this message'].
	
	^ MessageAsTempNode new
		receiver: receiver
		selector: #namedTempAt:put:
		arguments: (arguments copyWith: expr)
		precedence: precedence
		from: encoder! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/11/2019 06:25:51'!
compileSelectionFor: anObject in: evalContext

	^(self compileSelectionFor: anObject in: evalContext ifFail: [ ^ nil ]) at: #method
! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/11/2019 08:57:10'!
debugIt

	| provider method receiver context |

	self lineSelectAndEmptyCheck: [^self].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver _ provider doItReceiver.
			context _ provider doItContext]
		ifFalse: [
			receiver _ context _ nil].

	method _ self compileSelectionFor: receiver in: context.
	method ifNotNil: [ self debug: method receiver: receiver in: context ]! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/11/2019 08:42:34'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result receiver context methodAndCompiler |

	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver _ provider doItReceiver.
			context _ provider doItContext]
		ifFalse: [receiver _ context _ nil].

	methodAndCompiler _ self compileSelectionFor: receiver in: context ifFail: [^ failBlock value].

	result _ (methodAndCompiler at: #compiler)
		evaluateMethod: (methodAndCompiler at: #method)
		to: receiver
		logged: true
		profiled: doProfile.
		
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	
	^ aBlock value: result! !


!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'HAW 3/11/2019 08:39:26'!
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."

	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |

	abstractSourceRanges ifNotNil: [ ^abstractSourceRanges].

	methodNode encoder hasGeneratedMethod
		ifTrue: [
			rawSourceRanges := methodNode encoder rawSourceRanges.
		 	theMethodToScan := self method ]
		ifFalse: [
			"If the methodNode hasn't had a method generated it doesn't have pcs set in its
			 nodes so we must generate a new method and might as well use it for scanning."
			 [methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
				rawSourceRanges := ranges.
			 	theMethodToScan := method ]]
			on: UndeclaredVariableWarning
			do: [ :ex | ex resume ]].

	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [ | realRange |
			realRange := (range isKindOf: OrderedCollection) ifTrue: [ range last ] ifFalse: [ range ].
			concreteSourceRanges at: node pc put: realRange ]].

	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	
	^abstractSourceRanges! !

!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'HAW 3/9/2019 03:26:53'!
sourceText

	^methodNode sourceText! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'HAW 3/11/2019 08:55:37'!
forMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required."
	
	| methodNode |
	
	^self protected: [ 
		MapCache
			at: aMethod
			ifAbsent: [
				[ methodNode _ aMethod methodNode ]
					on: UndeclaredVariableWarning
					do: [ :ex | ex resume ].
				self
					cacheDebugMap: (self
								forMethod: aMethod
								methodNode: methodNode)
					forMethod: aMethod] ]! !

!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'HAW 3/9/2019 03:16:29'!
forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^self new
		forMethod: aMethod
		methodNode: methodNode! !

!methodRemoval: Parser #method:context:!
Parser removeSelector: #method:context:!
!methodRemoval: Parser #parse:class:category:noPattern:context:notifying:ifFail:!
Parser removeSelector: #parse:class:category:noPattern:context:notifying:ifFail:!
!methodRemoval: Compiler #evaluateMethod:in:to:logged:profiled:!
Compiler removeSelector: #evaluateMethod:in:to:logged:profiled:!
!methodRemoval: Compiler #evaluateMethod:in:to:notifying:ifFail:logged:profiled:!
Compiler removeSelector: #evaluateMethod:in:to:notifying:ifFail:logged:profiled:!
!methodRemoval: Compiler #evaluateMethod:in:to:notifying:logged:profiled:!
Compiler removeSelector: #evaluateMethod:in:to:notifying:logged:profiled:!
!methodRemoval: CompiledMethod #doItSourceCodeIfAbsent:!
CompiledMethod removeSelector: #doItSourceCodeIfAbsent:!
!methodRemoval: CompiledMethod #generateDoItSourceCodeWith:!
CompiledMethod removeSelector: #generateDoItSourceCodeWith:!
!methodRemoval: CompiledMethod #keepDoItSourceCodeWith:!
CompiledMethod removeSelector: #keepDoItSourceCodeWith:!

!Compiler reorganize!
('public access' compile:in:classified:notifying:ifFail: compile:in:notifying:ifFail: compileDoIt:in:context:notifying:ifFail: compileNoPattern:in:context:notifying:ifFail: evaluate:in:to: evaluate:in:to:notifying:ifFail: evaluate:in:to:notifying:ifFail:logged:profiled: evaluateMethod:to:logged:profiled: format:in:notifying: parser parser: parserClass sourceStreamGetter:)
('private' format:noPattern:ifFail: from:class:context:notifying: interactive translate:noPattern:doIt:ifFail: translate:noPattern:ifFail:)
('error handling' notify: notify:at:)
!


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt selectionDoItSourceCodeIn:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameOf:in: ifRenameCanBeAppliedDo: rename:in: renameClassOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameTemporary: withMethodNodeAndClassDo:ifErrorsParsing:)
!

