'From Cuis 4.2 of 25 July 2013 [latest update: #2330] on 20 May 2015 at 12:37:05.390846 pm'!

!Behavior methodsFor: 'accessing' stamp: 'jmv 5/26/2013 15:40'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler! !

!Behavior methodsFor: 'accessing' stamp: 'jmv 12/7/2009 17:52'!
decompilerClass
	"Answer a decompiler class appropriate for compiled methods of this class."

	^Smalltalk actualDecompilerClass! !

!Behavior methodsFor: 'accessing'!
parserClass
	"Answer a parser class to use for parsing method headers."

	^self compilerClass parserClass! !


!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 4/30/2009 18:13'!
compilerClass
	^self methodClass 
		ifNil: [Compiler] 
		ifNotNil: [:class | class compilerClass].! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 9/5/2009 14:17'!
decompilerClass
	^self compilerClass decompilerClass! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 4/30/2009 18:14'!
parserClass
	^self methodClass 
		ifNil: [Compiler parserClass] 
		ifNotNil: [:class | class parserClass].! !


!SystemDictionary methodsFor: 'Compiler swapping' stamp: 'jmv 12/29/2009 11:46'!
actualParserClass
	"Sending this message replaces direct references to some class"
	^Parser! !


!Compiler methodsFor: 'public access' stamp: 'eem 5/15/2008 15:05'!
parserClass

	^parser ifNil: [self class parserClass] ifNotNil: [parser class]! !


!Compiler class methodsFor: 'accessing' stamp: 'md 3/1/2006 21:12'!
decompilerClass
	^Decompiler! !

!Compiler class methodsFor: 'accessing' stamp: 'eem 5/13/2008 11:37'!
parserClass
	"Answer a parser class to use for parsing methods compiled by instances of the receiver."

	^Parser! !


!MethodNode methodsFor: 'code generation' stamp: 'ajh 7/6/2003 15:25'!
parserClass
	"Which parser produces this class of parse node"

	^ Parser! !


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 5/26/2013 15:42'!
compilerClass
	^ (Smalltalk at: name ifAbsent: [^ Compiler]) compilerClass! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 5/26/2013 15:42'!
parserClass

	^ Compiler parserClass! !


!CodeProvider methodsFor: 'message list' stamp: 'jmv 12/4/2010 00:03'!
decompiledSource
	"Obtain a source string by decompiling the method's code.
	Also return the string."
	| class selector method answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	"Was method deleted while in another project?"
	method _ class
		compiledMethodAt: selector
		ifAbsent: [ ^ '' ].
	currentCompiledMethod _ method.
	"decompile without temp names "
	answer _ (class decompilerClass new
		decompile: selector
		in: class
		method: method) decompileString.
	answer _ answer asText makeSelectorBoldIn: class.
	^ answer copy.! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 1/21/2013 16:25'!
sourceStringPrettifiedAndDiffed
	"Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies"
	| class selector sourceString |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	(class isNil or: [ selector isNil ]) ifTrue: [ ^ 'missing' ].
	sourceString _ class
		ultimateSourceCodeAt: selector
		ifAbsent: [ ^ 'error' ].
	"Can be expensive for methods of ridiculous size"
	sourceString size < 2000 ifTrue: [
		self
			validateMessageSource: sourceString
			forSelector: selector
			inClass: class ].
	(self showingPrettyPrint or: [ self showingAnyKindOfPrettyDiffs ]) ifTrue: [
		sourceString _ class compilerClass new
			format: sourceString
			in: class
			notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		sourceString _ self diffFromPriorSourceFor: sourceString ].
	^ sourceString! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 5/20/2015 12:36'!
validateMessageSource: sourceString forSelector: aSelector inClass: theClass
	"Check whether there is evidence that method source is invalid"

	(theClass parserClass selectorFrom: sourceString asString) = aSelector
		ifFalse: [
			self inform: 'There may be a problem with your sources file!!

The source code for every method should (usually) start with the
method selector but this is not the case with this method!! You may
proceed with caution but it is recommended that you get a new source file.'].! !


!Browser methodsFor: 'message functions' stamp: 'jmv 5/20/2015 12:35'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString.
	(self metaClassIndicated
		and: [(self selectedClassOrMetaClass includesSelector: selector) not
		and: [Metaclass isScarySelector: selector]])
		ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
				(self confirm: ((selector , ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: selector size))
				ifFalse: [^nil]].
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!TimeProfileBrowser methodsFor: 'message list' stamp: 'jmv 8/18/2009 18:01'!
selectedMessage
	"Answer the source method for the currently selected message."
	| source |
	self setClassAndSelectorIn: [:class :selector | 
		source _ class sourceMethodAt: selector ifAbsent: [^ 'Missing'].
		Preferences browseWithPrettyPrint ifTrue:
			[source _ class compilerClass new
				format: source in: class notifying: nil ].
		self selectedClass: class.
		self selectedSelector: selector.
		^ source asText makeSelectorBoldIn: class].
	^''! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 5/20/2015 12:34'!
scanCategory: category class: class meta: meta stamp: stamp
	| itemPosition method |
	[
		itemPosition _ file position.
		method _ file nextChunk.
		method size > 0 ]						"done when double terminators"
			whileTrue: [
				self
					addItem: (ChangeRecord new file: file position: itemPosition type: #method
							class: class category: category meta: meta stamp: stamp)
					text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
						, ((Smalltalk at: class ifAbsent: [Object]) parserClass selectorFrom: method)
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/26/2013 15:47'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and: [
				(class _ change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/5/2013 11:29'!
acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class strm changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer asText makeSelectorBoldIn: class ]
		ifNil: [
			strm _ WriteStream on: (String new: 100).
			(myChangeSet classChangeAt: currentClassName) do: [ :each |
				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
				each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
			].
			^ strm contents].! !


!Debugger methodsFor: 'accessing' stamp: 'jmv 5/20/2015 12:34'!
contents: aText notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."
	| result selector classOfMethod category h ctxt newMethod |
	contextStackIndex = 0 ifTrue:
		[^false].
	self selectedContext isExecutingBlock ifTrue:
		[h := self selectedContext activeHome.
		 h ifNil:
			[self inform: 'Method for block not found on stack, can''t edit and continue'.
			 ^false].
		 (self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse:
			[^false].
		self resetContext: h.
		(result := self contents: aText notifying: aController) ifTrue: [
			self acceptedContentsChanged].
		^result].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: aText.
	(selector == self selectedMessageName
	 or: [(self selectedMessageName beginsWith: 'DoIt')
		and: [selector numArgs = self selectedMessageName numArgs]]) ifFalse:
		[self inform: 'can''t change selector'.
		 ^false].
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.
	newMethod isQuick ifTrue:
		[self down.
		 self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].
	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	^true! !


!Behavior methodsFor: 'testing' stamp: 'mtf 6/3/2010 11:03'!
sourceMatchesBytecodeAt: selector
	"Answers true if the source code at the selector compiles to the bytecode at the selector, and false otherwise. Implemented to detect an error where Monticello did not recompile sources when the class shape changed"
	"This code was copied from #recompile:from:, with few changes. Several methods would benefit from a method which turned a selector and class into a CompiledMethod, without  installing it into the methodDictionary"

	| method trailer methodNode |
	method := self compiledMethodAt: selector.
	trailer := method trailer.
	methodNode := self compilerClass new
				compile: (self sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ false].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!!'].
	^ (methodNode generate: trailer) = method! !

!Behavior methodsFor: 'compiling'!
decompile: selector 
	"Find the compiled code associated with the argument, selector, as a 
	message selector in the receiver's method dictionary and decompile it. 
	Answer the resulting source code as a string. Create an error notification 
	if the selector is not in the receiver's method dictionary."

	^self decompilerClass new decompile: selector in: self! !

!Behavior methodsFor: 'compiling' stamp: 'NS 1/28/2004 09:22'!
recompile: selector from: oldClass
	"Compile the method associated with selector in the receiver's method dictionary."
	"ar 7/10/1999: Use oldClass compiledMethodAt: not self compiledMethodAt:"
	| method trailer methodNode |
	method _ oldClass compiledMethodAt: selector.
	trailer _ method trailer.
	methodNode _ self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ self].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!!'].
	self addSelectorSilently: selector withMethod: (methodNode generate: trailer).
! !

!Behavior methodsFor: 'compiling' stamp: 'ajh 6/11/2001 17:05'!
recompileNonResidentMethod: method atSelector: selector from: oldClass
	"Recompile the method supplied in the context of this class."

	| trailer methodNode |
	trailer _ method trailer.
	methodNode _ self compilerClass new
			compile: (method getSourceFor: selector in: oldClass)
			in: self
			notifying: nil
			ifFail: ["We're in deep doo-doo if this fails (syntax error).
				Presumably the user will correct something and proceed,
				thus installing the result in this methodDict.  We must
				retrieve that new method, and restore the original (or remove)
				and then return the method we retrieved."
				^ self error: 'see comment'].
	selector == methodNode selector ifFalse: [self error: 'selector changed!!'].
	^ methodNode generate: trailer
! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 6/20/2001 15:46'!
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MessageSet isPseudoSelector: selector)
		ifTrue:
			["Not really a selector"
			^ nil].
	source _ self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
	parser _ self parserClass new.
	tree _ 
		parser
			parse: (ReadStream on: source)
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 5/20/2015 12:28'!
methodHeaderFor: selector 
	"Answer the string corresponding to the method header for the given selector"

	| methodSource |
	methodSource _ self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
"
! !

!Behavior methodsFor: 'private' stamp: 'jmv 12/7/2009 18:05'!
basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^ Smalltalk actualCompiledMethodWithNodeClass generateMethodFromNode: methodNode trailer: bytes.! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/18/2009 17:55'!
reformatMethodAt: selector 
	| newCodeString method | 
	newCodeString _ (self compilerClass new)
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method _ self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		inFile: 2 priorMethod: method! !


!BreakpointManager class methodsFor: 'private' stamp: 'emm 5/30/2002 09:36'!
breakpointMethodSourceFor: aSymbol in: aClass 
	"Compose new source containing a break statement (currently it will be the first,
	later we want to insert it in any place)"

	| oldSource methodNode breakOnlyMethodNode sendBreakMessageNode |
	oldSource := aClass sourceCodeAt: aSymbol.
	methodNode := aClass compilerClass new
		compile: oldSource
		in: aClass 
		notifying: nil 
		ifFail: [self error: '[breakpoint] unable to install breakpoint'].
	breakOnlyMethodNode := aClass compilerClass new
		compile: 'temporaryMethodSelectorForBreakpoint
self break.
^self'
		in: aClass 
		notifying: nil 
		ifFail: [self error: '[breakpoint] unable to install breakpoint'].
	sendBreakMessageNode := breakOnlyMethodNode block statements first.
	methodNode block statements addFirst: sendBreakMessageNode.
	^methodNode printString
	! !

!BreakpointManager class methodsFor: 'private' stamp: 'jmv 3/2/2010 15:39'!
compilePrototype: aSymbol in: aClass 
	"Compile and return a new method containing a break statement"

	| source node |
	source := self breakpointMethodSourceFor: aSymbol in: aClass.
	node := aClass compilerClass new
		compile: source
		in: aClass 
		notifying: nil 
		ifFail: [self error: '[breakpoint] unable to install breakpoint'].
	^node ifNotNil: [ node generate: #(0 0 0 0) ]! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 5/20/2015 12:30'!
methodSelector
	^type == #method ifTrue:
		[(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string]! !


!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:09'!
removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Smalltalk actualScannerClass new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _ tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class metaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/11/2010 22:45'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."
	| flagByte source |
	flagByte := self last.
	(flagByte = 0
		or: [flagByte = 251 "some source-less methods have flag = 251, rest = 0"
			and: [((1 to: 3) allSatisfy: [:i | (self at: self size - i) = 0])]])
		ifTrue:
		["No source pointer -- decompile without temp names"
		^ (class decompilerClass new decompile: selector in: class method: self)
			decompileString].
	flagByte < 252 ifTrue:
		["Magic sources -- decompile with temp names"
		^ ((class decompilerClass new withTempNames: self tempNamesString)
				decompile: selector in: class method: self)
			decompileString].

	"Situation normal;  read the sourceCode from the file"
	
	source := [self getSourceFromFile]
				on: Error
		"An error can happen here if, for example, the changes file has been truncated by an aborted download.  The present solution is to ignore the error and fall back on the decompiler.  A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling into this error case, random source code will get returned."
				do: [ :ex | ex return: nil].
		
	^source
		ifNil: [
			"Something really wrong -- decompile blind (no temps)"
			 (class decompilerClass new decompile: selector in: class method: self)
				decompileString]! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'md 2/16/2006 17:08'!
decompile
	"Return the decompiled parse tree that represents self"

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].
	^class decompilerClass new decompile: selector in: class method: self.! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 8/12/2010 13:52'!
decompileWithTemps
	"Return the decompiled parse tree that represents self, but get the temp names
	 by compiling the sourcecode..."

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].

	(self fileIndex > 0 and: [(SourceFiles at: self fileIndex) isNil]) ifTrue: [
			"Emergency or no source file -- decompile without temp names "
			^self decompile.
	].
	^((self decompilerClass new withTempNames: self methodNode schematicTempNamesString)
						decompile: selector
						in: class
						method: self)! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 7/6/2009 15:57'!
methodNode
	"Return the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
	^(aClass parserClass new
		encoderClass: (self isBlueBookCompiled
						ifTrue: [EncoderForV3]
						ifFalse: [EncoderForV3PlusClosures]);
		parse: source class: aClass)
			sourceText: source;
			yourself! !


!Text methodsFor: 'emphasis' stamp: 'jmv 5/20/2015 12:31'!
makeSelectorBold
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."

	| i |
	string size = 0 ifTrue: [^ self].
	i _ 0.
	[(string at: (i _ i + 1)) isSeparator] whileTrue.
	(string at: i) = $[ ifTrue: [^ self].  "block, no selector"
	self makeBoldFrom: 1 to: (Compiler parserClass methodHeaderLengthFrom: string).! !

!Text methodsFor: 'emphasis' stamp: 'jmv 5/20/2015 12:32'!
makeSelectorBoldIn: aClass
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."
	| s |
	string size = 0 ifTrue: [^self].
	"Don't waste time on methods of absurd size"
	s _ string size < 2000
		ifTrue: [ string ]
		ifFalse: [ string readStream crLfNextLine ].
	self makeBoldFrom: 1 to: (aClass parserClass methodHeaderLengthFrom: s).! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/18/2011 14:41'!
macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes _ Smalltalk allClasses select: [:c | c name < 'B3'].
	badOnes _ OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes detectSum: [:c | c selectors size])
during: [:bar | n _ 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | bar value: (n _ n+1).
			oldMethod _ cls compiledMethodAt: selector.
			oldCodeString _ (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
testDecompiler
	"Smalltalk testDecompiler"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldMethod _ cls compiledMethodAt: selector.
					oldCodeString _ (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					newCodeString _ cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode _ cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldMethod _ cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldCodeString _ (cls sourceCodeAt: selector) asString.
					newCodeString _ cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens _ oldCodeString findTokens: Character separators.
					newTokens _ newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !


!Compiler methodsFor: 'public access' stamp: 'eem 5/15/2008 15:13'!
parser

	parser ifNil: [parser := self parserClass new].
	^parser! !


!Compiler class methodsFor: 'accessing' stamp: 'eem 5/15/2008 15:12'!
new
	^ super new parser: self parserClass new! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 8/24/2010 12:38'!
displayPatchFrom: srcString to: dstString tryWords: aBoolean prettyPrintedIn: aClass
	| formattedSrcString formattedDstString |
	formattedSrcString _ aClass
		ifNil: [ srcString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: srcString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					srcString ]].
	formattedDstString _ aClass
		ifNil: [ dstString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: dstString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					dstString ]].
	^ self
		displayPatchFrom: formattedSrcString
		to: formattedDstString
		tryWords: aBoolean.! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/25/2010 14:40'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: ['"is an argument to this ']
		ifFalse: ['"is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method"']
		ifFalse: ['block"'])! !


!BlockContext methodsFor: 'printing' stamp: 'md 2/22/2006 15:53'!
decompile
	^ home method decompilerClass new decompileBlock: self! !


!MethodNode methodsFor: 'source mapping' stamp: 'Igor.Stasenko 12/20/2009 03:54'!
rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := encoder classEncoding parserClass new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method! !

!MethodNode methodsFor: 'debugger support' stamp: 'Igor.Stasenko 12/20/2009 03:59'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := encoder classEncoding parserClass new
						encoderClass: encoder class;
						parse: (sourceText ifNil: [self decompileString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]! !


!PseudoClass methodsFor: 'methods' stamp: 'jmv 5/20/2015 12:33'!
addMethodChange: aChangeRecord
	| selector |
	selector _ self parserClass selectorFrom: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord! !

