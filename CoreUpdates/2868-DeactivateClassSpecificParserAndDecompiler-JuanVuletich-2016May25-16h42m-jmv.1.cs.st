'From Cuis 4.2 of 25 July 2013 [latest update: #2767] on 25 May 2016 at 4:44:29.997038 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'jmv 5/22/2016 22:32'!
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
"	answer _ (class decompilerClass new"
	"See comment at #useOldCompiler"
	answer _ (Smalltalk clsDecompiler new
		decompile: selector
		in: class
		method: method) decompileString.
	^ answer copy! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 5/22/2016 22:39'!
validateMessageSource: sourceString forSelector: aSelector inClass: theClass
	"Check whether there is evidence that method source is invalid"

"	(theClass parserClass selectorFrom: sourceString asString) = aSelector"
	"See comment at #useOldCompiler"
	(Smalltalk clsParser selectorFrom: sourceString asString) = aSelector
		ifFalse: [
			self inform: 'There may be a problem with your sources file!!

The source code for every method should (usually) start with the
method selector but this is not the case with this method!! You may
proceed with caution but it is recommended that you get a new source file.'].! !


!Browser methodsFor: 'message functions' stamp: 'jmv 5/22/2016 22:38'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
"	selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString."
	"See comment at #useOldCompiler"
	selector _ Smalltalk clsParser selectorFrom: aString.
	(self metaClassIndicated
		and: [(self selectedClassOrMetaClass includesSelector: selector) not
		and: [Metaclass isScarySelector: selector]])
		ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
				(self confirm: (selector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
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


!ChangeList methodsFor: 'scanning' stamp: 'jmv 5/22/2016 22:39'!
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
"						, ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method)"
	"See comment at #useOldCompiler"
						, (Smalltalk clsParser selectorFrom: method)
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/22/2016 22:31'!
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
							on: Smalltalk clsSyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
"								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: Smalltalk clsSyntaxErrorNotification do: [ :ex | ex return ]."
								"See comment at #useOldCompiler"
								currentCmWithNode _ [class basicCompile: (Smalltalk clsDecompiler new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: Smalltalk clsSyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !


!Debugger methodsFor: 'accessing' stamp: 'jmv 5/22/2016 22:41'!
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
"	selector :=self selectedClass parserClass selectorFrom: aText."
	"See comment at #useOldCompiler"
	selector :=Smalltalk clsParser selectorFrom: aText.
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


!Behavior methodsFor: 'compiling' stamp: 'jmv 5/22/2016 22:30'!
decompile: selector 
	"Find the compiled code associated with the argument, selector, as a 
	message selector in the receiver's method dictionary and decompile it. 
	Answer the resulting source code as a string. Create an error notification 
	if the selector is not in the receiver's method dictionary."

"	^self decompilerClass new decompile: selector in: self"
	"See comment at #useOldCompiler"
	^Smalltalk clsDecompiler new decompile: selector in: self! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 5/22/2016 22:37'!
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MessageSet isPseudoSelector: selector)
		ifTrue:
			["Not really a selector"
			^ nil].
	source _ self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
"	parser _ self parserClass new."
	"See comment at #useOldCompiler"
	parser _ Smalltalk clsParser new.
	tree _ 
		parser
			parse: (ReadStream on: source)
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 5/22/2016 22:37'!
methodHeaderFor: selector 
	"Answer the string corresponding to the method header for the given selector"

	| methodSource |
	methodSource _ self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
"	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)"
	"See comment at #useOldCompiler"
	^methodSource copyFrom: 1 to: (Smalltalk clsParser methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
"
! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 5/22/2016 22:39'!
methodSelector
	^type == #method ifTrue:
"		[(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string]"
	"See comment at #useOldCompiler"
		[Smalltalk clsParser new parseSelector: self string]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/22/2016 22:34'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."
	| flagByte source |
	flagByte := self last.
	(flagByte = 0
		or: [flagByte = 251 "some source-less methods have flag = 251, rest = 0"
			and: [((1 to: 3) allSatisfy: [:i | (self at: self size - i) = 0])]])
		ifTrue:
		["No source pointer -- decompile without temp names"
"		^ (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
		^ (Smalltalk clsDecompiler new decompile: selector in: class method: self)
			decompileString].
	flagByte < 252 ifTrue:
		["Magic sources -- decompile with temp names"
"		^ ((class decompilerClass new withTempNames: self tempNamesString)"
	"See comment at #useOldCompiler"
		^ ((Smalltalk clsDecompiler new withTempNames: self tempNamesString)
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
"			 (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
			 (Smalltalk clsDecompiler new decompile: selector in: class method: self)
				decompileString]! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/22/2016 22:32'!
decompile
	"Return the decompiled parse tree that represents self"

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].
"	^class decompilerClass new decompile: selector in: class method: self."
	"See comment at #useOldCompiler"
	^Smalltalk clsDecompiler new decompile: selector in: class method: self.! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/22/2016 22:33'!
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
"	^((self decompilerClass new withTempNames: self methodNode schematicTempNamesString)"
	"See comment at #useOldCompiler"
	^((Smalltalk clsDecompiler new withTempNames: self methodNode schematicTempNamesString)
						decompile: selector
						in: class
						method: self)! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/25/2016 16:43'!
methodNode
	"Return the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
"	^(aClass parserClass new"
	"See comment at #useOldCompiler"
	^(Smalltalk clsParser new
		encoderClass: (self isBlueBookCompiled
						ifTrue: [Smalltalk clsEncoderForV3]
						ifFalse: [Smalltalk clsEncoderForV3PlusClosures]);
		parse: source class: aClass)
			sourceText: source;
			yourself! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/22/2016 22:34'!
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
"			oldCodeString _ (cls decompilerClass new"
	"See comment at #useOldCompiler"
			oldCodeString _ (Smalltalk clsDecompiler new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
"			oldCodeString = (cls decompilerClass new"
	"See comment at #useOldCompiler"
			oldCodeString = (Smalltalk clsDecompiler new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/22/2016 22:35'!
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
"					oldCodeString _ (cls decompilerClass new"
	"See comment at #useOldCompiler"
					oldCodeString _ (Smalltalk clsDecompiler new
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
"						(cls decompilerClass new"
	"See comment at #useOldCompiler"
						(Smalltalk clsDecompiler new
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

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/22/2016 22:29'!
useOldCompiler
	"While #useOldCompiler is present and active, deactivate the possibility of classes specifying their own #parserClass and #decompilerClass.
	Reason is Compiler class implementing them: When having several Compilers present, each of them would ask for its own #parserClass and #decompilerClass, even when the system current should be used"
	UseZCompiler ifNil: [ UseZCompiler _ true ].
	^ UseZCompiler! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 7/27/2016 22:41'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
"	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass."
	"See comment at #useOldCompiler"
	methodNode := Smalltalk clsParser new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !


!BlockContext methodsFor: 'printing' stamp: 'jmv 5/22/2016 22:31'!
decompile
	"^ home method decompilerClass new decompileBlock: self"
	"See comment at #useOldCompiler"
	^ Smalltalk clsDecompiler new decompileBlock: self! !


!PseudoClass methodsFor: 'methods' stamp: 'jmv 5/22/2016 22:41'!
addMethodChange: aChangeRecord
	| selector |
"	selector _ self parserClass selectorFrom: aChangeRecord string."
	"See comment at #useOldCompiler"
	selector _ Smalltalk clsParser selectorFrom: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord! !


!ZMethodNode methodsFor: 'source mapping' stamp: 'jmv 5/22/2016 22:44'!
rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
"	methNode := encoder classEncoding parserClass new"
	"See comment at #useOldCompiler"
	methNode := Smalltalk clsParser new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method! !

!ZMethodNode methodsFor: 'debugger support' stamp: 'jmv 5/22/2016 22:43'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
"		methNode := encoder classEncoding parserClass new"
	"See comment at #useOldCompiler"
		methNode := Smalltalk clsParser new
						encoderClass: encoder class;
						parse: (sourceText ifNil: [self decompileString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]! !

