"
The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode.

See Documentation/HitchHikersCompiler.md
"
Class {
	#name : 'Compiler',
	#superclass : 'Object',
	#instVars : [
		'sourceStream',
		'requestor',
		'class',
		'category',
		'context',
		'parser'
	],
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'user interface support' }
Compiler class >> autoCompleterClass [
	^SmalltalkCompleter
]

{ #category : 'accessing' }
Compiler class >> decompilerClass [
	^Decompiler
]

{ #category : 'user interface support' }
Compiler class >> editorClass [
	^SmalltalkEditor
]

{ #category : 'evaluating' }
Compiler class >> evaluate: textOrString  [
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	"See SHST80RangeType>>#receiverAsNumber - Hernan"
	^[ self evaluate: textOrString for: nil logged: false ]
		on: SyntaxErrorNotification 
		do: [ :anError | anError return: nil ]
]

{ #category : 'evaluating' }
Compiler class >> evaluate: textOrString for: anObject logged: logFlag  [
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag
]

{ #category : 'evaluating' }
Compiler class >> evaluate: textOrString for: anObject notifying: aRequestor logged: logFlag [
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
		profiled: false
]

{ #category : 'evaluating' }
Compiler class >> evaluate: textOrString logged: logFlag  [
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag
]

{ #category : 'evaluating' }
Compiler class >> evaluate: textOrString notifying: aRequestor logged: logFlag  [
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aRequestor logged: logFlag
]

{ #category : 'evaluating' }
Compiler class >> format: textOrStream in: aClass notifying: aRequestor [
	^self new format: textOrStream in: aClass notifying: aRequestor
]

{ #category : 'accessing' }
Compiler class >> new [
	^ super new parser: self parserClass new
]

{ #category : 'utilities' }
Compiler class >> notSameMethodsAfterCompilingAll [

	"
	self notSameMethodsAfterCompilingAll
	"
	
	| notEqual |
	
	notEqual := OrderedCollection new.

	ProtoObject withAllSubclassesDo: [ :class | 
		class selectorsDo: [ :selector | | currentCompiledMethod newCompiledMethod |
			currentCompiledMethod := class >> selector.
			newCompiledMethod := class
				basicCompile: currentCompiledMethod sourceCode
				notifying: nil
				trailer: class defaultMethodTrailer
				ifFail: [^self error: 'error compiling'].
			currentCompiledMethod = newCompiledMethod method ifFalse: [ notEqual add: currentCompiledMethod ]].	
		].

	^notEqual.
]

{ #category : 'accessing' }
Compiler class >> parserClass [
	"Answer a parser class to use for parsing methods compiled by instances of the receiver."

	^Parser
]

{ #category : 'utilities' }
Compiler class >> recompileAll [
	"Recompile all classes."

	Smalltalk allClasses do: [ :cls | cls compileAll] displayingProgress: 'Recompiling all classes'. 



]

{ #category : 'utilities' }
Compiler class >> recompileAllFrom: firstName  [
	"Recompile all classes, starting with given name."

	Smalltalk allClassesDo: [ :class | 
		class name >= firstName
			ifTrue: [
				Transcript show: class name; newLine.
				class compileAll ]]

	"Compiler recompileAllFrom: 'AAABodyShop'."

]

{ #category : 'user interface support' }
Compiler class >> textStylerClass [

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80
]

{ #category : 'public access' }
Compiler >> compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends [
	"Answer a MethodNode for the argument, textOrStream. If the
	MethodNode can not be created, notify the argument, aRequestor; if
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root
	of a parse tree. It can be told to generate a CompiledMethod to be
	installed in the method dictionary of the argument, aClass."

	| methodNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	category := aCategory.
	methodNode := self translate: sourceStream noPattern: false ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends.
	methodNode encoder requestor: requestor.
	^methodNode
]

{ #category : 'public access' }
Compiler >> compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock [
	^self compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock optimizeSpecialSends: true 
]

{ #category : 'public access' }
Compiler >> compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends [
	^self compile: textOrStream in: aClass classified: nil notifying: aRequestor ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends 
]

{ #category : 'public access' }
Compiler >> compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock [
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: true
		ifFail: failBlock
]

{ #category : 'public access' }
Compiler >> evaluate: aString in: aContext to: aReceiver [
	"evaluate aString in the given context, and return the result.
	Anser nil in case of failure"
	^self
		evaluate: aString
		in: aContext
		to: aReceiver
		notifying: nil
		ifFail: nil
]

{ #category : 'public access' }
Compiler >> evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock [

	^ self evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: false profiled: false
]

{ #category : 'public access' }
Compiler >> evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method |
	
	class := (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	methodNode := self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method := methodNode generate.
	method methodNode: methodNode.
	
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile
]

{ #category : 'public access' }
Compiler >> evaluateMethod: method to: receiver logged: doLog profiled: doProfile [
	
	"See evaluate:in:to:notifying:ifFail:logged:profiled:
	It does the same but without compiling because it recevies the result of the compilation 
	as the parameter method. 
	self should have compile method"

	| value toLog itsSelection itsSelectionString |
	
	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog := ((requestor respondsTo: #selection)  
			and: [ (itsSelection := requestor selection) notNil
			and: [ (itsSelectionString := itsSelection asPlainString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	"Evaluate now."
	doProfile
		ifTrue: [
			TimeProfileBrowser onBlock: [
				value := receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value := receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value
]

{ #category : 'public access' }
Compiler >> format: textOrStream in: aClass notifying: aRequestor [
	"Compile a parse tree from the incoming text, and then print the parse tree to yield the answer, a string containing the original code in standard format."

	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode := self format: sourceStream noPattern: false ifFail: [^nil].
	^aNode decompileString
]

{ #category : 'private' }
Compiler >> format: aStream noPattern: noPattern ifFail: failBlock [
	^(self parser
		parse: aStream
		class: class
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]) preen
]

{ #category : 'private' }
Compiler >> from: textOrStream class: aClass context: aContext notifying: req [

	sourceStream := (textOrStream is: #Stream)
		ifTrue: [ textOrStream ]
		ifFalse: [ textOrStream asPlainString readStream ].
	class := aClass.
	context := aContext.
	requestor := req.
]

{ #category : 'private' }
Compiler >> interactive [
	"this version of the method is necessary to load code from MC else the interactive mode is one. 
	This method is really bad since it links the compiler package with the Tools
	one. The solution would be to have a real SyntaxError exception belonging to the 
	compiler package and not a subclass of StringHolder - sd Nov 2005"
	"the code submitted by PlusTools is ideally the one that should be used
	interactive

	      ^requestor ~~ nil "

	^ requestor notNil
]

{ #category : 'error handling' }
Compiler >> notify: aString  [
	"Refer to the comment in Object|notify:."

	^self notify: aString at: sourceStream position + 1
]

{ #category : 'error handling' }
Compiler >> notify: aString at: location [
	"Refer to the comment in Object|notify:."

	^requestor == nil
		ifTrue: [SyntaxErrorNotification
					inClass: class
					category: category
					withCode: 
						(sourceStream contents
							copyReplaceFrom: location
							to: location - 1
							with: aString)
					doitFlag: false
					errorMessage: aString
					location: location]
		ifFalse: [requestor
					notify: aString
					at: location
					in: sourceStream]
]

{ #category : 'public access' }
Compiler >> parser [

	parser ifNil: [parser := self parserClass new].
	^parser
]

{ #category : 'public access' }
Compiler >> parser: aParser [

	parser := aParser
]

{ #category : 'public access' }
Compiler >> parserClass [

	^parser ifNil: [self class parserClass] ifNotNil: [parser class]
]

{ #category : 'private' }
Compiler >> translate: aStream noPattern: noPattern ifFail: failBlock [

	^self translate: aStream noPattern: noPattern ifFail: failBlock optimizeSpecialSends: true
]

{ #category : 'private' }
Compiler >> translate: aStream noPattern: noPattern ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends [

	^self parser
		optimizeSpecialSends: doOptimizeSpecialSends;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]
]
