'From Cuis 4.2 of 25 July 2013 [latest update: #2768] on 22 May 2016 at 8:33:01.899451 pm'!

!CompilationCue commentStamp: 'nice 9/20/2013 02:07' prior: 0!
A CompilationCue is a helper class holding enough context for evaluating/compiling Smalltalk code.

That is mainly the source code, and the source code editor to interact with if the Compiler is used interactively.
But that is also any additional information necessary to resolve variable names.

When compiling a method, the Compiler typically need to know the target class in which to install the method.

When evaluating an expression, the Compiler also needs a receiver (for accessing the value of its instance variables), its class (for resolving instance/class variable names), and optionnally a context of execution when debugging a method (for accessing values of temporaries and parameters).

Instance Variables
	class:		<Behavior>
	context:		<ContextPart | nil>
	environment:		<Environment | nil>
	receiver:		<Object>
	requestor:		<TextEditor | nil>
	source:		<Stream>

class
	- the target class in which to install the compiled method;
	  this enables to resolve the instance variable names, class variable names and shared pool variable names.
	  When evaluating, this should be the receiver class

context
	- the context introspected when evaluating the code: this is typically for accessing parameters and temporary variables when debugging

environment
	- the environment in which to resolve global variable names

receiver
	- the receiver into which to evaluate the code: this is typically for accessing instance variables in an inspector

requestor
	- typically the text editor containing the source code being compiled/evaluated. This enables the Compiler to interact in case of syntax error.

source
	- a ReadStream on the source code to be compiled
!


!CompiledMethodTrailer commentStamp: 'ul 12/31/2009 19:03' prior: 0!
I am responsible for encoding and decoding various kinds of compiled method trailer data.
I should not expose any binary data outside of myself, so all tools which working with compiled methods
should ask me to encode the meta-data, they want to be added to the compiled method trailer, as well as retrieve it.

To add a new kind of trailer, you should give it a proper name and define it in the #trailerKinds method at my class side.
Then you need to implement a corresponding #encode<your name> and #decode<your name> methods at instance side. Then add any public accessor methods, which will use a newly introduced trailer kind for communicating with outer layer(s). And finally evaluate: self generateTrailerKindDecoders.

An encodeXXX methods should store result (byte array) into encodedData instance variable.

A decodeXXX methods should read the data from compiled method instance, held by 'method' ivar,
and always set 'size' ivar (denoting a total length of trailer in compiled method) and optionally 'data' ivar which should keep a decoded data, ready to be used by outer layer(s) using accessor method(s) you providing.

The kind of compiled method trailer is determined by the last byte of compiled method.

The byte format used is following: 
	"2rkkkkkkdd"

where 'k' bits stands for 'kind' , allowing totally 64 different kinds of method trailer
and 'd' bits is data.

Following is the list of currently defined trailer kinds:

NoTrailer , k = 000000, dd unused
method has no trailer, and total trailer size bytes is always 1

ClearedTrailer, k = 000001, 
method has cleared trailer (it was set to something else, but then cleared) 
dd+1  determines the number of bytes for size field, and size is a total length of trailer bytes
So a total length of trailer is: 1 + (dd + 1) + size

TempsNamesQCompress, k = 000010
the trailer contains a list of method temp names,  compressed using qCompress: method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer.
So a total length of trailer is:  1 + (dd + 1) + size

TempsNamesZip, k = 000011
the trailer contains a list of method temp names,  compressed using GZIP compression method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
So a total length of trailer is: 1 + (dd + 1) + size

SourceBySelector, k = 000100
the trailer indicates , that method source is determined by a class + selector where it is installed to. 
Trailer size = 1.

SourceByStringIdentifier, k = 000101
the trailer indicates , that method source is determined by a class + some ByteString identifier. 
dd+1  determines the number of bytes for size of ByteString identifier, and size is number of bytes of string.
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceQCompress, k = 000110
the trailer contains an utf-8 encoded method source code, compressed using qCompress method
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed source code
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceZip, k = 000111
the trailer contains an utf-8 encoded method source code, comressed using GZIP 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
A total length of trailer is:  1 + (dd + 1) + size

VarLengthSourcePointer, k = 001000
the trailer is variable-length encoded source pointer. 
dd bits is unused.

ExtendedKind, k = 001001
the next byte of trailer (one that prepends the last byte of compiled method)
denotes an extended kind of trailer, allowing to use additional 256 kinds of encoding method's trailer in future. 

SourcePointer, k = 111111 
the trailer is encoded source pointer. Total trailer size is 4-bytes 
(this kind of encoding is backwards compatible with most of existing compiled methods)

!


!Compiler commentStamp: 'cwp 12/26/2012 23:17' prior: 0!
The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor (usually a kind of StringHolderController) is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode.!


!SyntaxErrorNotification commentStamp: 'nice 9/18/2013 22:16' prior: 0!
A SyntaxErrorNotification is an Exception occuring when compiling a Smalltalk source code with incorrect syntax.
Note that in interactive mode, this exception is not raised because the Compiler will interact directly with source code editor.
The defaultAction is to raise a SyntaxError pop up window so as to enable interactive handling even in non interactive mode.

Instance Variables
	category:		<String | nil>
	code:		<String | Text | Stream>
	doitFlag:		<Boolean>
	errorMessage:		<String>
	inClass:		<Behavior>
	location:		<Integer>
	newSource:		<String | Text | Stream | nil>

category
	- the category in which the method will be classified

code
	- the source code to be compiled or evaluated

doitFlag
	- true if this is a doIt (code to evaluate), false if this is a method (code of a method to be compiled)

errorMessage
	- contains information about the syntax error

inClass
	- target class in which to compile the method

location
	- position in the source code where the syntax error occured

newSource
	- eventually hold a source code replacement typically passed by the SyntaxError window
!


!AmbiguousSelector commentStamp: 'nice 2/23/2010 15:40' prior: 0!
An AmbiguousSelector is a notification produced by the Scanner/Parser/Compiler when encountering this ambiguous construct:

1@-2

Upper expression can be interpreted both
1 @ -2 (regular st-80 and former Squeak syntax, the minus is attached to the literal number)
1 @- 2 (extended binary selector, the minus sign is allowed at any position and thus part of the binary selector)
!


!ReparseAfterSourceEditing commentStamp: 'nice 9/18/2013 22:05' prior: 0!
A ReparseAfterSourceEditing is a Notification used to restart the syntax parsing phase of a compilation after a source code edition.

Instance Variables
	newSource:		<UndefinedObject | String | Text | Stream>

newSource
	- this is the new source code to be used for restarting compilation if non interactive

In case of interactive compilation, newSource variable is nil because source code edition is performed directly in the source code editor, and the new source code will be picked directly there by the compiler.

In case of non interactive compilation, source code edition typically occurs in a SyntaxError window popping up. But the compiler has no direct access to this object, so newSource has to be passed by our intermediate.
!


!InstructionClient commentStamp: 'md 4/8/2003 12:50' prior: 0!
My job is to make it easier to implement clients for InstructionStream. See InstVarRefLocator
as an example. !


!BlockLocalTempCounter commentStamp: '<historical>' prior: 0!
I am a support class for the decompiler that is used to find the number of local temps in a block by finding out what the stack offset is at the end of a block.!


!ClosureExtractor commentStamp: 'mha 9/21/2010 11:16' prior: 0!
A ClosureExtractor is a utility class that is used to extract all BlockClosures from a CompiledMethod. It inherits from InstructionClient and understands only one single message, namely that corresponding to the push closure bytecode instruction. Being sent this message, a ClosureExtractor will create a BlockClosure instance and evaluate the block it holds as an instance variable with that closure as parameter.!


!InstVarRefLocator commentStamp: 'md 4/8/2003 12:50' prior: 0!
My job is to scan bytecodes for instance variable references.

BlockContext allInstances collect: [ :x |
	{x. x hasInstVarRef}
].!


!InstructionPrinter commentStamp: 'md 4/8/2003 12:47' prior: 0!
My instances can print the object code of a CompiledMethod in symbolic format. They print into an instance variable, stream, and uses oldPC to determine how many bytes to print in the listing. The variable method  is used to hold the method being printed.!


!Decompiler commentStamp: 'nice 2/3/2011 22:54' prior: 0!
I decompile a method in three phases:
	Reverser: postfix byte codes -> prefix symbolic codes (nodes and atoms)
	Parser: prefix symbolic codes -> node tree (same as the compiler)
	Printer: node tree -> text (done by the nodes)
	

instance vars:

	constructor <DecompilerConstructor> an auxiliary knowing how to generate Abstract Syntax Tree (node tree)
	method <CompiledMethod> the method being decompiled
	instVars <Array of: String> the instance variables of the class implementing method
	tempVars <String | (OrderedCollection of: String)> hold the names of temporary variables (if known)
		NOTE: POLYMORPHISM WILL BE RESOLVED IN #initSymbols:
	constTable <Collection of: ParseNode> parse node associated with byte encoded constants (nil true false 0 1 -1 etc...)
	stack <OrderedCollection of: (ParseNode | String | Integer) > multipurpose...
	statements <OrderedCollection of: ParseNode> the statements of the method being decompiled 
	lastPc <Integer>
	exit <Integer>
	caseExits <OrderedCollection of: Integer> - stack of exit addresses that have been seen in the branches of caseOf:'s
	lastJumpPc <Integer>
	lastReturnPc <Integer>
	limit <Integer>
	hasValue <Boolean>
	blockStackBase <Integer>
	numLocaltemps <Integer | Symbol> - number of temps local to a block; also a flag indicating decompiling a block
	blockStartsToTempVars <Dictionary key: Integer value: (OrderedCollection of: String)>
	tempVarCount <Integer> number of temp vars used by the method
	lastJumpIfPcStack <OrderedCollection of: Integer> the value of program counter just before the last encountered conditional jumps!


!ParseNode commentStamp: '<historical>' prior: 0!
This superclass of most compiler/decompiler classes declares common class variables, default messages, and the code emitters for jumps. Some of the class variables are initialized here; the rest are initialized in class VariableNode.!


!AssignmentNode commentStamp: '<historical>' prior: 0!
AssignmentNode comment: 'I represent a (var_expr) construct.'!


!BlockNode commentStamp: '<historical>' prior: 0!
I represent a bracketed block with 0 or more arguments and 1 or more statements. If I am initialized with no statements, I create one. I have a flag to tell whether my last statement returns a value from the enclosing method. My last three fields remember data needed for code generation. I can emit for value in the usual way, in which case I create a literal method (actually a context remotely copied) to be evaluated by sending it value: at run time. Or I can emit code to be evaluated in line; this only happens at the top level of a method and in conditionals and while-loops, none of which have arguments.!


!BraceNode commentStamp: '<historical>' prior: 0!
Used for compiling and decompiling brace constructs.

These now compile into either a fast short form for 4 elements or less:
	Array braceWith: a with: b ... 
or a long form of indefinfite length:
	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.

The erstwhile brace assignment form is no longer supported.!


!CascadeNode commentStamp: '<historical>' prior: 0!
The first message has the common receiver, the rest have receiver == nil, which signifies cascading.!


!DecompilerConstructor commentStamp: '<historical>' prior: 0!
I construct the node tree for a Decompiler.!


!Encoder commentStamp: 'cwp 12/26/2012 23:29' prior: 0!
I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.!


!BytecodeEncoder commentStamp: '<historical>' prior: 0!
I am an abstract superclass for different bytecode set encoders.  Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.!


!EncoderForV3 commentStamp: 'eem 5/17/2014 11:58' prior: 0!
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book.

	0-15 		0000iiii 	Push Receiver Variable #iiii
	16-31 		0001iiii 	Push Temporary Location #iiii
	32-63 		001iiiii 		Push Literal Constant #iiiii
	64-95 		010iiiii 		Push Literal Variable #iiiii
	96-103 	01100iii 	Pop and Store Receiver Variable #iii
	104-111 	01101iii 	Pop and Store Temporary Location #iii
	112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message
	124-125 	0111110i 	Return Stack Top From (Message, Block) [i]
	(126-127 unassigned)
	128 		10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	131 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	133 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments
	134 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	135 		10000111 	Pop Stack Top
	136 		10001000 	Duplicate Stack Top
	137 		10001001 	Push Active Context
	(138-143 unassigned)
	144-151 	10010iii 		Jump iii + 1 (i.e., 1 through 8)
	152-159 	10011iii 		Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj
	168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj
	176-191 	1011iiii 		Send Arithmetic Message #iiii
	192-207 	1100iiii 		Send Special Message #iiii
	208-223 	1101iiii 		Send Literal Selector #iiii With No Arguments
	224-239 	1110iiii 		Send Literal Selector #iiii With 1 Argument
	240-255 	1111iiii 		Send Literal Selector #iiii With 2 Arguments
!


!EncoderForV3PlusClosures commentStamp: '<historical>' prior: 0!
An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation.
	138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)

	140   10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	141   10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	142   10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii
This is an exact duplicate of EncoderForLongFormV3PlusClosures.
Could be a trait (or in Newspeak, a Mixin).
For now we impose upon you to synchronise any and all changes between these two classes.!


!FutureNode commentStamp: 'jcg 12/17/2009 02:03' prior: 0!
Compile-time transformation of #future and #future: messages.  Use is best described through examples:

	receiver future doSomething: arg1 withArgs: arg2.
	(receiver future: 2000) doSomethingElse

The first means to immediately schedule #doSomething:withArgs: for asyncronous evaluation.  The second means to wait 2000 milliseconds before scheduling #doSomethingElse for asynchronous evaluation.

These are transformed into either #futureDo:at:args: or #futureSend:at:args:, depending on whether the result is used.  Let's look at a few examples.

	[receiver future foo. 2+2] value.
	true ifTrue: [^receiver future foo].
	arraySize := receiver future getArray wait size.
	
In the first case, the result is never used, so the message #futureDo:at:args: is generated.  In the second case, the result is answered from the current method.  Since we don't do any cross-method analysis, we have to assume that the result is needed for a later computation.  The result is provided in the form of a Promise, which will resolve to a value when the asynchronous evaluation has completed.  Creating and resolving this Promise is the responsibility of #futureSend:at:args:, which is generated instead of #futureDo:at:args: when code-analysis indicates that the result of the message might be used.  The third example is another one where #futureSend:at:args: is generated.

See the default implementations of #futureDo:at:args: and #futureSend:at:args: in Object.  Subclasses are free to override the default implementations to achieve specific effects.  For example, this functionality originated in the Croquet class TFarRef.  If you have a TFarRef to a replicated object, then sending 'aTFarRef future foo' results in a message being sent over the network to each replica of the object referenced by aTFarRef.  We might also use far-refs, for example, to send a message to an object in another Hydra object-memory.!


!LeafNode commentStamp: '<historical>' prior: 0!
I represent a leaf node of the compiler parse tree. I am abstract.
	
Types (defined in class ParseNode):
	1 LdInstType (which uses class VariableNode)
	2 LdTempType (which uses class VariableNode)
	3 LdLitType (which uses class LiteralNode)
	4 LdLitIndType (which uses class VariableNode)
	5 SendType (which uses class SelectorNode).

Note that Squeak departs slightly from the Blue Book bytecode spec.

In order to allow access to more than 63 literals and instance variables,
bytecode 132 has been redefined as DoubleExtendedDoAnything:
		byte2				byte3			Operation
(hi 3 bits)  (lo 5 bits)
	0		nargs			lit index			Send Literal Message 0-255
	1		nargs			lit index			Super-Send Lit Msg 0-255
	2		ignored			rcvr index		Push Receiver Variable 0-255
	3		ignored			lit index			Push Literal Constant 0-255
	4		ignored			lit index			Push Literal Variable 0-255
	5		ignored			rcvr index		Store Receiver Variable 0-255
	6		ignored			rcvr index		Store-pop Receiver Variable 0-255
	7		ignored			lit index			Store Literal Variable 0-255

	This has allowed bytecode 134 also to be redefined as a second extended send
	that can access literals up to 64 for nargs up to 3 without needing three bytes.
	It is just like 131, except that the extension byte is aallllll instead of aaalllll,
	where aaa are bits of argument count, and lll are bits of literal index.!


!LiteralNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a literal string or number.!


!SelectorNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a selector.!


!KeyWordNode commentStamp: '<historical>' prior: 0!
I am a part of a selector.   #at:put: is owned by a SelectorNode, and #put: within it is owned by a KeyWordNode.!


!SpecialSelectorNode commentStamp: 'eem 8/5/2014 20:18' prior: 0!
A SpecialSelectorNode is a subclass of SelectorNode that handles the special selectors, a high static and/or dynamic frequency set of selectors that are assigned their own bytecodes.  Special selectors both save space in the literal frame and allow an interpreter to implement these sends directly for certain classes of receiver and argument, for example the SmallIntegers, a technique known as static receiver prediction.!


!VariableNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a variable. Note that my name and key are different for pool variables: the key is the Object Reference.!


!FieldNode commentStamp: '<historical>' prior: 0!
FileNode handles field access in Tweak, e.g. self fieldName := foo => self fieldName: foo.!


!MaybeContextInstanceVariableNode commentStamp: '<historical>' prior: 0!
This class conspires to arrange that inst var access for contexts is done exclusively using the long-form instance variabl;e access bytecodes.  See InstructionStream class>>variablesAndOffsetsDo:.

A virtual machine can benefit in performance by organizing method and block activations using a  more conventional stack organization than by using first-class activation records (contexts).  But such a virtual machine is also cabable of hiding the stack and making it appear as if contexts are still used.  This means the system has better performance but still has all the benefits of first-class activation records.  To pull this off the VM needs to intercept any and all accesses to context objects so that it can make contexts function as proxy objects for stack frames.

Without help from the image such a virtual machine based on an interpreter would have to perform an expensive check on all instance variable accesses to determine if the instance variable was that of a context serving as a proxy for a stack frame.  A simple hack is to take advantage of the short and long forms of instance variable access bytecodes.  The BlueBook instruction set (and likely any bytecode set evolved from it) has short form bytecodes for fetching and storing the first few bytecodes (BlueBook fetch first 16, store first 8).  Contexts typically have at most 6 instance variables.  If we arrange to use the long-form bytecodes for all context inst var accesses then we only have to check for context inst var access in long-form bytecodes, and then only if the index is within the context inst var range.  This effectively makes the check free because on modern processors checking an index fetched from memory into a register against a constant costs far less than the memry read to fetch the index.!


!TempVariableNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a temporary variable!


!RemoteTempVectorNode commentStamp: '<historical>' prior: 0!
I am a node for a vector of remote temps, created to share temps between closures when those temps are written to in closures other than their defining ones.!


!MessageNode commentStamp: '<historical>' prior: 0!
I represent a receiver and its message.
	
Precedence codes:
	1 unary
	2 binary
	3 keyword
	4 other
	
If special>0, I compile special code in-line instead of sending messages with literal methods as remotely copied contexts.!


!MessageAsTempNode commentStamp: '<historical>' prior: 0!
This node represents accesses to temporary variables for do-its in the debugger.  Since they execute in another context, they must send a message to the original context to access the value of the temporary variable in that context.!


!MethodNode commentStamp: 'eem 8/15/2010 10:49' prior: 0!
I am the root of the parse tree..

Instance Variables
	arguments:			<SequenceableCollection>
	block:				<BlockNode>
	encoder:			<BytecodeEncoder>
	localsPool:			<IdentitySet>
	locationCounter:	<Integer>
	precedence:		<Integer>
	primitive:			<Integer>
	properties:			<AdditionalMethodState|nil>
	selectorOrFalse:	<Object>
	sourceText:		<String|Text>
	temporaries:		<SequenceableCollection>

arguments
	- the collection of parsed or decompiled method arguments

block
	- the BlockNode holding the method's statements

encoder
	- the object that comprises the copiler's scope table, literal pool and back-end bytecode generator

localsPool
	- a set used to determine the set of copied values for each block in the method

locationCounter
	- an integer used to mark block scopes for the purposes of the closure transformation.  See BlockNode>>#analyseArguments:temporaries:rootNode:

precedence
	- the precedence of the method's selector (see Symbol>>precedence)

primitive
	- if non-zero this is the integer code of the method's primitive

properties
	- the object used to accumulate method properties (a.k.a. pragmas)

selectorOrFalse
	- the method's selector or false if this is a doit

sourceText
	- the source test from which the method was compiled

temporaries
	- the collection of parsed or decompiled method temporaries
!


!NewArrayNode commentStamp: '<historical>' prior: 0!
I represent a node for the genPushNewArray: opcode.!


!ReturnNode commentStamp: '<historical>' prior: 0!
I represent an expression of the form ^expr.!


!ParseNodeVisitor commentStamp: '<historical>' prior: 0!
I am an abstract superclass for ParseNode visitors that functions as a null visitor.  Here's the code that defines my interface:

(SystemNavigation default allImplementorsOf: #accept: localTo: ParseNode) do:
	[:methodReference|
	methodReference compiledMethod messages do:
		[:sel|
		((sel beginsWith: 'visit')
		and: [sel numArgs = 1]) ifTrue:
			[ParseNodeVisitor
				compile: (String streamContents:
							[:str|
							str nextPutAll: sel;
								space;
								nextPut: $a.
							methodReference classSymbol first isVowel ifTrue:
								[str nextPut: $n].
							str nextPutAll: methodReference classSymbol])
				classified: 'visiting']]]!


!OptimizedBlockLocalTempReadBeforeWrittenVisitor commentStamp: '<historical>' prior: 0!
Answer the set of temporary variables that are read before they are written in the visited parse tree.  Used by the compiler to detect those block-local temporaries of blocks in optimized loops that require nilling to prevent a value from a previous iteration persisting into a subsequent one.!


!ParseNodeEnumerator commentStamp: 'eem 8/31/2010 11:41' prior: 0!
ParseNodeEnumerator implements ParseNode>>nodesDo:.  It can be used to enumerate an entire tree via
	aParseNode accept: (ParseNodeEnumerator ofBlock: aBlock)
or selectively, excluding the node and subnodes for which selectBlock answers false, via
	aParseNode accept: (ParseNodeEnumerator
							ofBlock: aBlock
							select: selectBlock)

Here's a doIt that generates and compiles the visiting methods:

self superclass selectors do:
	[:s|
	self compile: (String streamContents:
		[:str| | arg |
		arg := 'a', (s allButFirst: 5) allButLast.
		str nextPutAll: s, ' ', arg; crtab;
			nextPutAll: '(theSelectBlock isNil or: [theSelectBlock value: '; nextPutAll: arg; nextPutAll: ']) ifFalse:'; crtab;
			tab: 2; nextPutAll: '[^nil].'; crtab;
			nextPutAll: 'theBlock value: '; nextPutAll: arg; nextPut: $.; crtab;
			nextPutAll: '^super '; nextPutAll: s, ' ', arg])]!


!VariableScopeFinder commentStamp: 'eem 8/14/2010 19:45' prior: 0!
A VariableScopeFinder is used to find the minimum enclosing scope of a variable in a method.  This is used when auto-declaring temporaries to find the smallest enclosing block in which to declare the temp.

Instance Variables
	theVariable:		<VariableNode>

theVariable
	- the varable whose scope is to be determined
!


!ParseStack commentStamp: '<historical>' prior: 0!
I keep track of the current and high position of the stack that will be needed by code being compiled.!


!Scanner commentStamp: 'ul 4/3/2011 02:04' prior: 0!
I scan a string or text, picking out Smalltalk syntactic tokens. I look one character ahead. I put each token found into the instance variable, token, and its type (a Symbol) into the variable, tokenType. At the end of the input stream, I pretend to see an endless sequence of special characters called doIts.

Instance Variables
	aheadChar:		<Character>
	buffer:		<WriteStream>
	currentComment:		<OrderedCollection>
	hereChar:		<Character>
	mark:		<Integer>
	source:		<ReadStream>
	token:		<Symbol|String|NumberCharacter|Boolean|nil>
	tokenType:		<Symbol>
	typeTable:		<Array>

aheadChar
	- the next character in the input stream

buffer
	- a reusable WriteStream on a String which is used for building strings. Shouldn't be used from multiple methods without resetting.

currentComment
	- an OrderedCollection of strings which contain all comments between the current token and the previous token or the beginning of the source.

hereChar
	- the current character

mark
	- the position of the current token in the source stream

source
	- the input stream of characters

token
	- the current token

tokenType
	- the type of the current token. The possible token types are: #binary, #character, #colon, #doIt, #keyword, #leftArrow, #leftBrace, #leftBracket, #leftParenthesis, #literal, #period, #rightBrace, #rightBracket, #rightParenthesis, #semicolon, #string, #upArrow, #verticalBar, #word, #xBinary, #xColon, #xDelimiter, #xDigit, #xDollar, #xDoubleQuote, #xLetter, #xLitQuote, #xSingleQuote, #xUnderscore

typeTable
	- an array that maps each an evaluable tokenType to each character with asciiValue between 0 and 255!


!Parser commentStamp: 'cwp 12/26/2012 23:34' prior: 0!
I parse Smalltalk syntax and create a MethodNode that is the root of the parse tree. I look one token ahead.!


!CompilationCue methodsFor: 'binding' stamp: 'cwp 6/20/2012 09:39'!
bindingOf: aSymbol
	^ class bindingOf: aSymbol environment: environment! !

!CompilationCue methodsFor: 'binding' stamp: 'cwp 6/22/2012 15:39'!
literalScannedAs: anObject notifying: anEncoder
	^ class literalScannedAs: anObject environment: environment notifying: anEncoder! !

!CompilationCue methodsFor: 'accessing' stamp: 'cwp 12/26/2012 23:19'!
context
	^ context! !

!CompilationCue methodsFor: 'accessing' stamp: 'cwp 6/19/2012 11:15'!
environment
	^ environment! !

!CompilationCue methodsFor: 'accessing' stamp: 'cwp 6/19/2012 11:16'!
getClass
	^ class! !

!CompilationCue methodsFor: 'accessing' stamp: 'cwp 6/19/2012 11:15'!
receiver
	^ receiver! !

!CompilationCue methodsFor: 'accessing' stamp: 'cwp 6/19/2012 11:16'!
requestor
	^ requestor! !

!CompilationCue methodsFor: 'accessing' stamp: 'nice 9/17/2013 23:04'!
sourceStream
	^source! !

!CompilationCue methodsFor: 'accessing' stamp: 'nice 9/17/2013 23:04'!
stringToLog
	"Answer a string to be logged in a change log.
	Implementation note:
	If the requestor is a TextEditor, preferably take its selection.
	This convoluted code is presumably crafted to avoid broken contents
	(ReadStream on: '123456' from: 3 to: 4) contents -> '1234'
	As long as selectionAsStream is using such construct this might be required."
	| itsSelection itsSelectionString |
	^((requestor respondsTo: #selection)  
			and:[(itsSelection := requestor selection) notNil
			and:[(itsSelectionString := itsSelection asString) isEmptyOrNil not]])
				ifTrue:[itsSelectionString]
				ifFalse:[self sourceStream contents]! !

!CompilationCue methodsFor: 'initialization' stamp: 'nice 9/20/2013 02:03'!
initializeWithSource: aTextOrString context: aContext receiver: recObject class: aClass environment: anEnvironment requestor: reqObject
	self initialize.
	source := (aTextOrString isKindOf: PositionableStream)
		ifTrue: [ aTextOrString ]
		ifFalse: [ ReadStream on: aTextOrString asString ].
	context := aContext.
	receiver := recObject.
	class := aClass.
	environment := anEnvironment.
	requestor := reqObject! !


!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:04'!
class: aClass
	^ self 
		source: nil
		context: nil
		receiver: nil
		class: aClass
		environment: (aClass ifNotNil: [aClass environment])
		requestor: nil! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 00:50'!
source: aTextOrStream
	^ self
		source: aTextOrStream
		class: nil
		requestor: nil! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:03'!
source: aTextOrStream class: aClass environment: anEnvironment requestor: anObject
	^ self
		source: aTextOrStream
		context: nil
		receiver: nil
		class: aClass
		environment: anEnvironment
		requestor: anObject! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:04'!
source: aTextOrStream class: aClass requestor: anObject
	^ self 
		source: aTextOrStream 
		context: nil 
		receiver: nil
		class: aClass 
		environment: (aClass ifNotNil: [aClass environment])
		requestor: anObject! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:04'!
source: aTextOrStream context: aContext class: aClass requestor: anObject
	^ self 
		source: aTextOrStream 
		context: aContext 
		receiver: (aContext ifNotNil: [aContext receiver])
		class: aClass 
		environment: (aClass ifNotNil: [aClass environment])
		requestor: anObject! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:03'!
source: aTextOrStream context: aContext receiver: recObject class: aClass environment: anEnvironment requestor: reqObject
	^ self basicNew
		initializeWithSource: aTextOrStream 
		context: aContext 
		receiver: recObject 
		class: aClass 
		environment: anEnvironment 
		requestor: reqObject! !

!CompilationCue class methodsFor: 'instance creation' stamp: 'nice 9/20/2013 02:05'!
source: aString environment: anEnvironment
	^ self 
		source: aString
		context: nil
		receiver: nil
		class: UndefinedObject
		environment: anEnvironment
		requestor: nil! !


!CompiledMethodTrailer methodsFor: 'initialize-release' stamp: 'Igor.Stasenko 12/13/2009 11:38'!
clear
	kind := #NoTrailer.
	size := 1.
	data := encodedData := method := nil! !

!CompiledMethodTrailer methodsFor: 'initialize-release' stamp: 'Igor.Stasenko 12/13/2009 11:51'!
initialize
	self clear! !

!CompiledMethodTrailer methodsFor: 'initialize-release' stamp: 'ul 12/31/2009 17:05'!
method: aMethod

	| flagByte index |
	data := size := nil.
	method := aMethod.
	flagByte := method at: method size.

	"trailer kind encoded in 6 high bits of last byte"
	index := flagByte >> 2 + 1.
	kind := self class trailerKinds at: index.

	"decode the trailer bytes"
	self perform: (self class trailerKindDecoders at: index).
	
	"after decoding the trailer, size must be set"
	self assert: size notNil
	
! !

!CompiledMethodTrailer methodsFor: 'creating a method' stamp: 'eem 5/20/2012 11:42'!
compiledMethodClass
	"Allow subclasses to create instyances of classes other than CompiledMethod."

	^CompiledMethod! !

!CompiledMethodTrailer methodsFor: 'creating a method' stamp: 'eem 9/13/2011 15:14'!
createMethod: numberOfBytesForAllButTrailer class: aCompiledMethodClass header: headerWord 
	| meth |
	encodedData ifNil: [self encode].
	
	meth := aCompiledMethodClass newMethod: numberOfBytesForAllButTrailer + size header: headerWord.
	"copy the encoded trailer data"
	1 to: size do:
		[:i | meth at: meth size - size + i put: (encodedData at: i)].

	^meth! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 15:31'!
decodeClearedTrailer
	"Size is set in #decodeLengthField"

	self decodeLengthField.
! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 15:57'!
decodeEmbeddedSourceQCompress

	"data is string with method's source code, encoded using qCompress method"
	self qDecompress.! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 11:56'!
decodeNoTrailer
	"Not much to decode here"
	size := 1. ! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 16:35'!
decodeSourceBySelector

	"no data, size = 1"
	
	size := 1.! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/20/2009 21:36'!
decodeSourceByStringIdentifier

	"A method source is determined by a class + string identifier"
	| len |
	
	len := self decodeLengthField.

	data := (ReadStream on: method from: method size - size+1 to: method size - size + len) contents asString convertFromEncoding: 'utf8'! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 11:25'!
decodeSourcePointer

	"Trailer is a source pointer"
	| msz |
	
	size := 4.
	msz := method size.
	data := (method at: msz) - 251 << 8 + (method at: msz-1) << 8
	 + (method at: msz-2) << 8 + (method at: msz-3).
	
! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 15:57'!
decodeTempsNamesQCompress

	"data is string with method's temporary names, encoded using qCompress method"
	self qDecompress.! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 16:34'!
decodeUndefined
	self error: 'undefined method encoding'! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/13/2009 19:34'!
decodeVarLengthSourcePointer

	| pos shift |
	
	pos := method size-1.
	shift := data := 0.
	
	[ | value | 
		value := method at: pos.
		data := (value bitAnd: 16r7F) << shift + data.
		pos := pos - 1.
		shift := shift + 7.
		value > 127 ] whileTrue.

	size := method size - pos.! !

!CompiledMethodTrailer methodsFor: 'decoding' stamp: 'Igor.Stasenko 12/20/2009 21:33'!
qDecompress 
	"Trailer is compressed string using qCompress method + length field + 1 byte
	
	Decompress strings compressed by qCompress:.
	Most common 11 chars get values 0-10 packed in one 4-bit nibble;
	next most common 52 get values 12-15 (2 bits) * 16 plus next nibble;
	escaped chars get three nibbles"
	| len str input |
	
	len := self decodeLengthField.
 	input := (ReadStream on: method from: method size - size+1 to: method size - size + len).
	
	str := String streamContents:
		[:strm | | nextNibble nibble peek charTable |
		charTable :=  "Character encoding table must match qCompress:"
		'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'.
		peek := true.
		nextNibble := [peek
						ifTrue: [peek := false. input peek ifNil: [0] ifNotNil: [:b| b // 16]]
						ifFalse: [peek := true. input next ifNil: [0] ifNotNil: [:b| b \\ 16]]].
		[input atEnd] whileFalse:
			[(nibble := nextNibble value) = 0
				ifTrue: [input atEnd ifFalse:
						[strm nextPut: (Character value: nextNibble value * 16 + nextNibble value)]]
				ifFalse:
					[nibble <= 11
						ifTrue:
							[strm nextPut: (charTable at: nibble)]
						ifFalse:
							[strm nextPut: (charTable at: nibble-12 * 16 + nextNibble value)]]]].
			
	data := str convertFromEncoding: 'utf8'! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 14:30'!
decodeExtendedKind

	"reserved for future use"
	self shouldBeImplemented.
! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 18:40'!
encode

	encodedData := nil.
	
	"encode the trailer into byte array"
	self perform: ('encode' , kind) asSymbol.

	self assert: (encodedData notNil and: [encodedData size > 0 ]).
	
	"set the size"
	size := encodedData size.! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 15:16'!
encodeClearedTrailer

	"A cleared trailer is replaced by #NoTrailer, when used for encoding"
	self clear.
	kind := #NoTrailer.
	
	^ self encode! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 14:29'!
encodeEmbeddedSourceQCompress

	"data is string with method's source code, encode it using qCompress method"
	self encodeUsingQCompress
! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 15:02'!
encodeExtendedKind

	"reserved for future use"
	self error: 'Not yet implemented'.
! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 11:55'!
encodeNoTrailer

	encodedData := ByteArray with: self kindAsByte! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 14:23'!
encodeSourceBySelector

	"A method source is determined by a class + selector where it is installed to"
	encodedData := ByteArray with: self kindAsByte! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'nice 12/26/2009 01:20'!
encodeSourceByStringIdentifier

	"A method source is determined by a class + string identifier"
	
	self assert: (data isString).
	
	encodedData := ByteArray streamContents: [:str |
		| utf8str len |
		utf8str := (data convertToEncoding: 'utf8') asByteArray.
		str nextPutAll: utf8str.
		len := self encodeLengthField: utf8str size.
		str nextPutAll: len.
		str nextPut: self kindAsByte + (len size -1)
	]! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 11:43'!
encodeSourcePointer

	encodedData := ByteArray new: 4.
	encodedData at: 4 put: (data >> 24) + 251.

	1 to: 3 do: [:i |
		encodedData at: 4-i put: ((data bitShift: (i-3)*8) bitAnd: 16rFF)]! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 13:30'!
encodeTempsNamesQCompress

	"data is string with method's temporary names, encode it using qCompress method"
	self encodeUsingQCompress
! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 14:38'!
encodeUndefined

	self error: 'use of an undefined kind of trailer encoding'! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'Igor.Stasenko 12/13/2009 13:26'!
encodeUsingQCompress

	"data is string, encode it using qCompress method"
	| str length encodedLength |

	self assert: data isString.
	str := self qCompress: data.
	length := str position.
	encodedLength := self encodeLengthField: length.

	str nextPutAll: encodedLength.
	"trailing byte"
	str nextPut: (self kindAsByte + encodedLength size - 1).
	
	encodedData := str contents
	! !

!CompiledMethodTrailer methodsFor: 'encoding' stamp: 'IgorStasenko 5/10/2011 13:21'!
encodeVarLengthSourcePointer

	"source pointer must be >=0"
	[data >= 0] assert.
	
	encodedData := 
		data = 0 ifTrue: [ #[0] ] 
		ifFalse: [ ByteArray streamContents: [:str |
		| value |
		value := data.
		[value > 0] whileTrue: [
			value > 127 ifTrue: [ str nextPut: 128 + (value bitAnd: 16r7F) ]
				ifFalse: [ str nextPut: value. ].
			value := value >> 7.
			].
		]].
	encodedData := encodedData reversed copyWith: (self kindAsByte)! !

!CompiledMethodTrailer methodsFor: 'private' stamp: 'Igor.Stasenko 12/13/2009 15:29'!
decodeLengthField

	"used in various encodings, where length field is preceeding the last trailer byte.
	Two least significant bits in last byte denoting the number of bytes for length field"
	
	| numBytes pos length |

	pos := method size.
	numBytes := ((method at: pos) bitAnd: 3) + 1.
	
	length := 0.
	1 to: numBytes do: [:i |
		length := length << 8 + (method at: pos - i ).
		].
	size := 1 + numBytes + length.
	
	^ length! !

!CompiledMethodTrailer methodsFor: 'private' stamp: 'Igor.Stasenko 12/13/2009 13:28'!
encodeLengthField: integer

	| bytes value |
	self assert: (integer > 0).

	value := integer.
	
	bytes := ByteArray streamContents: [:str |
		[ value > 0 ] whileTrue: [
			str nextPut: (value bitAnd: 255).
			value := value >> 8 ]].

	"no more than 4 bytes for length field"
	self assert: (bytes size <=4).

	^ bytes! !

!CompiledMethodTrailer methodsFor: 'private' stamp: 'ul 11/15/2010 10:11'!
kindAsByte
	| index |
	index := self class trailerKinds indexOf: kind.
	self assert: (index ~= 0).

	^ (index - 1) << 2! !

!CompiledMethodTrailer methodsFor: 'private' stamp: 'nice 12/27/2009 03:12'!
qCompress: string
	"A very simple text compression routine designed for method temp names.
	 Most common 11 chars get values 1-11 packed in one 4-bit nibble;
	 the next most common get values 12-15 (2 bits) * 16 plus next nibble;
	 unusual ones get three nibbles, the first being the escape nibble 0.

	Answer the write stream with compressed data inside"

	| utf8str stream oddNibble |

	string isEmpty ifTrue:
		[^self qCompress: ' '].
	utf8str := string convertToEncoding: 'utf8'.

	stream := WriteStream on: (ByteArray new: utf8str size).
	oddNibble := nil.

	utf8str do:	[:char | | ix |
		ix := 'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'
			indexOf: char ifAbsent: 0.
		(ix = 0
			ifTrue:
				[{ 0. char asInteger // 16. char asInteger \\ 16 }]
			ifFalse:
				[ix <= 11
					ifTrue: [{ ix }]
					ifFalse: [{ ix//16+12. ix\\16 }]])
				do: [:nibble |
					oddNibble
						ifNotNil: [stream nextPut: oddNibble*16 + nibble. oddNibble := nil]
						ifNil: [oddNibble := nibble]]].
	oddNibble ifNotNil: "4 = 'ear tonsil' indexOf: Character space"
		[stream nextPut: oddNibble * 16 + 4].
	^ stream
! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 15:50'!
endPC
	"Answer the index of the last bytecode."
	
	method ifNil: [ self error: 'Cannot determine the endPC without compiled method' ].
	
	"if method set, then size should be set as well"
	^ method size - size! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 19:15'!
kind
	"Answer a symbolic name of trailer kind.
	See #trailerKinds on class side and class comment for details"
	
	^ kind! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 21:49'!
setSourceBySelector

	"Trailer is used to indicate that method's source code can be retrieved by 
	sending #getSourceCodeBySelector: message to method's class" 
	
	self clear.
	kind := #SourceBySelector! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 16:45'!
size
	"Answer the size of method's trailer , in bytes"
	^ size! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 21:47'!
sourceCode
	"Answer the source code of compiled method.
	Note: it does not attempts to read from source files using sourcePointer,
	nor reconstruct the source code using temp names"
	
	(kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ]) ifTrue: [
		^ data ].
	
	kind == #SourceBySelector ifTrue: [
		^ method methodClass getSourceCodeBySelector: method selector ].
	
	kind == #SourceByStringIdentifier ifTrue: [
		^ method methodClass getSourceCodeByIdentifier: data ].

	^ nil! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/22/2009 22:23'!
sourceCode: aString
	"Embed the source code into compiled method trailer, 
	pick best compression method"
	| temp |
	self clear.
	kind := #EmbeddedSourceQCompress.
	data := aString asString. "add Text support in future?"
	
	self encode.
	temp := encodedData.

	kind := #EmbeddedSourceZip.
	self encode.
	encodedData size > temp size ifTrue: [
		encodedData := temp.
		kind := #EmbeddedSourceQCompress.
		size := encodedData size.
		]! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 21:03'!
sourceIdentifier
	"Trailer is used to indicate that method's source code can be retrieved by 
	sending #getSourceCodeByIdentifier: message to method's class" 

	^ (kind == #SourceByStringIdentifier) ifTrue: [ data ] ifFalse: [ nil ].

! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 21:03'!
sourceIdentifier: aString

	"Trailer is used to indicate that method's source code can be retrieved by 
	sending #getSourceCodeByIdentifier: message to method's class" 
	
	self clear.
	data := aString.
	kind := #SourceByStringIdentifier.
	self encode.
! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 11:27'!
sourcePointer

	^ (kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ] )
		ifTrue: [ data ]
		ifFalse: [ 0 ]
! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 11:39'!
sourcePointer: ptr

	self clear.
	data := ptr.
	"see if we can encode pointer using 4-byte trailer"
	kind := (ptr between: 16r1000000 and: 16r4FFFFFF) 
		ifTrue: [ #SourcePointer ] ifFalse: [ #VarLengthSourcePointer ].
	! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 20:54'!
tempNames
	"Answer the string, containing the temps names or nil "
	^ (kind == #TempsNamesQCompress or: [ kind == #TempsNamesZip ]) 
		ifTrue: [ data ] ifFalse: [ nil ]! !

!CompiledMethodTrailer methodsFor: 'accessing' stamp: 'Igor.Stasenko 12/13/2009 20:52'!
tempNames: aString
	"Embed the temp names string into compiled method trailer, 
	pick best compression method"
	| temp |
	self clear.
	kind := #TempsNamesQCompress.
	data := aString.
	
	self encode.
	temp := encodedData.

	kind := #TempsNamesZip.
	self encode.
	encodedData size > temp size ifTrue: [
		encodedData := temp.
		kind := #TempsNamesQCompress.
		size := encodedData size.
		]! !

!CompiledMethodTrailer methodsFor: 'testing' stamp: 'Igor.Stasenko 12/13/2009 16:52'!
hasSourcePointer
	^  kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ] ! !

!CompiledMethodTrailer methodsFor: 'testing' stamp: 'Igor.Stasenko 12/13/2009 16:51'!
hasTempNames
	^ kind == #TempsNamesQCompress or: [ kind == #TempsNamesZip ]! !

!CompiledMethodTrailer methodsFor: 'testing' stamp: 'Igor.Stasenko 12/13/2009 16:54'!
isEmpty
	^ kind == #NoTrailer or: [ kind == #ClearedTrailer ]! !

!CompiledMethodTrailer methodsFor: 'testing' stamp: 'Igor.Stasenko 12/13/2009 20:12'!
testEncoding

	"Since we are using basic protocol (#at:, #at:put: , #size) for accessing compiled method data,
	we can pass the ByteArray instance into #method: accessor and check if encoding/decoding 
	operations	is symmetrical.
	Use this method only for unit-testing purposes"
	
	encodedData ifNil: [ self encode ].
	^ CompiledMethodTrailer new method: encodedData! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'Igor.Stasenko 12/13/2009 16:33'!
decodeEmbeddedSourceZip

	"data is string with method's source code, compressed using zip compression"
	self decodeZip.! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'Igor.Stasenko 12/13/2009 16:33'!
decodeTempsNamesZip

	"data is string with method's temporary names, compressed using zip compression"
	self decodeZip.! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'ul 12/22/2009 20:10'!
decodeZip

	"data := <trailer> unzip utf8ToSqueak"
	| len bytes |
	len := self decodeLengthField.
	bytes := ByteArray new: len.
	1 to: len do: [ :i |
		bytes at: i put: (method at: method size - size + i) ].
	
	data := (ZipReadStream on: bytes) contents asString convertFromEncoding: 'utf8'! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'Igor.Stasenko 12/13/2009 14:29'!
encodeEmbeddedSourceZip

	"data is string with method's source code, encode it using Zip compression method"
	self encodeUsingZip
! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'Igor.Stasenko 12/13/2009 15:56'!
encodeTempsNamesZip

	"data is string with method's temporary names, encode it using zip compression"
	self encodeUsingZip
	
	"data is string with method's source code, encoded using qCompress method"


! !

!CompiledMethodTrailer methodsFor: '*Compression-Streams' stamp: 'Igor.Stasenko 12/20/2009 21:32'!
encodeUsingZip

	"data is string, encode it using gzip compression"
	| utf8str stream length encodedLength |

	self assert: (data isString).
	utf8str := data convertToEncoding: 'utf8'.
	
	stream := ((ZipWriteStream on: (ByteArray new: utf8str size))
		nextPutAll: utf8str asByteArray;
		close;
		encodedStream).
		
	length := stream position.
	encodedLength := self encodeLengthField: length.

	stream nextPutAll: encodedLength.
	"trailing byte"
	stream nextPut: (self kindAsByte + encodedLength size - 1).
	
	encodedData := stream contents
	! !


!CompiledMethodTrailer class methodsFor: 'as yet unclassified' stamp: 'Igor.Stasenko 12/14/2009 10:08'!
empty
	"answer the empty trailer"
	^ self new! !

!CompiledMethodTrailer class methodsFor: 'as yet unclassified' stamp: 'ul 12/31/2009 19:01'!
generateTrailerKindDecoders

	self class
		compile: (String streamContents: [ :stream |
			stream
				nextPutAll: 'trailerKindDecoders'; cr;
				cr;
				tab; nextPut: $^; print: (
					self trailerKinds collect: [ :each | 
						('decode', each) asSymbol ]) ])
		classified: 'generated'
		! !

!CompiledMethodTrailer class methodsFor: 'as yet unclassified' stamp: 'ul 10/21/2014 12:33'!
sourcePointerInFile: fileIndex

	^self new
		sourcePointer: (SourceFiles
			sourcePointerFromFileIndex: fileIndex
			andPosition: (SourceFiles at: fileIndex) position);
		yourself
		! !

!CompiledMethodTrailer class methodsFor: 'as yet unclassified' stamp: 'ul 12/31/2009 18:54'!
trailerKinds
	" see class comment for description. If you change this method, evaluate this:
	self generateTrailerKindDecoders"
	
	^#(
"000000" #NoTrailer
"000001" #ClearedTrailer
"000010" #TempsNamesQCompress
"000011" #TempsNamesZip
"000100" #SourceBySelector
"000101" #SourceByStringIdentifier
"000110" #EmbeddedSourceQCompress
"000111" #EmbeddedSourceZip
"001000" #VarLengthSourcePointer
"001001" #ExtendedKind
"001010" #Undefined
"001011" #Undefined
"001100" #Undefined
"001101" #Undefined
"001110" #Undefined
"001111" #Undefined
"010000" #Undefined
"010001" #Undefined
"010010" #Undefined
"010011" #Undefined
"010100" #Undefined
"010101" #Undefined
"010110" #Undefined
"010111" #Undefined
"011000" #Undefined
"011001" #Undefined
"011010" #Undefined
"011011" #Undefined
"011100" #Undefined
"011101" #Undefined
"011110" #Undefined
"011111" #Undefined
"100000" #Undefined
"100001" #Undefined
"100010" #Undefined
"100011" #Undefined
"100100" #Undefined
"100101" #Undefined
"100110" #Undefined
"100111" #Undefined
"101000" #Undefined
"101001" #Undefined
"101010" #Undefined
"101011" #Undefined
"101100" #Undefined
"101101" #Undefined
"101110" #Undefined
"101111" #Undefined
"110000" #Undefined
"110001" #Undefined
"110010" #Undefined
"110011" #Undefined
"110100" #Undefined
"110101" #Undefined
"110110" #Undefined
"110111" #Undefined
"111000" #Undefined
"111001" #Undefined
"111010" #Undefined
"111011" #Undefined
"111100" #Undefined
"111101" #Undefined
"111110" #Undefined
"111111" #SourcePointer
	)! !

!CompiledMethodTrailer class methodsFor: 'generated' stamp: 'ul 12/31/2009 19:01'!
trailerKindDecoders

	^#(#decodeNoTrailer #decodeClearedTrailer #decodeTempsNamesQCompress #decodeTempsNamesZip #decodeSourceBySelector #decodeSourceByStringIdentifier #decodeEmbeddedSourceQCompress #decodeEmbeddedSourceZip #decodeVarLengthSourcePointer #decodeExtendedKind #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeSourcePointer)! !


!CompiledMethodWithNode methodsFor: 'accessing' stamp: 'NS 1/28/2004 09:03'!
method
	^ method! !

!CompiledMethodWithNode methodsFor: 'accessing' stamp: 'NS 1/28/2004 09:04'!
node
	^ node! !

!CompiledMethodWithNode methodsFor: 'accessing' stamp: 'NS 1/28/2004 09:04'!
selector
	^ self node selector! !

!CompiledMethodWithNode methodsFor: 'private' stamp: 'NS 1/28/2004 09:03'!
method: aCompiledMethod
	method := aCompiledMethod! !

!CompiledMethodWithNode methodsFor: 'private' stamp: 'NS 1/28/2004 09:04'!
node: aMethodNode
	node := aMethodNode! !


!CompiledMethodWithNode class methodsFor: 'instance creation' stamp: 'NS 1/28/2004 09:05'!
generateMethodFromNode: aMethodNode trailer: bytes
	^ self method: (aMethodNode generate: bytes) node: aMethodNode.! !

!CompiledMethodWithNode class methodsFor: 'instance creation' stamp: 'NS 1/28/2004 09:05'!
method: aCompiledMethod node: aMethodNode
	^ self new method: aCompiledMethod; node: aMethodNode.! !


!Compiler methodsFor: 'public access' stamp: 'nice 9/16/2013 22:57'!
compile: aCue ifFail: failBlock 
	"Answer a MethodNode. If the MethodNode can not be created, notify 
	the requestor in the contxt. If the requestor is nil, evaluate failBlock 
	instead. The MethodNode is the root  of a parse tree. It can be told 
	to generate a CompiledMethod to be installed in the method dictionary 
	of the class specified by the context."
	
	^self
		compileCue: aCue
		noPattern: false
		ifFail: failBlock ! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/20/2013 01:56'!
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock 
	"Answer a MethodNode for the argument, textOrStream. If the 
	MethodNode can not be created, notify the argument, aRequestor; if 
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root 
	of a parse tree. It can be told to generate a CompiledMethod to be 
	installed in the method dictionary of the argument, aClass."

	^self
		compileCue: (CompilationCue
			source: textOrStream
			class: aClass
			requestor: aRequestor)
		noPattern: false
		ifFail: failBlock
! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/16/2013 23:00'!
compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	^self
		compileCue: (CompilationCue
			source: textOrStream
			context: aContext
			class: aClass
			requestor: aRequestor)
		noPattern: true
		ifFail: failBlock
! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/19/2013 23:06'!
compileNoPattern: textOrStream in: aClass notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	^self
		compileCue: (CompilationCue
			source: textOrStream
			class: aClass
			requestor: aRequestor)
		noPattern: true
		ifFail: failBlock
! !

!Compiler methodsFor: 'public access' stamp: 'nice 12/16/2013 15:19'!
compiledMethodFor: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock
	"Compiles the sourceStream into a parse tree, then generates code
	 into a method, and answers it.  If receiver is not nil, then the text can
	 refer to instance variables of that receiver (the Inspector uses this).
	 If aContext is not nil, the text can refer to temporaries in that context
	 (the Debugger uses this). If aRequestor is not nil, then it will receive a 
	 notify:at: message before the attempt to evaluate is aborted."

	| methodNode method theClass |
	theClass := (aContext == nil ifTrue: [receiver] ifFalse: [aContext receiver]) class.
	methodNode := self
		compileNoPattern: textOrStream
		in: theClass
		context: aContext
		notifying: aRequestor
		ifFail: [^failBlock value].
	method := self interactive
		ifTrue: [ methodNode generateWithTempNames ] 
		ifFalse: [ methodNode generate ].
	^method! !

!Compiler methodsFor: 'public access' stamp: 'nice 12/16/2013 14:53'!
evaluate: textOrString 
	"See evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString for: nil! !

!Compiler methodsFor: 'public access' stamp: 'nice 12/16/2013 14:51'!
evaluate: textOrString for: anObject
	"See evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil! !

!Compiler methodsFor: 'public access' stamp: 'nice 12/16/2013 14:52'!
evaluate: textOrString for: anObject notifying: aController
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController."

	^ self
		evaluate: textOrString
		in: nil
		to: anObject
		notifying: aController
		ifFail: [^nil]! !

!Compiler methodsFor: 'public access' stamp: 'nice 2/23/2011 22:14'!
evaluate: aString in: aContext to: aReceiver
	"evaluate aString in the given context, and return the result.  2/2/96 sw"
	^self
		evaluate: aString
		in: aContext
		to: aReceiver
		notifying: nil
		ifFail: [^ #failedDoit]! !

!Compiler methodsFor: 'public access' stamp: 'topa 1/24/2015 15:19'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."
	
	| theClass |
	theClass := aContext ifNil: [receiver class] ifNotNil: [:ctx | ctx methodClass].
	^self
		evaluateCue: (CompilationCue
			source: textOrStream
			context: aContext
			receiver: receiver
			class: theClass
			environment: theClass environment
			requestor: aRequestor)
		ifFail: failBlock! !

!Compiler methodsFor: 'public access' stamp: 'nice 12/16/2013 14:53'!
evaluate: textOrString notifying: aController
	"See evaluate:for:notifying:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/19/2013 23:04'!
format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"

	| aNode |
	self from: textOrStream
		class: aClass
		notifying: aRequestor.
	aNode := self formatNoPattern: false ifFail: [^ nil].

	"aSymbol == #colorPrint ifTrue:
		[^aNode asColorizedSmalltalk80Text]." "deprecating #colorPrint in favor of Shout --Ron Spengler"

	^aNode decompileString! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/19/2013 23:05'!
format: textOrStream in: aClass notifying: aRequestor decorated: aBoolean
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"
	| aNode |
	self from: textOrStream
		class: aClass
		notifying: aRequestor.
	aNode := self formatNoPattern: false ifFail: [^ nil].
	^ aBoolean
		ifTrue: [aNode decompileText]
		ifFalse: [aNode decompileString]! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/19/2013 23:05'!
parse: textOrStream in: aClass notifying: req
	"Compile the argument, textOrStream, with respect to the class, aClass, and
	 answer the MethodNode that is the root of the resulting parse tree.  Notify the
	 argument, req, if an error occurs. The failBlock is defaulted to an empty block."

	self from: textOrStream class: aClass notifying: req.
	^self parser
		parseCue: cue
		noPattern: false
		ifFail: []! !

!Compiler methodsFor: 'public access' stamp: 'cwp 12/27/2012 10:09'!
parser

	parser ifNil: [parser := (cue getClass ifNil: [self class]) newParser].
	^parser! !

!Compiler methodsFor: 'public access' stamp: 'eem 5/15/2008 15:07'!
parser: aParser

	parser := aParser! !

!Compiler methodsFor: 'public access' stamp: 'eem 5/15/2008 15:06'!
parserClass: aParserClass

	parser := aParserClass new! !

!Compiler methodsFor: 'public access' stamp: 'nice 9/17/2013 22:14'!
translate: aStream noPattern: noPattern ifFail: failBlock parser: parser
	| tree |
	tree := parser
			parseCue: cue 
			noPattern: noPattern
			ifFail: [^ failBlock value].
	^ tree! !

!Compiler methodsFor: 'private' stamp: 'nice 9/17/2013 22:55'!
compileCue: aCue noPattern: aBoolean ifFail: failBlock 
	"Answer a MethodNode corresponding to cue source.
	If the MethodNode can not be created, notify the cue requestor.
	If the cue requestor is nil, evaluate failBlock instead.
	The MethodNode is the root  of a parse tree.
	It can be told to generate a CompiledMethod
	- either to be evaluated in cue context if aBoolean is true, with cue receiver as method receiver,
	- or to be installed in the method dictionary of the target class specified by the cue if aBoolean is false."
	
	self setCue: aCue.
	^self translateNoPattern: aBoolean	ifFail: failBlock! !

!Compiler methodsFor: 'private' stamp: 'nice 12/16/2013 14:54'!
evaluateCue: aCue ifFail: failBlock
	"Compiles the cue source into a parse tree, then generates code into
	a method. Finally, the compiled method is invoked from here via 	withArgs:executeMethod:, hence the system no longer creates Doit method
	litter on errors."

	| methodNode method value |
	methodNode := self compileCue: aCue noPattern: true ifFail: [^failBlock value].

	method := self interactive
				ifTrue: [methodNode generateWithTempNames]
				ifFalse: [methodNode generate].

	value := cue receiver
				withArgs: (cue context ifNil: [#()] ifNotNil: [{cue context}])
				executeMethod: method.
	^ value
! !

!Compiler methodsFor: 'private' stamp: 'nice 12/16/2013 15:21'!
evaluateCue: aCue ifFail: failBlock logged: logFlag
	"Compiles the cue source into a parse tree, then generates code into
	a method. Finally, the compiled method is invoked from here via 	withArgs:executeMethod:, hence the system no longer creates Doit method
	litter on errors."

	| value |
	value := self evaluateCue: aCue ifFail: [^failBlock value].
	logFlag ifTrue:
		[SystemChangeNotifier uniqueInstance evaluated: cue stringToLog context: cue context].
	^ value
! !

!Compiler methodsFor: 'private' stamp: 'nice 9/17/2013 22:58'!
formatNoPattern: noPattern ifFail: failBlock
	^(self parser
		parseCue: cue 
		noPattern: noPattern
		ifFail: [^failBlock value]) preen! !

!Compiler methodsFor: 'private' stamp: 'nice 9/19/2013 23:04'!
from: textOrStream class: aClass notifying: req
	self setCue:
		(CompilationCue
			source: textOrStream
			class: aClass
			requestor: req)
! !

!Compiler methodsFor: 'private' stamp: 'cwp 12/27/2012 10:08'!
interactive
	"Answer true if compilation is interactive"

	^ cue requestor notNil! !

!Compiler methodsFor: 'private' stamp: 'nice 9/17/2013 22:56'!
setCue: aCue
	cue := aCue! !

!Compiler methodsFor: 'private' stamp: 'nice 9/17/2013 22:55'!
translate: ignored noPattern: noPattern ifFail: failBlock
	^self translateNoPattern: noPattern ifFail: failBlock! !

!Compiler methodsFor: 'private' stamp: 'nice 9/17/2013 22:55'!
translateNoPattern: noPattern ifFail: failBlock
	^self parser
		parseCue: cue 
		noPattern: noPattern
		ifFail: [^failBlock value]! !

!Compiler methodsFor: 'public access logging' stamp: 'nice 12/16/2013 15:20'!
compiledMethodFor: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	"Compiles the sourceStream into a parse tree, then generates code
	 into a method, and answers it.  If receiver is not nil, then the text can
	 refer to instance variables of that receiver (the Inspector uses this).
	 If aContext is not nil, the text can refer to temporaries in that context
	 (the Debugger uses this). If aRequestor is not nil, then it will receive a 
	 notify:at: message before the attempt to evaluate is aborted."

	| method |
	method := self
		compiledMethodFor: textOrStream
		in: aContext
		to: receiver
		notifying: aRequestor
		ifFail: [^failBlock value].
	logFlag ifTrue:
		[SystemChangeNotifier uniqueInstance evaluated: cue stringToLog context: aContext].
	^method! !

!Compiler methodsFor: 'public access logging' stamp: 'nice 2/23/2011 22:27'!
evaluate: textOrString for: anObject logged: logFlag 
	"See evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag! !

!Compiler methodsFor: 'public access logging' stamp: 'nice 2/23/2011 22:14'!
evaluate: textOrString for: anObject notifying: aController logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self
		evaluate: textOrString
		in: nil
		to: anObject
		notifying: aController
		ifFail: [^nil]
		logged: logFlag.! !

!Compiler methodsFor: 'public access logging' stamp: 'cmm 3/23/2015 19:41'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."
	| theClass |
	theClass := (aContext == nil ifTrue: [receiver class] ifFalse: [aContext methodClass]).
	^self
		evaluateCue: (CompilationCue
			source: textOrStream
			context: aContext
			receiver: receiver
			class: theClass
			environment: theClass environment
			requestor: aRequestor)
		ifFail: failBlock
		logged: logFlag! !

!Compiler methodsFor: 'public access logging' stamp: 'nice 2/23/2011 22:27'!
evaluate: textOrString logged: logFlag 
	"See evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag! !

!Compiler methodsFor: 'public access logging' stamp: 'nice 2/23/2011 22:27'!
evaluate: textOrString notifying: aController logged: logFlag 
	"See evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController logged: logFlag! !


!Compiler class methodsFor: 'accessing' stamp: 'nk 8/30/2004 07:56'!
couldEvaluate: anObject
	"Answer true if anObject can be passed to my various #evaluate: methods."
	^anObject isString or: [ anObject isText or: [ anObject isStream ]]! !

!Compiler class methodsFor: 'accessing' stamp: 'md 3/1/2006 21:12'!
decompilerClass
	^Decompiler! !

!Compiler class methodsFor: 'accessing' stamp: 'nice 2/23/2011 21:52'!
new
	^ super new parser: self newParser! !

!Compiler class methodsFor: 'accessing' stamp: 'eem 5/13/2008 11:37'!
parserClass
	"Answer a parser class to use for parsing methods compiled by instances of the receiver."

	^Parser! !

!Compiler class methodsFor: 'evaluating' stamp: 'nice 12/16/2013 15:27'!
evaluate: textOrString 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString for: nil! !

!Compiler class methodsFor: 'evaluating' stamp: 'nice 12/16/2013 15:26'!
evaluate: aString environment: anEnvironment
	^  self new
		evaluateCue: (CompilationCue
			source: aString
			environment: anEnvironment)
		ifFail: [^ nil]! !

!Compiler class methodsFor: 'evaluating' stamp: 'nice 12/16/2013 15:27'!
evaluate: textOrString for: anObject  
	"See Compiler|evaluate:for:notifying:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil! !

!Compiler class methodsFor: 'evaluating' stamp: 'nice 12/16/2013 15:27'!
evaluate: textOrString for: anObject notifying: aController
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController."

	^ self new
		evaluate: textOrString
		in: nil
		to: anObject
		notifying: aController
		ifFail: [^nil]! !

!Compiler class methodsFor: 'evaluating' stamp: 'nice 12/16/2013 15:28'!
evaluate: textOrString notifying: aController
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController! !

!Compiler class methodsFor: 'evaluating' stamp: 'rss 8/23/2009 12:53'!
format: textOrStream in: aClass notifying: aRequestor
	^self new format: textOrStream in: aClass notifying: aRequestor! !

!Compiler class methodsFor: 'evaluating' stamp: 'nk 2/23/2005 16:53'!
format: textOrStream in: aClass notifying: aRequestor decorated: aBoolean
	^self new format: textOrStream in: aClass notifying: aRequestor decorated: aBoolean! !

!Compiler class methodsFor: 'utilities' stamp: 'ul 6/17/2011 12:21'!
recompileAll	"Compiler recompileAll"
	"Recompile all classes and traits in the system"

	Smalltalk allClassesAndTraits 
		do: [:classOrTrait | classOrTrait compileAll] 
		displayingProgress:[:classOrTrait| 'Recompiling ', classOrTrait]


! !

!Compiler class methodsFor: 'utilities' stamp: 'ul 6/17/2011 12:20'!
recompileAllFrom: firstName 
	"Recompile all classes, starting with given name."

	Smalltalk allClassesDo: 
		[:class | class name >= firstName
			ifTrue: 
				[Transcript show: class name; cr.
				class compileAll]]

	"Compiler recompileAllFrom: 'AAABodyShop'."
! !

!Compiler class methodsFor: 'class initialization' stamp: 'ar 9/5/2009 22:18'!
initialize
	"For the benefit of MC running a required script in the proper order."
	self recompileAll.! !

!Compiler class methodsFor: 'evaluating logged' stamp: 'nice 9/16/2013 23:16'!
evaluate: aString environment: anEnvironment logged: aBoolean
	^ self new
		evaluateCue: (CompilationCue
			source: aString
			environment: anEnvironment)
		ifFail: [^ nil]
		logged: aBoolean! !

!Compiler class methodsFor: 'evaluating logged'!
evaluate: textOrString for: anObject logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag! !

!Compiler class methodsFor: 'evaluating logged' stamp: 'NS 1/19/2004 09:50'!
evaluate: textOrString for: anObject notifying: aController logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
				evaluate: textOrString
				in: nil
				to: anObject
				notifying: aController
				ifFail: [^nil]
				logged: logFlag.! !

!Compiler class methodsFor: 'evaluating logged'!
evaluate: textOrString logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag! !

!Compiler class methodsFor: 'evaluating logged'!
evaluate: textOrString notifying: aController logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController logged: logFlag! !


!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'ar 9/27/2005 19:11'!
doitFlag
	^doitFlag! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'ar 9/27/2005 19:10'!
errorClass
	^inClass! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'ar 9/27/2005 19:10'!
errorCode
	^code! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'eem 9/23/2008 14:23'!
errorMessage
	^errorMessage! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'eem 9/23/2008 14:23'!
location
	^location! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'ar 9/27/2005 19:14'!
messageText
	^ super messageText
		ifNil: [messageText := code]! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'nice 9/18/2013 21:45'!
newSource
	^newSource! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'nice 9/18/2013 02:17'!
newSource: aTextOrString
	newSource := aTextOrString! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'nice 9/20/2013 01:53'!
reparse: aString notifying: aController ifFail: failBlock
	"Try to parse if aString has correct syntax, but do not evaluate/install any code.
	In case of incorrect syntax, execute failBlock and let a Compiler interact with the requestor.
	In case of correct syntax, set newSource."
	doitFlag
		ifTrue: [nil class newCompiler compileNoPattern: aString in: nil class notifying: aController ifFail: failBlock]
		ifFalse: [inClass newCompiler compile: aString in: inClass notifying: aController ifFail: failBlock].
	newSource := aString! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'nice 9/20/2013 01:59'!
setClass: aClass code: codeString doitFlag: aBoolean errorMessage: errorString location: anInteger
	inClass := aClass.
	code := codeString.
	doitFlag := aBoolean.
	errorMessage := errorString.
	location := anInteger! !

!SyntaxErrorNotification methodsFor: 'accessing' stamp: 'nice 9/18/2013 02:48'!
tryNewSourceIfAvailable
	newSource ifNotNil: [ReparseAfterSourceEditing withNewSource: newSource]! !

!SyntaxErrorNotification methodsFor: '*System-exceptionDescription' stamp: 'ar 9/27/2005 19:13'!
defaultAction
	^ToolSet debugSyntaxError: self! !


!SyntaxErrorNotification class methodsFor: 'exceptionInstantiator' stamp: 'nice 9/20/2013 01:59'!
inClass: aClass withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location
	^self new
		setClass: aClass
		code: codeString
		doitFlag: doitFlag
		errorMessage: errorString
		location: location! !


!ParserNotification methodsFor: 'handling' stamp: 'cwp 10/17/2007 21:36'!
openMenuIn: aBlock
	self subclassResponsibility! !

!ParserNotification methodsFor: 'private' stamp: 'cwp 10/17/2007 23:29/eem 9/5/2009 11:10 - => :='!
setName: aString
	name := aString! !

!ParserNotification methodsFor: '*System-default handlers' stamp: 'cwp 8/25/2009 20:04'!
defaultAction
	
	self openMenuIn: 
		[:labels :lines :caption | 
		UIManager default chooseFrom: labels lines: lines title: caption]! !


!ParserNotification class methodsFor: 'as yet unclassified' stamp: 'cwp 10/17/2007 23:31'!
name: aString
	^ (self new setName: aString) signal! !


!AmbiguousSelector methodsFor: 'handling' stamp: 'nice 2/23/2010 16:54'!
openMenuIn: aBlock 
	"Ask the user which selector to choose.
	Answer the choosen selector or nil if cancellation is requested."
	
	| labels actions lines caption choice |
	labels := {
		'selector is ' , (name copyFrom: 1 to: name size - 1) , ' argument is negative'.
		'selector is ' , name , ' argument is positive'.
		'cancel'}.
	actions := {
		name copyReplaceFrom: name size to: name size - 1 with: ' '.
		name copyReplaceFrom: name size + 1 to: name size with: ' '.
		nil.
	}.
	lines := {2}.
	caption := 'Ambiguous selector: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	self resume: (actions at: choice ifAbsent: [nil])! !

!AmbiguousSelector methodsFor: 'private' stamp: 'nice 2/23/2010 16:51'!
setName: aString range: anInterval 
	name := aString.
	interval := anInterval! !


!AmbiguousSelector class methodsFor: 'instance creation' stamp: 'nice 2/23/2010 16:52'!
signalName: aString inRange: anInterval 
	^ (self new setName: aString range: anInterval) signal! !


!UndeclaredVariable methodsFor: 'as yet unclassified' stamp: 'nice 4/20/2011 00:35'!
openMenuIn: aBlock 
	| alternatives labels actions lines caption choice |
	alternatives := parser possibleVariablesFor: name.
	labels := OrderedCollection new.
	actions := OrderedCollection new.
	lines := OrderedCollection new.
	name first isLowercase 
		ifTrue: 
			[labels add: 'declare method temp'.
			actions add: [parser declareTemp: name at: #method].
			labels add: 'declare block-local temp'.
			actions add: [parser declareTemp: name at: #block].
			parser canDeclareInstanceVariable 
				ifTrue: 
					[labels add: 'declare instance'.
					actions add: [parser declareInstVar: name]]]
		ifFalse: 
			[labels add: 'define new class'.
			actions add: [parser defineClass: name].
			labels add: 'declare global'.
			actions add: [parser declareGlobal: name].
			parser canDeclareClassVariable 
				ifTrue: 
					[labels add: 'declare class variable'.
					actions add: [parser declareClassVar: name]]].
	lines add: labels size.
	alternatives do: 
		[:each | 
		labels add: each.
		actions add: [parser substituteVariable: each atInterval: interval]].
	lines add: labels size.
	labels add: 'cancel'.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	self resume: (actions at: choice ifAbsent: [nil])! !

!UndeclaredVariable methodsFor: 'as yet unclassified' stamp: 'cwp 10/15/2007 23:04'!
setParser: aParser name: aString range: anInterval 
	parser := aParser.
	name := aString.
	interval := anInterval! !


!UndeclaredVariable class methodsFor: 'instance creation' stamp: 'cwp 10/15/2007 22:37'!
signalFor: aParser name: aString inRange: anInterval 
	^ (self new setParser: aParser name: aString range: anInterval) signal! !


!UndefinedVariable methodsFor: 'as yet unclassified' stamp: 'cwp 10/17/2007 22:39/eem 9/5/2009 11:10 - => :='!
openMenuIn: aBlock
	| labels caption index |
	labels := #('yes' 'no').
	caption := name, ' appears to be 
undefined at this point.
Proceed anyway?'.

	index := aBlock value: labels value: #() value: caption.
	^ self resume: index = 1! !


!UnknownSelector methodsFor: 'as yet unclassified' stamp: 'cwp 10/17/2007 23:34/eem 9/5/2009 11:10 - => :='!
openMenuIn: aBlock
	| alternatives labels lines caption choice |
	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents:
				[:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withCRs.
	
	choice := aBlock value: labels value: lines value: caption.
	choice = 0 ifTrue: [self resume: nil].
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1)! !


!UnusedVariable methodsFor: 'as yet unclassified' stamp: 'eem 9/5/2009 11:10'!
openMenuIn: aBlock
	| index |
	index := aBlock value: #('yes' 'no')
					value: #()
					value: name, ' appears to be\unused in this method.\OK to remove it?' withCRs.
	self resume: index = 1! !


!ReparseAfterSourceEditing methodsFor: 'accessing' stamp: 'nice 9/18/2013 02:04'!
newSource
	^newSource! !

!ReparseAfterSourceEditing methodsFor: 'initialize-release' stamp: 'nice 9/18/2013 02:03'!
withNewSource: aStringOrStream
	newSource := aStringOrStream! !


!ReparseAfterSourceEditing class methodsFor: 'instance creation' stamp: 'nice 9/18/2013 02:19'!
withNewSource: aStringOrStream
	^(self new withNewSource: aStringOrStream) signal! !


!UndeclaredVariableReference methodsFor: 'exceptionDescription' stamp: 'ar 12/8/2002 13:13'!
defaultAction
	^parser correctVariable: varName interval: (varStart to: varEnd)! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:13'!
parser
	^parser! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:13'!
parser: aParser
	parser := aParser! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:14'!
varEnd
	^varEnd! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:14'!
varEnd: aNumber
	varEnd := aNumber! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:13'!
varName
	^varName! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:13'!
varName: aString
	varName := aString! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:14'!
varStart
	^varStart! !

!UndeclaredVariableReference methodsFor: 'accessing' stamp: 'ar 12/8/2002 13:14'!
varStart: aNumber
	varStart := aNumber! !


!UndeclaredVariableWarning methodsFor: 'exceptionDescription' stamp: 'bgf 3/10/2009 19:03'!
defaultAction
	"The user should be notified of the occurrence of an exceptional occurrence and
	 given an option of continuing or aborting the computation. The description of the
	 occurrence should include any text specified as the argument of the #signal: message."
	
	selector ifNotNil: [Transcript cr; nextPutAll: class name, '>>', selector, ' ']
			ifNil: [Transcript cr ].
	Transcript show: '(' , name , ' is Undeclared) '.
	^true! !

!UndeclaredVariableWarning methodsFor: 'initialize-release' stamp: 'ar 2/13/2010 15:21'!
defaultResumeValue
	"Answer the value that by default should be returned if the exception is resumed"
	^true! !

!UndeclaredVariableWarning methodsFor: 'initialize-release' stamp: 'eem 7/27/2008 17:37'!
name: aString selector: aSymbolOrNil class: aBehavior
	name := aString.
	selector := aSymbolOrNil.
	class := aBehavior! !


!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
blockReturnTop
	"Return Top Of Stack bytecode."

! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
doDup
	"Duplicate Top Of Stack bytecode."

! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
doPop
	"Remove Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
jump: offset
	"Unconditional Jump bytecode."

! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
jump: offset if: condition 
	"Conditional Jump bytecode."

! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
methodReturnConstant: value 
	"Return Constant bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!
methodReturnReceiver
	"Return Self bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
methodReturnTop
	"Return Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
popIntoLiteralVariable: anAssociation 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
popIntoReceiverVariable: offset 
	"Remove Top Of Stack And Store Into Instance Variable bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:51'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
popIntoTemporaryVariable: offset 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
pushActiveContext
	"Push Active Context On Top Of Its Own Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:49'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Push Closure bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 6/16/2008 14:26'!
pushConsArrayWithElements: numElements
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
pushConstant: value
	"Push Constant, value, on Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
pushLiteralVariable: anAssociation
	"Push Contents Of anAssociation On Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:50'!
pushNewArrayOfSize: numElements 
	"Push New Array of size numElements bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
pushReceiver
	"Push Active Context's Receiver on Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!
pushReceiverVariable: offset
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:54'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Push Contents at Offset in Temp Vector bytecode."! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!
pushTemporaryVariable: offset
	"Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!
send: selector super: supered numArgs: numberArguments
	"Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!
storeIntoLiteralVariable: anAssociation 
	"Store Top Of Stack Into Literal Variable Of Method bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!
storeIntoReceiverVariable: offset 
	"Store Top Of Stack Into Instance Variable Of Method bytecode."
! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:52'!
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Store Top Of Stack And Store Into Offset of Temp Vector bytecode."! !

!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!
storeIntoTemporaryVariable: offset 
	"Store Top Of Stack Into Temporary Variable Of Method bytecode."
! !


!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 11:36'!
blockReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	scanner pc < blockEnd ifTrue:
		[self doJoin]! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:13'!
doDup
	"Duplicate Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:17'!
doPop
	"Remove Top Of Stack bytecode."
	stackPointer := stackPointer - 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 13:40'!
jump: offset
	"Unconditional Jump bytecode."
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer.
		 self doJoin]! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 13:40'!
jump: offset if: condition 
	"Conditional Jump bytecode."
	stackPointer := stackPointer - 1.
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer]! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 11:36'!
methodReturnConstant: value 
	"Return Constant bytecode."
	self doJoin! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 11:36'!
methodReturnReceiver
	"Return Self bytecode."
	self doJoin! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 11:36'!
methodReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	self doJoin! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:19'!
popIntoLiteralVariable: anAssociation 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."
	stackPointer := stackPointer - 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:19'!
popIntoReceiverVariable: offset 
	"Remove Top Of Stack And Store Into Instance Variable bytecode."
	stackPointer := stackPointer - 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:19'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."
	stackPointer := stackPointer - 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:20'!
popIntoTemporaryVariable: offset 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."
	stackPointer := stackPointer - 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:20'!
pushActiveContext
	"Push Active Context On Top Of Its Own Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:16'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Push Closure bytecode.  Either compute the end of the block if this is
	 the block we're analysing, or skip it, adjusting the stack as appropriate."
	blockEnd
		ifNil: [blockEnd := scanner pc + blockSize]
		ifNotNil:
			[stackPointer := stackPointer - numCopied + 1.
			 scanner pc: scanner pc + blockSize]! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:21'!
pushConsArrayWithElements: numElements
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."
	stackPointer := stackPointer - numElements + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:21'!
pushConstant: value
	"Push Constant, value, on Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:22'!
pushLiteralVariable: anAssociation
	"Push Contents Of anAssociation On Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:22'!
pushNewArrayOfSize: numElements 
	"Push New Array of size numElements bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:22'!
pushReceiver
	"Push Active Context's Receiver on Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:23'!
pushReceiverVariable: offset
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:23'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Push Contents at Offset in Temp Vector bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:23'!
pushTemporaryVariable: offset
	"Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."
	stackPointer := stackPointer + 1! !

!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 9/23/2008 16:24'!
send: selector super: supered numArgs: numberArguments
	"Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	stackPointer := stackPointer - numberArguments! !

!BlockLocalTempCounter methodsFor: 'private' stamp: 'eem 1/22/2011 12:48'!
doJoin
	scanner pc < blockEnd ifTrue:
		[stackPointer := joinOffsets at: scanner pc ifAbsent: [scanner followingPc]]

	"the ifAbsent: handles a caseOf:otherwise: where all cases return, which results
	 in the branch around the otherwise being unreached.  e.g. in the following
		jumpTo: L2
	 is unreached.

		| t |
		t caseOf: { [nil] -> [^thisContext method abstractSymbolic] }
		  otherwise: ['Oh no Mr Bill!!']

		pushTemp: 0
		pushConstant: nil
		send: #= (1 arg)
		jumpFalseTo: L1
		pushThisContext: 
		send: #method (0 args)
		send: #abstractSymbolic (0 args)
		returnTop
		jumpTo: L2
	L1:
		pushConstant: 'Oh no Mr Bill!!'
	L2:
		returnTop"! !

!BlockLocalTempCounter methodsFor: 'initialize-release' stamp: 'nice 8/27/2010 19:42'!
tempCountForBlockAt: pc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.

	 There are short-cuts.  The ones we take here are
		- if there is no sequence of push nils there can be no local temps
		- we follow forward jumps to shorten the amount of scanning"
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: pc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[scanner interpretNextInstructionFor: self]].
	^stackPointer! !

!BlockLocalTempCounter methodsFor: 'initialize-release' stamp: 'nice 8/27/2010 19:42'!
testTempCountForBlockAt: startPc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.There are short-cuts.  The only
	 one we take here is
		- if there is no sequence of push nils there can be no local temps"

	| symbolicLines line prior thePc |
	symbolicLines := Dictionary new.
	method symbolicLinesDo:
		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: startPc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[line := symbolicLines at: scanner pc.
			 prior := stackPointer.
			 thePc := scanner pc.
			 scanner interpretNextInstructionFor: self.
			 Transcript cr; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line; flush]].
	^stackPointer! !


!BlockLocalTempCounter class methodsFor: 'instance creation' stamp: 'eem 9/23/2008 16:07'!
tempCountForBlockAt: pc in: method
	^self new tempCountForBlockAt: pc in: method! !

!BlockLocalTempCounter class methodsFor: 'instance creation' stamp: 'eem 11/6/2012 15:23'!
tempCountForBlockStartingAt: startpc in: method
	^self new
		tempCountForBlockAt: (method encoderClass pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method)
		in: method! !


!BlockStartLocator methodsFor: 'initialize-release' stamp: 'eem 5/31/2008 13:43'!
initialize
	nextJumpIsAroundBlock := false! !

!BlockStartLocator methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:48'!
jump: offset
	"If this jump is around a block answer the size of that block."

	nextJumpIsAroundBlock ifTrue:
		[nextJumpIsAroundBlock := false.
		 ^offset]! !

!BlockStartLocator methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 13:54'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Answer the size of the block"
	^blockSize! !

!BlockStartLocator methodsFor: 'instruction decoding' stamp: 'eem 5/31/2008 14:16'!
send: selector super: supered numArgs: numberArguments
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector
	"Don't use
		nextJumpIsAroundBlock := #(blockCopy: closureCopy:copiedValues:) includes: selector
	 since BlueBook BlockContexts do not have their own temps."! !


!ClosureExtractor methodsFor: 'accessing' stamp: 'mha 9/21/2010 11:18'!
action
	^ action! !

!ClosureExtractor methodsFor: 'accessing' stamp: 'mha 9/21/2010 11:18'!
action: aBlock
	action := aBlock! !

!ClosureExtractor methodsFor: 'accessing' stamp: 'mha 9/21/2010 11:35'!
scanner
	^ scanner! !

!ClosureExtractor methodsFor: 'accessing' stamp: 'eem 3/20/2013 13:51'!
scanner: anInstructionStream
	scanner := anInstructionStream.
	currentContext := MethodContext
							sender: nil
							receiver: nil
							method: scanner method
							arguments: (Array new: scanner method numArgs)! !

!ClosureExtractor methodsFor: 'instruction decoding' stamp: 'eem 10/24/2012 14:08'!
blockReturnTop
	currentContext := currentContext sender! !

!ClosureExtractor methodsFor: 'instruction decoding' stamp: 'eem 10/24/2012 14:11'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Create a BlockClosure corresponding to the closure bytecode
	 and execute the action block with it. The created BlockClosure is only a pseudo value,
	 it is not populated with meaningful context and argument information."
	| block |
	block := BlockClosure
				outerContext: currentContext
				startpc: scanner pc
				numArgs: numArgs
				copiedValues: (Array new: numCopied)..
	currentContext := block asContextWithSender: currentContext.
	action value: block! !


!ClosureExtractor class methodsFor: 'instance creation' stamp: 'mha 9/21/2010 11:34'!
withAction: aBlock andScanner: anInstructionStream
	"The passed block must accept one value, which will be a BlockClosure."
	^ self new action: aBlock; scanner: anInstructionStream! !


!InstVarRefLocator methodsFor: 'initialize-release' stamp: 'md 4/8/2003 11:35'!
interpretNextInstructionUsing: aScanner 
	
	bingo := false.
	aScanner interpretNextInstructionFor: self.
	^bingo! !

!InstVarRefLocator methodsFor: 'instruction decoding' stamp: 'RAA 1/5/2001 08:46'!
popIntoReceiverVariable: offset 

	bingo := true! !

!InstVarRefLocator methodsFor: 'instruction decoding' stamp: 'RAA 1/5/2001 08:46'!
pushReceiverVariable: offset

	bingo := true! !

!InstVarRefLocator methodsFor: 'instruction decoding' stamp: 'RAA 1/5/2001 08:46'!
storeIntoReceiverVariable: offset 

	bingo := true! !


!InstructionPrinter methodsFor: 'accessing' stamp: 'ajh 6/27/2003 22:25'!
indent

	^ indent ifNil: [0]! !

!InstructionPrinter methodsFor: 'accessing' stamp: 'md 4/8/2003 11:20'!
method
	^method.! !

!InstructionPrinter methodsFor: 'accessing' stamp: 'eem 5/29/2008 14:00'!
method: aMethod
	method :=  aMethod.
	printPC := true.
	indentSpanOfFollowingJump := false! !

!InstructionPrinter methodsFor: 'accessing' stamp: 'eem 5/29/2008 13:50'!
printPC
	^printPC! !

!InstructionPrinter methodsFor: 'accessing' stamp: 'eem 5/29/2008 13:50'!
printPC: aBoolean
	printPC := aBoolean! !

!InstructionPrinter methodsFor: 'initialize-release' stamp: 'ajh 2/9/2003 14:16'!
indent: numTabs

	indent := numTabs! !

!InstructionPrinter methodsFor: 'initialize-release' stamp: 'eem 5/29/2008 13:26'!
printInstructionsOn: aStream 
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream."
	
	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self]! !

!InstructionPrinter methodsFor: 'initialize-release' stamp: 'eem 8/4/2008 16:26'!
printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream. Evaluate aBlock with the receiver, the scanner and
	 the stream after each instruction."

	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self.
		 aBlock value: self value: scanner value: stream]! !

!InstructionPrinter methodsFor: 'instruction decoding'!
blockReturnTop
	"Print the Return Top Of Stack bytecode."

	self print: 'blockReturn'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 4/10/2015 11:06'!
callPrimitive: index
	"Print the callPrimitive bytecode."

	self print: 'callPrimitive: ' , index printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
doDup
	"Print the Duplicate Top Of Stack bytecode."

	self print: 'dup'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 12:14'!
doPop
	"Print the Remove Top Of Stack bytecode."

	self print: 'pop'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/29/2008 14:02'!
jump: offset
	"Print the Unconditional Jump bytecode."

	self print: 'jumpTo: ' , (scanner pc + offset) printString.
	indentSpanOfFollowingJump ifTrue:
		[indentSpanOfFollowingJump := false.
		 innerIndents atAll: (scanner pc to: scanner pc + offset - 1) put: (innerIndents at: scanner pc - 1) + 1]! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 11:13'!
jump: offset if: condition 
	"Print the Conditional Jump bytecode."

	self print: 
		(condition
			ifTrue: ['jumpTrue: ']
			ifFalse: ['jumpFalse: '])
			, (scanner pc + offset) printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
methodReturnConstant: value 
	"Print the Return Constant bytecode."

	self print: 'return: ' , value printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
methodReturnReceiver
	"Print the Return Self bytecode."

	self print: 'returnSelf'! !

!InstructionPrinter methodsFor: 'instruction decoding'!
methodReturnTop
	"Print the Return Top Of Stack bytecode."

	self print: 'returnTop'! !

!InstructionPrinter methodsFor: 'instruction decoding'!
popIntoLiteralVariable: anAssociation 
	"Print the Remove Top Of Stack And Store Into Literal Variable bytecode."

	self print: 'popIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding'!
popIntoReceiverVariable: offset 
	"Print the Remove Top Of Stack And Store Into Instance Variable 
	bytecode."

	self print: 'popIntoRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/25/2008 14:06'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self print: 'popIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
popIntoTemporaryVariable: offset 
	"Print the Remove Top Of Stack And Store Into Temporary Variable 
	bytecode."

	self print: 'popIntoTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
pushActiveContext
	"Print the Push Active Context On Top Of Its Own Stack bytecode."

	self print: 'pushThisContext: '! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 6/16/2008 14:04'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	self print: 'closureNumCopied: ', numCopied printString
			, ' numArgs: ', numArgs printString
			, ' bytes ', scanner pc printString
			, ' to ', (scanner pc + blockSize - 1) printString.
	innerIndents
		atAll: (scanner pc to: scanner pc + blockSize - 1)
		put: (innerIndents at: scanner pc - 1) + 1! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/30/2008 17:42'!
pushConsArrayWithElements: numElements 
	self print: 'pop ', numElements printString, ' into (Array new: ', numElements printString, ')'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 8/5/2014 11:37'!
pushConstant: obj
	"Print the Push Constant, obj, on Top Of Stack bytecode."

	self print: (String streamContents:
				[:s |
				s nextPutAll: 'pushConstant: '.
				obj isVariableBinding
					ifTrue:
						[obj key
							ifNotNil: [s nextPutAll: '##'; nextPutAll: obj key]
							ifNil: [s nextPutAll: '###'; nextPutAll: obj value soleInstance name]]
					ifFalse:
						[obj isClosure
							ifTrue: [s nextPutAll: obj sourceString]
							ifFalse: [obj printOn: s]]]).

	obj isCompiledMethod ifTrue:
		[obj longPrintOn: stream indent: self indent + 2.
		^self]! !

!InstructionPrinter methodsFor: 'instruction decoding'!
pushLiteralVariable: anAssociation
	"Print the Push Contents Of anAssociation On Top Of Stack bytecode."

	self print: 'pushLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/25/2008 15:02'!
pushNewArrayOfSize: numElements 
	self print: 'push: (Array new: ', numElements printString, ')'! !

!InstructionPrinter methodsFor: 'instruction decoding'!
pushReceiver
	"Print the Push Active Context's Receiver on Top Of Stack bytecode."

	self print: 'self'! !

!InstructionPrinter methodsFor: 'instruction decoding'!
pushReceiverVariable: offset
	"Print the Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

	self print: 'pushRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/25/2008 00:00'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self print: 'pushTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
pushTemporaryVariable: offset
	"Print the Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."

	self print: 'pushTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 3/15/2013 15:15'!
send: selector super: supered numArgs: numArgs
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered
				ifTrue: ['superSend: ']
				ifFalse: ['send: '])
			, (self stringForSelector: selector numArgs: numArgs).
	indentSpanOfFollowingJump := #(blockCopy: #closureCopy:copiedValues:) includes: selector! !

!InstructionPrinter methodsFor: 'instruction decoding'!
storeIntoLiteralVariable: anAssociation 
	"Print the Store Top Of Stack Into Literal Variable Of Method bytecode."

	self print: 'storeIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding'!
storeIntoReceiverVariable: offset 
	"Print the Store Top Of Stack Into Instance Variable Of Method bytecode."

	self print: 'storeIntoRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/25/2008 14:06'!
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self print: 'storeIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString! !

!InstructionPrinter methodsFor: 'instruction decoding'!
storeIntoTemporaryVariable: offset 
	"Print the Store Top Of Stack Into Temporary Variable Of Method 
	bytecode."

	self print: 'storeIntoTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'printing' stamp: 'eem 5/29/2008 13:53'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	printPC ifTrue: [stream print: oldPC; space].
	stream tab: (innerIndents at: oldPC).
	stream nextPut: $<.
	oldPC to: scanner pc - 1 do: 
		[:i | 
		code := (method at: i) radix: 16.
		stream nextPut: 
			(code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
		stream nextPut: code last; space].
	stream skip: -1.
	stream nextPut: $>.
	stream space.
	stream nextPutAll: instruction.
	stream cr.
	oldPC := scanner pc.
	"(InstructionPrinter compiledMethodAt: #print:) symbolic."
! !

!InstructionPrinter methodsFor: 'printing' stamp: 'eem 3/15/2013 15:14'!
stringForSelector: selector numArgs: numArgs
	^(selector isSymbol and: [selector numArgs = numArgs])
		ifTrue: [selector]
		ifFalse: [selector printString
				, (numArgs = 1
					ifTrue: [' (1 arg)']
					ifFalse: [' (', numArgs printString, ' args)'])]! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'md 4/8/2003 11:19'!
on: aMethod
	^self new method: aMethod.
	! !

!InstructionPrinter class methodsFor: 'printing' stamp: 'nice 1/15/2010 22:57'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."
	| file |
	file := FileStream newFileNamed: class name , '.bytes'.
	class selectorsAndMethodsDo: 
		[:sel :m | 
		file cr; nextPutAll: sel; cr.
		(self on: m) printInstructionsOn: file].
	file close
	"InstructionPrinter printClass: Parser."
! !


!RelativeInstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/15/2008 11:29'!
jump: offset
	"Print the Unconditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 'jumpBy: ', offset printString,
				' to: ', (scanner pc + offset - method initialPC) printString]
		ifFalse:
			[self print: 'jumpTo: ', (labels at: scanner pc + offset + 1)]! !

!RelativeInstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 5/15/2008 11:29'!
jump: offset if: condition 
	"Print the Conditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 
				(condition ifTrue: ['jumpTrueBy: '] ifFalse: ['jumpFalseBy: ']), offset printString,
				' to: ', (labelling
							ifTrue: [(scanner pc + offset - method initialPC) printString]
							ifFalse: [labels at: scanner pc + offset])]
		ifFalse:
			[self print: 
				(condition ifTrue: ['jumpTrueTo: '] ifFalse: ['jumpFalseTo: ']), (labels at: scanner pc + offset + 1)]! !

!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'eem 5/29/2008 13:49'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue:
		[stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: 
			[:i | | code |
			code := (method at: i) radix: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skip: -1; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream cr.
	labelling ifFalse:
		[(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; cr]].
	oldPC := scanner pc! !

!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'eem 5/15/2008 10:56'!
printCode
	^printCode ~~ false! !

!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'eem 5/21/2008 12:24'!
printInstructionsOn: aStream
	"Append to the stream, aStream, a description of each bytecode in the instruction stream."
	
	| label |
	labelling := true.
	labels := Array new: method size + 1 withAll: false.
	super printInstructionsOn: (String new: 1024) writeStream.
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream! !

!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'eem 5/15/2008 13:17'!
printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the instruction stream.
	  Evaluate aBlock with the receiver, the scanner and the stream after each instruction."
	
	| label |
	labelling := true.
	labels := Array new: method size withAll: false.
	super printInstructionsOn: (String new: 1024) writeStream do: [:ig :no :re|].
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream do: aBlock! !

!RelativeInstructionPrinter methodsFor: 'initialize-release' stamp: 'eem 5/15/2008 10:56'!
printCode: aBoolean
	printCode := aBoolean! !


!Decompiler methodsFor: 'control' stamp: 'tao 8/20/97 22:51'!
blockForCaseTo: end
	"Decompile a range of code as in statementsForCaseTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsForCaseTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block! !

!Decompiler methodsFor: 'control'!
blockTo: end
	"Decompile a range of code as in statementsTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block! !

!Decompiler methodsFor: 'control' stamp: 'eem 5/29/2008 13:16'!
checkForBlock: receiver selector: selector arguments: arguments
	selector == #blockCopy: ifTrue:
		[^self checkForBlockCopy: receiver].
	self assert: selector == #closureCopy:copiedValues:.
	^self checkForClosureCopy: receiver arguments: arguments! !

!Decompiler methodsFor: 'control' stamp: 'eem 7/29/2008 17:42'!
checkForBlockCopy: receiver
	"We just saw a blockCopy: message. Check for a following block."

	| savePc jump args argPos block |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) ifNil:
		[pc := savePc.  ^false].
	self sawBlueBookBlock.
	"Definitely a block"
	jump := jump + pc.
	argPos := statements size.
	[self willStorePop]
		whileTrue:
			[stack addLast: ArgumentFlag.  "Flag for doStore:"
			self interpretNextInstructionFor: self].
	args := Array new: statements size - argPos.
	1 to: args size do:  "Retrieve args"
		[:i | args at: i put: statements removeLast.
		(args at: i) scope: -1  "flag args as block temps"].
	block := self blockTo: jump.
	stack addLast: (constructor codeArguments: args block: block).
	^true! !

!Decompiler methodsFor: 'control' stamp: 'eem 5/29/2008 17:02'!
checkForClosureCopy: receiver arguments: arguments
	"We just saw a closureCopy:copiedValues: message. Check for and construct a following block."

	| savePc jump |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) notNil ifFalse:
		[pc := savePc.
		 ^nil].
	"Definitely a block"
	self doClosureCopyCopiedValues: arguments last "<BraceNode>" elements
		numArgs: arguments first key
		blockSize: jump.
	^true! !

!Decompiler methodsFor: 'control' stamp: 'eem 11/6/2012 15:23'!
doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block |
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := BlockLocalTempCounter
							tempCountForBlockStartingAt: pc
							in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 't', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 't', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[self interpretNextInstructionFor: self.
		 stack removeLast].
	tempVarCount := tempVarCount + numArgs + numLocalTemps.
	block := self blockTo: jump.
	stack addLast: ((constructor
						codeArguments: (tempVars copyFrom: 1 to: numArgs)
						temps: (tempVars copyFrom: blockTempsOffset + 1 to: blockTempsOffset + numLocalTemps)
						block: block)
							pc: startpc;
							yourself).
	tempVars := savedTemps.
	tempVarCount := savedTempVarCount.
	numLocalTemps := savedNumLocalTemps! !

!Decompiler methodsFor: 'control' stamp: 'ls 1/28/2004 13:29'!
statementsForCaseTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a CaseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for CaseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == CaseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos! !

!Decompiler methodsFor: 'control'!
statementsTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end."

	| blockPos stackPos t |
	blockPos := statements size.
	stackPos := stack size.
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[statements addLast: stack removeLast].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	^self popTo: blockPos! !

!Decompiler methodsFor: 'initialize-release' stamp: 'eem 7/1/2009 14:45'!
initSymbols: aClass
	constructor method: method class: aClass literals: method literals.
	constTable := constructor codeConstants.
	instVars := Array new: aClass instSize.
	tempVarCount := method numTemps.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars := self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars := blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps := tempVars ifNil: [(1 to: tempVarCount) collect: [:i| 't', i printString]].
			tempVars := (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i) beMethodArg]! !

!Decompiler methodsFor: 'initialize-release' stamp: 'nice 10/21/2009 00:29'!
mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys asArray sort
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector|
		tempNameTupleVector isEmpty ifFalse:
			[| subMap numTemps tempVector |
			subMap := Dictionary new.
			"Find how many temp slots there are (direct & indirect temp vectors)
			 and for each indirect temp vector find how big it is."
			tempNameTupleVector do:
				[:tuple|
				tuple last isArray
					ifTrue:
						[subMap at: tuple last first put: tuple last last.
						 numTemps := tuple last first]
					ifFalse:
						[numTemps := tuple last]].
			"create the temp vector for this scope level."
			tempVector := Array new: numTemps.
			"fill it in with any indirect temp vectors"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector at: index put: (Array new: size)].
			"fill it in with temp nodes."
			tempNameTupleVector do:
				[:tuple| | itv |
				tuple last isArray
					ifTrue:
						[itv := tempVector at: tuple last first.
						 itv at: tuple last last
							put: (aDecompilerConstructor
									codeTemp: tuple last last - 1
									named: tuple first)]
					ifFalse:
						[tempVector
							at: tuple last
							put: (aDecompilerConstructor
									codeTemp: tuple last - 1
									named: tuple first)]].
			"replace any indirect temp vectors with proper RemoteTempVectorNodes"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector
					at: index
					put: (aDecompilerConstructor
							codeRemoteTemp: index
							remoteTemps: (tempVector at: index))].
			"and update the entry in the map"
			map at: startpc put: tempVector]].
	^map! !

!Decompiler methodsFor: 'initialize-release' stamp: 'eem 6/29/2009 09:41'!
withTempNames: tempNames "<Array|String>"
	"Optionally initialize the temp names to be used when decompiling.
	 For backward-copmpatibility, if tempNames is an Array it is a single
	 vector of temp names, probably for a blue-book-compiled method.
	 If tempNames is a string it is a schematic string that encodes the
	 layout of temp vars in the method and any closures/blocks within it.
	 Decoding encoded tempNames is done in decompile:in:method:using:
	 which has the method from which to derive blockStarts.
	 See e.g. BytecodeEncoder>>schematicTempNamesString for syntax."
	tempVars := tempNames! !

!Decompiler methodsFor: 'instruction decoding'!
blockReturnTop
	"No action needed"! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 9/29/2008 15:02'!
case: dist
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns wierdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].! !

!Decompiler methodsFor: 'instruction decoding'!
doDup

	stack last == CascadeFlag
		ifFalse:
			["Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: CascadeFlag].
	stack addLast: CascadeFlag! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'di 2/5/2000 09:34'!
doPop

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == CaseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].! !

!Decompiler methodsFor: 'instruction decoding'!
doStore: stackOrBlock
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 2/3/2011 22:56'!
jump: dist
	| blockBody destPc nextPC |
	destPc := pc + dist.
	(lastJumpIfPcStack isEmpty or: [dist < 0 and: [destPc > lastJumpIfPcStack last]])
		ifTrue:
			["Rule: aBackward jump not crossing a Bfp/Btp must be a repeat"
			nextPC := pc.
			pc := destPc.
			blockBody := self statementsTo: lastPc.
			blockBody size timesRepeat: [statements removeLast].
			pc := nextPC.
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: #repeat
								code: #macro)
					arguments: #()).
			]
		ifFalse:
			[exit := destPc.
			lastJumpPc := lastPc]! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 11/6/2012 12:53'!
jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack blockBody blockArgs |
	lastJumpIfPcStack addLast: lastPc.
	stack last == CascadeFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			["Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by re-decompiling."
			stack := saveStack.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			blockArgs := thenBlock statements = constructor codeEmptyBlock statements
							ifTrue: [#()]
							ifFalse: [{ thenBlock }].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: (blockArgs isEmpty
												ifTrue:
													[sign
														ifTrue: [#whileFalse]
														ifFalse: [#whileTrue]]
												ifFalse:
													[sign
														ifTrue: [#whileFalse:]
														ifFalse: [#whileTrue:]])
								code: #macro)
					arguments: blockArgs).
			pc := elseStart.
			self convertToDoLoop]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: (Array with: thenBlock)]
						ifFalse:
							[constructor
								codeMessage: ifExpr
								selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
								arguments:	(sign
												ifTrue: [{elseBlock. thenBlock}]
												ifFalse: [{thenBlock. elseBlock}])].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast.! !

!Decompiler methodsFor: 'instruction decoding'!
methodReturnConstant: value

	self pushConstant: value; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding'!
methodReturnReceiver

	self pushReceiver; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 9/26/2008 15:43'!
methodReturnTop
	| last |
	last := stack removeLast "test test" asReturnNode.
	stack size > blockStackBase  "get effect of elided pop before return"
		ifTrue: [statements addLast: stack removeLast].
	exit := pc.
	lastJumpPc := lastReturnPc := lastPc.
	statements addLast: last! !

!Decompiler methodsFor: 'instruction decoding'!
popIntoLiteralVariable: value

	self pushLiteralVariable: value; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding'!
popIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 6/4/2008 14:44'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 8/4/2009 11:43'!
popIntoTemporaryVariable: offset
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars notNil "implies we were intialized with temp names."
			ifTrue: "Use the provided temps"
				[self assert: ((tempVector := tempVars at: offset + 1 ifAbsent: [ParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]])]
			ifFalse: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 't', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	self pushTemporaryVariable: offset; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding'!
pushActiveContext

	stack addLast: constructor codeThisContext! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 8/12/2010 13:51'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	| copiedValues |
	self sawClosureBytecode.
	copiedValues := ((1 to: numCopied) collect: [:ign| stack removeLast]) reversed.
	self doClosureCopyCopiedValues: copiedValues numArgs: numArgs blockSize: blockSize! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 6/4/2008 14:44'!
pushConsArrayWithElements: numElements 
	| array |
	self sawClosureBytecode.
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: stack removeLast].
	stack addLast: (constructor codeBrace: array)! !

!Decompiler methodsFor: 'instruction decoding'!
pushConstant: value

	| node |
	node := value == true ifTrue: [constTable at: 2]
		ifFalse: [value == false ifTrue: [constTable at: 3]
		ifFalse: [value == nil ifTrue: [constTable at: 4]
		ifFalse: [constructor codeAnyLiteral: value]]].
	stack addLast: node! !

!Decompiler methodsFor: 'instruction decoding'!
pushLiteralVariable: assoc

	stack addLast: (constructor codeAnyLitInd: assoc)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 6/4/2008 14:45'!
pushNewArrayOfSize: size
	self sawClosureBytecode.
	stack addLast: #pushNewArray -> (Array new: size)! !

!Decompiler methodsFor: 'instruction decoding'!
pushReceiver

	stack addLast: (constTable at: 1)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'nk 2/20/2004 11:56'!
pushReceiverVariable: offset

	| var |
	(var := instVars at: offset + 1 ifAbsent: []) == nil
		ifTrue:
			["Not set up yet"
			var := constructor codeInst: offset.
			instVars size < (offset + 1) ifTrue: [
				instVars := (Array new: offset + 1)
					replaceFrom: 1 to: instVars size with: instVars; yourself ].
			instVars at: offset + 1 put: var].
	stack addLast: var! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 9/25/2008 09:48'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	stack addLast: ((tempVars at: tempVectorIndex + 1) remoteTemps at: remoteTempIndex + 1)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 4/5/2013 22:21'!
pushTemporaryVariable: offset

	stack addLast: (offset >= tempVars size
		ifTrue:
			["Handle the case of chained LiteralVariableBinding assigments"
			stack at: (offset + 1 - tempVars size)]
		ifFalse:
			["A regular argument or temporary"
			tempVars at: offset + 1])! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 9/29/2008 19:23'!
send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	((#(blockCopy: closureCopy:copiedValues:) includes: selector)
	  and: [self checkForBlock: rcvr selector: selector arguments: args]) ifFalse:
		[selNode := constructor codeAnySelector: selector.
		rcvr == CascadeFlag
			ifTrue:
				["May actually be a cascade or an ifNil: for value."
				self willJumpIfFalse
					ifTrue: "= generated by a case macro"
						[selector == #= ifTrue:
							[" = signals a case statement..."
							statements addLast: args first.
							stack addLast: rcvr. "restore CascadeFlag"
							^ self].
						selector == #== ifTrue:
							[" == signals an ifNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]]
					ifFalse:
						[(self willJumpIfTrue and: [selector == #==]) ifTrue:
							[" == signals an ifNotNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]].
				msgNode := constructor
								codeCascadedMessage: selNode
								arguments: args.
				stack last == CascadeFlag ifFalse:
					["Last message of a cascade"
					statements addLast: msgNode.
					messages := self popTo: stack removeLast.  "Depth saved by first dup"
					msgNode := constructor
									codeCascade: stack removeLast
									messages: messages]]
			ifFalse:
				[msgNode := constructor
							codeMessage: rcvr
							selector: selNode
							arguments: args].
		stack addLast: msgNode]! !

!Decompiler methodsFor: 'instruction decoding'!
storeIntoLiteralVariable: assoc

	self pushLiteralVariable: assoc; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding'!
storeIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 6/4/2008 14:45'!
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding'!
storeIntoTemporaryVariable: offset

	self pushTemporaryVariable: offset; doStore: stack! !

!Decompiler methodsFor: 'public access' stamp: 'eem 9/27/2011 08:49'!
decompile: aSelector in: aClass 
	"See Decompiler|decompile:in:method:. The method is found by looking up 
	the message, aSelector, in the method dictionary of the class, aClass."

	^self
		decompile: aSelector
		in: aClass
		method: (aClass compiledMethodAt: aSelector) methodForDecompile! !

!Decompiler methodsFor: 'public access' stamp: 'eem 10/20/2008 14:09'!
decompile: aSelector in: aClass method: aMethod
	"Answer a MethodNode that is the root of the parse tree for the 
	argument, aMethod, which is the CompiledMethod associated with the 
	message, aSelector. Variables are determined with respect to the 
	argument, aClass."

	^self
		decompile: aSelector
		in: aClass
		method: aMethod
		using: (self constructorForMethod: aMethod)! !

!Decompiler methodsFor: 'public access' stamp: 'eem 8/5/2014 19:02'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
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

!Decompiler methodsFor: 'public access' stamp: 'nice 4/22/2012 16:34'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| startpc end homeClass blockNode methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(homeClass := home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompileWithTemps)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	constructor := self constructorForMethod: aBlock method.
	
	self withTempNames: method methodNode tempNames.

	self initSymbols: homeClass.
	startpc := aBlock startpc.
	end := aBlock endPC.
	stack := OrderedCollection new: method frameSize.
	lastJumpIfPcStack := OrderedCollection new.
	caseExits := OrderedCollection new.
	statements := OrderedCollection new: 20.
	super method: method pc: startpc - 5.
	blockNode := self blockTo: end.
	stack isEmpty ifFalse: [self error: 'stack not empty'].
	^blockNode statements first! !

!Decompiler methodsFor: 'public access'!
tempAt: offset
	"Needed by BraceConstructor<PopIntoTemporaryVariable"

	^tempVars at: offset + 1! !

!Decompiler methodsFor: 'private' stamp: 'eem 9/6/2008 08:45'!
blockScopeRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1! !

!Decompiler methodsFor: 'private' stamp: 'eem 10/20/2008 15:49'!
constructorForMethod: aMethod
	^(aMethod isBlueBookCompiled
		ifTrue: [DecompilerConstructor]
		ifFalse: [DecompilerConstructorForClosures]) new! !

!Decompiler methodsFor: 'private' stamp: 'eem 2/11/2013 13:23'!
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| leaveOnStack initStmt toDoStmt limitStmt |
	leaveOnStack := false.
	(stack notEmpty
	 and: [stack last isAssignmentNode])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast.
			 statements removeLast; addLast: toDoStmt.
			 leaveOnStack := true]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue:
		[leaveOnStack ifTrue:
			[stack addLast: statements removeLast].
			 ^self].
	limitStmt := statements last.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: AssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[leaveOnStack ifTrue:
						[stack addLast: statements removeLast].
					^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]! !

!Decompiler methodsFor: 'private' stamp: 'eem 11/6/2012 18:11'!
interpretNextInstructionFor: client

	| code varNames |

"Change false here will trace all state in Transcript."
true ifTrue: [^super interpretNextInstructionFor: client].

	varNames := self class allInstVarNames.
	code := (self method at: pc) radix: 16.
	Transcript cr; cr; print: pc; space; nextPutAll: '<' , code, '>'.
	(varNames indexOf: 'stack') to: varNames size do:
		[:i |
		i <= 10 ifTrue: [Transcript cr] ifFalse: [Transcript space; space].
		Transcript nextPutAll: (varNames at: i); nextPutAll: ': '; print: (self instVarAt: i)].
	Transcript flush.
	^super interpretNextInstructionFor: client! !

!Decompiler methodsFor: 'private' stamp: 'di 2/6/2000 10:55'!
methodRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner |
	nRefs := 0.
	offset <= 15
		ifTrue:
			[byteCode := 16 + offset.
			(InstructionStream on: method) scanFor:
				[:instr | instr = byteCode ifTrue: [nRefs := nRefs + 1].
				nRefs > 1]]
		ifFalse:
			[extension := 64 + offset.
			scanner := InstructionStream on: method.
			scanner scanFor:
				[:instr | (instr = 128 and: [scanner followingByte = extension])
							ifTrue: [nRefs := nRefs + 1].
				nRefs > 1]].
	^ nRefs = 1
! !

!Decompiler methodsFor: 'private'!
popTo: oldPos

	| t |
	t := Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t! !

!Decompiler methodsFor: 'private' stamp: 'di 12/26/1998 21:29'!
quickMethod
	| |
	method isReturnSpecial
		ifTrue: [^ constructor codeBlock:
				(Array with: (constTable at: method primitive - 255)) returns: true].
	method isReturnField
		ifTrue: [^ constructor codeBlock:
				(Array with: (constructor codeInst: method returnField)) returns: true].
	self error: 'improper short method'! !

!Decompiler methodsFor: 'private' stamp: 'eem 7/29/2008 17:41'!
sawBlueBookBlock
	constructor isForClosures ifTrue:
		[constructor primitiveChangeClassTo: DecompilerConstructor new]! !

!Decompiler methodsFor: 'private' stamp: 'eem 6/4/2008 14:43'!
sawClosureBytecode
	constructor isForClosures ifFalse:
		[constructor primitiveChangeClassTo: DecompilerConstructorForClosures new]! !

!Decompiler methodsFor: 'private' stamp: 'eem 9/6/2008 09:27'!
scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner
	| bsl maybeBlockSize |
	bsl := BlockStartLocator new.
	scanner pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[refpc = scanner pc ifTrue:
			[scanner pc: startpc.
			 [scanner pc <= endpc] whileTrue:
				[(scan value: scanner firstByte) ifTrue:
					[^endpc].
				 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
					[scanner pc: scanner pc + maybeBlockSize]].
			   ^self].
		 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
			[refpc <= (scanner pc + maybeBlockSize)
				ifTrue: [^self scanBlockScopeFor: refpc from: scanner pc to: scanner pc + maybeBlockSize with: scan scanner: scanner]
				ifFalse: [scanner pc: scanner pc + maybeBlockSize]]]! !


!Decompiler class methodsFor: 'class initialization' stamp: 'di 1/28/2000 22:21'!
initialize

	CascadeFlag := 'cascade'.  "A unique object"
	CaseFlag := 'case'. "Ditto"
	ArgumentFlag := 'argument'.  "Ditto"
	IfNilFlag := 'ifNil'.  "Ditto"

	"Decompiler initialize"! !

!Decompiler class methodsFor: '*Tests' stamp: 'fbs 7/12/2013 20:50'!
recompileAllTest
	"[Decompiler recompileAllTest]"
	"decompile every method and compile it back; if the decompiler is correct then the system should keep running.  :)"
	
	SystemNavigation default allBehaviorsDo: [ :behavior |
		UIManager default informUser: (behavior printString) during: [
			behavior selectors do: [ :sel |
				| decompiled ast compiled |
				decompiled := Decompiler new decompile: sel in: behavior.
				ast := Compiler new compile: decompiled in: behavior notifying: nil ifFail: [ self error: 'failed' ].
				compiled := ast generate: (behavior compiledMethodAt: sel) trailer.
				behavior addSelector: sel withMethod: compiled. ] ] ]! !


!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForBlockValue: stack encoder: encoder
	"Generate code for evaluating the last statement in a block"
	^self emitCodeForValue: stack encoder: encoder! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:37'!
emitCodeForBranchOn: condition dist: dist pop: stack encoder: encoder
	stack pop: 1.
	dist = 0 ifTrue: [^encoder genPop].
	condition
		ifTrue: [encoder genBranchPopTrue: dist]
		ifFalse: [encoder genBranchPopFalse: dist]! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:38'!
emitCodeForEffect: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genPop.
	stack pop: 1! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:39'!
emitCodeForJump: dist encoder: encoder

	dist = 0 ifFalse: [encoder genJump: dist]! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:38'!
emitCodeForReturn: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genReturnTop! !

!ParseNode methodsFor: 'code generation' stamp: 'nk 7/10/2004 10:04'!
pc
	"Used by encoder source mapping."

	^pc ifNil: [ 0 ]
! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 8/4/2008 13:57'!
pc: anInteger
	"Used by encoder source mapping."

	pc := anInteger! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:13'!
sizeCode: encoder forBranchOn: condition dist: dist
	dist = 0 ifTrue: [^encoder sizePop].
	^condition
		ifTrue: [encoder sizeBranchPopTrue: dist]
		ifFalse: [encoder sizeBranchPopFalse: dist]! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/15/2008 09:52'!
sizeCode: encoder forJump: dist

	^dist = 0 ifTrue: [0] ifFalse: [encoder sizeJump: dist]! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForBlockValue: encoder
	"Answer the size for evaluating the last statement in a block"
	^self sizeCodeForValue: encoder! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:53'!
sizeCodeForEffect: encoder

	^(self sizeCodeForValue: encoder) + encoder sizePop! !

!ParseNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:57'!
sizeCodeForReturn: encoder

	^(self sizeCodeForValue: encoder) + encoder sizeReturnTop! !

!ParseNode methodsFor: 'comment'!
comment

	^comment! !

!ParseNode methodsFor: 'comment'!
comment: newComment

	comment := newComment! !

!ParseNode methodsFor: 'converting'!
asReturnNode

	^ReturnNode new expr: self! !

!ParseNode methodsFor: 'encoding'!
encodeSelector: selector

	^nil! !

!ParseNode methodsFor: 'printing' stamp: 'tk 10/16/2000 13:57'!
nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := WriteStream on: (String new: 500)].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString copyWithout: Character cr.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; cr.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isKindOf: SequenceableCollection) ifTrue: [
				var do: [:aNode | 
					(aNode respondsTo: #asReturnNode) ifTrue: [
						aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm
! !

!ParseNode methodsFor: 'printing' stamp: 'eem 9/5/2009 11:27'!
printCommentOn: aStream indent: indent 
	| thisComment |
	self comment == nil ifTrue: [^ self].
	1 to: self comment size
	   do: [:index |
		index > 1 ifTrue: [aStream crtab: indent].
		aStream nextPut: $".
		thisComment := self comment at: index.
		self printSingleComment: thisComment
			on: aStream
			indent: indent.
		aStream nextPut: $"]! !

!ParseNode methodsFor: 'printing' stamp: 'eem 5/6/2008 15:18'!
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printOn: aStream indent: 0.
	aStream nextPut: $}.! !

!ParseNode methodsFor: 'printing'!
printOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop."

	super printOn: aStream! !

!ParseNode methodsFor: 'printing'!
printOn: aStream indent: level precedence: p

	self printOn: aStream indent: level! !

!ParseNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:51'!
printWithClosureAnalysis

	^String streamContents: [:str| self printWithClosureAnalysisOn: str]! !

!ParseNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printWithClosureAnalysisOn: aStream indent: 0.
	aStream nextPut: $}.! !

!ParseNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop."

	super printWithClosureAnalysisOn: aStream! !

!ParseNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level precedence: p

	self printWithClosureAnalysisOn: aStream indent: level! !

!ParseNode methodsFor: 'printing' stamp: 'ms 8/1/2006 16:47'!
shortPrintOn: aStream 
	self printOn: aStream indent: 0! !

!ParseNode methodsFor: 'testing'!
assignmentCheck: encoder at: location
	"For messageNodes masquerading as variables for the debugger.
	For now we let this through - ie we allow stores ev
	into args.  Should check against numArgs, though."
	^ -1! !

!ParseNode methodsFor: 'testing'!
canCascade

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'eem 2/3/2011 09:12'!
ensureCanCascade: encoder! !

!ParseNode methodsFor: 'testing'!
isArg

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'eem 6/16/2008 09:37'!
isAssignmentNode
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'eem 9/25/2008 12:11'!
isBlockNode
	^false! !

!ParseNode methodsFor: 'testing'!
isComplex
	"Used for pretty printing to determine whether to start a new line"

	^false! !

!ParseNode methodsFor: 'testing'!
isConstantNumber  "Overridden in LiteralNode"
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'md 1/20/2006 16:22'!
isDoIt
	"polymorphic with RBNodes; called by debugger"

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'eem 7/18/2008 16:22'!
isFutureNode
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ls 1/29/2004 21:11'!
isJust: node
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ul 11/23/2010 13:06'!
isLiteralNode

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'md 7/27/2006 19:14'!
isMessage
	^false! !

!ParseNode methodsFor: 'testing'!
isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"See comment in MessageNode."

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ar 7/10/2009 22:42'!
isMessageNode
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'eem 8/31/2010 11:34'!
isOnlySubnodeOf: aSubtree "<ParseNode>" in: aParseTree "<ParseNode>"
	"Answer if the receiver only occurs within aSubtree of aParseTree, not in the rest of aParseTree.
	 Assumes that aSubtree is in fact a subnode of aParseTree."
	| isSubnode |
	isSubnode := false.
	aSubtree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [isSubnode := true]]).
	isSubnode ifFalse:
		[^false].
	aParseTree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [^false]]
							select: [:node| node ~= aSubtree]).
	^true! !

!ParseNode methodsFor: 'testing'!
isReturnSelf

	^false! !

!ParseNode methodsFor: 'testing'!
isReturningIf

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'tk 8/2/1999 18:39'!
isSelfPseudoVariable	
	"Overridden in VariableNode."
	^false! !

!ParseNode methodsFor: 'testing'!
isSpecialConstant
	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'di 10/12/1999 15:28'!
isTemp
	^ false! !

!ParseNode methodsFor: 'testing'!
isUndefTemp
	^ false! !

!ParseNode methodsFor: 'testing'!
isUnusedTemp
	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'ar 11/19/2002 14:58'!
isVariableNode
	^false! !

!ParseNode methodsFor: 'testing'!
isVariableReference

	^false! !

!ParseNode methodsFor: 'testing'!
nowHasDef  "Ignored in all but VariableNode"! !

!ParseNode methodsFor: 'testing'!
nowHasRef  "Ignored in all but VariableNode"! !

!ParseNode methodsFor: 'testing'!
toDoIncrement: ignored
	"Only meant for Messages or Assignments - else return nil"
	^ nil! !

!ParseNode methodsFor: 'private' stamp: 'ls 1/29/2004 21:17'!
ifNilReceiver
	"assuming this object is the receiver of an ifNil:, what object is being asked about?"
	^self! !

!ParseNode methodsFor: 'private' stamp: 'sma 5/28/2000 10:47'!
nextWordFrom: aStream setCharacter: aBlock
	| outStream char |
	outStream := WriteStream on: (String new: 16).
	[(aStream peekFor: Character space) 
		or: [aStream peekFor: Character tab]] whileTrue.
	[aStream atEnd
		or:
			[char := aStream next.
			char = Character cr or: [char = Character space]]]
		whileFalse: [outStream nextPut: char].
	aBlock value: char.
	^ outStream contents! !

!ParseNode methodsFor: 'private' stamp: 'nk 7/11/2004 13:39'!
printSingleComment: aString on: aStream indent: indent 
	"Print the comment string, assuming it has been indented indent tabs.
	Break the string at word breaks, given the widths in the default
	font, at 450 points."

	| readStream word position lineBreak font wordWidth tabWidth spaceWidth lastChar |
	readStream := ReadStream on: aString.
	font := TextStyle default defaultFont.
	tabWidth := TextConstants at: #DefaultTab.
	spaceWidth := font widthOf: Character space.
	position := indent * tabWidth.
	lineBreak := 450.
	[readStream atEnd]
		whileFalse: 
			[word := self nextWordFrom: readStream setCharacter: [:lc | lastChar := lc].
			wordWidth := word inject: 0 into: [:width :char | width + (font widthOf: char)].
			position := position + wordWidth.
			position > lineBreak
				ifTrue: 
					[aStream skip: -1; crtab: indent.
					position := indent * tabWidth + wordWidth + spaceWidth.
					lastChar = Character cr
						ifTrue: [[readStream peekFor: Character tab] whileTrue].
					word isEmpty ifFalse: [aStream nextPutAll: word; space]]
				ifFalse: 
					[aStream nextPutAll: word.
					readStream atEnd
						ifFalse: 
							[position := position + spaceWidth.
							aStream space].
					lastChar = Character cr
						ifTrue: 
							[aStream skip: -1; crtab: indent.
							position := indent * tabWidth.
							[readStream peekFor: Character tab] whileTrue]]]! !

!ParseNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	"Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:.
	 All such implementations under ParseNode should answer the result of the dispatch, e.g.
		^aVisitor visitBlockNode: self"
	^self subclassResponsibility! !

!ParseNode methodsFor: 'visiting' stamp: 'eem 7/20/2009 19:44'!
nodesDo: aBlock
	self accept: (ParseNodeEnumerator ofBlock: aBlock)! !

!ParseNode methodsFor: '*Etoys-tiles' stamp: 'ab 7/13/2004 13:47'!
addCommentToMorph: aMorph
	| row |
	(self comment isNil or: [self comment isEmpty]) ifTrue: [^ self].
	row := aMorph addTextRow:
		(String streamContents: [:strm | self printCommentOn: strm indent: 1]).
	row firstSubmorph color: (SyntaxMorph translateColor: #comment).
	row parseNode: (self as: CommentNode).
! !

!ParseNode methodsFor: '*Etoys-tiles' stamp: 'RAA 8/24/1999 13:06'!
currentValueIn: aContext

	^nil! !

!ParseNode methodsFor: '*Etoys-tiles' stamp: 'RAA 8/24/1999 18:18'!
explanation

	^self class printString! !


!ParseNode class methodsFor: 'class initialization' stamp: 'eem 5/17/2014 17:46'!
initialize
	"ParseNode initialize. VariableNode initialize"
	LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
	SendType := 5.
	"Back in the day the following constants corresponded to bytecodes.
	 Now they're just unique values that need to be eliminated when and if
	 things like code:type: are cleaned up."
	CodeBases := #(0 16 32 64 208 ).
	CodeLimits := #(16 16 32 32 16 ).
	LdSelf := 112.
	LdTrue := 113.
	LdFalse := 114.
	LdNil := 115.
	LdMinus1 := 116.
	LoadLong := 128.
	LdSuper := 133.
	LdThisContext := 137.
	SendPlus := 176.
	Send := 208
	
	"((ParseNode class >> #initialize) literals select: [:l| l isVariableBinding and: [(ParseNode classPool includesAssociation: l) and: [(self systemNavigation allCallsOn: l localTo: ParseNode) size = 1]]]) sort: [:a :b| a key <= b key]"

	"ParseNode classPool associations select: [:a| (self systemNavigation allCallsOn: a localTo: ParseNode) isEmpty]"! !

!ParseNode class methodsFor: 'accessing' stamp: 'eem 5/21/2008 13:18'!
pushNilCode

	^LdNil! !

!ParseNode class methodsFor: 'accessing' stamp: 'eem 8/4/2009 12:34'!
tempSortBlock
	"Answer a block that can sort a set of temporaries into a stable
	 order so that different compilations produce the same results."
	^[:t1 :t2| | be1 be2 bs1 bs2 |
	   t1 index < t2 index "simple sort by index."
	   or: [t1 index = t2 index "complex tie break" 
		  and: [t1 isRemote ~= t2 isRemote
				ifTrue: [t2 isRemote] "put direct temps before indirect temps"
				ifFalse: 
					[((be1 := t1 definingScope blockExtent) isNil
					  or: [(be2 := t2 definingScope blockExtent) isNil])
						ifTrue: [t1 name < t2 name] "only have the name left to go on"
						ifFalse: "put temps from outer scopes before those from inner scopes"
							[(bs1 := be1 first) < (bs2 := be2 first)
							 or: [bs1 = bs2 and: [t1 name < t2 name]]]]]]] "only have the name left to go on"! !


!AssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:30'!
emitCodeForEffect: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: false encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStorePop: stack encoder: encoder! !

!AssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:30'!
emitCodeForValue: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: true encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStore: stack encoder: encoder! !

!AssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForEffect: encoder

	^(variable sizeCodeForLoad: encoder forValue: false)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStorePop: encoder)! !

!AssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForValue: encoder

	^(variable sizeCodeForLoad: encoder forValue: true)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStore: encoder)! !

!AssignmentNode methodsFor: 'equation translation'!
variable
	^variable! !

!AssignmentNode methodsFor: 'initialize-release'!
toDoIncrement: var
	var = variable ifFalse: [^ nil].
	(value isMemberOf: MessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]! !

!AssignmentNode methodsFor: 'initialize-release'!
value
	^ value! !

!AssignmentNode methodsFor: 'initialize-release'!
variable: aVariable value: expression

	variable := aVariable.
	value := expression! !

!AssignmentNode methodsFor: 'initialize-release' stamp: 'di 3/22/1999 12:00'!
variable: aVariable value: expression from: encoder

	(aVariable isMemberOf: MessageAsTempNode)
		ifTrue: ["Case of remote temp vars"
				^ aVariable store: expression from: encoder].
	variable := aVariable.
	value := expression! !

!AssignmentNode methodsFor: 'initialize-release' stamp: 'hmm 7/15/2001 21:17'!
variable: aVariable value: expression from: encoder sourceRange: range

	encoder noteSourceRange: range forNode: self.
	^self
		variable: aVariable
		value: expression
		from: encoder! !

!AssignmentNode methodsFor: 'printing' stamp: 'eem 5/6/2008 13:48'!
printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printOn: aStream indent: level + 2! !

!AssignmentNode methodsFor: 'printing' stamp: 'eem 5/9/2008 18:44'!
printOn: aStream indent: level precedence: p

	aStream nextPut: $(.
	self printOn: aStream indent: level.
	aStream nextPut: $)! !

!AssignmentNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level 
	variable printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printWithClosureAnalysisOn: aStream indent: level + 2! !

!AssignmentNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level precedence: p

	aStream nextPut: $(.
	self printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPut: $)! !

!AssignmentNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitAssignmentNode: self! !

!AssignmentNode methodsFor: 'code generation (closures)' stamp: 'eem 5/30/2008 09:37'!
analyseTempsWithin: scopeBlock "<BlockNode>"  rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"N.B.  since assigment happens _after_ the value is evaluated the value is sent the message _first_."
	value analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools.
	variable beingAssignedToAnalyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools! !

!AssignmentNode methodsFor: 'testing' stamp: 'eem 6/16/2008 09:37'!
isAssignmentNode
	^true! !

!AssignmentNode methodsFor: '*Etoys-tiles' stamp: 'RAA 8/15/1999 16:31'!
explanation

	^'The value of ',value explanation,' is being stored in ',variable explanation
! !


!BlockNode methodsFor: 'accessing' stamp: 'eem 6/2/2008 14:00'!
addArgument: aTempVariableNode
	temporaries := temporaries copyWith: aTempVariableNode! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/31/2010 12:31'!
arguments
	^arguments ifNil: [#()]! !

!BlockNode methodsFor: 'accessing'!
arguments: argNodes 
	"Decompile."

	arguments := argNodes! !

!BlockNode methodsFor: 'accessing' stamp: 'tk 8/4/1999 22:53'!
block
	^ self! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/22/2008 10:01'!
closureCreationNode
	closureCreationNode ifNil:
		[closureCreationNode := LeafNode new
									key: #closureCreationNode
									code: nil].
	^closureCreationNode! !

!BlockNode methodsFor: 'accessing'!
firstArgument
	^ arguments first! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 5/30/2008 12:12'!
nArgsSlot
	"Private for the Encoder to use in bindArg"
	^nArgsNode! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 5/30/2008 12:12'!
nArgsSlot: anInteger
	"Private for the Encoder to use in bindArg"
	nArgsNode := anInteger! !

!BlockNode methodsFor: 'accessing'!
numberOfArguments

	^arguments size! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 7/24/2008 12:37'!
optimized
	^optimized! !

!BlockNode methodsFor: 'accessing'!
returnLast

	self returns
		ifFalse: 
			[returns := true.
			statements at: statements size put: statements last asReturnNode]! !

!BlockNode methodsFor: 'accessing' stamp: 'ar 11/17/2002 19:57'!
returnNilIfNoOther

	self returns
		ifFalse: 
			[statements last == NodeNil ifFalse: [statements add: NodeNil].
			self returnLast]! !

!BlockNode methodsFor: 'accessing' stamp: 'gk 4/6/2006 11:29'!
returnSelfIfNoOther: encoder

	self returns ifTrue:[^self].
	statements last == NodeSelf ifFalse: [
		statements := statements copyWith: (encoder encodeVariable: 'self').
	].
	self returnLast.
! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/4/2008 10:48'!
startOfLastStatement
	^startOfLastStatement! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/4/2008 10:50'!
startOfLastStatement: anInteger
	"Note the source index of the start of the last full statement.  The
	 last full statement is the value answered by a block and hence the
	 expression the debugger should display as the value of the block."
	startOfLastStatement := anInteger! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/31/2010 12:30'!
temporaries
	^temporaries ifNil: [#()]! !

!BlockNode methodsFor: 'accessing' stamp: 'sma 2/27/2000 22:37'!
temporaries: aCollection
	temporaries := aCollection! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/13/2010 15:44'!
tempsMark
	^tempsMark! !

!BlockNode methodsFor: 'accessing' stamp: 'eem 8/13/2010 15:44'!
tempsMark: anInteger
	tempsMark := anInteger! !

!BlockNode methodsFor: 'code generation'!
code

	^statements first code! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 6/2/2008 13:29'!
emitCodeExceptLast: stack encoder: encoder
	| position nextToLast |
	position := stack position.
	nextToLast := statements size - 1.
	1 to: nextToLast do:
		[:i | | statement |
		statement := statements at: i.
		statement emitCodeForEffect: stack encoder: encoder.
		self assert: stack position = position].! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/21/2008 11:28'!
emitCodeForEvaluatedEffect: stack encoder: encoder
	| position |
	position := stack position.
	self returns
		ifTrue: 
			[self emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1]
		ifFalse: 
			[self emitCodeExceptLast: stack encoder: encoder.
			statements last emitCodeForEffect: stack encoder: encoder].
	self assert: stack position = position! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/21/2008 11:36'!
emitCodeForEvaluatedValue: stack encoder: encoder
	| position |
	position := stack position.
	self emitCodeExceptLast: stack encoder: encoder.
	statements last emitCodeForBlockValue: stack encoder: encoder.
	self assert: stack position - 1 = position! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 16:55'!
emitCodeForValue: stack encoder: encoder

	self generateAsClosure ifTrue:
		[^self emitCodeForClosureValue: stack encoder: encoder].
	encoder genPushThisContext.
	stack push: 1.
	nArgsNode emitCodeForValue: stack encoder: encoder.
	remoteCopyNode
		emitCode: stack
		args: 1
		encoder: encoder.
	"Force a two byte jump."
	encoder genJumpLong: size.
	stack push: arguments size.
	arguments reverseDo: [:arg | arg emitCodeForStorePop: stack encoder: encoder].
	self emitCodeForEvaluatedValue: stack encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		pc := encoder methodStreamPosition].
	stack pop: 1! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/29/2008 15:21'!
sizeCodeExceptLast: encoder
	| codeSize |
	codeSize := 0.
	1 to: statements size - 1 do: 
		[:i | | statement |
		 statement := statements at: i.
		 codeSize := codeSize + (statement sizeCodeForEffect: encoder)].
	^codeSize! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:13'!
sizeCodeForEvaluatedEffect: encoder

	^self returns
		ifTrue: [self sizeCodeForEvaluatedValue: encoder]
		ifFalse: [(self sizeCodeExceptLast: encoder)
				+ (statements last sizeCodeForEffect: encoder)]! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForEvaluatedValue: encoder

	^(self sizeCodeExceptLast: encoder)
		+ (statements last sizeCodeForBlockValue: encoder)! !

!BlockNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 16:55'!
sizeCodeForValue: encoder
	self generateAsClosure ifTrue:
		[^self sizeCodeForClosureValue: encoder].

	nArgsNode := encoder encodeLiteral: arguments size.
	remoteCopyNode := encoder encodeSelector: #blockCopy:.
	size := self sizeCodeForEvaluatedValue: encoder.
	self returns ifFalse:
		[size := size + encoder sizeReturnTopToCaller]. "endBlock"
	arguments := arguments collect:  "Chance to prepare debugger remote temps"
						[:arg | arg asStorableNode: encoder].
	arguments do: [:arg | size := size + (arg sizeCodeForStorePop: encoder)].
	^encoder sizePushThisContext
	 + (nArgsNode sizeCodeForValue: encoder) 
	 + (remoteCopyNode sizeCode: encoder args: 1 super: false)
	 + (encoder sizeJumpLong: size)
	 + size! !

!BlockNode methodsFor: 'equation translation'!
statements
	^statements! !

!BlockNode methodsFor: 'equation translation'!
statements: val
	statements := val! !

!BlockNode methodsFor: 'initialize-release' stamp: 'eem 5/12/2014 11:23'!
arguments: argNodes statements: statementsCollection returns: returnBool from: encoder
	"Compile."

	arguments := argNodes.
	statements := statementsCollection size > 0
					ifTrue: [statementsCollection]
					ifFalse: [Array with: NodeNil].
	optimized := false.
	returns := returnBool! !

!BlockNode methodsFor: 'initialize-release' stamp: 'eem 8/4/2008 14:12'!
noteSourceRangeStart: start end: end encoder: encoder
	"Note two source ranges for this node.  One is for the debugger
	 and is of the last expression, the result of the block.  One is for
	 source analysis and is for the entire block."
	encoder
		noteSourceRange: (start to: end)
		forNode: self closureCreationNode.
	startOfLastStatement
		ifNil:
			[encoder
				noteSourceRange: (start to: end)
				forNode: self]
		ifNotNil:
			[encoder
				noteSourceRange: (startOfLastStatement to: end - 1)
				forNode: self]! !

!BlockNode methodsFor: 'initialize-release' stamp: 'eem 5/20/2008 13:40'!
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
						ifTrue: [Array with: NodeNil]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]! !

!BlockNode methodsFor: 'printing' stamp: 'md 2/22/2006 16:37'!
decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^ self decompileText asString
! !

!BlockNode methodsFor: 'printing' stamp: 'md 2/22/2006 16:37'!
decompileText
	"Answer a text description of the parse tree whose root is the receiver."

	^ ColoredCodeStream contents: [:strm | self printOn: strm indent: 0]
! !

!BlockNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:28'!
printArgumentsOn: aStream indent: level
	arguments size = 0 ifTrue: [^ self].
	arguments do:
		[:arg | aStream nextPut: $:;  nextPutAll: arg key;  space].
	aStream nextPut: $|; space.
	"If >0 args and >1 statement, put all statements on separate lines"
	statements size > 1 ifTrue:
		[aStream crtab: level]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 9/25/2008 12:48'!
printOn: aStream indent: level

	"statements size <= 1 ifFalse: [aStream crtab: level]."
	aStream nextPut: $[.
	self printArgumentsOn: aStream indent: level.
	(self printTemporaries: temporaries on: aStream doPrior: []) ifTrue:
		["If >0 temps and >1 statement, put all statements on separate lines"
		 statements size > 1
			ifTrue: [aStream crtab: level]
			ifFalse: [aStream space]].
	self printStatementsOn: aStream indent: level.
	aStream nextPut: $]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 9/23/2008 15:05'!
printStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level := 1 max: levelOrZero.
	comment == nil
		ifFalse: 
			[self printCommentOn: aStream indent: level.
			aStream crtab: level].
	len := shown := statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [shown := 1 max: shown - 1]
		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."
				(arguments size = 0
				and: [len >= 1
				and: [(statements at: len) == NodeNil
				and: [len = 1
					or: [len > 1
						and: [(statements at: len - 1) isMessageNode
						and: [(statements at: len - 1) isNilIf]]]]]])
					ifTrue: [shown := shown - 1]].
	1 to: shown do: 
		[:i | 
		thisStatement := statements at: i.
		thisStatement printOn: aStream indent: level.
		i < shown ifTrue: [aStream nextPut: $.; crtab: level].
		(thisStatement comment ~~ nil and: [thisStatement comment size > 0])
			ifTrue: 
				[i = shown ifTrue: [aStream crtab: level].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [aStream crtab: level]]]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 7/21/2009 13:12'!
printTemporaries: tempSequence on: aStream doPrior: aBlock
	"Print any in-scope temporaries.  If there are any evaluate aBlock
	 prior to printing.  Answer whether any temporaries were printed."
	| tempStream seen |
	tempSequence ifNil:
		[^false].
	tempStream := (String new: 16) writeStream.
	"This is for the decompiler which canmot work out which optimized block a particular temp is
	 local to and hence may produce diplicates as in
		expr ifTrue: [| aTemp | ...] ifFalse: [| aTemp | ...]"
	seen := Set new.
	tempSequence do:
		[:tempNode |
		tempNode isIndirectTempVector
			ifTrue:
				[tempNode remoteTemps do:
					[:tempVariableNode|
					 (tempVariableNode scope >= 0
					  and: [(seen includes: tempNode key) not]) ifTrue:
						[tempStream space; nextPutAll: (seen add: tempVariableNode key)]]]
			ifFalse:
				[(tempNode scope >= -1
				  and: ["This is for the decompiler which may create a block arg when converting
						a while into a to:do: but won't remove it form temporaries"
					   tempNode isBlockArg not
				  and: [(seen includes: tempNode key) not]]) ifTrue:
					[tempStream space; nextPutAll: (seen add: tempNode key)]]].
	tempStream position = 0 ifTrue:
		[^false].
	aBlock value.
	aStream nextPut: $|; nextPutAll: tempStream contents; space; nextPut: $|.
	^true! !

!BlockNode methodsFor: 'printing' stamp: 'eem 6/2/2008 12:06'!
printWithClosureAnalysisArgumentsOn: aStream indent: level
	arguments size = 0 ifTrue: [^self].
	arguments do:
		[:tempNode |
		 aStream space; nextPut: $:.
		 tempNode printDefinitionForClosureAnalysisOn: aStream].
	aStream nextPut: $|; space.
	"If >0 args and >1 statement, put all statements on separate lines"
	statements size > 1 ifTrue:
		[aStream crtab: level]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:53'!
printWithClosureAnalysisOn: aStream indent: level
	aStream nextPut: $[.
	blockExtent ifNotNil: [aStream print: blockExtent].
	self printWithClosureAnalysisArgumentsOn: aStream indent: level.
	self printWithClosureAnalysisTemporariesOn: aStream indent: level.
	self printWithClosureAnalysisStatementsOn: aStream indent: level.
	aStream nextPut: $]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:48'!
printWithClosureAnalysisStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level := 1 max: levelOrZero.
	comment == nil
		ifFalse: 
			[self printCommentOn: aStream indent: level.
			aStream crtab: level].
	len := shown := statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [shown := 1 max: shown - 1]
		ifFalse: [(len = 1 and: [((statements at: 1) == NodeNil) & (arguments size = 0)])
					ifTrue: [shown := shown - 1]].
	1 to: shown do: 
		[:i | 
		thisStatement := statements at: i.
		thisStatement printWithClosureAnalysisOn: aStream indent: level.
		i < shown ifTrue: [aStream nextPut: $.; crtab: level].
		(thisStatement comment ~~ nil and: [thisStatement comment size > 0])
			ifTrue: 
				[i = shown ifTrue: [aStream crtab: level].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [aStream crtab: level]]]! !

!BlockNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:54'!
printWithClosureAnalysisTemporariesOn: aStream indent: level

	(temporaries == nil or: [temporaries size = 0]) ifFalse:
		[aStream nextPut: $|.
		temporaries do: 
			[:tempNode |
			 aStream space.
			 tempNode printDefinitionForClosureAnalysisOn: aStream].
		aStream nextPutAll: ' | '.
		"If >0 args and >1 statement, put all statements on separate lines"
		statements size > 1 ifTrue: [aStream crtab: level]]! !

!BlockNode methodsFor: 'testing' stamp: 'eem 7/17/2008 12:20'!
generateAsClosure
	"Answer if we're compiling under the closure regime.  If blockExtent has been set by
	analyseTempsWithin:rootNode: et al then we're compiling under the closure regime."
	^blockExtent ~~ nil! !

!BlockNode methodsFor: 'testing' stamp: 'eem 9/25/2008 12:10'!
isBlockNode
	^true! !

!BlockNode methodsFor: 'testing'!
isComplex

	^statements size > 1 or: [statements size = 1 and: [statements first isComplex]]! !

!BlockNode methodsFor: 'testing'!
isJust: node

	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]! !

!BlockNode methodsFor: 'testing'!
isJustCaseError

	^ statements size = 1 and:
		[statements first
			isMessage: #caseError
			receiver: [:r | r==NodeSelf]
			arguments: nil]! !

!BlockNode methodsFor: 'testing'!
isQuick
	^ statements size = 1
		and: [statements first isVariableReference
				or: [statements first isSpecialConstant]]! !

!BlockNode methodsFor: 'testing'!
returns

	^returns or: [statements last isReturningIf]! !

!BlockNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitBlockNode: self! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 9/3/2009 12:55'!
actualScope
	"Answer the actual scope for the receiver.  If this is an unoptimized block then it is its
	 actual scope, but if this is an optimized block then the actual scope is some outer block."
	^actualScopeIfOptimized ifNil: [self]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2009 11:42'!
addHoistedTemps: additionalTemporaries "<SequenceableCollection>"
	additionalTemporaries do:
		[:temp|
		temp definingScope ifNil:
			[temp definingScope: self]].
	temporaries := (temporaries isNil or: [temporaries isEmpty])
					ifTrue: [additionalTemporaries copy]
					ifFalse:
						[temporaries last isIndirectTempVector
							ifTrue: [temporaries allButLast, additionalTemporaries, { temporaries last }]
							ifFalse: [temporaries, additionalTemporaries]]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'nice 3/2/2010 00:58'!
addRemoteTemp: aTempVariableNode rootNode: rootNode "<MethodNode>"
	"Add aTempVariableNode to my actualScope's sequence of
	 remote temps.  If I am an optimized block then the actual
	 scope is my actualScopeIfOptimized, otherwise it is myself."
	remoteTempNode == nil ifTrue:
		[remoteTempNode := RemoteTempVectorNode new
								name: self remoteTempNodeName
								index: arguments size + temporaries size
								type: LdTempType
								scope: 0.
		 actualScopeIfOptimized
			ifNil:
				[self addTempNode: remoteTempNode.
				 remoteTempNode definingScope: self]
			ifNotNil: [actualScopeIfOptimized addHoistedTemps: { remoteTempNode }]].
	remoteTempNode addRemoteTemp: aTempVariableNode encoder: rootNode encoder.
	"use remove:ifAbsent: because the deferred analysis for optimized
	 loops can result in the temp has already been hoised into the root."
	self removeTempNode: aTempVariableNode ifAbsent: [
		self actualScope removeTempNode: aTempVariableNode ifAbsent: ["should not happen"]].
	^remoteTempNode! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'nice 3/1/2010 23:14'!
addTempNode: aTempVariableNode
	"Utilities for when we want to add some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries add: aTempVariableNode! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2009 13:29'!
analyseArguments: methodArguments temporaries: methodTemporaries rootNode: rootNode "<MethodNode>" "^<Sequence of: <TempVarNade>>"
	"Top level entry-point for analysing temps within the hierarchy of blocks in the receiver's method.
	 Answer the (possibly modified) sequence of temp vars.
	 Need to hoist temps out of macro-optimized blocks into their actual blocks.
	 Need to note reads and writes to temps from blocks other than their actual blocks to determine
	 whether blocks can be local (simple slots within a block/method context) or remote (slots in
	 indirection vectors that are shared between contexts by sharing indirection vectors).

	 The algorithm is based on numbering temporary reads and writes and block extents.
	 The index used for numbering starts at zero and is incremented on every block entry
	 and block exit.  So the following
		| a b blk r1 r2 t |
		a := 1. b := 2. t := 0.
		blk := [ | s | s := a + b. t := t + s].
		r1 := blk value.
		b := -100.
		r2 := blk value.
		r1 -> r2 -> t
	is numbered as
		method block 0 to: 6:
		| a b blk r1 r2 t |
		a w@1 := 1. b w@1 := 2. t w@1 := 0.
		blk w@5 := [entry@2 | s |
					 t  w@3 := t r@3 + a r@3 + b r@3
					] exit@4.
		r1 w@5 := blk r@5 value.
		b w@5 := nil.
		r2 w@5 := blk r@5 value.
		r1 r@5 -> r2 r@5 -> t r@5
	So:
		b and blk cannot be copied because for both there exists a write @5 that follows a
			read @4 within block 2 through 4
		t must be remote because there exists a write @3 within block (2 to: 4)
	Complications are introduced by optimized blocks.  In the following temp is written to
	after it is closed over by [ temp ] since the inlined block is executed more than once.
		| temp coll |
		coll := OrderedCollection new.
		1 to: 5 do: [ :index | 
			temp := index. 
			coll add: [ temp ] ].
		self assert: (coll collect: [:ea| ea value]) asArray = #(5 5 5 5 5)
	In the following i is local to the block and must be initialized each time around the loop
	but if the block is inlined it must be declared at method level.
		| col |
		col := OrderedCollection new.
		1 to: 3 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
		self assert: (col collect: [ :each | each value ]) asArray = #(2 3 4)"
	self assert: (arguments isEmpty or: [arguments hasEqualElements: methodArguments]).
	arguments := methodArguments asArray. "won't change"
	self assert: (temporaries isNil or: [temporaries isEmpty or: [temporaries hasEqualElements: methodTemporaries]]).
	temporaries := OrderedCollection withAll: methodTemporaries.

	self assert: optimized not. "the top-level block should not be optimized."
	self analyseTempsWithin: self rootNode: rootNode assignmentPools: Dictionary new.

	"The top-level block needs to reindex temporaries since analysis may have rearranged them.
	 This happens when temps are made remote and/or a remote node is added."
	temporaries withIndexDo:
		[:temp :offsetPlusOne| temp index: arguments size + offsetPlusOne - 1].

	"Answer the (possibly modified) sequence of temps."
	^temporaries asArray! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 9/6/2009 19:23'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	| effectiveScope blockStart |
	effectiveScope := optimized
						ifTrue: [actualScopeIfOptimized := scopeBlock]
						ifFalse: [self].

	arguments ifNotNil:
		[arguments do: [:temp| temp definingScope: self]].
	temporaries ifNotNil:
		[temporaries do: [:temp| temp definingScope: self]].

	optimized ifFalse: "if optimized this isn't an actual scope"
		[rootNode noteBlockEntry:
			[:entryNumber|
			 blockExtent := (blockStart := entryNumber) to: 0]].

	"Need to enumerate a copy because closure analysis can add a statement
	 via ifHasRemoteTempNodeEnsureInitializationStatementExists:."
	statements copy do:
		[:statement|
		 statement analyseTempsWithin: effectiveScope rootNode: rootNode assignmentPools: assignmentPools].

	optimized
		ifTrue: "if optimized loop need to add nils for any temps read before written"
			[optimizedMessageNode isOptimizedLoop ifTrue:
				[self nilReadBeforeWrittenTemps]]
		ifFalse: "if optimized this isn't an actual scope"
			[rootNode noteBlockExit:
				[:exitNumber|
				 blockExtent := blockStart to: exitNumber]].

	"Now that the analysis is done move any temps that need to be moved."
	self postNumberingProcessTempsWithin: effectiveScope rootNode: rootNode.

	"This is simply a nicety for compiler developers..."
	temporaries do:
		[:temp|
		(temp isIndirectTempVector and: [temp name includes: $?]) ifTrue:
			[temp name: temp definingScope remoteTempNodeName]]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 12:16'!
blockExtent "^<Interval>"
	^blockExtent! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'nice 8/19/2010 16:31'!
computeCopiedValues: rootNode
	| referencedValues |
	referencedValues := rootNode referencedValuesWithinBlockExtent: blockExtent.
	^(referencedValues reject: [:temp| temp isDefinedWithinBlockExtent: blockExtent])
		asArray sort: ParseNode tempSortBlock! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2008 14:10'!
constructClosureCreationNode: encoder
	copiedValues := self computeCopiedValues: encoder rootNode.
	encoder supportsClosureOpcodes ifTrue:
		[^self closureCreationNode].
	"Without the bytecode we can still get by."
	^MessageNode new
		receiver: (encoder encodeVariable: 'thisContext')
		selector: #closureCopy:copiedValues:
		arguments: (Array
						with: (encoder encodeLiteral: arguments size)
						with: (copiedValues isEmpty
								ifTrue: [NodeNil]
								ifFalse: [BraceNode new elements: copiedValues]))
		precedence: 3
		from: encoder! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 2/3/2011 09:19'!
deoptimize
	optimized := false.
	optimizedMessageNode := nil! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2008 14:10'!
emitCodeForClosureValue: stack encoder: encoder
	"if not supportsClosureOpcodes closureCreationSupportNode is the
	 node for thisContext closureCopy: numArgs [ copiedValues: { values } ]"
	encoder supportsClosureOpcodes
		ifTrue:
			[copiedValues do:
				[:copiedValue| copiedValue emitCodeForValue: stack encoder: encoder].
			 closureCreationNode pc: encoder methodStreamPosition + 1.
			 encoder
				genPushClosureCopyNumCopiedValues: copiedValues size
				numArgs: arguments size
				jumpSize: size.
			 stack
				pop: copiedValues size;
				push: 1]
		ifFalse:
			[closureCreationNode emitCodeForValue: stack encoder: encoder.
			 encoder genJumpLong: size]. "Force a two byte jump."
	"Emit the body of the block"
	self emitCodeForEvaluatedClosureValue: stack encoder: encoder! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/8/2015 17:06'!
emitCodeForEvaluatedClosureValue: stack encoder: encoder
	| position |
	position := stack position.
	stack position: arguments size + temporaries size + copiedValues size.
	encoder genPushNClosureTemps: temporaries size.
	self
		reindexingLocalsDo: [self emitCodeForEvaluatedValue: stack encoder: encoder]
		encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		 pc := encoder methodStreamPosition].
	stack position: position! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2009 18:56'!
ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode
	"If a remoteTempNode has been added ensure a statement exists to initialize it."
	remoteTempNode ~~ nil ifTrue:
		[(statements notEmpty
		  and: [statements first isAssignmentNode
		  and: [statements first variable isTemp
		  and: [statements first variable isIndirectTempVector]]])
			ifTrue: "If this is a decompiled tree, or if a temporary has been added later in
					the analysis then there already is a temp vector initialization node."
				[(statements first variable ~~ remoteTempNode) ifTrue:
					[statements first variable become: remoteTempNode].
				 statements first value numElements: remoteTempNode remoteTemps size]
			ifFalse:
				[statements addFirst: (remoteTempNode nodeToInitialize: rootNode encoder)]].! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'nice 3/1/2010 23:01'!
makeTemporariesRemovable
	"Utilities for when we want to remove some temporaries."
	
	temporaries isArray ifTrue:
		[temporaries := temporaries asOrderedCollection].! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 9/6/2009 13:36'!
nilReadBeforeWrittenTemps
	| visitor readBeforeWritten |
	self accept: (visitor := OptimizedBlockLocalTempReadBeforeWrittenVisitor new).
	readBeforeWritten := visitor readBeforeWritten.
	temporaries reverseDo:
		[:temp|
		((readBeforeWritten includes: temp)
		 and: [temp isRemote not]) ifTrue:
			[statements addFirst: (AssignmentNode new variable: temp value: NodeNil)]]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 9/6/2009 13:30'!
noteOptimizedIn: anOptimizedMessageNode
	optimized := true.
	optimizedMessageNode := anOptimizedMessageNode! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/20/2009 09:52'!
optimizedBlockHoistTempsInto: scopeBlock "<BlockNode>"
	"This is a No-op for all nodes except non-optimized BlockNodes."
	"Let's assume the special > 0 guard in MessageNode>>analyseTempsWithin:forValue:encoder: is correct.
	 Then we can simply hoist our temps up."
	self assert: (arguments isNil or: [arguments size <= 1]).
	(arguments notNil and: [arguments notEmpty]) ifTrue:
		[scopeBlock addHoistedTemps: arguments.
		arguments := #()].
	temporaries notEmpty ifTrue:
		[scopeBlock addHoistedTemps: temporaries.
		temporaries := #()]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2009 16:23'!
postNumberingProcessTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>"
	"A temp can be local (and copied) if it is not written to after it is captured.
	 A temp cannot be local if it is written to remotely.
	 Need to enumerate a copy of the temporaries because any temps becoming remote
	 will be removed from temporaries in analyseClosure: (and a single remote temp node
	 will get added)"
	temporaries copy do:
		[:each|
		each isIndirectTempVector ifFalse:
			[each analyseClosure: rootNode]].

	"If this is an optimized node we need to hoist temporaries up into the relevant block scope."
	optimized ifTrue:
		[self optimizedBlockHoistTempsInto: scopeBlock].

	"Now we may have added a remoteTempNode.  So we need a statement to initialize it."
	self ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode.

	"Now add all arguments and locals to the pool so that copiedValues can be computed during sizing."
	rootNode
		addLocalsToPool: arguments;
		addLocalsToPool: temporaries! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 6/2/2008 16:37'!
reindexingLocalsDo: aBlock encoder: encoderOrNil
	"Evaluate aBlock wih arguments, temporaries and copiedValues reindexed for
	 their positions within the receiver's block, restoring the correct indices afterwards.
	 If encoder is not nil remember the temps for this block's extent."
	| tempIndices result tempsToReindex |
	self assert: copiedValues notNil.
	tempsToReindex := arguments asArray, copiedValues, temporaries.
	tempIndices := tempsToReindex collect: [:temp| temp index].
	tempsToReindex withIndexDo:
		[:temp :newIndex| temp index: newIndex - 1. self assert: temp index + 1 = newIndex].
	encoderOrNil ifNotNil:
		[encoderOrNil noteBlockExtent: blockExtent hasLocals: tempsToReindex].
	result := aBlock ensure:
				["Horribly pragmatic hack.  The copiedValues will have completely
				  unrelated indices within the closure method and sub-method.
				  Avoiding the effort of rebinding temps in the inner scope simply
				  update the indices to their correct ones during the generation of
				  the closure method and restore the indices immedately there-after."
				 tempsToReindex with: tempIndices do:
					[:temp :oldIndex| temp index: oldIndex. self assert: temp index = oldIndex]].
	^result! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/22/2009 10:48'!
remoteTempNodeName
	"Answer a useful name for a RemoteTempVectorNode in the receiver."
	| prefix scope extent |
	prefix := actualScopeIfOptimized ifNil: ['<'] ifNotNil: [ '<...'].
	scope := self.
	[extent := scope blockExtent.
	 extent == nil
	 and: [scope actualScope ~~ scope]] whileTrue:
		[scope := scope actualScope].
	^extent
		ifNil: [prefix, '?-?>']
		ifNotNil:
			[prefix, extent first printString, '-',
				(extent last isZero
					ifTrue: ['?']
					ifFalse: [extent last printString]), '>']! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'nice 3/1/2010 23:13'!
removeTempNode: aTempVariableNode ifAbsent: aBlock
	"Utilities for when we want to remove some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries remove: aTempVariableNode ifAbsent: aBlock
	! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2008 14:11'!
sizeCodeForClosureValue: encoder
	"Compute the size for the creation of the block and its code."
	"If we have the closure bytecodes constructClosureCreationNode: will note
	 the copied values in the copiedValues inst var and answer #pushCopiedValues."
	closureCreationNode := self constructClosureCreationNode: encoder.
	"Remember size of body for emit time so we know the size of the jump around it."
	size := self sizeCodeForEvaluatedClosureValue: encoder.
	^encoder supportsClosureOpcodes
		ifTrue:
			[(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
			 + (encoder sizePushClosureCopyNumCopiedValues: copiedValues size numArgs: arguments size jumpSize: size)
			 + size]
		ifFalse:
			["closureCreationSupportNode is send closureCopy:copiedValues:"
			(closureCreationNode sizeCodeForValue: encoder)
			 + (encoder sizeJumpLong: size)
			 + size]! !

!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 5/29/2015 15:28'!
sizeCodeForEvaluatedClosureValue: encoder
	"The closure value primitives push the arguments and the copied values.
	 The compiler guarantees that any copied values come before all local temps.
	 So on closure activation we only need to push nils for the remaining temporaries."
	^(encoder sizePushNClosureTemps: temporaries size)
	+ (self
		reindexingLocalsDo: [self sizeCodeForEvaluatedValue: encoder]
		encoder: nil "don't store temps yet")
	+ (self returns ifTrue: [0] ifFalse: [encoder sizeReturnTopToCaller])! !

!BlockNode methodsFor: 'converting' stamp: 'eem 9/27/2011 15:23'!
ensureNotQuick: encoder
	"If the receiver is quick (can be generated as a Quick method, ^self,^inst var or ^ special constant)
	 make it not so.  This is used to create break-pointable versions of quick methods.   Answer if the
	 receiver was quick."
	self isQuick ifFalse:
		[^false].
	"Making statements size > 1 is sufficient to cause isQuick to answer false (see BlockNode>>isQuick).
	 N.B. This is a no-op since statements generate via emitCodeForEffect: and VariableNodes have no effect."
	statements addFirst: (encoder encodeVariable: 'self').
	^true! !


!BlockNode class methodsFor: 'instance creation' stamp: 'sma 3/3/2000 13:34'!
statements: statements returns: returns
	^ self new statements: statements returns: returns! !

!BlockNode class methodsFor: 'instance creation' stamp: 'eem 5/19/2008 17:10'!
withJust: aNode
	^ self new statements: (Array with: aNode) returns: false! !


!BraceNode methodsFor: 'code generation' stamp: 'eem 5/30/2008 17:40'!
emitCodeForValue: stack encoder: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[elements do: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder! !

!BraceNode methodsFor: 'code generation' stamp: 'di 1/4/2000 11:24'!
selectorForShortForm: nElements

	nElements > 4 ifTrue: [^ nil].
	^ #(braceWithNone braceWith: braceWith:with:
			braceWith:with:with: braceWith:with:with:with:) at: nElements + 1! !

!BraceNode methodsFor: 'code generation' stamp: 'eem 5/30/2008 17:22'!
sizeCodeForValue: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				MessageNode new
					receiver: (encoder encodeVariable: #Array)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				CascadeNode new
					receiver: (MessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | MessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (MessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder! !

!BraceNode methodsFor: 'enumerating'!
casesForwardDo: aBlock
	"For each case in forward order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	1 to: (numCases := elements size) do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]! !

!BraceNode methodsFor: 'enumerating'!
casesReverseDo: aBlock
	"For each case in reverse order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	(numCases := elements size) to: 1 by: -1 do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]! !

!BraceNode methodsFor: 'initialize-release'!
elements: collection
	"Decompile."

	elements := collection! !

!BraceNode methodsFor: 'initialize-release'!
elements: collection sourceLocations: locations
	"Compile."

	elements := collection.
	sourceLocations := locations! !

!BraceNode methodsFor: 'initialize-release' stamp: 'di 11/19/1999 11:06'!
matchBraceStreamReceiver: receiver messages: messages

	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])
		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])
		ifFalse: [^ nil "no match"].

	"Appears to be a long form brace construct"
	self elements: (messages allButLast collect:
		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)
					ifFalse: [^ nil "not a brace element"].
		msg arguments first])! !

!BraceNode methodsFor: 'initialize-release' stamp: 'di 11/19/1999 11:19'!
matchBraceWithReceiver: receiver selector: selector arguments: arguments

	selector = (self selectorForShortForm: arguments size)
		ifFalse: [^ nil "no match"].

	"Appears to be a short form brace construct"
	self elements: arguments! !

!BraceNode methodsFor: 'printing' stamp: 'di 11/19/1999 09:17'!
printOn: aStream indent: level

	aStream nextPut: ${.
	1 to: elements size do: 
		[:i | (elements at: i) printOn: aStream indent: level.
		i < elements size ifTrue: [aStream nextPutAll: '. ']].
	aStream nextPut: $}! !

!BraceNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level

	aStream nextPut: ${.
	1 to: elements size do: 
		[:i | (elements at: i) printWithClosureAnalysisOn: aStream indent: level.
		i < elements size ifTrue: [aStream nextPutAll: '. ']].
	aStream nextPut: $}! !

!BraceNode methodsFor: 'testing' stamp: 'eem 9/25/2008 14:48'!
blockAssociationCheck: encoder
	"If all elements are MessageNodes of the form [block]->[block], and there is at
	 least one element, answer true.
	 Otherwise, notify encoder of an error."

	elements size = 0
		ifTrue: [^encoder notify: 'At least one case required'].
	elements with: sourceLocations do:
			[:x :loc |
			(x 	isMessage: #->
				receiver:
					[:rcvr |
					rcvr isBlockNode and: [rcvr numberOfArguments = 0]]
				arguments:
					[:arg |
					arg isBlockNode and: [arg numberOfArguments = 0]])
			  ifFalse:
				[^encoder notify: 'Association between 0-argument blocks required' at: loc]].
	^true! !

!BraceNode methodsFor: 'testing'!
numElements

	^ elements size! !

!BraceNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitBraceNode: self! !

!BraceNode methodsFor: 'code generation (closures)' stamp: 'eem 7/20/2009 09:33'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	elements do:
		[:node|
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]! !

!BraceNode methodsFor: 'code generation (closures)' stamp: 'eem 5/21/2008 10:40'!
elements
	^elements! !

!BraceNode methodsFor: 'code generation (closures)' stamp: 'eem 5/30/2008 17:22'!
maxElementsForConsArray
	"Hack; we have no way of knowing how much stack space is available during sizing"
	^8! !


!BraceNode class methodsFor: 'examples' stamp: 'di 11/19/1999 09:05'!
example
	"Test the {a. b. c} syntax."

	| x |
	x := {1. {2. 3}. 4}.
	^ {x first. x second first. x second last. x last. 5} as: Set

"BraceNode example Set (0 1 2 3 4 5 )"
! !


!CascadeNode methodsFor: 'accessing' stamp: 'eem 9/10/2008 15:15'!
messages
	^messages! !

!CascadeNode methodsFor: 'accessing' stamp: 'tk 10/22/2000 16:55'!
receiver
	^receiver! !

!CascadeNode methodsFor: 'code generation' stamp: 'eem 5/15/2008 09:41'!
emitCodeForValue: stack encoder: encoder
	receiver emitCodeForValue: stack encoder: encoder.
	1 to: messages size - 1 do: 
		[:i | 
		encoder genDup.
		stack push: 1.
		(messages at: i) emitCodeForValue: stack encoder: encoder.
		encoder genPop.
		stack pop: 1].
	messages last emitCodeForValue: stack encoder: encoder! !

!CascadeNode methodsFor: 'code generation' stamp: 'eem 5/15/2008 09:39'!
sizeCodeForValue: encoder
	| size |
	size := (receiver sizeCodeForValue: encoder)
			 + (messages size - 1 * (encoder sizeDup + encoder sizePop)).
	messages do: [:aMessage | size := size + (aMessage sizeCodeForValue: encoder)].
	^size! !

!CascadeNode methodsFor: 'initialize-release'!
receiver: receivingObject messages: msgs
	" Transcript show: 'abc'; cr; show: 'def' "

	receiver := receivingObject.
	messages := msgs! !

!CascadeNode methodsFor: 'printing'!
printOn: aStream indent: level
	self printOn: aStream indent: level precedence: 0! !

!CascadeNode methodsFor: 'printing' stamp: 'di 4/25/2000 19:17'!
printOn: aStream indent: level precedence: p 

	p > 0 ifTrue: [aStream nextPut: $(].
	messages first printReceiver: receiver on: aStream indent: level.
	1 to: messages size do: 
		[:i | (messages at: i) printOn: aStream indent: level.
		i < messages size ifTrue: 
				[aStream nextPut: $;.
				messages first precedence >= 2 ifTrue: [aStream crtab: level + 1]]].
	p > 0 ifTrue: [aStream nextPut: $)]! !

!CascadeNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level
	self printWithClosureAnalysisOn: aStream indent: level precedence: 0! !

!CascadeNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level precedence: p 

	p > 0 ifTrue: [aStream nextPut: $(].
	messages first printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	1 to: messages size do: 
		[:i | (messages at: i) printWithClosureAnalysisOn: aStream indent: level.
		i < messages size ifTrue: 
				[aStream nextPut: $;.
				messages first precedence >= 2 ifTrue: [aStream crtab: level + 1]]].
	p > 0 ifTrue: [aStream nextPut: $)]! !

!CascadeNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitCascadeNode: self! !

!CascadeNode methodsFor: 'code generation (closures)' stamp: 'eem 5/19/2008 20:26'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	{ receiver }, messages do:
		[:node| node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]! !


!CommentNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitCommentNode: self! !


!DecompilerConstructor methodsFor: 'constructor'!
codeAnyLitInd: association

	^VariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType! !

!DecompilerConstructor methodsFor: 'constructor'!
codeAnyLiteral: value

	^LiteralNode new
		key: value
		index: 0
		type: LdLitType! !

!DecompilerConstructor methodsFor: 'constructor'!
codeAnySelector: selector

	^SelectorNode new
		key: selector
		index: 0
		type: SendType! !

!DecompilerConstructor methodsFor: 'constructor'!
codeArguments: args block: block

	^block arguments: args! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'eem 5/21/2008 13:28'!
codeArguments: args temps: temps block: block 
	block
		arguments: args;
		temporaries: temps.
	^block! !

!DecompilerConstructor methodsFor: 'constructor'!
codeAssignTo: variable value: expression

	^AssignmentNode new variable: variable value: expression! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'sma 3/3/2000 13:34'!
codeBlock: statements returns: returns
	^ BlockNode statements: statements returns: returns! !

!DecompilerConstructor methodsFor: 'constructor'!
codeBrace: elements

	^BraceNode new elements: elements! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'di 11/19/1999 11:06'!
codeCascade: receiver messages: messages

	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [CascadeNode new receiver: receiver messages: messages]! !

!DecompilerConstructor methodsFor: 'constructor'!
codeCascadedMessage: selector arguments: arguments

	^self
		codeMessage: nil
		selector: selector
		arguments: arguments! !

!DecompilerConstructor methodsFor: 'constructor'!
codeConstants
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'sma 3/3/2000 13:35'!
codeEmptyBlock
	^ BlockNode withJust: NodeNil! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'eem 8/21/2008 14:02'!
codeInst: index

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'eem 3/12/2013 13:30'!
codeMessage: receiver selector: selector arguments: arguments
	| symbol |
	symbol := selector key.
	(BraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	(self
		decodeLiteralVariableValueDereferenceWithReceiver: receiver
		selector: symbol
		arguments: arguments) ifNotNil: [:node| ^node].
	(self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	^MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'eem 9/23/2008 22:06'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| node methodTemps arguments temporaries |
	node := self codeSelector: selector code: nil.
	tempVars := vars.
	methodTemps := tempVars select: [:t | t scope >= 0].
	arguments := methodTemps copyFrom: 1 to: nArgs.
	temporaries := methodTemps copyFrom: nArgs + 1 to: methodTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	^MethodNode new
		selector: node
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (Encoder new initScopeAndLiteralTables
					temps: tempVars
					literals: literalValues
					class: class)
		primitive: primitive! !

!DecompilerConstructor methodsFor: 'constructor'!
codeSelector: sel code: code

	^SelectorNode new key: sel code: code! !

!DecompilerConstructor methodsFor: 'constructor'!
codeSuper

	^NodeSuper! !

!DecompilerConstructor methodsFor: 'constructor'!
codeTemp: index

	^ TempVariableNode new
		name: 't' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0! !

!DecompilerConstructor methodsFor: 'constructor'!
codeTemp: index named: tempName

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0! !

!DecompilerConstructor methodsFor: 'constructor'!
codeThisContext

	^NodeThisContext! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 3/3/2011 09:17'!
decodeIfNilWithReceiver: receiver selector: selector arguments: arguments
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
	^ (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3)
		noteSpecialSelector: #ifNil:ifNotNil:! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'eem 3/12/2013 13:30'!
decodeLiteralVariableValueDereferenceWithReceiver: receiver selector: selector arguments: arguments
	| varNode |
	(receiver notNil "cascades"
	 and: [receiver isLiteralNode
	 and: [receiver key isVariableBinding]]) ifFalse:
		[^nil].
	varNode := self codeAnyLitInd: receiver key.
	selector = #value ifTrue:
		[^varNode].
	^selector = #value: ifTrue:
		[self codeAssignTo: varNode value: arguments first]! !

!DecompilerConstructor methodsFor: 'initialize-release'!
method: aMethod class: aClass literals: literals

	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals! !

!DecompilerConstructor methodsFor: 'visiting' stamp: 'eem 5/30/2008 09:44'!
accept: aVisitor
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement! !

!DecompilerConstructor methodsFor: 'testing' stamp: 'eem 6/4/2008 14:41'!
isForClosures
	^false! !


!DecompilerConstructorForClosures methodsFor: 'constructor' stamp: 'eem 11/6/2012 18:51'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: []) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode! !

!DecompilerConstructorForClosures methodsFor: 'constructor' stamp: 'eem 10/20/2008 13:01'!
codeRemoteTemp: index remoteTemps: tempVector

	^(RemoteTempVectorNode new
		name: '_r', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself! !

!DecompilerConstructorForClosures methodsFor: 'testing' stamp: 'eem 6/4/2008 14:41'!
isForClosures
	^true! !


!Encoder methodsFor: 'encoding'!
cantStoreInto: varName

	^StdVariables includesKey: varName! !

!Encoder methodsFor: 'encoding' stamp: 'eem 9/5/2009 20:04'!
doItInContextName
	^'ThisContext'! !

!Encoder methodsFor: 'encoding' stamp: 'cwp 12/27/2012 11:39'!
encodeLiteral: object

	^self
		name: object
		key: (cue  literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet! !

!Encoder methodsFor: 'encoding' stamp: 'cwp 12/27/2012 11:40'!
encodeSelector: aSelector

	^self
		name: aSelector
		key: aSelector
		class: SelectorNode
		type: SendType
		set: selectorSet! !

!Encoder methodsFor: 'encoding' stamp: 'di 12/4/1999 20:09'!
encodeVariable: name
	^ self encodeVariable: name sourceRange: nil ifUnknown: [ self undeclared: name ]! !

!Encoder methodsFor: 'encoding' stamp: 'ls 1/19/2001 12:59'!
encodeVariable: name ifUnknown: action
	^self encodeVariable: name sourceRange: nil ifUnknown: action! !

!Encoder methodsFor: 'encoding' stamp: 'eem 5/16/2010 17:33'!
encodeVariable: name sourceRange: range ifUnknown: action
	| varNode |
	varNode := scopeTable
					at: name
					ifAbsent: 
						[(self lookupInPools: name 
							ifFound: [:assoc | varNode := self global: assoc name: name])
							ifTrue: [varNode]
							ifFalse: [^action value]].
	range ifNotNil:
		[name first canBeGlobalVarInitial ifTrue:
			[globalSourceRanges addLast: { name. range. false }]].

	(varNode isTemp and: [varNode scope < 0]) ifTrue:
		[^OutOfScopeNotification signal
			ifTrue: [action value]
			ifFalse: [self notify: 'out of scope']].
	^varNode! !

!Encoder methodsFor: 'encoding' stamp: 'cwp 12/27/2012 11:40'!
environment
	"Answer the environment of the current compilation context,
	 be it in a class or global (e.g. a workspace)"
	^cue environment! !

!Encoder methodsFor: 'encoding'!
litIndex: literal
	| p |
	p := literalStream position.
	p = 256 ifTrue:
		[self notify: 'More than 256 literals referenced. 
You must split or otherwise simplify this method.
The 257th literal is: ', literal printString. ^nil].
		"Would like to show where it is in the source code, 
		 but that info is hard to get."
	literalStream nextPut: literal.
	^ p! !

!Encoder methodsFor: 'encoding' stamp: 'eem 8/15/2010 08:29'!
lookupVariable: name ifAbsent: aBlock
	"Answer the binding of name in the scope table or aBlock's vaue if none.
	 Do not bind and do not lookup in pools.  Used for correction, explanation etc"
	^scopeTable at: name ifAbsent: aBlock! !

!Encoder methodsFor: 'encoding' stamp: 'nice 3/30/2011 23:26'!
noteOptimizedSelector: aSymbol
	"Register a selector as being optimized.
	These optimized selectors will later be registered into the literals so that tools can easily browse senders."
	optimizedSelectors add: aSymbol! !

!Encoder methodsFor: 'encoding' stamp: 'ul 10/24/2010 06:34'!
sharableLitIndex: literal
	"Special access prevents multiple entries for post-allocated super send special selectors"
	1 to: literalStream position do:
		[:index|
		(literal literalEqual: (literalStream originalContents at: index)) ifTrue:
			[^index - 1]].
	^self litIndex: literal! !

!Encoder methodsFor: 'encoding' stamp: 'cmm 10/3/2013 12:50'!
undeclared: name 
	| sym |
	requestor interactive ifTrue:
		[ requestor requestor == #error: ifTrue: [ requestor error: 'Undeclared' ].
		^ self notify: 'Undeclared' ].
	"Allow knowlegeable clients to squash the undeclared warning if they want (e.g.
	 Diffing pretty printers that are simply formatting text).  As this breaks
	 compilation it should only be used by clients that want to discard the result
	 of the compilation.  To squash the warning use e.g.
		[Compiler format: code in: class notifying: nil decorated: false]
			on: UndeclaredVariableWarning
			do: [:ex| ex resume: false]"
	sym := name asSymbol.
	^ (UndeclaredVariableWarning new
		name: name
		selector: selector
		class: cue getClass) signal
		ifTrue:
			[ | undeclared |
			undeclared := cue environment undeclared.
			[ undeclared
				at: sym
				put: nil ]
				on: AttemptToWriteReadOnlyGlobal
				do: [ : noti | noti resume: true ].
			self
				global: (undeclared associationAt: sym)
				name: sym ]
		ifFalse:
			[ self
				global: (Association key: sym)
				name: sym ]! !

!Encoder methodsFor: 'error handling'!
notify: string
	"Put a separate notifier on top of the requestor's window"
	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false! !

!Encoder methodsFor: 'error handling'!
notify: string at: location

	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string at: location].
	^false! !

!Encoder methodsFor: 'initialize-release'!
fillDict: dict with: nodeClass mapping: keys to: codeArray
	| codeStream |
	codeStream := ReadStream on: codeArray.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]! !

!Encoder methodsFor: 'initialize-release' stamp: 'eem 10/6/2014 11:51'!
init: aCue notifying: anObject
	"The use of the variable requestor is a bit confusing here. This is
	*not* the original requestor, which is available through the cue.
	It's the Parser instance that is using the encoder."

	self setCue: aCue.
	requestor := anObject.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	cue getClass variablesAndOffsetsDo:
		[:variable "<String|CFieldDefinition>" :offset "<Integer|nil>" |
		offset isNil
			ifTrue: [scopeTable at: variable name put: (FieldNode new fieldDefinition: variable)]
			ifFalse: [scopeTable
						at: variable
						put: (offset >= 0
								ifTrue: [InstanceVariableNode new
											name: variable index: offset]
								ifFalse: [MaybeContextInstanceVariableNode new
											name: variable index: offset negated])]].
	cue context ~~ nil ifTrue:
		[| homeNode |
		 homeNode := self bindTemp: self doItInContextName.
		 "0th temp = aContext passed as arg"
		 cue context tempNames withIndexDo:
			[:variable :index|
			variable ~= self doItInContextName ifTrue:
				[scopeTable
					at: variable
					put: (MessageAsTempNode new
							receiver: homeNode
							selector: #namedTempAt:
							arguments: (Array with: (self encodeLiteral: index))
							precedence: 3
							from: self)]]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32
! !

!Encoder methodsFor: 'initialize-release' stamp: 'nice 3/30/2011 23:04'!
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
	litIndSet := Dictionary new: 16.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new! !

!Encoder methodsFor: 'initialize-release'!
noteSuper

	supered := true! !

!Encoder methodsFor: 'initialize-release'!
release

	requestor := nil! !

!Encoder methodsFor: 'initialize-release' stamp: 'cwp 12/26/2012 23:58'!
temps: tempVars literals: lits class: cl 
	"Initialize this encoder for decompilation."

	self setCue: (CompilationCue class: cl).
	supered := false.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	literalStream := WriteStream on: (Array new: lits size).
	literalStream nextPutAll: lits.
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32.! !

!Encoder methodsFor: 'results' stamp: 'nice 3/31/2011 00:35'!
allLiterals
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 > 255
				ifTrue: [self litIndex: optimizedSelectors asArray]
				ifFalse: [optimizedSelectors do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents! !

!Encoder methodsFor: 'results' stamp: 'cwp 12/27/2012 10:26'!
associationForClass
	| assoc |
	assoc := self environment associationAt: cue getClass name ifAbsent: [nil].
	^assoc value == cue getClass
		ifTrue: [assoc]
		ifFalse: [Association new value: cue getClass]! !

!Encoder methodsFor: 'results'!
literals
	"Should only be used for decompiling primitives"
	^ literalStream contents! !

!Encoder methodsFor: 'results' stamp: 'di 10/12/1999 16:12'!
tempNames 

	^ self tempNodes collect:
		[:node | (node isMemberOf: MessageAsTempNode)
					ifTrue: [scopeTable keyAtValue: node]
					ifFalse: [node key]]! !

!Encoder methodsFor: 'results' stamp: 'eem 5/27/2008 12:07'!
tempNodes 
	| tempNodes |
	tempNodes := SortedCollection sortBlock: [:n1 :n2 | n1 code <= n2 code].
	scopeTable associationsDo:
		[:assn |
		assn value isArray
			ifTrue: [assn value do: [:temp| tempNodes add: temp]]
			ifFalse: [assn value isTemp ifTrue: [tempNodes add: assn value]]].
	^tempNodes! !

!Encoder methodsFor: 'results' stamp: 'eem 9/8/2008 18:27'!
tempsAndBlockArgs
	| tempNodes |
	tempNodes := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | var |
		var := assn value.
		(var isTemp
		 and: [var isMethodArg not
		 and: [var scope = 0 or: [var scope = -1]]]) ifTrue:
			[tempNodes add: var]].
	^tempNodes! !

!Encoder methodsFor: 'results' stamp: 'eem 8/13/2010 16:51'!
undeclaredTemps 
	^(scopeTable select: [:var | var isVariableNode and: [var isUndeclared]]) values! !

!Encoder methodsFor: 'results' stamp: 'eem 6/24/2008 14:24'!
unusedTempNames 
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= self doItInContextName ifTrue: [unused add: name]]].
	^ unused! !

!Encoder methodsFor: 'source mapping' stamp: 'di 12/4/1999 22:27'!
globalSourceRanges

	^ globalSourceRanges! !

!Encoder methodsFor: 'source mapping'!
noteSourceRange: range forNode: node

	sourceRanges at: node put: range! !

!Encoder methodsFor: 'source mapping' stamp: 'RAA 8/21/1999 06:52'!
rawSourceRanges

	^ sourceRanges ! !

!Encoder methodsFor: 'source mapping'!
sourceMap
	"Answer with a sorted set of associations (pc range)."

	^ (sourceRanges keys collect: 
		[:key |  Association key: key pc value: (sourceRanges at: key)])
			asSortedCollection! !

!Encoder methodsFor: 'source mapping' stamp: 'ar 11/19/2002 14:41'!
sourceRangeFor: node

	^sourceRanges at: node! !

!Encoder methodsFor: 'temps' stamp: 'ar 9/9/2006 12:05'!
autoBind: name 
	"Declare a block argument as a temp if not already declared."
	| node |
	node := scopeTable 
			at: name
			ifAbsent: 
				[(self lookupInPools: name ifFound: [:assoc | assoc])
					ifTrue: [self warnAboutShadowed: name].
				^ (self reallyBind: name) nowHasDef nowHasRef scope: 1].
	node isTemp
		ifTrue: [node scope >= 0 ifTrue:
					[^ self notify: 'Name already used in this method'].
				node nowHasDef nowHasRef scope: 1]
		ifFalse: [^ self notify: 'Name already used in this class'].
	^node! !

!Encoder methodsFor: 'temps' stamp: 'di 10/12/1999 16:53'!
bindAndJuggle: name

	| node nodes first thisCode |
	node := self reallyBind: name.

	"Declared temps must precede block temps for decompiler and debugger to work right"
	nodes := self tempNodes.
	(first := nodes findFirst: [:n | n scope > 0]) > 0 ifTrue:
		[node == nodes last ifFalse: [self error: 'logic error'].
		thisCode := (nodes at: first) code.
		first to: nodes size - 1 do:
			[:i | (nodes at: i) key: (nodes at: i) key
							code: (nodes at: i+1) code].
		nodes last key: nodes last key code: thisCode].
	
	^ node! !

!Encoder methodsFor: 'temps' stamp: 'jm 9/18/97 21:06'!
bindArg: name 
	"Declare an argument."
	| node |
	nTemps >= 15
		ifTrue: [^self notify: 'Too many arguments'].
	node := self bindTemp: name.
	^ node nowHasDef nowHasRef! !

!Encoder methodsFor: 'temps' stamp: 'eem 5/30/2008 12:05'!
bindBlockArg: name within: aBlockNode
	"With standard Smalltalk-80 (BlueBook) blocks it used to be legal to use a
	 method temp as a block argument.  This shouldn't be the case with the
	 current compiler, which checks for temp names already being used as
	 block arguments.  But it is easily fooled by local block temps in optimized
	 blocks, e.g.
		false
			ifTrue: [| temp |]
			ifFalse:[[:temp|]]
	Rather than fix this we keep the semantics and fix it in the closure compiler."
	^self autoBind: name! !

!Encoder methodsFor: 'temps' stamp: 'crl 2/26/1999 12:18'!
bindBlockTemp: name 
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |

	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^ self notify: 'Name already used in this method'].
			node scope: 0]
		ifFalse: [^self notify: 'Name already used in this class'].
	^node
! !

!Encoder methodsFor: 'temps' stamp: 'eem 5/30/2008 14:14'!
bindBlockTemp: name within: aBlockNode
	"The BlockContext compiler (the Smalltalk-80 BlueBook compiler)
	 does provide support for ANSI block syntax, but not for ANSI block
	 semantics.  Here all temps live at the same level, the method level.
	 The approach taken to two block-local temps in different blocks is to
	 merge them into a single temp.  e.g.
		expr
			ifTrue: [|temp| self statementOne]
			ifFalse: [|temp| self statementTwo]
	 is effectvely transformed into
		| temp |
		expr
			ifTrue: [self statementOne]
			ifFalse: [self statementTwo]
	 and
		expr do: [:each| | temp | ...].
		expr do: [:each| | temp | ...].
	 is also effectively transformed into
		| temp |
		expr do: [:each|  ...].
		expr do: [:each| ...].

	 The closure compiler treats the former similarly, but not the latter.
	 The indirection through #bindBlockTemp:within: allows the closure encoder to do this."
	^self bindBlockTemp: name! !

!Encoder methodsFor: 'temps' stamp: 'ar 9/9/2006 12:06'!
bindTemp: name 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		(node isTemp)
			ifTrue:[^self notify:'Name is already defined']
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name! !

!Encoder methodsFor: 'temps' stamp: 'cwp 12/27/2012 10:25'!
bindTemp: name in: methodSelector
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		(node isTemp or:[requestor interactive])
			ifTrue:[^self notify:'Name is already defined']
			ifFalse:[Transcript 
				show: '(', name, ' is shadowed in "' , cue getClass printString , '>>' , methodSelector printString , '")']].
	^self reallyBind: name! !

!Encoder methodsFor: 'temps' stamp: 'eem 12/1/2008 12:07'!
fixTemp: name
	| node |
	node := scopeTable at: name ifAbsent: [].
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node! !

!Encoder methodsFor: 'temps' stamp: 'eem 7/13/2007 14:13'!
floatTemp: node
	(node ~~ (scopeTable at: node name ifAbsent: [])
	or: [node class ~~ TempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1! !

!Encoder methodsFor: 'temps'!
maxTemp

	^nTemps! !

!Encoder methodsFor: 'temps'!
newTemp: name

	nTemps := nTemps + 1.
	^ TempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0! !

!Encoder methodsFor: 'temps' stamp: 'eem 8/14/2010 19:35'!
newUndeclaredTemp: name
	^UndeclaredVariableNode new name: name! !

!Encoder methodsFor: 'private' stamp: 'eem 8/13/2010 16:06'!
bindUndeclaredTemp: name
	^scopeTable at: name put: (self newUndeclaredTemp: name)! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/27/2012 10:25'!
classEncoding
	"This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view."
	^ cue getClass! !

!Encoder methodsFor: 'private' stamp: 'ar 8/14/2001 23:12'!
global: ref name: name

	^self
		name: name
		key: ref
		class: LiteralVariableNode
		type: LdLitIndType
		set: litIndSet! !

!Encoder methodsFor: 'private' stamp: 'ar 3/4/2010 23:39'!
interactive
	"Answer true if compilation is interactive"

	^requestor interactive! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/27/2012 11:41'!
lookupInPools: varName ifFound: assocBlock

	^Symbol
		hasInterned: varName
		ifTrue:
			[:sym|
			(cue bindingOf: sym)
				ifNil: [^false]
				ifNotNil: [:assoc| assocBlock value: assoc]]! !

!Encoder methodsFor: 'private' stamp: 'Lukas Renggli 11/2/2009 00:29'!
name: name key: key class: leafNodeClass type: type set: dict 
	^dict at: key
		ifAbsentPut: 
			[leafNodeClass new 
				name: name
				key: key
				index: nil
				type: type]! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/27/2012 10:24'!
possibleNamesFor: proposedName
	| results |
	results := cue getClass 
		possibleVariablesFor: proposedName 
		continuedFrom: nil.
	^ proposedName correctAgainst: nil continuedFrom: results.
! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/27/2012 10:24'!
possibleVariablesFor: proposedVariable

	| results |
	results := proposedVariable correctAgainstDictionary: scopeTable
								continuedFrom: nil.
	proposedVariable first canBeGlobalVarInitial ifTrue:
		[ results := cue getClass possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results.
! !

!Encoder methodsFor: 'private'!
reallyBind: name

	| node |
	node := self newTemp: name.
	scopeTable at: name put: node.
	^node! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/26/2012 23:30'!
setCue: aCue
	cue := aCue.
	
	"Also set legacy instance variables for methods that
	don't use cue yet"
	class := cue getClass.! !

!Encoder methodsFor: 'private' stamp: 'cwp 12/27/2012 10:23'!
warnAboutShadowed: name
	requestor addWarning: name,' is shadowed'.
	selector ifNotNil:
		[Transcript cr; show: cue getClass name,'>>', selector, '(', name,' is shadowed)']! !

!Encoder methodsFor: 'visiting' stamp: 'eem 5/30/2008 09:44'!
accept: aVisitor
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement! !

!Encoder methodsFor: 'accessing' stamp: 'eem 5/29/2008 09:36'!
methodNodeClass
	^MethodNode! !

!Encoder methodsFor: 'accessing' stamp: 'ar 9/9/2006 12:06'!
selector
	^selector! !

!Encoder methodsFor: 'accessing' stamp: 'ar 9/9/2006 12:06'!
selector: aSymbol
	selector := aSymbol! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 6/23/2008 10:55'!
bindAndJuggle: name
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 9/8/2008 18:24'!
bindBlockArg: name within: aBlockNode
	"Read the comment in the superclass's method.
	 If we have closures we should check the argument
	 count against the block, not the method.

	(Note that this isn't entirely adequate either since optimized blocks
	 will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|)
	 are charged against their enclosing block, not themselves))."
	| nArgs |
	self supportsClosureOpcodes ifFalse:
		[^super bindBlockArg: name within: aBlockNode].
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs  >= 15 ifTrue:
		[^self notify: 'Too many arguments'].
	aBlockNode nArgsSlot: nArgs + 1.
	^(self bindTemp: name)
		beBlockArg;
		nowHasDef;
		nowHasRef;
		yourself! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 8/18/2011 17:44'!
bindBlockTemp: name
	"This shouldn't be used with BytecodeEncoder.  Use bindBlockTemp:within: instead."
	self shouldNotImplement! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 5/30/2008 14:35'!
bindBlockTemp: name within: aBlockNode
	"Read the comment in the superclass's bindBlockArg:within: method.
	 If we have closures we should check the argument
	 count against the block, not the method.

	(Note that this isn't entirely adequate either since optimized blocks
	 will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|)
	 are charged against their enclosing block, not themselves))."
	| nArgs |
	self supportsClosureOpcodes ifFalse:
		[^super bindBlockTemp: name within: aBlockNode].
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= (CompiledMethod fullFrameSize - 1) ifTrue:
		[^self notify: 'Too many temporaries'].
	aBlockNode nArgsSlot: nArgs + 1.
	^self bindTemp: name! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 7/18/2008 07:33'!
bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp.
	 Read the comment in Encoder>>bindBlockArg:within: and subclass implementations."
	self supportsClosureOpcodes ifFalse:
		[^super bindTemp: name].
	scopeTable at: name ifPresent:
		[:node|
		"When non-interactive raise the error only if it is a duplicate"
		node isTemp
			ifTrue:[node scope >= 0 ifTrue:
						[^self notify:'Name is already defined']]
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'nice 12/29/2010 14:41'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."
	| blockExtentsToTempsMap |
	blockExtentsToLocals ifNil:
		[^nil].
	blockExtentsToTempsMap := Dictionary new.
	blockExtentsToLocals keysAndValuesDo:
		[:blockExtent :locals|
		blockExtentsToTempsMap
			at: blockExtent
			put: (Array streamContents:
					[:aStream|
					locals withIndexDo:
						[:local :index|
						local isIndirectTempVector
							ifTrue: [local remoteTemps withIndexDo:
										[:remoteLocal :innerIndex| aStream nextPut: { remoteLocal key. { index. innerIndex } }]]
							ifFalse: [aStream nextPut: { local key. index }]]])].
	^blockExtentsToTempsMap! !

!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 6/3/2008 12:33'!
noteBlockExtent: blockExtent hasLocals: tempNodes
	blockExtentsToLocals ifNil:
		[blockExtentsToLocals := Dictionary new].
	blockExtentsToLocals at: blockExtent put: tempNodes asArray! !

!BytecodeEncoder methodsFor: 'special literal encodings' stamp: 'eem 5/14/2008 16:02'!
if: code isSpecialLiteralForPush: aBlock
	"If code is that of a special literal for push then evaluate aBlock with the special literal
	 The special literals for push are nil true false -1 0 1 & 2 which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil + 4)
	    and: [aBlock value: (#(true false nil -1 0 1 2) at: code - LdSelf).
			true]! !

!BytecodeEncoder methodsFor: 'special literal encodings' stamp: 'eem 5/14/2008 17:49'!
if: code isSpecialLiteralForReturn: aBlock
	"If code is that of a special literal for return then evaluate aBlock with the special literal.
	 The special literals for return are nil true false which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil)
	   and: [aBlock value: (#(true false nil) at: code - LdSelf).
			true]! !

!BytecodeEncoder methodsFor: 'accessing' stamp: 'eem 5/14/2008 17:47'!
methodStreamPosition
	^stream position! !

!BytecodeEncoder methodsFor: 'accessing' stamp: 'eem 5/24/2008 11:56'!
rootNode "^<BlockNode>"
	^rootNode! !

!BytecodeEncoder methodsFor: 'accessing' stamp: 'eem 5/24/2008 11:56'!
rootNode: node "<BlockNode>"
	rootNode := node! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 7/27/2008 00:39'!
nextPut: aByte
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 18:22'!
sizeBranchPopFalse: distance
	^self sizeOpcodeSelector: #genBranchPopFalse: withArguments: {distance}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 18:22'!
sizeBranchPopTrue: distance
	^self sizeOpcodeSelector: #genBranchPopTrue: withArguments: {distance}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 10/23/2012 16:50'!
sizeCallPrimitive: primitiveIndex
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:28'!
sizeDup
	^self sizeOpcodeSelector: #genDup withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:40'!
sizeJump: distance
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:40'!
sizeJumpLong: distance
	^self sizeOpcodeSelector: #genJumpLong: withArguments: {distance}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 7/27/2008 00:39'!
sizeOpcodeSelector: genSelector withArguments: args
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:28'!
sizePop
	^self sizeOpcodeSelector: #genPop withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/30/2008 16:46'!
sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	^self
		sizeOpcodeSelector: #genPushClosureCopyNumCopiedValues:numArgs:jumpSize:
		withArguments: {numCopied. numArgs. jumpSize}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/30/2008 16:36'!
sizePushConsArray: numElements
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 16:22'!
sizePushInstVar: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 6/19/2008 08:54'!
sizePushInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:43'!
sizePushLiteral: literalIndex
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:43'!
sizePushLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'ClementBera 4/2/2015 15:37'!
sizePushNClosureTemps: numTemps
	^self sizeOpcodeSelector: #genPushNClosureTemps: withArguments: {numTemps}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/24/2008 12:35'!
sizePushNewArray: size
	^self sizeOpcodeSelector: #genPushNewArray: withArguments: {size}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 16:21'!
sizePushReceiver
	^self sizeOpcodeSelector: #genPushReceiver withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/24/2008 22:59'!
sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genPushRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:37'!
sizePushSpecialLiteral: specialLiteral
	^self sizeOpcodeSelector: #genPushSpecialLiteral: withArguments: {specialLiteral}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:26'!
sizePushTemp: tempIndex
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 4/3/2013 12:48'!
sizePushTempLong: tempIndex
	^self sizeOpcodeSelector: #genPushTempLong: withArguments: {tempIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:28'!
sizePushThisContext
	^self sizeOpcodeSelector: #genPushThisContext withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/15/2008 09:07'!
sizeReturnReceiver
	^self sizeOpcodeSelector: #genReturnReceiver withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:38'!
sizeReturnSpecialLiteral: specialLiteral
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:34'!
sizeReturnTop
	^self sizeOpcodeSelector: #genReturnTop withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/15/2008 09:06'!
sizeReturnTopToCaller
	^self sizeOpcodeSelector: #genReturnTopToCaller withArguments: #()! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 16:11'!
sizeSend: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 8/5/2014 18:29'!
sizeSendSpecial: specialSelectorIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSpecial:numArgs: withArguments: {specialSelectorIndex. nArgs}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 16:11'!
sizeSendSuper: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:44'!
sizeStoreInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 6/19/2008 08:54'!
sizeStoreInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:43'!
sizeStoreLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/15/2008 10:00'!
sizeStorePopInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 6/19/2008 08:54'!
sizeStorePopInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/15/2008 10:20'!
sizeStorePopLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genStorePopLiteralVar: withArguments: {literalIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/24/2008 23:02'!
sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:36'!
sizeStorePopTemp: tempIndex
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/24/2008 23:02'!
sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}! !

!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 5/14/2008 17:45'!
sizeStoreTemp: tempIndex
	^self sizeOpcodeSelector: #genStoreTemp: withArguments: {tempIndex}! !

!BytecodeEncoder methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 16:52'!
outOfRangeError: string index: index range: rangeStart to: rangeEnd
	"For now..."
	^self error: thisContext sender method selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString! !

!BytecodeEncoder methodsFor: 'initialize-release' stamp: 'eem 7/24/2008 17:24'!
streamToMethod: aCompiledMethod
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1! !

!BytecodeEncoder methodsFor: 'testing' stamp: 'eem 6/29/2009 11:11'!
hasGeneratedMethod
	^blockExtentsToLocals notNil! !

!BytecodeEncoder methodsFor: 'testing' stamp: 'eem 7/17/2008 12:34'!
supportsClosureOpcodes
	"Answer if the receiver supports the
		genPushNewArray:/genPushConsArray:
		genPushRemoteTemp:inVectorAt:
		genStoreRemoteTemp:inVectorAt:
		genStorePopRemoteTemp:inVectorAt:
		genPushClosureCopyCopiedValues:numArgs:jumpSize:
	 opcodes"
	^false! !

!BytecodeEncoder methodsFor: 'results' stamp: 'eem 6/5/2009 17:53'!
printSchematicTempNamesOn: aStream blockExtents: blockExtents fromIndex: startIndex
	"Print the locals in the blockExtent startIndex, recursing to print any locals in nested blockExtents.
	 Answer the index of the last blockExtent printed."
	| blockExtent subsequentIndex |
	blockExtent := blockExtents at: startIndex.
	blockExtent first > 0 ifTrue:
		[aStream nextPut: $[ ].
	((blockExtentsToLocals at: blockExtent) reject: [:local| local isRemote])
		do:	[:local|
			local isIndirectTempVector
				ifTrue:
					[aStream nextPut: $(.
					 local remoteTemps
						do: [:remoteLocal| aStream nextPutAll: remoteLocal key]
						separatedBy: [aStream space].
					 aStream nextPut: $)]
				ifFalse: [aStream nextPutAll: local key]]
		separatedBy: [aStream space].
	subsequentIndex := startIndex + 1.
	[subsequentIndex <= blockExtents size
	 and: [(blockExtents at: subsequentIndex) last < blockExtent last]] whileTrue:
		[subsequentIndex := self printSchematicTempNamesOn: aStream
								blockExtents: blockExtents
								fromIndex: subsequentIndex].
	blockExtent first > 0 ifTrue:
		[aStream nextPut: $] ].
	^subsequentIndex! !

!BytecodeEncoder methodsFor: 'results' stamp: 'nice 10/21/2009 00:02'!
schematicTempNamesString
	"Answer the temp names for the current method node in a form that captures
	 temp structure.  The temps at each method and block scope level occurr
	 space-separated, with any indirect temps enclosed in parentheses.  Each block
	 level is enclosed in square brackets.  e.g.
		'method level temps (indirect temp)[block args and temps (indirect)]'
	 This representation can be reconstituted into a blockExtentsToTempsMap
	 by a CompiledMethod that has been copied with teh schematicTempNamesString."
	blockExtentsToLocals ifNil:
		[self error: 'blockExtentsToLocals uninitialized.  method not yet generated?'].
	^String streamContents:
		[:aStream|
		self printSchematicTempNamesOn: aStream
			blockExtents: (blockExtentsToLocals keys asArray sort:
							[:range1 :range2|
							range1 first <= range2 first])
			fromIndex: 1]! !

!BytecodeEncoder methodsFor: 'method generation' stamp: 'eem 8/5/2014 21:33'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	"Compute the compiled method header that encodes the arguments
	 in the receiver's header format (see CompiledMehtod's class comment)."
	self subclassResponsibility! !

!BytecodeEncoder methodsFor: 'method generation' stamp: 'eem 8/5/2014 21:34'!
generateMethodOfClass: aCompiledMethodClass trailer: trailer from: methodNode
	"methodNode is the root of a parse tree. Answer an instance of aCompiledMethodClass
	 in the receiver's bytecode set and using the receiver's method header format.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	self subclassResponsibility! !


!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:55'!
bindingReadScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:35'!
bindingWriteScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:48'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:31'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:31'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 4/9/2015 16:46'!
markerOrNilFor: aMethod
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	^self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:51'!
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:51'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."
	
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 18:53'!
bytecodeSize: aByte
	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:39'!
extensionsAt: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:41'!
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct jump decoder for the instruction set."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch instruction interpretation through the encoder
	 to select the correct instruction set decoder."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:43'!
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:32'!
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:32'!
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends
	 the range of the following bytecode."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:31'!
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:18'!
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
isRealSendAt: pc in: method
	"Answer whether the bytecode at pc is a real message-send, not blockCopy:."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:28'!
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 9/2/2014 14:08'!
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:29'!
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:34'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:37'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 21:04'!
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/14/2012 12:09'!
callPrimitiveCode
	"Answer the call primitive bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 15:17'!
pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method
	"Answer the pc of the push closure bytecode whose block starts at startpc in method."
	^self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 11:02'!
pushClosureBytecodeSize
	"Answer the size of the push closure bytecode, if there is one."
	^self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 3/20/2013 16:46'!
unusedBytecode
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^nil! !

!BytecodeEncoder class methodsFor: 'scanning' stamp: 'eem 12/5/2012 15:45'!
method: method readsField: varIndex
	"Answer if method loads the instance variable indexed by varIndex."
	^self subclassResponsibility! !

!BytecodeEncoder class methodsFor: 'scanning' stamp: 'eem 12/5/2012 15:45'!
method: method writesField: varIndex
	"Answer if method stores into the instance variable indexed by varIndex."
	^self subclassResponsibility! !


!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 16:59'!
genBranchPopFalse: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	(distance > 0 and: [distance < 9]) ifTrue:
		["152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8)"
		 stream nextPut: 152 + distance - 1.
		 ^self].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 16:59'!
genBranchPopTrue: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	distance < 1024 ifTrue:
		["168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj"
		 stream
			nextPut: 168 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/15/2008 09:40'!
genDup
	"See BlueBook page 596"
	"136 	10001000 	Duplicate Stack Top"
	stream nextPut: 136! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:41'!
genJump: distance
	"See BlueBook page 596"
	(distance > 0 and: [distance < 9]) ifTrue:
		["144-151 	10010iii 	Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 144 + distance - 1.
		 ^self].
	"160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	^self genJumpLong: distance! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 16:59'!
genJumpLong: distance
	"See BlueBook page 596"
	(distance >= -1024 and: [distance < 1024]) ifTrue:
		["160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
		 stream
			nextPut: 160 + (distance + 1024 bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: -1024 to: 1023! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:27'!
genPop
	"See BlueBook page 596"
	"135 	10000111 	Pop Stack Top"
	stream nextPut: 135! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:40'!
genPushInstVar: instVarIndex
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 16 ifTrue:
			["0-15 	0000iiii 	Push Receiver Variable #iiii"
			 stream nextPut: 0 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 128;
				nextPut: instVarIndex.
			 ^self]].
	self genPushInstVarLong: instVarIndex! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:51'!
genPushInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 64;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:00'!
genPushLiteral: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal Constant #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 128 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 96;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 16:59'!
genPushLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["64-95 	010iiiii 	Push Literal Variable #iiiii"
		 stream nextPut: 64 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 128;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 16:16'!
genPushReceiver
	"See BlueBook page 596"
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	stream nextPut: 112! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:38'!
genPushSpecialLiteral: aLiteral
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	| index |
	index := #(true false nil -1 0 1 2) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil -1 0 1 2'].
	stream nextPut: index + 112! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:00'!
genPushTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 16 ifTrue: 
		["16-31 	0001iiii 	Push Temporary Location #iiii"
		 stream nextPut: 16 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 4/3/2013 12:50'!
genPushTempLong: tempIndex
	"See BlueBook page 596"
	(tempIndex >= 0 and: [tempIndex < 64]) ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:36'!
genPushThisContext
	"See BlueBook page 596"
	"137 	10001001 	Push Active Context"
	stream nextPut: 137! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:40'!
genReturnReceiver
	"See BlueBook page 596"
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	stream nextPut: 120! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:39'!
genReturnSpecialLiteral: aLiteral
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 120 + index! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:35'!
genReturnTop
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 124! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/14/2008 17:35'!
genReturnTopToCaller
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 125! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:00'!
genSend: selectorLiteralIndex numArgs: nArgs
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!!!"].
	selectorLiteralIndex < 0 ifTrue:
		["Special selector sends.
			176-191 	1011iiii 	Send Arithmetic Message #iiii
			192-207 	1100iiii 	Send Special Message #iiii"
		self flag: #yuck.
		 (selectorLiteralIndex negated between: 176 and: 207) ifFalse:
			[^self outOfRangeError: 'special selector code' index: selectorLiteralIndex negated range: 176 to: 207].
		 stream nextPut: selectorLiteralIndex negated.
		 ^self].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue:
		["	208-223 	1101iiii 	Send Literal Selector #iiii With No Arguments
			224-239 	1110iiii 	Send Literal Selector #iiii With 1 Argument
			240-255 	1111iiii 	Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 208 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments"
		 stream
			nextPut: 131;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 64 and: [nArgs < 4]) ifTrue: 
	 	["In Squeak V3
			134 	10000110 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk To Superclass With jjjjjjjj Arguments
		 is replaced by
			134 	10000110 jjkkkkkk 	Send Literal Selector #kkkkkk With jj Arguments"
		 stream
			nextPut: 134;
			nextPut: ((nArgs bitShift: 6) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 10/15/2012 16:10'!
genSendSpecial: specialSelectorIndex numArgs: nArgs
	"See BlueBook page 596"
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		176-191 	1011iiii 	Send Arithmetic Message #iiii
		192-207 	1100iiii 	Send Special Message #iiii"
	stream nextPut: specialSelectorIndex + 175! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:00'!
genSendSuper: selectorLiteralIndex numArgs: nArgs
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!!!"].
	selectorLiteralIndex < 0 ifTrue:
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"
		 stream
			nextPut: 133;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: 32 + nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:37'!
genStoreInstVar: instVarIndex
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:51'!
genStoreInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 160;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:01'!
genStoreLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:43'!
genStorePopInstVar: instVarIndex
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 8 ifTrue:
			["96-103 	01100iii 	Pop and Store Receiver Variable #iii"
			 stream nextPut: 96 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 130;
				nextPut: instVarIndex.
			 ^self]].
	self genStorePopInstVarLong: instVarIndex! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 6/19/2008 08:52'!
genStorePopInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 192;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:01'!
genStorePopLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 self genPop.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:01'!
genStorePopTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue: 
		["104-111 	01101iii 	Pop and Store Temporary Location #iii"
		 stream nextPut: 104 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63! !

!EncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:01'!
genStoreTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63! !

!EncoderForV3 methodsFor: 'method generation' stamp: 'eem 4/9/2013 12:09'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	| primBits |
	numTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 255 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	primBits := primitiveIndex <= 16r1FF
					ifTrue: [primitiveIndex]
					ifFalse: "For now the high bit of primitive no. is in the 29th bit of header"
						[primitiveIndex > 16r3FF ifTrue: [self error: 'prim num too large'].
						 (primitiveIndex bitAnd: 16r1FF) + ((primitiveIndex bitAnd: 16r200) bitShift: 19)].
	^(numArgs bitShift: 24)
	+ (numTemps bitShift: 18)
	"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
	+ (numLits bitShift: 9)
	+ primBits! !

!EncoderForV3 methodsFor: 'method generation' stamp: 'eliot 10/25/2012 15:48'!
generateMethodOfClass: aCompiledMethodClass trailer: trailer from: methodNode
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits literals header method stack |
	primErrNode := methodNode primitiveErrorVariableName ifNotNil:
						[self fixTemp: methodNode primitiveErrorVariableName].
	blkSize := (methodNode block sizeCodeForEvaluatedValue: self)
				+ (primErrNode
					ifNil: [0]
					ifNotNil: [primErrNode sizeCodeForStore: self "The VM relies on storeIntoTemp: (129)"]).
	header := self computeMethodHeaderForNumArgs: methodNode arguments size
					numTemps: self maxTemp
					numLits: (nLits := (literals := self allLiterals) size)
					primitive: methodNode primitive.
	method := trailer
					createMethod: blkSize
					class: aCompiledMethodClass
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	self streamToMethod: method.
	stack := ParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: self].
	stack position: method numTemps.
	[methodNode block emitCodeForEvaluatedValue: stack encoder: self]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	self methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	^method! !


!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:34'!
bindingReadScanBlockFor: bindingLitIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 64
	   and: [b <= 95
			ifTrue: [b - 64 = bindingLitIndex]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte - 192 = bindingLitIndex]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 128 and: 159)
						 and: [scanner thirdByte = bindingLitIndex]]]]]]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:34'!
bindingWriteScanBlockFor: bindingLitIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index bindingLitIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	  (b = 129 or: [b = 130])
		ifTrue: [scanner followingByte - 192 = bindingLitIndex]
		ifFalse:
			[b = 132
			 and: [scanner followingByte >= 224
			 and: [scanner thirdByte = bindingLitIndex]]]]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:49'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	^[:b| false]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:30'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b < 16
		ifTrue: [b = varIndexCode]
		ifFalse:
			[b = 128
				ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
				ifFalse:
					[b = 132
					 and: [(scanner followingByte between: 64 and: 95)
					 and: [scanner thirdByte = varIndexCode]]]]]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:30'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 96
	   and: [b <= 103
			ifTrue: [b - 96 = varIndexCode]
			ifFalse:
				[(b = 129 or: [b = 130])
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 160 and: 223)
						 and: [scanner thirdByte = varIndexCode]]]]]]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 4/9/2015 16:48'!
markerOrNilFor: aMethod
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	| e |
	^(((e := aMethod endPC) = 19 or: [e = 20])
	  and: [aMethod numLiterals = 3
	  and: [(aMethod at: 17) = 16r70	"push self"
	  and: [(aMethod at: 18) = 16rD0]]])	"send <literal 1>"
		ifTrue: [aMethod literalAt: 1]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:52'!
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	^[:instr |
	   instr = 16r85
	   or: [instr = 16r84
		and: [scanner followingByte between: 16r20 and: 16r3F]]]! !

!EncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:52'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^false! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/17/2014 17:12'!
bytecodeSize: bytecode
	"Answer the number of bytes in the bytecode."
	bytecode <= 125 ifTrue:
		[^1].
	bytecode >= 176 ifTrue:
		[^1].
	bytecode >= 160 ifTrue: "long jumps"
		[^2].
	bytecode >= 144 ifTrue: "short jumps"
		[^1].
	"extensions"
	bytecode >= 128 ifTrue:
		[^#(2 2 2 2 3 2 2 1 1 1 nil nil nil nil nil nil) at: bytecode - 127].
	^nil! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:39'!
extensionsAt: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:41'!
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:32'!
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretV3JumpIfCond! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:28'!
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct jump interpreter for the instruction set."
	^anInstructionStream interpretV3Jump! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:20'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextV3InstructionFor: aClient! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:43'!
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."

	^(method at: pc) = 125! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:34'!
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 152 and: 159) or: [bytecode between: 172 and: 175]! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:33'!
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."
	^(method at: pc) between: 168 and: 171! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/17/2014 17:59'!
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends the
	 range of the following bytecode.  The Smalltalk-80/V3 sets don't use extensions."
	^false! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:36'!
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 144 and: 151) or: [bytecode between: 160 and: 167]! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:28'!
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	^(method at: pc) = 135	"135 		10000111 	Pop Stack Top"! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:01'!
isRealSendAt: pc in: method
	"Answer whether the bytecode at pc is a real message-send, not blockCopy:."

	| byte |
	byte := method at: pc.
	^(byte >= 176			"special send or short send"
	   and: [byte ~= 200	"special selector blockCopy:"
			or: [(Smalltalk specialSelectorAt: 200 - 175) ~~ #blockCopy:]])
	 or: [byte >= 131
		 and: [byte <= 134	"long sends"	
		 and: [byte ~= 132	"double extended do anything"
			or: [(method at: pc + 1) // 32 <= 1]]]]! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:11'!
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return."

	^(method at: pc) between: 120 and: 125! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 9/2/2014 14:10'!
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	^(method at: pc) = 124! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:00'!
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	| byte |
	byte := method at: pc.
	^byte >= 176			"special send or short send"
	 or: [byte >= 131
		 and: [byte <= 134	"long sends"	
		 and: [byte ~= 132	"double extended do anything"
			or: [(method at: pc + 1) // 32 <= 1]]]]! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:14'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 132)
		and: [byte <= 111				"96 103	storeAndPopReceiverVariableBytecode"
										"104 111	storeAndPopTemporaryVariableBytecode"
			or: [byte >= 129			"129		extendedStoreBytecode"
				and: [byte <= 130		"130		extendedStoreAndPopBytecode"
					or: [byte = 132		"132		doubleExtendedDoAnythingBytecode"
						and: [(method at: pc+1) >= 160]]]]]! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:46'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 111)	"96 103	storeAndPopReceiverVariableBytecode"
									"104 111	storeAndPopTemporaryVariableBytecode"
	  or: [byte = 130]				"130		extendedStoreAndPopBytecode"! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 11:28'!
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	^method at: pc! !

!EncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 21:03'!
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	| byte byte2 |
	byte := method at: pc.
	byte < 131 ifTrue: [^anInstructionStream].
	byte >= 176
		ifTrue: 
			["special byte or short send"
			byte >= 208
				ifTrue: [^method literalAt: (byte bitAnd: 15) + 1]
				ifFalse: [^Smalltalk specialSelectorAt: byte - 176 + 1]]
		ifFalse: 
			[byte <= 134 ifTrue: 
				[byte2 := method at: pc + 1.
				 byte = 131 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 132 ifTrue: [byte2 < 64 ifTrue: [^method literalAt: (method at: pc + 2) + 1]].
				 byte = 133 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 134 ifTrue: [^method literalAt: byte2 \\ 64 + 1]]].
	^anInstructionStream! !

!EncoderForV3 class methodsFor: 'scanning' stamp: 'eem 12/5/2012 16:08'!
method: method readsField: varIndex
	"Answer if method loads the instance variable indexed by varIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available.
	0-15 		0000iiii 	Push Receiver Variable #iiii
	128 		10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)"
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	method isReturnField ifTrue:
		[^method returnField = varIndexCode].
	^(scanner := InstructionStream on: method) scanFor:
		[:b|
		b < 16
			ifTrue: [b = varIndexCode]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 64 and: 95)
						 and: [scanner thirdByte = varIndexCode]]]]]! !

!EncoderForV3 class methodsFor: 'scanning' stamp: 'eem 12/5/2012 16:10'!
method: method writesField: varIndex
	"Answer if method stores into the instance variable indexed by varIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available.
	96-103 	01100iii 	Pop and Store Receiver Variable #iii
	129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)"
	| varIndexCode scanner |
	method isQuick ifTrue: [^false].
	varIndexCode := varIndex - 1.
	^(scanner := InstructionStream on: method) scanFor:
		[:b|
		b >= 96
		and: [b <= 103
				ifTrue: [b - 96 = varIndexCode]
				ifFalse:
					[(b = 129 or: [b = 130])
						ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 160 and: 223)
							 and: [scanner thirdByte = varIndexCode]]]]]]! !

!EncoderForV3 class methodsFor: 'bytecode decoding' stamp: 'eem 3/20/2013 16:46'!
unusedBytecode
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^126! !


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:11'!
genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	"143 	10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 15]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 15].
	(numArgs < 0 or: [numArgs > 15]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 15].
	stream
		nextPut: 143;
		nextPut: numArgs + (numCopied bitShift: 4);
		nextPut: (jumpSize bitShift: -8);
		nextPut: (jumpSize bitAnd: 16rFF)! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:03'!
genPushConsArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 1kkkkkkk 	Push (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size + 128! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/29/2015 15:21'!
genPushNClosureTemps: numTemps
	numTemps timesRepeat: [self genPushSpecialLiteral: nil]! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:06'!
genPushNewArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 0kkkkkkk 	Pop kkkkkkk into: (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 6/16/2008 09:45'!
genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		["140 	10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
		 stream
			nextPut: 140;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:02'!
genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"142 	10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 142;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]! !

!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/30/2008 17:02'!
genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"141 	10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 141;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]! !

!EncoderForV3PlusClosures methodsFor: 'testing' stamp: 'eem 5/24/2008 18:12'!
supportsClosureOpcodes
	^true! !

!EncoderForV3PlusClosures methodsFor: 'method generation' stamp: 'eliot 10/26/2012 11:54'!
generateMethodOfClass: aCompiledMethodClass trailer: trailer from: methodNode
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits locals literals header method stack |
	primErrNode := methodNode primitiveErrorVariableName ifNotNil:
						[self fixTemp: methodNode primitiveErrorVariableName].
	methodNode ensureClosureAnalysisDone.
	self rootNode: methodNode. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (methodNode block sizeCodeForEvaluatedValue: self)
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: methodNode arguments size + methodNode temporaries size;
							sizeCodeForStore: self "The VM relies on storeIntoTemp: (129)"]).
	locals := methodNode arguments, methodNode temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	self noteBlockExtent: methodNode block blockExtent hasLocals: locals.
	header := self computeMethodHeaderForNumArgs: methodNode arguments size
					numTemps: locals size
					numLits: (nLits := (literals := self allLiterals) size)
					primitive: methodNode primitive.
	method := trailer
					createMethod: blkSize
					class: aCompiledMethodClass
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	self streamToMethod: method.
	stack := ParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: self].
	stack position: method numTemps.
	[methodNode block emitCodeForEvaluatedValue: stack encoder: self]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	self methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	^method! !


!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 18:57'!
bytecodeSize: bytecode
	"Answer the number of bytes in the bytecode."
	bytecode <= 125 ifTrue:
		[^1].
	bytecode >= 176 ifTrue:
		[^1].
	bytecode >= 160 ifTrue: "long jumps"
		[^2].
	bytecode >= 144 ifTrue: "short jumps"
		[^1].
	"extensions"
	bytecode >= 128 ifTrue:
		[^#(2 2 2 2 3 2 2 1 1 1 2 nil 3 3 3 4) at: bytecode - 127].
	^nil! !

!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:19'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextV3ClosuresInstructionFor: aClient! !

!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:33'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 142)
		and: [byte <= 111				"96 103	storeAndPopReceiverVariableBytecode"
										"104 111	storeAndPopTemporaryVariableBytecode"
			or: [byte >= 129			"129		extendedStoreBytecode"
				and: [byte <= 130		"130		extendedStoreAndPopBytecode"
					or: [(byte = 132	"132		doubleExtendedDoAnythingBytecode"
						and: [(method at: pc+1) >= 160])
					or: [byte = 141		"141		storeRemoteTempLongBytecode"
					or: [byte = 142		"142		storeAndPopRemoteTempLongBytecode"]]]]]]! !

!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:45'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 111)	"96 103	storeAndPopReceiverVariableBytecode"
									"104 111	storeAndPopTemporaryVariableBytecode"
	  or: [byte = 130				"130		extendedStoreAndPopBytecode"
	  or: [byte = 142]]				"142		storeAndPopRemoteTempLongBytecode"! !

!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:49'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	^[ :bc | bc = 143]! !

!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:52'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^true! !

!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 15:19'!
pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method
	"Answer the pc of the push closure bytecode whose block starts at startpc in method.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^startpc - 4! !

!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 11:03'!
pushClosureBytecodeSize
	"Answer the size of the push closure bytecode.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^4! !


!FutureNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:33'!
accept: aVisitor
	^aVisitor visitFutureNode: self! !

!FutureNode methodsFor: 'code generation (closures)' stamp: 'eem 7/17/2008 19:58'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	{ receiver. futureDelta }, (futureArgs ifNil: [#()]) do:
		[:node|
		node == nil ifFalse:
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]]! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForBlockValue: stack encoder: encoder
	"Generate code for evaluating the last statement in a block."
	^effectNode emitCodeForValue: stack encoder: encoder! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForEffect: stack encoder: encoder
	^effectNode emitCodeForEffect: stack encoder: encoder! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForValue: stack encoder: encoder
	^valueNode emitCodeForValue: stack encoder: encoder! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 7/18/2008 12:28'!
sizeCodeForBlockValue: encoder
	receiver == NodeSuper ifTrue: [^self error: 'Futures cannot send to future'].
	(futureArgs isNil or: [futureSelector isNil]) ifTrue:
		[^self error: 'Futures must be sent messages'].
	encoder sharableLitIndex: originalSelector. "to find its senders"
	futureDelta ifNil:[futureDelta := encoder encodeLiteral: 0].
	effectNode := MessageNode new
		receiver: receiver
		selector: #futureDo:at:args:
		arguments: (Array 
			with: (encoder encodeLiteral: futureSelector) 
			with: futureDelta
			with: (BraceNode new elements: futureArgs))
		precedence: 3 
		from: encoder.
	^effectNode sizeCodeForValue: encoder! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 7/18/2008 12:27'!
sizeCodeForEffect: encoder
	receiver == NodeSuper ifTrue: [^self error: 'Futures cannot send to future'].
	(futureArgs isNil or: [futureSelector isNil]) ifTrue:
		[^self error: 'Futures must be sent messages'].
	encoder sharableLitIndex: originalSelector. "to find its senders"
	futureDelta ifNil:[futureDelta := encoder encodeLiteral: 0].
	effectNode := MessageNode new
		receiver: receiver
		selector: #futureDo:at:args:
		arguments: (Array 
			with: (encoder encodeLiteral: futureSelector) 
			with: futureDelta
			with: (BraceNode new elements: futureArgs))
		precedence: 3 
		from: encoder.
	^effectNode sizeCodeForEffect: encoder! !

!FutureNode methodsFor: 'code generation' stamp: 'eem 7/18/2008 12:28'!
sizeCodeForValue: encoder
	receiver == NodeSuper ifTrue: [^self error: 'Futures cannot send to future'].
	(futureArgs isNil or: [futureSelector isNil]) ifTrue:
		[^self error: 'Futures must be sent messages'].
	encoder sharableLitIndex: originalSelector. "to find its senders"
	futureDelta ifNil:[futureDelta := encoder encodeLiteral: 0].
	valueNode := MessageNode new
		receiver: receiver
		selector: #futureSend:at:args:
		arguments: (Array 
			with: (encoder encodeLiteral: futureSelector) 
			with: futureDelta
			with: (BraceNode new elements: futureArgs))
		precedence: 3 
		from: encoder.
	^valueNode sizeCodeForValue: encoder! !

!FutureNode methodsFor: 'initialize-release' stamp: 'ar 10/18/2005 17:23'!
futureMessage: selName arguments: args  from: encoder sourceRange: range
	futureSelector := selName.
	futureArgs := args.
	^self! !

!FutureNode methodsFor: 'initialize-release' stamp: 'ar 10/18/2005 17:23'!
receiver: rcvr selector: selector arguments: args precedence: p from: encoder sourceRange: range
	receiver := rcvr.
	originalSelector := selector.
	originalSelector == #future: ifTrue:[futureDelta := args first].
	encoder noteSourceRange: range forNode: self.! !

!FutureNode methodsFor: 'accessing' stamp: 'das 1/6/2006 16:28'!
futureSelector
	^futureSelector! !

!FutureNode methodsFor: 'accessing' stamp: 'eem 9/10/2008 15:21'!
originalSelector
	^originalSelector! !

!FutureNode methodsFor: 'accessing' stamp: 'eem 9/10/2008 15:21'!
receiver
	^receiver! !

!FutureNode methodsFor: 'testing' stamp: 'eem 7/18/2008 16:22'!
isFutureNode
	^true! !


!LeafNode methodsFor: 'accessing'!
key

	^key! !

!LeafNode methodsFor: 'code generation' stamp: 'ab 7/6/2004 17:39'!
code

	^ code! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForEffect: stack encoder: encoder

	^self! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Default is to do nothing.
	 Subclasses may need to override."! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 15:57'!
reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder litIndex: key) type: 0 - code]! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForEffect: encoder

	^0! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:34'!
sizeCodeForLoad: encoder forValue: forValue
	"Default is to do nothing.
	 Subclasses may need to override."
	^0! !

!LeafNode methodsFor: 'code generation' stamp: 'eem 5/19/2008 15:10'!
sizeCodeForValue: encoder
	self subclassResponsibility! !

!LeafNode methodsFor: 'copying' stamp: 'eem 5/14/2008 11:25'!
veryDeepFixupWith: deepCopier
	"If fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!!!"

super veryDeepFixupWith: deepCopier.
key := (deepCopier references at: key ifAbsent: [key]).
! !

!LeafNode methodsFor: 'copying' stamp: 'eem 7/12/2008 17:24'!
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"key := key.		Weakly copied"
code := code veryDeepCopyWith: deepCopier.
index := index veryDeepCopyWith: deepCopier.
! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ar 3/26/2004 15:44'!
key: object code: byte

	key := object.
	code := byte! !

!LeafNode methodsFor: 'initialize-release' stamp: 'eem 5/14/2008 15:56'!
key: object index: i type: type

	key := object.
	code := (self code: i type: type).
	index := i! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ar 3/26/2004 15:44'!
name: ignored key: object code: byte

	key := object.
	code := byte! !

!LeafNode methodsFor: 'private'!
code: index type: type

	index isNil 
		ifTrue: [^type negated].
	(CodeLimits at: type) > index 
		ifTrue: [^(CodeBases at: type) + index].
	^type * 256 + index! !

!LeafNode methodsFor: 'code generation (closures)' stamp: 'eem 6/16/2008 09:32'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self! !


!LiteralNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:19'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 encoder genPushSpecialLiteral: specialLiteral])
		ifFalse:
			[encoder genPushLiteral: index]! !

!LiteralNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 13:34'!
reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue:
		[index := key isVariableBinding "true if sending value[:] to a special binding"
					ifTrue: [encoder sharableLitIndex: key]
					ifFalse: [encoder litIndex: key].
		 code := self code: index type: 0 - code]! !

!LiteralNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:03'!
sizeCodeForValue: encoder
	self reserve: encoder.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder sizePushSpecialLiteral: specialLiteral])
		ifFalse:
			[^encoder sizePushLiteral: index]! !

!LiteralNode methodsFor: 'evaluation' stamp: 'tk 8/4/1999 17:35'!
eval
	"When everything in me is a constant, I can produce a value.  This is only used by the Scripting system (TilePadMorph tilesFrom:in:)"

	^ key! !

!LiteralNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:46'!
printOn: aStream indent: level

	key isVariableBinding
		ifTrue:
			[key key isNil
				ifTrue:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifFalse:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]! !

!LiteralNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level

	key isVariableBinding
		ifTrue:
			[key key isNil
				ifTrue:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifFalse:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]! !

!LiteralNode methodsFor: 'testing'!
isConstantNumber
	^ key isNumber! !

!LiteralNode methodsFor: 'testing' stamp: 'ul 11/23/2010 13:06'!
isLiteralNode

	^ true! !

!LiteralNode methodsFor: 'testing'!
isSpecialConstant
	^ code between: LdTrue and: LdMinus1+3! !

!LiteralNode methodsFor: 'testing'!
literalValue

	^key! !

!LiteralNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitLiteralNode: self! !

!LiteralNode methodsFor: 'initialize-release' stamp: 'eem 5/14/2008 09:30'!
name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type! !

!LiteralNode methodsFor: '*Etoys-tiles' stamp: 'ar 8/16/2001 13:27'!
explanation

	(key isVariableBinding) ifFalse: [
		^'Literal ', key storeString
	].
	key key isNil ifTrue: [
		^'Literal ', ('###',key value soleInstance name) 
	] ifFalse: [
		^'Literal ', ('##', key key) 
	].	! !


!SpecialLiteralNode methodsFor: 'code generation (closures)' stamp: 'eem 10/22/2012 15:58'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	encoder genPushSpecialLiteral: key! !

!SpecialLiteralNode methodsFor: 'code generation (closures)' stamp: 'eem 10/22/2012 15:58'!
sizeCodeForValue: encoder
	^encoder sizePushSpecialLiteral: key! !


!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCode: stack args: nArgs encoder: encoder

	self emitCode: stack
		args: nArgs
		encoder: encoder
		super: false! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:47'!
emitCode: stack args: nArgs encoder: encoder super: supered
	stack pop: nArgs.
	^supered
		ifTrue:
			[encoder genSendSuper: index numArgs: nArgs]
		ifFalse:
			[encoder
				genSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForEffect: stack encoder: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForValue: stack encoder: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:08'!
reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder sharableLitIndex: key) type: 0 - code]! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:15'!
sizeCode: encoder args: nArgs super: supered
	self reserve: encoder.
	^supered
		ifTrue:
			[code < Send "i.e. its a special selector" ifTrue:
				[code := self code: (index := encoder sharableLitIndex: key) type: 5].
			 encoder sizeSendSuper: index numArgs: nArgs]
		ifFalse:
			[self flag: #yuck. "special selector sends cause this problem"
			 encoder
				sizeSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForEffect: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForValue: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'inappropriate'!
emitForEffect: stack on: strm

	self shouldNotImplement! !

!SelectorNode methodsFor: 'inappropriate'!
emitForValue: stack on: strm

	self shouldNotImplement! !

!SelectorNode methodsFor: 'inappropriate'!
sizeForEffect: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'inappropriate'!
sizeForValue: encoder

	self shouldNotImplement! !

!SelectorNode methodsFor: 'printing' stamp: 'eem 9/25/2008 14:56'!
key: aSelector
	"This is for printing of FFI selectors."
	key := aSelector! !

!SelectorNode methodsFor: 'printing' stamp: 'eem 9/25/2008 15:01'!
printAsFFICallWithArguments: aSequence on: aStream indent: level
	aStream nextPutAll: (key copyUpTo: $)).
	aSequence
		do: [:arg| arg printOn: aStream indent: level]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!SelectorNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:50'!
printOn: aStream indent: level 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])! !

!SelectorNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])! !

!SelectorNode methodsFor: 'testing' stamp: 'eem 9/25/2008 15:18'!
isForFFICall
	^key includesSubString: '()/'! !

!SelectorNode methodsFor: 'testing'!
isPvtSelector
	"Answer if this selector node is a private message selector."

	^key isPvtSelector! !

!SelectorNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitSelectorNode: self! !

!SelectorNode methodsFor: 'initialize-release' stamp: 'eem 5/14/2008 09:30'!
name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type! !


!SpecialSelectorNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:31'!
emitCode: stack args: nArgs encoder: encoder super: supered
	"Generate a special selector send.
	 A super send of a special selector must be handled like a normal send."
	supered
		ifTrue:
			[super emitCode: stack args: nArgs encoder: encoder super: supered]
		ifFalse:
			[stack pop: nArgs.
			 encoder genSendSpecial: code numArgs: nArgs]! !

!SpecialSelectorNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:32'!
sizeCode: encoder args: nArgs super: supered
	"Size a special selector send.
	 A super send of a special selector must be handled like a normal send."
	^supered
		ifTrue: [super sizeCode: encoder args: nArgs super: supered]
		ifFalse: [encoder sizeSendSpecial: code numArgs: nArgs]! !


!VariableNode methodsFor: 'accessing' stamp: 'tk 1/30/2001 13:45'!
name
	^ name! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Do nothing"! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:41'!
emitCodeForReturn: stack encoder: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			"short returns"
			 encoder genReturnSpecialLiteral: specialLiteral.
			 stack push: 1 "doesnt seem right".
			 ^self].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue: 
		["short returns"
		 encoder genReturnReceiver.
		 stack push: 1 "doesnt seem right".
		 ^self].
	super emitCodeForReturn: stack encoder: encoder! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:08'!
emitCodeForStore: stack encoder: encoder

	self shouldNotImplement! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:41'!
emitCodeForStorePop: stack encoder: encoder
	self type ~= 1 ifTrue:
		[self halt].
	encoder genStorePopInstVar: index.
	stack pop: 1! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:07'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder genPushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder genPushReceiver].
	code = LdThisContext ifTrue:
		[^encoder genPushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder genPushInstVar: index! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 9/5/2008 18:14'!
fieldOffset
	"Return temp or instVar offset for this variable"
	^index ifNil: [code < 256
					ifTrue: [code \\ 16]
					ifFalse: [code \\ 256]]! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 11/2/2012 10:15'!
sizeCodeForReturn: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			 ^encoder sizeReturnSpecialLiteral: specialLiteral].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue:
		[^encoder sizeReturnReceiver].
	^super sizeCodeForReturn: encoder! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:09'!
sizeCodeForStore: encoder
	self shouldNotImplement! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:09'!
sizeCodeForStorePop: encoder
	self shouldNotImplement! !

!VariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:54'!
sizeCodeForValue: encoder
	self reserve: encoder.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral| "i.e. the pseudo-variables nil true & false"
			 ^encoder sizePushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder sizePushReceiver].
	code = LdThisContext ifTrue:
		[^encoder sizePushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder sizePushInstVar: index! !

!VariableNode methodsFor: 'initialize-release'!
asStorableNode: encoder
	^ self! !

!VariableNode methodsFor: 'initialize-release' stamp: 'eem 12/1/2008 13:51'!
name: string
	"Change name"

	name := string! !

!VariableNode methodsFor: 'initialize-release' stamp: 'eem 5/14/2008 09:33'!
name: varName index: i type: type
	"Only used for initting instVar refs"
	^self name: varName key: varName index: i type: type! !

!VariableNode methodsFor: 'initialize-release' stamp: 'ar 3/26/2004 15:46'!
name: string key: object code: byte
	"Only used for initting std variables, nil, true, false, self, etc."
	name := string.
	key := object.
	code := byte! !

!VariableNode methodsFor: 'initialize-release' stamp: 'eem 5/14/2008 16:01'!
name: varName key: objRef index: i type: type
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)! !

!VariableNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:37'!
printOn: aStream indent: level 

	aStream nextPutAll: name! !

!VariableNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level 

	aStream nextPutAll: name! !

!VariableNode methodsFor: 'testing' stamp: 'eem 5/14/2008 09:11'!
assignmentCheck: encoder at: location
	^(encoder cantStoreInto: name) ifTrue: [location] ifFalse: [-1]! !

!VariableNode methodsFor: 'testing' stamp: 'eem 12/3/2013 10:01'!
index
	"If index is nil, this code attempts to reconstruct the index from its encoding in code."
	index ifNotNil:
		[^index].
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[self assert: index = (code \\ 256).
		^code \\ 256].
	code >= (CodeBases at: self type) ifTrue:
		[self assert: index = (code - (CodeBases at: self type)).
		^code - (CodeBases at: self type)].
	self assert: index = (code - self type).
	^code - self type! !

!VariableNode methodsFor: 'testing' stamp: 'eem 5/14/2008 09:13'!
isSelfPseudoVariable
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [name = '{{self}}']! !

!VariableNode methodsFor: 'testing' stamp: 'eem 8/13/2010 15:34'!
isUndeclared
	^false! !

!VariableNode methodsFor: 'testing' stamp: 'ar 11/19/2002 14:58'!
isVariableNode
	^true! !

!VariableNode methodsFor: 'testing'!
isVariableReference

	^true! !

!VariableNode methodsFor: 'testing' stamp: 'eem 5/14/2008 09:18'!
type
	"This code attempts to reconstruct the type from its encoding in code.
		This allows one to test, for instance, (aNode type = LdInstType)."
	| type |
	code < 0 ifTrue: [^code negated].
	code >= 256 ifTrue: [^code // 256].
	type := CodeBases findFirst: [:one | code < one].
	^type = 0 ifTrue: [5] ifFalse: [type - 1]! !

!VariableNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitVariableNode: self! !

!VariableNode methodsFor: 'tiles' stamp: 'RAA 8/24/1999 16:34'!
currentValueIn: aContext

	aContext ifNil: [^nil].
	^((self variableGetterBlockIn: aContext) ifNil: [^nil]) value printString
	

! !

!VariableNode methodsFor: 'tiles' stamp: 'nice 12/29/2010 15:11'!
variableGetterBlockIn: aContext

	| temps tempIndex ivars |

	(self type = 4 and: [self key isVariableBinding]) ifTrue: [
		^[self key value]
	].
	aContext ifNil: [^nil].
	self isSelfPseudoVariable ifTrue: [^[aContext receiver]].
	self type = 1 ifTrue: [
		ivars := aContext receiver class allInstVarNames.
		tempIndex := ivars indexOf: self name ifAbsent: [^nil].
		^[aContext receiver instVarAt: tempIndex]
	].
	self type = 2 ifTrue: [
		temps := aContext tempNames.
		tempIndex := temps indexOf: self name ifAbsent: [^nil].
		^[aContext tempAt: tempIndex]
	].
	^nil
! !

!VariableNode methodsFor: 'code generation (closures)' stamp: 'eem 7/20/2009 10:38'!
beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"No-op overridden by TempVariableNode"! !

!VariableNode methodsFor: '*Etoys-tiles' stamp: 'ab 7/13/2004 13:53'!
explanation

	self isSelfPseudoVariable ifTrue: [^'the pseudo variable <self> (refers to the receiver)'].
	^(#('instance' 'temporary' 'LIT3' 'global') 
			at: self type 
			ifAbsent: ['UNK',self type printString]),' variable <',self name,'>'
		

	"LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
"

! !


!VariableNode class methodsFor: 'class initialization' stamp: 'eem 10/18/2012 11:38'!
initialize    "VariableNode initialize.  Decompiler initialize"
	| encoder |
	encoder := Encoder new.
	StdVariables := Dictionary new: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary new: 64.
	encoder
		fillDict: StdSelectors
		with: SpecialSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (1 to: Smalltalk specialSelectorSize) asArray.
	StdLiterals := PluggableDictionary new equalBlock: [ :x :y | x literalEqual: y ].
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !


!FieldNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitFieldNode: self! !

!FieldNode methodsFor: 'testing' stamp: 'eem 5/12/2008 13:40'!
assignmentCheck: encoder at: location
	(encoder cantStoreInto: name) ifTrue: [^location].
	fieldDef toSet ifNil:[
		encoder interactive ifTrue:[^location].
		fieldDef := fieldDef clone assignDefaultSetter.
	].
	^-1! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForEffect: stack encoder: encoder! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	rcvrNode emitCodeForValue: stack encoder: encoder.
	fieldDef accessKey ifNotNil:
		[super emitCodeForValue: stack encoder: encoder]! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForStore: stack encoder: encoder
	fieldDef accessKey ifNil:[
		writeNode emitCode: stack args: 1 encoder: encoder super: false.
	] ifNotNil:[
		writeNode emitCode: stack args: 2 encoder: encoder super: false.
	].! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:18'!
emitCodeForStorePop: stack encoder: encoder
	self emitCodeForStore: stack encoder: encoder.
	encoder genPop.
	stack pop: 1.! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForValue: stack encoder: encoder
	fieldDef accessKey ifNil:[
		rcvrNode emitCodeForValue: stack encoder: encoder.
		readNode emitCode: stack args: 0 encoder: encoder super: false.
	] ifNotNil:[
		rcvrNode emitCodeForValue: stack encoder: encoder.
		super emitCodeForValue: stack encoder: encoder.
		readNode emitCode: stack args: 1 encoder: encoder super: false.
	].! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/12/2008 13:40'!
encodeReceiverOn: encoder
	"encode the receiver node"
	rcvrNode := encoder encodeVariable: 'self'.! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForEffect: encoder
	^0! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForStore: encoder
	rcvrNode ifNil:[self encodeReceiverOn: encoder].
	fieldDef accessKey ifNil:[
		writeNode ifNil:[writeNode := encoder encodeSelector: fieldDef toSet].
		^(rcvrNode sizeCodeForValue: encoder) + 
			(writeNode sizeCode: encoder args: 1 super: false)	
	].
	writeNode ifNil:[writeNode := encoder encodeSelector: #set:to:].
	^(rcvrNode sizeCodeForValue: encoder) + 
		(super sizeCodeForValue: encoder) +
			(writeNode sizeCode: encoder args: 2 super: false)! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:17'!
sizeCodeForStorePop: encoder
	^(self sizeCodeForStore: encoder) + encoder sizePop! !

!FieldNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForValue: encoder
	rcvrNode ifNil:[self encodeReceiverOn: encoder].
	fieldDef accessKey ifNil:[
		readNode ifNil:[readNode := encoder encodeSelector: fieldDef toGet].
		^(rcvrNode sizeCodeForValue: encoder) + 
			(readNode sizeCode: encoder args: 0 super: false)
	].
	readNode ifNil:[readNode := encoder encodeSelector: #get:].
	^(rcvrNode sizeCodeForValue: encoder) + 
		(super sizeCodeForValue: encoder) + 
			(readNode sizeCode: encoder args: 1 super: false)! !

!FieldNode methodsFor: 'accessing' stamp: 'eem 5/12/2008 13:40'!
fieldDef
	^fieldDef! !

!FieldNode methodsFor: 'initialize-release' stamp: 'eem 5/12/2008 13:40'!
fieldDefinition: fieldDefinition
	self name: fieldDefinition name key: fieldDefinition index: nil type: LdLitType! !

!FieldNode methodsFor: 'initialize-release' stamp: 'eem 5/12/2008 13:40'!
name: varName key: objRef index: i type: type
	fieldDef := objRef.
	^super name: varName key: objRef key index: nil type: LdLitType! !


!InstanceVariableNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitInstanceVariableNode: self! !

!InstanceVariableNode methodsFor: 'initialize-release' stamp: 'eem 5/13/2008 10:17'!
name: varName index: varIndex
	^self name: varName index: varIndex-1 type: LdInstType! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:08'!
emitCodeForStore: stack encoder: encoder
	encoder genStoreInstVar: index! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/15/2008 10:05'!
emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVar: index.
	stack pop: 1! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:08'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	^encoder genPushInstVar: index! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:57'!
sizeCodeForStore: encoder
	^encoder sizeStoreInstVar: index! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:07'!
sizeCodeForStorePop: encoder
	^encoder sizeStorePopInstVar: index! !

!InstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:53'!
sizeCodeForValue: encoder
	^encoder sizePushInstVar: index! !


!MaybeContextInstanceVariableNode methodsFor: 'accessing' stamp: 'eem 6/19/2008 09:27'!
code
	"Answer a bogus code to avoid creating quick methods.
	 See MethodNode>>generate:ifQuick:"
	^LoadLong! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:08'!
emitCodeForStore: stack encoder: encoder
	encoder genStoreInstVarLong: index! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 6/19/2008 09:36'!
emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVarLong: index.
	stack pop: 1! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:08'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	^encoder genPushInstVarLong: index! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:57'!
sizeCodeForStore: encoder
	^encoder sizeStoreInstVarLong: index! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 18:07'!
sizeCodeForStorePop: encoder
	^encoder sizeStorePopInstVarLong: index! !

!MaybeContextInstanceVariableNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:53'!
sizeCodeForValue: encoder
	^encoder sizePushInstVarLong: index! !


!LiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:37'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"If a normal literal variable (not sending value:), do nothing.
	 If for value (e.g. v := Binding := expr) do nothing; the work will be done in emitCodeForStore:encoder:.
	 If not for value then indeed load.  The rest of the work will be done in  emitCodeForStorePop:encoder:."
	(writeNode isNil or: [forValue]) ifTrue: [^self].
	encoder genPushLiteral: index.
	stack push: 1! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/12/2013 16:05'!
emitCodeForStore: stack encoder: encoder
	| exprOffset |
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop.
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver.  If they
	 did we could generate much simpler code, e.g.
		encoder genPushLiteral: index.
		stack push: 1.
		writeNode emitCode: stack args: 1 encoder: encoder super: false"
	exprOffset := stack position - 1.
	encoder genPushLiteral: index.
	stack push: 1.
	encoder genPushTempLong: exprOffset.
	stack push: 1.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'nice 4/5/2013 02:20'!
emitCodeForStorePop: stack encoder: encoder
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'nice 7/23/2010 23:17'!
emitCodeForValue: stack encoder: encoder
	^readNode
		ifNil: [stack push: 1.
			encoder genPushLiteralVar: index]
		ifNotNil: [readNode emitCodeForValue: stack encoder: encoder]! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:37'!
sizeCodeForLoad: encoder forValue: forValue
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding and: [forValue not]])
		ifTrue: [encoder sizePushLiteral: index]
		ifFalse: [0]! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 13:14'!
sizeCodeForStore: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialWriteBinding]) ifFalse:
		[^encoder sizeStoreLiteralVar: index].
	writeNode := encoder encodeSelector: #value:.
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop."
	^(encoder sizePushLiteral: index)
	  + (encoder sizePushTempLong: 0) "we don't know yet, hence long, sigh..."
	  + (writeNode sizeCode: encoder args: 1 super: false)
	  + encoder sizePop! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'nice 4/5/2013 02:29'!
sizeCodeForStorePop: encoder
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding])
		ifTrue: [	writeNode := encoder encodeSelector: #value:.
				^ (writeNode sizeCode: encoder args: 1 super: false)
	  			+ encoder sizePop]
		ifFalse: [encoder sizeStorePopLiteralVar: index]! !

!LiteralVariableNode methodsFor: 'code generation' stamp: 'nice 7/23/2010 23:58'!
sizeCodeForValue: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialReadBinding]) 
		ifFalse:
			[^encoder sizePushLiteralVar: index].
	readNode := MessageNode new 
		receiver: (encoder encodeLiteral: key)
		selector: (encoder encodeSelector: #value)
		arguments: #()
		precedence: #value precedence.
	^readNode sizeCodeForValue: encoder! !

!LiteralVariableNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitLiteralVariableNode: self! !

!LiteralVariableNode methodsFor: 'testing' stamp: 'eem 5/20/2010 09:12'!
assignmentCheck: encoder at: location
	^(key isVariableBinding and: [key canAssign not])
		ifTrue: [location]
		ifFalse: [-1]! !


!TempVariableNode methodsFor: 'initialize-release' stamp: 'eem 9/8/2008 18:27'!
name: varName index: i type: type scope: level
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type! !

!TempVariableNode methodsFor: 'initialize-release'!
nowHasDef
	hasDefs := true! !

!TempVariableNode methodsFor: 'initialize-release'!
nowHasRef
	hasRefs := true! !

!TempVariableNode methodsFor: 'initialize-release'!
scope: level
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level! !

!TempVariableNode methodsFor: 'printing' stamp: 'eem 7/23/2008 21:21'!
printDefinitionForClosureAnalysisOn: aStream 
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes notNil ifTrue:
		[refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asSortedCollection do: [:read| aStream nextPutAll: ' r@'; print: read]].
	writingScopes notNil ifTrue:
		[refs := Set new.
		writingScopes do: [:elems| refs addAll: elems].
		refs asSortedCollection do: [:write| aStream nextPutAll: ' w@'; print: write]].
	aStream nextPut: $}! !

!TempVariableNode methodsFor: 'printing' stamp: 'eem 5/8/2008 11:39'!
printOn: aStream indent: level 

	aStream nextPutAll: name! !

!TempVariableNode methodsFor: 'printing' stamp: 'eem 7/24/2009 12:44'!
printWithClosureAnalysisOn: aStream indent: level 

	aStream nextPutAll: name.
	readingScopes notNil ifTrue:
		[(readingScopes inject: Set new into: [:them :reads| them addAll: reads. them]) asSortedCollection do:
			[:location|
			aStream space; nextPut: $r; nextPut: $@; print: location]].
	writingScopes notNil ifTrue:
		[(writingScopes inject: Set new into: [:them :writes| them addAll: writes. them]) asSortedCollection do:
			[:location|
			aStream space; nextPut: $w; nextPut: $@; print: location]]! !

!TempVariableNode methodsFor: 'testing' stamp: 'ul 6/10/2011 22:24'!
assignmentCheck: encoder at: location
	^((self isBlockArg and: [Scanner allowBlockArgumentAssignment not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 9/8/2008 18:22'!
beBlockArg
	argType := #block! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 9/8/2008 18:21'!
beMethodArg
	argType := #method! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 9/8/2008 18:24'!
isArg
	^argType notNil! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 9/8/2008 18:20'!
isBlockArg
	^#block == argType! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 8/14/2010 11:02'!
isDeclaredAtMethodLevel
	"For the explainer."
	^scope = 0! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 9/8/2008 18:20'!
isMethodArg
	^#method == argType! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 5/29/2008 15:51'!
isRemote
	^remoteNode notNil! !

!TempVariableNode methodsFor: 'testing'!
isTemp
	^ true! !

!TempVariableNode methodsFor: 'testing'!
isUndefTemp
	^ hasDefs not! !

!TempVariableNode methodsFor: 'testing'!
isUnusedTemp
	^ hasRefs not! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 5/29/2008 15:51'!
remoteNode
	^remoteNode! !

!TempVariableNode methodsFor: 'testing' stamp: 'eem 5/30/2008 12:31'!
scope
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	^scope! !

!TempVariableNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitTempVariableNode: self! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 8/3/2009 18:54'!
addReadWithin: scopeBlock "<BlockNode>" at: location "<Integer>"
	readingScopes ifNil: [readingScopes := Dictionary new].
	(readingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 8/3/2009 18:55'!
addWriteWithin: scopeBlock "<BlockNode>" at: location "<Integer>"
	writingScopes ifNil: [writingScopes := Dictionary new].
	(writingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2009 15:07'!
analyseClosure: rootNode "<MethodNode>"
	"Analyse whether the temporary needs to be made remote
	 or not, and answer whether it was made remote.
	 A temp cannot be local if it is written to remotely,
	 or if it is written to after it is closed-over.  An exception
	 is an inlined block argument that appears to be written
	 remotely but is actually local to a block."
	| latestWrite |
	self isBlockArg ifTrue: [^false].
	remoteNode ifNotNil: [^false]. "If already remote, don't remote a second time"
	latestWrite := 0.
	((writingScopes notNil
	 and: [writingScopes associations anySatisfy: [:assoc|
			[:blockScope :refs|
			refs do: [:write| latestWrite := write max: latestWrite].
			"A temp cannot be local if it is written to remotely."
			blockScope actualScope ~~ definingScope actualScope]
				value: assoc key value: assoc value]])
	or: [readingScopes notNil
		and: [readingScopes associations anySatisfy: [:assoc|
				[:blockScope :refs|
				 "A temp cannot be local if it is written to after it is closed-over."
				 blockScope actualScope ~~ definingScope actualScope
				 and: [refs anySatisfy: [:read| read < latestWrite]]]
					value: assoc key value: assoc value]]]) ifTrue:
		[remoteNode := definingScope addRemoteTemp: self rootNode: rootNode.
		 ^true].
	^false! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 10:56'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	self addReadWithin: scopeBlock at: rootNode locationCounter! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2009 11:46'!
beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	self addWriteWithin: scopeBlock at: rootNode locationCounter.
	"For analysis of optimized blocks also record the set of temporaries written to
	 within optimized blocks so that additional writes can be added at locations that
	 represent subsequent iterations of the loop. e.g. testInlineBlockCollectionSD1"
	assignmentPools keysAndValuesDo:
		[:outerScopeBlock :set|
		"definingScope can be nil in expr in expr ifNil: [:arg|...] expressions because
		 arg gets its definingScope set when [:arg|...] is analysed."
		outerScopeBlock actualScope
			= (definingScope
				ifNil: [scopeBlock]
				ifNotNil: [definingScope actualScope]) ifTrue:
			[set add: self]]! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 7/20/2009 16:33'!
definingScope
	^definingScope! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'topa 7/6/2015 17:22'!
definingScope: scopeBlock "<BlockNode>"
	definingScope = scopeBlock ifTrue: [^ self]. "No need to bail"
	definingScope ifNotNil:
		[self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2009 13:26'!
index: anInteger
	"For renumbering temps in the closure compiler."
	index := anInteger.
	code := self code: index type: LdTempType! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 7/20/2009 14:27'!
isDefinedWithinBlockExtent: anInterval
	^anInterval rangeIncludes: definingScope actualScope blockExtent first! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 6/2/2008 16:50'!
isIndirectTempVector
	^false! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 18:07'!
isReferencedWithinBlockExtent: anInterval 
	readingScopes ~~ nil ifTrue:
		[readingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	^false! !

!TempVariableNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 18:01'!
referenceScopesAndIndicesDo: aBinaryBlock
	"Evaluate aBinaryBlock with all read or write scopes and locations.
	 This is used to copy the reference information into RemoteTempVectorNodes"
	readingScopes ~~ nil ifTrue:
		[readingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]]! !

!TempVariableNode methodsFor: 'decompiler' stamp: 'eem 9/25/2008 09:45'!
remoteNode: aRemoteTempVectorNode
	remoteNode := aRemoteTempVectorNode! !

!TempVariableNode methodsFor: '*Etoys-tiles' stamp: 'ab 7/13/2004 13:54'!
explanation

	^(self isArg ifTrue: ['Method argument'] ifFalse: ['Temporary variable']),' <',self name,'>'
! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	remoteNode ~~ nil ifTrue:
		[remoteNode emitCodeForLoadFor: self stack: stack encoder: encoder]! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:53'!
emitCodeForStore: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStoreInto: self stack: stack encoder: encoder].
	encoder genStoreTemp: index! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:54'!
emitCodeForStorePop: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStorePopInto: self stack: stack encoder: encoder].
	encoder genStorePopTemp: index.
	stack pop: 1! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:53'!
emitCodeForValue: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForValueOf: self stack: stack encoder: encoder].
	encoder genPushTemp: index.
	stack push: 1! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForLoad: encoder forValue: forValue
	^remoteNode
		ifNil: [0]
		ifNotNil: [remoteNode sizeCodeForLoadFor: self encoder: encoder]! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:52'!
sizeCodeForStore: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStoreInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStoreTemp: index! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:52'!
sizeCodeForStorePop: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStorePopInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStorePopTemp: index! !

!TempVariableNode methodsFor: 'code generation' stamp: 'eem 5/20/2008 14:51'!
sizeCodeForValue: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForValueOf: self encoder: encoder].
	self reserve: encoder.
	^encoder sizePushTemp: index! !


!RemoteTempVectorNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:34'!
accept: aVisitor
	^aVisitor visitRemoteTempVectorNode: self! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/29/2008 16:19'!
addRemoteTemp: aTempVariableNode encoder: encoder
	remoteTemps isNil ifTrue:
		[remoteTemps := OrderedCollection new].
	remoteTemps addLast: aTempVariableNode.
	aTempVariableNode referenceScopesAndIndicesDo:
		[:scopeBlock "<BlockNode>" :location "<Integer>"|
		 self addReadWithin: scopeBlock at: location].
	encoder supportsClosureOpcodes ifFalse:
		[encoder encodeLiteral: remoteTemps size.
		 readNode := encoder encodeSelector: #at:.
		 writeNode := encoder encodeSelector: #at:put:]! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:20'!
emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder
	self assert: encoder supportsClosureOpcodes not.
	(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode))
		emitCodeForValue: stack encoder: encoder! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:19'!
emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^self].
	"Need to generate the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	super emitCodeForValue: stack encoder: encoder.
	self emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 23:02'!
emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[writeNode
				emitCode: stack
				args: 2
				encoder: encoder
				super: false]! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 23:02'!
emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[self emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder.
			 encoder genPop].
	stack pop: 1! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 23:20'!
emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genPushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index.
			 stack push: 1]
		ifFalse:
			[self emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder.
			 readNode
				emitCode: stack
				args: 1
				encoder: encoder
				super: false]! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 6/2/2008 16:50'!
isIndirectTempVector
	^true! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 9/25/2008 17:16'!
nodeToInitialize: encoder
	^AssignmentNode new
		variable: self
		value: (encoder supportsClosureOpcodes
					ifTrue: [NewArrayNode new numElements: remoteTemps size]
					ifFalse:
						[MessageNode new
							receiver: (encoder encodeVariable: 'Array')
							selector: #new:
							arguments: (Array with: (encoder encodeLiteral: remoteTemps size))
							precedence: 3
							from: encoder])! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 17:57'!
referenceScopesAndIndicesDo: aBinaryBlock
	self shouldNotImplement! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 9/8/2008 10:47'!
scope
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:
		-3	an indirect temp vector"
	^-3! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:20'!
sizeCodeForIndexOf: aTempVariableNode encoder: encoder
	self assert: encoder supportsClosureOpcodes not.
	^(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode)) sizeCodeForValue: encoder! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:19'!
sizeCodeForLoadFor: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^0].
	"Need to size the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	^(super sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:24'!
sizeCodeForStoreInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^writeNode sizeCode: encoder args: 2 super: false! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:23'!
sizeCodeForStorePopInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForStoreInto: aTempVariableNode encoder: encoder)
	+ encoder sizePop! !

!RemoteTempVectorNode methodsFor: 'code generation (closures)' stamp: 'eem 5/24/2008 18:23'!
sizeCodeForValueOf: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizePushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)
	+ (readNode sizeCode: encoder args: 1 super: false)! !

!RemoteTempVectorNode methodsFor: 'printing' stamp: 'jmv 3/2/2010 17:13'!
printDefinitionForClosureAnalysisOn: aStream 
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes ifNotNil: [
		refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asSortedCollection do: [:read| aStream nextPutAll: ' r@'; print: read]].
	remoteTemps
		do: [:rt| rt printDefinitionForClosureAnalysisOn: aStream]
		separatedBy: [aStream nextPut: $,; space].
	aStream nextPut: $}! !

!RemoteTempVectorNode methodsFor: 'accessing' stamp: 'eem 6/2/2008 16:47'!
remoteTemps
	^remoteTemps! !

!RemoteTempVectorNode methodsFor: 'decompiler' stamp: 'eem 9/25/2008 09:46'!
remoteTemps: anArray
	remoteTemps := anArray.
	anArray do: [:tempNode| tempNode remoteNode: self]! !


!UndeclaredVariableNode methodsFor: 'visiting' stamp: 'eem 8/13/2010 16:51'!
accept: aVisitor
	^aVisitor visitUndeclaredVariableNode: self! !

!UndeclaredVariableNode methodsFor: 'testing' stamp: 'eem 8/13/2010 15:34'!
isUndeclared
	^true! !

!UndeclaredVariableNode methodsFor: 'accessing' stamp: 'eem 8/17/2010 16:30'!
tag
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	^tag! !

!UndeclaredVariableNode methodsFor: 'accessing' stamp: 'eem 8/17/2010 16:30'!
tag: anObject
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	tag := anObject! !


!MessageNode methodsFor: 'cascading'!
cascadeReceiver
	"Nil out rcvr (to indicate cascade) and return what it had been."

	| rcvr |
	rcvr := receiver.
	receiver := nil.
	^rcvr! !

!MessageNode methodsFor: 'cascading' stamp: 'bf 2/19/2015 18:20'!
ensureCanCascade: encoder
	special > 0 ifTrue:
		[special := 0.
		receiver := originalReceiver.
		selector := encoder encodeSelector: originalSelector.
		arguments := originalArguments.
		receiver isBlockNode ifTrue: [receiver deoptimize].
		arguments do:
			[:each|
			each isBlockNode ifTrue:
				[each deoptimize]]]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 9/29/2008 14:45'!
emitCodeForCase: stack encoder: encoder value: forValue

	| braceNode sizeStream allReturn |
	forValue ifFalse:
		[^super emitCodeForEffect: stack encoder: encoder].
	braceNode := arguments first.
	sizeStream := ReadStream on: sizes.
	receiver emitCodeForValue: stack encoder: encoder.
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last | | thenSize elseSize |
		thenSize := sizeStream next.
		elseSize := sizeStream next.
		last ifFalse: [encoder genDup. stack push: 1].
		keyNode emitCodeForEvaluatedValue: stack encoder: encoder.
		equalNode emitCode: stack args: 1 encoder: encoder.
		self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
		last ifFalse: [encoder genPop. stack pop: 1].
		valueNode emitCodeForEvaluatedValue: stack encoder: encoder.
		last ifTrue: [stack pop: 1].
		valueNode returns ifFalse:
			[self emitCodeForJump: elseSize encoder: encoder.
			 allReturn := false].
		(last and: [allReturn]) ifTrue:
			[self emitCodeForJump: elseSize encoder: encoder]].
	arguments size = 2
		ifTrue:
			[arguments last emitCodeForEvaluatedValue: stack encoder: encoder] "otherwise: [...]"
		ifFalse:
			[NodeSelf emitCodeForValue: stack encoder: encoder.
			caseErrorNode emitCode: stack args: 0 encoder: encoder]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 8/15/2010 10:23'!
emitCodeForEffect: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: false]
		ifFalse: 
			[super emitCodeForEffect: stack encoder: encoder]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:12'!
emitCodeForIf: stack encoder: encoder value: forValue
	| thenExpr thenSize elseExpr elseSize |
	thenSize := sizes at: 1.
	elseSize := sizes at: 2.
	(forValue not and: [elseSize * thenSize > 0]) ifTrue:
		"Two-armed IFs forEffect share a single pop"
		[^super emitCodeForEffect: stack encoder: encoder].
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
			pc := encoder methodStreamPosition.
			thenExpr emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1.  "then and else alternate; they don't accumulate"
			thenExpr returns not ifTrue:
				"...not ifTrue: avoids using ifFalse: alone during this compile)"
				"Elide jump over else after a return"
				[self emitCodeForJump: elseSize encoder: encoder].
			elseExpr emitCodeForEvaluatedValue: stack encoder: encoder]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[thenSize > 0
				ifTrue:
					[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					thenExpr emitCodeForEvaluatedEffect: stack encoder: encoder]
				ifFalse:
					[self emitCodeForBranchOn: true dist: elseSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					elseExpr emitCodeForEvaluatedEffect: stack encoder: encoder]]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:28'!
emitCodeForIfNil: stack encoder: encoder value: forValue

	| theNode theSize ifNotNilSelector |
	theNode := arguments first.
	theSize := sizes at: 1.
	ifNotNilSelector := #ifNotNil:.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue ifTrue: [encoder genDup. stack push: 1].
	encoder genPushSpecialLiteral: nil. stack push: 1.
	equalNode emitCode: stack args: 1 encoder: encoder.
	self 
		emitCodeForBranchOn: (selector key == ifNotNilSelector)
		dist: theSize 
		pop: stack 
		encoder: encoder.
	pc := encoder methodStreamPosition.
	forValue 
		ifTrue: 
			[encoder genPop. stack pop: 1.
			theNode emitCodeForEvaluatedValue: stack encoder: encoder]	
		ifFalse: [theNode emitCodeForEvaluatedEffect: stack encoder: encoder]! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 2/3/2011 21:12'!
emitCodeForRepeat: stack encoder: encoder value: forValue 
	" L1: ... Jmp(L1)"
	| loopSize |
	loopSize := sizes at: 1.
	receiver emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 2/20/2012 20:50'!
emitCodeForToDo: stack encoder: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt limitInit test block incStmt blockSize |
	initStmt := arguments at: 4.
	limitInit := arguments at: 7.
	test := arguments at: 5.
	block := arguments at: 3.
	incStmt := arguments at: 6.
	blockSize := sizes at: 1.
	loopSize := sizes at: 2.
	limitInit == nil
		ifFalse: [limitInit emitCodeForEffect: stack encoder: encoder].
		
	"This will return the receiver of to:do: which is the initial value of the loop"
	forValue
		ifTrue: [initStmt emitCodeForValue: stack encoder: encoder.]
		ifFalse: [initStmt emitCodeForEffect: stack encoder: encoder].
	test emitCodeForValue: stack encoder: encoder.
	self emitCodeForBranchOn: false dist: blockSize pop: stack encoder: encoder.
	pc := encoder methodStreamPosition.
	block emitCodeForEvaluatedEffect: stack encoder: encoder.
	incStmt emitCodeForEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 8/15/2010 10:23'!
emitCodeForValue: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: true]
		ifFalse: 
			[receiver ~~ nil ifTrue: [receiver emitCodeForValue: stack encoder: encoder].
			arguments do: [:argument | argument emitCodeForValue: stack encoder: encoder].
			pc := encoder methodStreamPosition + 1. "debug pc is first byte of the send, i.e. the next byte".
			selector
				emitCode: stack
				args: arguments size
				encoder: encoder
				super: receiver == NodeSuper]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:36'!
emitCodeForWhile: stack encoder: encoder value: forValue 
	"L1: ... Bfp(L2)|Btp(L2) ... Jmp(L1) L2: "
	| cond stmt stmtSize loopSize |
	cond := receiver.
	stmt := arguments at: 1.
	stmtSize := sizes at: 1.
	loopSize := sizes at: 2.
	cond emitCodeForEvaluatedValue: stack encoder: encoder.
	self emitCodeForBranchOn: (selector key == #whileFalse:)  "Bfp for whileTrue"
					dist: stmtSize pop: stack encoder: encoder.   "Btp for whileFalse"
	pc := encoder methodStreamPosition.
	stmt emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 12/27/2009 03:11'!
sizeCodeForCase: encoder value: forValue

	| braceNode sizeIndex elseSize allReturn |
	forValue not ifTrue:
		[^super sizeCodeForEffect: encoder].
	equalNode := encoder encodeSelector: #=.
	braceNode := arguments first.
	sizes := Array new: 2 * braceNode numElements.
	sizeIndex := sizes size.
	elseSize := arguments size = 2
		ifTrue:
			[arguments last sizeCodeForEvaluatedValue: encoder] "otherwise: [...]"
		ifFalse:
			[caseErrorNode := encoder encodeSelector: #caseError.
			 (NodeSelf sizeCodeForValue: encoder)
			 + (caseErrorNode sizeCode: encoder args: 0 super: false)]. "self caseError"
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last |
		valueNode returns ifFalse: [allReturn := false]].
	braceNode casesReverseDo:
		[:keyNode :valueNode :last | | thenSize |
		sizes at: sizeIndex put: elseSize.
		thenSize := valueNode sizeCodeForEvaluatedValue: encoder.
		last ifFalse: [thenSize := thenSize + encoder sizePop].
		valueNode returns ifFalse: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		(last and: [allReturn]) ifTrue: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		sizes at: sizeIndex-1 put: thenSize.
		last ifFalse: [elseSize := elseSize + encoder sizeDup].
		elseSize := elseSize
					+ (keyNode sizeCodeForEvaluatedValue: encoder)
					+ (equalNode sizeCode: encoder args: 1 super: false)
					+ (self sizeCode: encoder forBranchOn: false dist: thenSize)
					+ thenSize.
		sizeIndex := sizeIndex - 2].
	^(receiver sizeCodeForValue: encoder) + elseSize! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 3/31/2011 00:24'!
sizeCodeForEffect: encoder

	special > 0 
		ifTrue:
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: false].
	^super sizeCodeForEffect: encoder! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 17:15'!
sizeCodeForIf: encoder value: forValue
	| thenExpr elseExpr branchSize thenSize elseSize |
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	(forValue
	 or: [(thenExpr isJust: NodeNil)
	 or: [elseExpr isJust: NodeNil]]) not
			"(...not ifTrue: avoids using ifFalse: alone during this compile)"
		ifTrue:  "Two-armed IFs forEffect share a single pop"
			[^super sizeCodeForEffect: encoder].
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[elseSize := elseExpr sizeCodeForEvaluatedValue: encoder.
			thenSize := (thenExpr sizeCodeForEvaluatedValue: encoder)
					+ (thenExpr returns
						ifTrue: [0]  "Elide jump over else after a return"
						ifFalse: [self sizeCode: encoder forJump: elseSize]).
			branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[(elseExpr isJust: NodeNil)
				ifTrue:
					[elseSize := 0.
					thenSize := thenExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
				ifFalse:
					[thenSize := 0.
					elseSize := elseExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: true dist: elseSize]].
	sizes := Array with: thenSize with: elseSize.
	^(receiver sizeCodeForValue: encoder)
	+ branchSize + thenSize + elseSize! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/15/2008 09:57'!
sizeCodeForIfNil: encoder value: forValue

	| theNode theSize theSelector |
	equalNode := encoder encodeSelector: #==.
	sizes := Array new: 1.
	theNode := arguments first.
	theSelector := #ifNotNil:.
	forValue
		ifTrue:
			[sizes at: 1 put: (theSize := (encoder sizePop + (theNode sizeCodeForEvaluatedValue: encoder))).
			 ^(receiver sizeCodeForValue: encoder)
			 + encoder sizeDup
			 + (encoder sizePushSpecialLiteral: nil)
			 + (equalNode sizeCode: encoder args: 1 super: false)
			 + (self 
					sizeCode: encoder forBranchOn: selector key == theSelector 
					dist: theSize)
			 + theSize]
		ifFalse:
			[sizes at: 1 put: (theSize := (theNode sizeCodeForEvaluatedEffect: encoder)).
			 ^(receiver sizeCodeForValue: encoder)
				+ (encoder sizePushSpecialLiteral: nil)
				+ (equalNode sizeCode: encoder args: 1 super: false)
				+ (self 
					sizeCode: encoder
					forBranchOn: selector key == theSelector 
					dist: theSize)
				+ theSize]! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:25'!
sizeCodeForRepeat: encoder value: forValue 
	"L1: ... Jmp(L1) nil (nil for value only);"
	| loopSize |
	"We assume long backward branches are always maximal size branches."
	loopSize := (receiver sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	sizes := Array with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 2/20/2012 20:49'!
sizeCodeForToDo: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := Array with: blockSize with: loopSize.
	^initSize
	+ loopSize! !

!MessageNode methodsFor: 'code generation' stamp: 'nice 3/31/2011 00:25'!
sizeCodeForValue: encoder
	| total |
	special > 0 
		ifTrue: 
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: true].
	receiver == NodeSuper
		ifTrue: [selector := selector copy "only necess for splOops"].
	total := selector sizeCode: encoder args: arguments size super: receiver == NodeSuper.
	receiver == nil 
		ifFalse: [total := total + (receiver sizeCodeForValue: encoder)].
	sizes := arguments collect: 
					[:arg | | argSize | 
					argSize := arg sizeCodeForValue: encoder.
					total := total + argSize.
					argSize].
	^total! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 11/5/2012 16:02'!
sizeCodeForWhile: encoder value: forValue 
	"L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only);
	justStmt, wholeLoop, justJump."
	| cond stmt stmtSize loopSize branchSize |
	cond := receiver.
	stmt := arguments at: 1.
	"We assume long backward branches are always maximal size branches."
	stmtSize := (stmt sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	branchSize := self
					sizeCode: encoder
					forBranchOn: selector key == #whileFalse:  "Btp for whileFalse"
					dist: stmtSize.
	loopSize := (cond sizeCodeForEvaluatedValue: encoder) + branchSize + stmtSize.
	sizes := Array with: stmtSize with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!MessageNode methodsFor: 'equation translation'!
arguments
	^arguments! !

!MessageNode methodsFor: 'equation translation' stamp: 'tk 10/27/2000 15:11'!
arguments: list
	arguments := list! !

!MessageNode methodsFor: 'equation translation' stamp: 'eem 9/25/2008 14:50'!
eval
	"When everything in me is a constant, I can produce a value.  This is only used by the Scripting system (TilePadMorph tilesFrom:in:)"

	| rec args |
	receiver isVariableNode ifFalse: [^ #illegal].
	rec := receiver key value.
	args := arguments collect: [:each | each eval].
	^ rec perform: selector key withArguments: args! !

!MessageNode methodsFor: 'equation translation'!
receiver
	^receiver! !

!MessageNode methodsFor: 'equation translation' stamp: 'RAA 2/14/2001 14:07'!
receiver: val
	"14 feb 2001 - removed return arrow"

	receiver := val! !

!MessageNode methodsFor: 'equation translation'!
selector
	^selector! !

!MessageNode methodsFor: 'initialize-release' stamp: 'nice 3/2/2011 23:57'!
receiver: rcvr selector: selNode arguments: args precedence: p 
	"Decompile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := selNode key.
	selNode code == #macro
		ifTrue: [self noteSpecialSelector: selNode key]
		ifFalse: [special := 0].
	selector := selNode.
	"self pvtCheckForPvtSelector: encoder"
	"We could test code being decompiled, but the compiler should've checked already. And where to send the complaint?"! !

!MessageNode methodsFor: 'initialize-release' stamp: 'nice 3/2/2011 23:55'!
receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder 
	"Compile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := aSelector.
	self noteSpecialSelector: aSelector.
	(self transform: encoder)
		ifTrue: 
			[selector isNil ifTrue:
				[selector := SelectorNode new 
								key: (MacroSelectors at: special)
								code: #macro]]
		ifFalse: 
			[selector := encoder encodeSelector: aSelector.
			rcvr == NodeSuper ifTrue: [encoder noteSuper]].
	self pvtCheckForPvtSelector: encoder! !

!MessageNode methodsFor: 'initialize-release' stamp: 'eem 7/18/2008 16:26'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	((selName == #future) or:[selName == #future:]) ifTrue:
		[Smalltalk at: #FutureNode ifPresent:
			[:futureNode|
			^futureNode new
				receiver: rcvr
				selector: selName
				arguments: args
				precedence: p
				from: encoder
				sourceRange: range]].
	(rcvr isFutureNode
	 and: [rcvr futureSelector == nil]) ifTrue:
		"Transform regular message into future"
		[^rcvr futureMessage: selName
			arguments: args
			from: encoder
			sourceRange: range].

	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !

!MessageNode methodsFor: 'initialize-release' stamp: 'tk 10/26/2000 15:37'!
selector: sel
	selector := sel! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 5/23/2008 13:14'!
noteSpecialSelector: selectorSymbol
	"special > 0 denotes specially treated (potentially inlined) messages. "

	special := MacroSelectors indexOf: selectorSymbol.
! !

!MessageNode methodsFor: 'macro transformations' stamp: 'nice 1/12/2011 22:07'!
toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: (Array with: variable).
	variable scope: -1.
	variable beBlockArg.
	^MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence! !

!MessageNode methodsFor: 'macro transformations'!
transform: encoder
	special = 0 ifTrue: [^false].
	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue: 
			[^true]
		ifFalse: 
			[special := 0. ^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 9/6/2009 13:33'!
transformAnd: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:20'!
transformBoolean: encoder
	^self
		checkBlock: (arguments at: 1)
		as: 'argument'
		from: encoder
		maxArgs: 0! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:22'!
transformCase: encoder

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: BraceNode) ifFalse: [^false].
	(caseNode blockAssociationCheck: encoder) ifFalse: [^false].
	(arguments size = 1
	 or: [self checkBlock: arguments last as: 'otherwise arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].
	 caseNode elements do:
		[:messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self].
	 arguments size = 2 ifTrue:
		[arguments last noteOptimizedIn: self].
	 ^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 9/6/2009 19:19'!
transformIfFalse: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:23'!
transformIfFalseIfTrue: encoder
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:25'!
transformIfNil: encoder

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := SelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := Array
						with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
						with: (arguments first noteOptimizedIn: self).
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:25'!
transformIfNilIfNotNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder maxArgs: 0)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:26'!
transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 9/6/2009 13:30'!
transformIfTrue: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:23'!
transformIfTrueIfFalse: encoder
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 9/6/2009 19:18'!
transformOr: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeTrue) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 11/13/2012 09:55'!
transformRepeat: encoder
	"answer true if this #repeat message can be optimized"
	
	^(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0)
	   and: [receiver noteOptimizedIn: self.
			true]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 6/19/2015 08:22'!
transformToDo: encoder
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange blockRange limitIsAssignedTo |
	block := arguments last.
	"First check for valid arguments"
	(block notNil
	 and: [block isBlockNode
	 and: [block numberOfArguments = 1
	 and: [block firstArgument isVariableReference "As with debugger remote vars"]]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	(limit := arguments at: 1) isVariableReference ifTrue:
		[limitIsAssignedTo := false.
		 block nodesDo:
			[:node|
			(node isAssignmentNode and: [node variable = limit]) ifTrue:
				[limitIsAssignedTo := true]].
		 limitIsAssignedTo ifTrue:
			[^false]].
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := SelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	blockRange := encoder rawSourceRanges at: block ifAbsent: [1 to: 0].
	blockVar := block firstArgument.
	initStmt := AssignmentNode new variable: blockVar value: receiver.
	limit isVariableReference | limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := AssignmentNode new
							variable: limit
							value: arguments first].
	test := MessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: {limit}
				precedence: precedence
				from: encoder
				sourceRange: (myRange first to: blockRange first).
	incStmt := AssignmentNode new
				variable: blockVar
				value: (MessageNode new
							receiver: blockVar selector: #+
							arguments: {increment}
							precedence: precedence
							from: encoder
							sourceRange: (myRange last to: (myRange last max: blockRange last)))
				from: encoder
				sourceRange: (myRange last to: (myRange last max: blockRange last)).
	arguments := {limit. increment. block. initStmt. test. incStmt. limitInit}.
	block noteOptimizedIn: self.
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:24'!
transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := SelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]! !

!MessageNode methodsFor: 'printing' stamp: 'RAA 6/9/2000 18:06'!
asMorphicCaseOn: parent indent: ignored
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"

	| braceNode otherwise |

	braceNode := arguments first.
	otherwise := arguments last.
	((arguments size = 1) or: [otherwise isJustCaseError]) ifTrue: [
		self morphFromKeywords: #caseOf: arguments: {braceNode} on: parent indent: nil.
		^parent
	].
	self morphFromKeywords: #caseOf:otherwise: arguments: arguments on: parent indent: nil.
	^parent
! !

!MessageNode methodsFor: 'printing' stamp: 'RAA 2/15/2001 19:25'!
macroPrinter

	special > 0 ifTrue: [^MacroPrinters at: special].
	^nil
! !

!MessageNode methodsFor: 'printing'!
precedence

	^precedence! !

!MessageNode methodsFor: 'printing' stamp: 'eem 5/6/2008 13:56'!
printCaseOn: aStream indent: level 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printOn: aStream indent: level]
		ifFalse: 
			[aStream nextPutAll: '{'; crtab: level + 1.
			 braceNode casesForwardDo:
				[:keyNode :valueNode :last | 
				keyNode printOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 crtab: level + 1]]].
	otherwise notNil ifTrue:
		[aStream crtab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						 1]
					ifFalse: [0].
		 otherwise printOn: aStream indent: level + 1 + extra]! !

!MessageNode methodsFor: 'printing' stamp: 'di 5/1/2000 23:20'!
printIfNil: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array with: arguments first)
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'di 5/1/2000 23:20'!
printIfNilNotNil: aStream indent: level

	self printReceiver: receiver ifNilReceiver on: aStream indent: level.

	(arguments first isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNotNil:
				arguments: { arguments second }
				on: aStream indent: level].
	(arguments second isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNil:
				arguments: { arguments first }
				on: aStream indent: level].
	^ self printKeywords: #ifNil:ifNotNil:
			arguments: arguments
			on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 5/6/2008 13:57'!
printIfOn: aStream indent: level

	receiver ifNotNil:
		[receiver printOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifTrue: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printKeywords: #and: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifFalse: arguments: (Array with: arguments last)
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printKeywords: #or: arguments: (Array with: arguments last)
					on: aStream indent: level].
	self printKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 9/25/2008 15:41'!
printKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue:
		[aStream space; nextPutAll: key.
		 ^self].
	keywords := key keywords.
	doCrTab := args size > 2
				or: [{receiver} , args anySatisfy:
						[:thisArg |
						thisArg notNil
						and: [thisArg isBlockNode
							 or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]]].
	1 to: (args size min: keywords size) do:
		[:i |
		arg := args at: i.
		kwd := keywords at: i.
		doCrTab
			ifTrue: [aStream crtab: level+1. indent := 1] "newline after big args"
			ifFalse: [aStream space. indent := 0].
		aStream nextPutAll: kwd; space.
		arg printOn: aStream
			indent: level + 1 + indent
			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 9/25/2008 16:12'!
printOn: aStream indent: level
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printReceiver: receiver on: aStream indent: level.
	selector isForFFICall
		ifTrue:
			[aStream space.
			 selector
				printAsFFICallWithArguments: arguments
				on: aStream
				indent: 0]
		ifFalse:
			[self printKeywords: selector key
				 arguments: arguments
				 on: aStream
				 indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'di 5/30/2000 23:17'!
printOn: strm indent: level precedence: outerPrecedence

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printOn: strm indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 9/25/2008 14:51'!
printParenReceiver: rcvr on: aStream indent: level
					
	rcvr isBlockNode ifTrue:
		[^rcvr printOn: aStream indent: level].
	aStream nextPut: $(.
	rcvr printOn: aStream indent: level.
	aStream nextPut: $)
! !

!MessageNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:09'!
printReceiver: rcvr on: aStream indent: level
					
	rcvr ifNil: [^ self].

	"Force parens around keyword receiver of kwd message"
	rcvr printOn: aStream indent: level precedence: precedence! !

!MessageNode methodsFor: 'printing' stamp: 'nice 2/3/2011 20:57'!
printRepeatOn: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array new)
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'ul 11/15/2010 11:53'!
printToDoOn: aStream indent: level

	| limitNode |
	self printReceiver: receiver on: aStream indent: level.

	(arguments last == nil or: [(arguments last isMemberOf: AssignmentNode) not])
		ifTrue: [limitNode := arguments first]
		ifFalse: [limitNode := arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 11/5/2012 15:01'!
printWhileOn: aStream indent: level
	self printReceiver: receiver on: aStream indent: level.
	self
		printKeywords: originalSelector
		arguments: originalArguments
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisCaseOn: aStream indent: level 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printWithClosureAnalysisOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printWithClosureAnalysisOn: aStream indent: level]
		ifFalse: 
			[aStream nextPutAll: '{'; crtab: level + 1.
			 braceNode casesForwardDo:
				[:keyNode :valueNode :last | 
				keyNode printWithClosureAnalysisOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printWithClosureAnalysisOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 crtab: level + 1]]].
	otherwise notNil ifTrue:
		[aStream crtab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						 1]
					ifFalse: [0].
		 otherwise printWithClosureAnalysisOn: aStream indent: level + 1 + extra]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisIfNil: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	^self printWithClosureAnalysisKeywords: selector key
		arguments: (Array with: arguments first)
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisIfNilNotNil: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver ifNilReceiver on: aStream indent: level.

	(arguments first isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifNotNil:
				arguments: { arguments second }
				on: aStream indent: level].
	(arguments second isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifNil:
				arguments: { arguments first }
				on: aStream indent: level].
	^self printWithClosureAnalysisKeywords: #ifNil:ifNotNil:
			arguments: arguments
			on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisIfOn: aStream indent: level

	receiver ifNotNil:
		[receiver printWithClosureAnalysisOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifTrue: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printWithClosureAnalysisKeywords: #and: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifFalse: arguments: (Array with: arguments last)
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printWithClosureAnalysisKeywords: #or: arguments: (Array with: arguments last)
					on: aStream indent: level].
	self printWithClosureAnalysisKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 9/25/2008 14:51'!
printWithClosureAnalysisKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue: [aStream space; nextPutAll: key. ^self].
	keywords := key keywords.
	doCrTab := args size > 2
				or: [{receiver} , args anySatisfy:
						[:thisArg |
						thisArg isBlockNode
						or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]].
	1 to: (args size min: keywords size) do:
		[:i |
		arg := args at: i.
		kwd := keywords at: i.
		doCrTab
			ifTrue: [aStream crtab: level+1. indent := 1] "newline after big args"
			ifFalse: [aStream space. indent := 0].
		aStream nextPutAll: kwd; space.
		arg printWithClosureAnalysisOn: aStream
			indent: level + 1 + indent
			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: aStream indent: level
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	self printWithClosureAnalysisKeywords: selector key
		 arguments: arguments
		 on: aStream
		 indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisOn: strm indent: level precedence: outerPrecedence

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printWithClosureAnalysisOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printWithClosureAnalysisOn: strm indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 9/25/2008 14:53'!
printWithClosureAnalysisParenReceiver: rcvr on: aStream indent: level
					
	rcvr isBlockNode ifTrue:
		[^rcvr printWithClosureAnalysisOn: aStream indent: level].
	aStream nextPut: $(.
	rcvr printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPut: $)! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisReceiver: rcvr on: aStream indent: level
					
	rcvr ifNil: [^self].

	"Force parens around keyword receiver of kwd message"
	rcvr printWithClosureAnalysisOn: aStream indent: level precedence: precedence! !

!MessageNode methodsFor: 'printing' stamp: 'ul 11/15/2010 11:54'!
printWithClosureAnalysisToDoOn: aStream indent: level

	| limitNode |
	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	limitNode := (arguments last == nil
				or: [arguments last isAssignmentNode not])
					ifTrue: [arguments first]
					ifFalse: [arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printWithClosureAnalysisKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printWithClosureAnalysisKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisWhileOn: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := SelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := Array new].
	self printWithClosureAnalysisKeywords: selector key arguments: arguments
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'di 5/2/2000 00:16'!
test

	3 > 4 ifTrue: [4+5 between: 6 and: 7]
			ifFalse: [4 between: 6+5 and: 7-2]! !

!MessageNode methodsFor: 'testing' stamp: 'eem 2/3/2011 09:08'!
canCascade

	^receiver ~~ NodeSuper! !

!MessageNode methodsFor: 'testing'!
isComplex
	
	^(special between: 1 and: 10) or: [arguments size > 2 or: [receiver isComplex]]! !

!MessageNode methodsFor: 'testing' stamp: 'md 7/27/2006 19:09'!
isMessage
	^true! !

!MessageNode methodsFor: 'testing'!
isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred
	 evaluate to true with respect to receiver and the list of arguments.  If selSymbol or
	 either predicate is nil, it means 'don't care'.  Note that argsPred takes numArgs
	 arguments.  All block arguments are ParseNodes."

	^(selSymbol isNil or: [selSymbol==selector key]) and:
		[(rcvrPred isNil or: [rcvrPred value: receiver]) and:
			[(argsPred isNil or: [argsPred valueWithArguments: arguments])]]! !

!MessageNode methodsFor: 'testing' stamp: 'ar 7/10/2009 22:42'!
isMessageNode
	^true! !

!MessageNode methodsFor: 'testing' stamp: 'eem 9/23/2008 14:06'!
isNilIf

	^(special between: 3 and: 4)
	   and: [(arguments first returns or: [arguments first isJust: NodeNil])
	   and: [(arguments last returns or: [arguments last isJust: NodeNil])]]! !

!MessageNode methodsFor: 'testing' stamp: 'eem 7/20/2009 09:31'!
isOptimized
	^special > 0! !

!MessageNode methodsFor: 'testing' stamp: 'eem 7/20/2009 10:44'!
isOptimizedLoop
	^special > 0
	   and: [#(transformWhile: transformToDo:) includes: (MacroTransformers at: special)]! !

!MessageNode methodsFor: 'testing' stamp: 'eem 9/26/2008 12:39'!
isReturningIf

	^((special between: 3 and: 4) "ifTrue:ifFalse:/ifFalse:ifTrue:"
	    or: [special between: 17 and: 18]) "ifNil:ifNotNil:/ifNotNil:ifNil:"
		and: [arguments first returns and: [arguments last returns]]! !

!MessageNode methodsFor: 'testing'!
toDoIncrement: variable
	(receiver = variable and: [selector key = #+]) 
		ifFalse: [^ nil].
	arguments first isConstantNumber
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]! !

!MessageNode methodsFor: 'testing'!
toDoLimit: variable
	(receiver = variable and: [selector key = #<= or: [selector key = #>=]]) 
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]! !

!MessageNode methodsFor: 'tiles' stamp: 'RAA 2/14/2001 22:26'!
morphFromKeywords: key arguments: args on: parent indent: ignored

	^parent
		messageNode: self 
		receiver: receiver 
		selector: selector 
		keywords: key 
		arguments: args
! !

!MessageNode methodsFor: 'private' stamp: 'ul 2/20/2011 17:17'!
checkBlock: node as: nodeName from: encoder maxArgs: maxArgs
	"Answer true if node is a BlockNode with at most maxArgs arguments.
	This check is required in order to inline some special messages.
	Notify some undue usage of these special messages."

	node isBlockNode ifFalse: [ ^false ].
	node numberOfArguments <= maxArgs ifTrue: [ ^true ].
	^encoder notify: '<- ', nodeName , ' of ' , (MacroSelectors at: special) , ' has too many arguments'! !

!MessageNode methodsFor: 'private' stamp: 'acg 1/28/2000 00:57'!
ifNilReceiver

	^receiver! !

!MessageNode methodsFor: 'private' stamp: 'tk 8/2/1999 18:40'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isPvtSelector ifTrue:
		[receiver isSelfPseudoVariable ifFalse:
			[encoder notify: 'Private messages may only be sent to self']].! !

!MessageNode methodsFor: 'private' stamp: 'bf 2/19/2015 18:20'!
receiver: rcvr arguments: args precedence: p

	receiver := rcvr.
	originalReceiver := rcvr copy.
	arguments := args.
	originalArguments := arguments copy.
	sizes := Array new: arguments size.
	precedence := p! !

!MessageNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:35'!
accept: aVisitor
	^aVisitor visitMessageNode: self! !

!MessageNode methodsFor: 'visiting' stamp: 'eem 9/23/2008 21:52'!
argumentsInEvaluationOrder
	"Answer the receivers arguments in evaluation order.
	 If the receiver is a transformed to:do: node this will undo the misordering done by the transformation."
	^(special > 0
	   and: [(MacroTransformers at: special) == #transformToDo:
	   and: [arguments size >= 7]])
		"arguments are in a weid order and may be nil in a transformed to:do: loop.  sigh...
		 c.f. emitCodeForToDo:encoder:value:"
		ifTrue:
			[(arguments at: 7)	"limitInit"
				ifNil: [{	(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]
				ifNotNil: [:limitInit|
						{ limitInit.
						(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]]
		ifFalse:
			[arguments]! !

!MessageNode methodsFor: 'code generation (closures)' stamp: 'eem 8/4/2009 11:58'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Assignments within optimized loops are tricky.  Because a loop repeats a
	 write to a temporary in an optimized loop effectively occurs after the loop.
	 To handle this collect the set of temps assigned to in optimized loops and
	 add extra writes after traversing the optimized loop constituents."
	| writtenToTemps |
	self isOptimizedLoop ifTrue:
		[{ receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[assignmentPools at: node put: Set new]]].
	"receiver is nil in cascades"
	receiver == nil ifFalse:
		[receiver analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools].
	arguments do:
		[:node|
		node == nil ifFalse: "last argument of optimized to:do: can be nil"
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]].
	"Add assignments representing subsequent iterations
	 and redo the closure analysis for the written-to temps."
	self isOptimizedLoop ifTrue:
		[writtenToTemps := Set new.
		 { receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[(assignmentPools removeKey: node) do:
					[:temp|
					temp isBlockArg ifFalse: "ignore added assignments to to:do: loop args"
						[writtenToTemps add: temp.
						 temp addWriteWithin: node at: rootNode locationCounter]]]].
		 writtenToTemps isEmpty ifFalse:
			[(writtenToTemps asSortedCollection: ParseNode tempSortBlock) do:
				[:each| each analyseClosure: rootNode].
			 (writtenToTemps collect: [:each| each definingScope]) do:
				[:blockNode|
				blockNode ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode]]]! !


!MessageNode class methodsFor: 'class initialization' stamp: 'nice 2/3/2011 21:27'!
initialize
	"MessageNode initialize"
	MacroSelectors := 
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:
			repeat ).
	MacroTransformers := 
		#(	transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase:
			transformIfNil: transformIfNil:  transformIfNilIfNotNil: transformIfNotNilIfNil:
			transformRepeat: ).
	MacroEmitters := 
		#(	emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForToDo:encoder:value: emitCodeForToDo:encoder:value:
			emitCodeForCase:encoder:value: emitCodeForCase:encoder:value:
			emitCodeForIfNil:encoder:value: emitCodeForIfNil:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForRepeat:encoder:value:).
	MacroSizers := 
		#(	sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value:
			sizeCodeForToDo:value: sizeCodeForToDo:value:
			sizeCodeForCase:value: sizeCodeForCase:value:
			sizeCodeForIfNil:value: sizeCodeForIfNil:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForRepeat:value:).
	MacroPrinters := 
		#(	printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent:
			printIfNil:indent: printIfNil:indent: printIfNilNotNil:indent: printIfNilNotNil:indent:
			printRepeatOn:indent:)! !


!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'di 3/22/1999 09:38'!
asStorableNode: encoder
	"This node is a message masquerading as a temporary variable.
	It currently has the form {homeContext tempAt: offset}.
	We need to generate code for {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack.
	This, in turn will get turned into {homeContext tempAt: offset put: expr}
	at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)"
	^ MessageAsTempNode new
		receiver: nil  "suppress code generation for reciever already on stack"
		selector: #storeAt:inTempFrame:
		arguments: (arguments copyWith: receiver)
		precedence: precedence
		from: encoder! !

!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'di 10/12/1999 17:29'!
code
	"Allow synthetic temp nodes to be sorted by code"
	^ arguments first literalValue! !

!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'di 3/22/1999 09:39'!
emitStorePop: stack on: codeStream
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^ self emitForEffect: stack on: codeStream! !

!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'di 3/22/1999 09:39'!
sizeForStorePop: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^ self sizeForEffect: encoder! !

!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'eem 6/24/2008 11:50'!
store: expr from: encoder 
	"ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment).
	For assigning into temps of a context being debugged."

	selector key ~= #namedTempAt: 
		ifTrue: [^self error: 'cant transform this message'].
	^ MessageAsTempNode new
		receiver: receiver
		selector: #namedTempAt:put:
		arguments: (arguments copyWith: expr)
		precedence: precedence
		from: encoder! !

!MessageAsTempNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
emitCodeForStorePop: stack encoder: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self emitCodeForEffect: stack encoder: encoder! !

!MessageAsTempNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForStorePop: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self sizeCodeForEffect: encoder! !


!MethodNode methodsFor: 'code generation'!
encoder
	^ encoder! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:01'!
generate
	"The receiver is the root of a parse tree. Answer a CompiledMethod. The
	argument, trailer, is the references to the source code that is stored with 
	every CompiledMethod."

	^self generate: CompiledMethodTrailer empty using: CompiledMethod! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:00'!
generate: trailer 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:01'!
generate: trailer ifQuick: methodBlock
	^self generate: trailer using: CompiledMethod ifQuick: methodBlock! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 8/10/2014 13:00'!
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits literals stack method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	encoder supportsClosureOpcodes ifTrue:
		[self ensureClosureAnalysisDone.
		 encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"].
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	method := aCompiledMethodClass
				newBytes: blkSize
				trailerBytes: trailer 
				nArgs: arguments size
				nTemps: (encoder supportsClosureOpcodes
							ifTrue: [| locals |
									locals := arguments,
											  temporaries,
											  (primErrNode
												ifNil: [#()]
												ifNotNil: [{primErrNode}]).
									encoder
										noteBlockExtent: block blockExtent
										hasLocals: locals.
									locals size]
							ifFalse: [encoder maxTemp])
				nStack: 0
				nLits: (nLits := (literals := encoder allLiterals) size)
				primitive: primitive.
	nLits > 255 ifTrue:
		[^self error: 'Too many literals referenced'].
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: encoder].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 14:59'!
generate: trailer using: aCompiledMethodClass ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 2/8/2013 14:02'!
generateWithTempNames
	"Answer a CompiledMethod with temps names encoded in its trailer."
	| methodSansTempNames |
	"The encoder computes the schematic temp names during generation, so 
	 generate a method without temp names first.  If the method is quick there are
	 no temps and hence no temp names."
	methodSansTempNames := self
									generate: CompiledMethodTrailer empty
									using: CompiledMethod.
	^methodSansTempNames
		copyWithTrailerBytes:
			((CompiledMethodTrailer new tempNames:
				(methodSansTempNames isQuick
					ifTrue: ['']
					ifFalse: [encoder schematicTempNamesString])))! !

!MethodNode methodsFor: 'code generation' stamp: 'ajh 7/6/2003 15:25'!
parserClass
	"Which parser produces this class of parse node"

	^ Parser! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 11/29/2008 18:53'!
properties
	^properties! !

!MethodNode methodsFor: 'code generation' stamp: 'yo 8/30/2002 14:07'!
selector 
	"Answer the message selector for the method represented by the receiver."

	(selectorOrFalse isSymbol)
		ifTrue: [^selectorOrFalse].
	^selectorOrFalse key.
! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/25/2008 15:20'!
selectorNode
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: SelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [SelectorNode new key: selectorOrFalse]! !

!MethodNode methodsFor: 'initialize-release' stamp: 'tk 8/3/1999 12:47'!
block
	^ block! !

!MethodNode methodsFor: 'initialize-release' stamp: 'ajh 1/24/2003 17:37'!
selector: symbol

	selectorOrFalse := symbol! !

!MethodNode methodsFor: 'initialize-release' stamp: 'jcg 9/30/2009 00:55'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim 
	
	self 
		selector: selOrFalse
		arguments: args
		precedence: p
		temporaries: temps
		block: blk encoder:
		anEncoder 
		primitive: prim 
		properties: AdditionalMethodState new.! !

!MethodNode methodsFor: 'initialize-release' stamp: 'ar 1/4/2002 00:23'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.! !

!MethodNode methodsFor: 'initialize-release' stamp: 'ajh 1/22/2003 17:53'!
sourceText: stringOrText

	sourceText := stringOrText! !

!MethodNode methodsFor: 'printing' stamp: 'ajh 1/22/2003 17:39'!
methodClass

	^ encoder classEncoding! !

!MethodNode methodsFor: 'printing' stamp: 'eem 12/1/2008 14:36'!
printOn: aStream
	| selectorNode |
	selectorNode := self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords with: arguments do:
				[:kwd :arg |
				aStream nextPutAll: kwd; space; nextPutAll: arg key; space]].
	comment == nil ifFalse:
		[aStream crtab: 1.
		 self printCommentOn: aStream indent: 1].
	block printTemporaries: temporaries on: aStream doPrior: [aStream crtab: 1].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream crtab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream crtab: 1.
	block printStatementsOn: aStream indent: 0! !

!MethodNode methodsFor: 'printing' stamp: 'eem 12/1/2008 14:35'!
printPragmasOn: aStream
	properties ifNil: [^self].
	properties pragmas do:
		[:pragma|
		"Primitives are printed in printPrimitiveOn:; skip these"
		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream crtab: 1.
			 pragma printOn: aStream]]! !

!MethodNode methodsFor: 'printing' stamp: 'eem 12/19/2014 10:08'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', (interpreterClass primitiveTable at: primitive + 1), '" ']! !

!MethodNode methodsFor: 'printing' stamp: 'eem 12/1/2008 14:25'!
printPropertiesOn: aStream
	properties ifNil: [^self].
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream crtab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]! !

!MethodNode methodsFor: 'printing' stamp: 'eem 8/15/2010 10:36'!
printWithClosureAnalysisOn: aStream 
	self ensureClosureAnalysisDone.
	precedence = 1
		ifTrue: 
			[(self selector includesSubString: '()/')
				ifTrue: [aStream nextPutAll: (self selector copyUpTo: $)).
						arguments
							do: [:arg| aStream nextPutAll: arg key]
							separatedBy: [aStream nextPutAll: ', '].
						aStream nextPut: $)]
				ifFalse: [aStream nextPutAll: self selector]]  "no node for method selector"
		ifFalse: 
			[self selector keywords with: arguments do: 
				[:kwd :arg | 
				aStream nextPutAll: kwd; space.
				arg printDefinitionForClosureAnalysisOn: aStream.
				aStream space]].
	comment == nil ifFalse: 
			[aStream crtab: 1.
			 self printCommentOn: aStream indent: 1].
	temporaries size > 0 ifTrue: 
			[aStream crtab: 1; nextPut: $|.
			temporaries do: [:temp | 
				aStream space.
				temp printDefinitionForClosureAnalysisOn: aStream].
			aStream space; nextPut: $|].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream crtab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream crtab: 1.
	block printWithClosureAnalysisStatementsOn: aStream indent: 0! !

!MethodNode methodsFor: 'printing' stamp: 'ajh 1/24/2003 17:41'!
sourceText

	^ sourceText ifNil: [self printString]! !

!MethodNode methodsFor: 'printing'!
tempNames
	^ encoder tempNames! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 7/21/2009 16:00'!
arguments
	"For transformations etc, not used in compilation"
	^arguments! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 7/21/2009 16:00'!
arguments: aSequence
	"For transformations etc, not used in compilation"
	arguments := aSequence! !

!MethodNode methodsFor: 'accessing' stamp: 'md 7/27/2006 19:12'!
body
	^block! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 1/19/2009 10:28'!
primitiveErrorVariableName
	"Answer the primitive error code temp name, or nil if none."
	(primitive isInteger and: [primitive > 0]) ifTrue:
		[properties pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil

	"(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: Parser) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' error: foo module: ''bar''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' module: ''bar'' error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate"! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 6/11/2009 17:27'!
removeProperty: aSymbol
	properties := properties copyWithout: (Association
											key: aSymbol
											value: (properties propertyValueAt: aSymbol))! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 7/21/2009 15:59'!
temporaries
	"For transformations etc, not used in compilation"
	^temporaries! !

!MethodNode methodsFor: 'accessing' stamp: 'eem 7/21/2009 15:59'!
temporaries: aSequence
	"For transformations etc, not used in compilation"
	temporaries := aSequence! !

!MethodNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:35'!
accept: aVisitor
	^aVisitor visitMethodNode: self! !

!MethodNode methodsFor: 'converting' stamp: 'eem 7/18/2008 06:37'!
asColorizedSmalltalk80Text
	"Answer a colorized Smalltalk-80-syntax string description of the parse tree whose root is the receiver."

	| printText |
	printText := self printString asText.
	^(Smalltalk at: #SHTextStylerST80 ifAbsent: [nil])
		ifNotNil: [:stylerClass| stylerClass new styledTextFor: printText]
		ifNil: [printText]! !

!MethodNode methodsFor: 'converting' stamp: 'ar 4/17/2010 16:50'!
decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^self fullPrintString
! !

!MethodNode methodsFor: 'converting' stamp: 'eem 5/6/2008 15:17'!
decompileText 
	"Answer a string description of the parse tree whose root is the receiver."

	^self asColorizedSmalltalk80Text! !

!MethodNode methodsFor: 'converting' stamp: 'eem 9/27/2011 15:15'!
ensureNotQuick
	"If the receiver is quick (can be generated as a Quick method, ^self,^inst var or ^ special constant)
	 make it not so.  This is used to create break-pointable versions of quick methods.   Answer if the
	 receiver was quick."
	^block ensureNotQuick: encoder! !

!MethodNode methodsFor: 'converting' stamp: 'eem 8/31/2010 11:54'!
preen
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.
	 Currently one case, hiding the assignment to the arg of an inlined block arg to ifNotNil:,
		(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...]."

	self preenLocalIfNotNilArg! !

!MethodNode methodsFor: 'converting' stamp: 'eem 8/31/2010 12:36'!
preenLocalIfNotNilArg
	"Try and spot a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| varsToHide |
	varsToHide := Set new.
	self nodesDo:
		[:node| | variable |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not
		and: [variable isOnlySubnodeOf: node in: self]]]]]]]) ifTrue:
			[node arguments last arguments isEmpty
				ifTrue: [node arguments last arguments: { variable }.
						varsToHide add: variable]
				ifFalse: [self assert: node arguments last arguments asArray =  { variable }].
			 node receiver receiver: node receiver receiver value]].
	varsToHide notEmpty ifTrue:
		[self nodesDo:
			[:node|
			((node == self or: [node isBlockNode])
			and: [node temporaries anySatisfy: [:temp| varsToHide includes: temp]]) ifTrue:
				[node temporaries: (node temporaries reject: [:temp| varsToHide includes: temp])]]]! !

!MethodNode methodsFor: 'source mapping' stamp: 'eem 6/4/2008 19:21'!
rawSourceRanges

	^self rawSourceRangesAndMethodDo: [:rawSourceRanges :method| rawSourceRanges]! !

!MethodNode methodsFor: 'source mapping' stamp: 'nice 2/23/2011 21:50'!
rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := encoder classEncoding newParser
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method! !

!MethodNode methodsFor: 'primitive error codes' stamp: 'eem 12/1/2008 14:56'!
removeAndRenameLastTempIfErrorCode
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 temporaries last
			name: primitiveErrorVariableName
			key: primitiveErrorVariableName
			code: temporaries last code.
		 temporaries removeLast].! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 5/29/2008 15:27'!
addLocalsToPool: locals "<Set of: TempVariableNode>"
	localsPool isNil ifTrue:
		[localsPool := IdentitySet new].
	localsPool addAll: locals! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2008 10:04'!
ensureClosureAnalysisDone
	block blockExtent ifNil:
		[temporaries := block analyseArguments: arguments temporaries: temporaries rootNode: self]! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 5/20/2008 13:43'!
locationCounter
	^locationCounter! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 5/30/2008 11:27'!
noteBlockEntry: aBlock
	"Evaluate aBlock with the numbering for the block entry."
	locationCounter isNil ifTrue:
		[locationCounter := -1].
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 6/2/2008 12:12'!
noteBlockExit: aBlock
	"Evaluate aBlock with the numbering for the block exit."
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2! !

!MethodNode methodsFor: 'code generation (closures)' stamp: 'eem 5/29/2008 16:07'!
referencedValuesWithinBlockExtent: anInterval 
	^(localsPool select:
		[:temp|
		 temp isReferencedWithinBlockExtent: anInterval]) collect:
			[:temp|
			temp isRemote ifTrue: [temp remoteNode] ifFalse: [temp]]! !

!MethodNode methodsFor: 'debugger support' stamp: 'nice 2/23/2011 21:51'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := encoder classEncoding newParser
						encoderClass: encoder class;
						parse: (sourceText ifNil: [self decompileString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]! !

!MethodNode methodsFor: 'debugger support' stamp: 'eem 7/1/2009 13:45'!
hasGeneratedMethod
	^encoder hasGeneratedMethod! !

!MethodNode methodsFor: 'debugger support' stamp: 'Igor.Stasenko 12/20/2009 03:58'!
schematicTempNamesString
	"Answer the temp names for the current method node in a form that captures
	 temp structure.  The temps at each method and block scope level occur
	 space-separated, with any indirect temps enclosed in parentheses.  Each block
	 level is enclosed in square brackets.  e.g.
		'method level temps (indirect temp)[block args and temps (indirect)]'
	 This representation can be reconstituted into a blockExtentsToTempsMap
	 by a CompiledMethod that has been copied with the schematicTempNamesString."
	encoder hasGeneratedMethod ifFalse:
		["create the encoder's blockExtentsToLoals map, except if the method is quick
		  in which case it has no temps."
		(self generate) isQuick ifTrue:
			[^'']].
	^encoder schematicTempNamesString! !


!NewArrayNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:35'!
accept: aVisitor
	^aVisitor visitNewArrayNode: self! !

!NewArrayNode methodsFor: 'code generation (closures)' stamp: 'eem 6/16/2008 09:31'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self! !

!NewArrayNode methodsFor: 'accessing' stamp: 'eem 5/25/2008 14:58'!
numElements
	^numElements! !

!NewArrayNode methodsFor: 'accessing' stamp: 'eem 5/25/2008 14:59'!
numElements: n
	numElements := n! !

!NewArrayNode methodsFor: 'code generation' stamp: 'eem 5/25/2008 14:58'!
emitCodeForValue: stack encoder: encoder
	encoder genPushNewArray: numElements.
	stack push: 1! !

!NewArrayNode methodsFor: 'code generation' stamp: 'eem 5/25/2008 14:58'!
sizeCodeForValue: encoder
	^encoder sizePushNewArray: numElements! !


!ReturnNode methodsFor: 'code generation'!
code

	^expr code! !

!ReturnNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:40'!
emitCodeForReturn: stack encoder: encoder

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition! !

!ReturnNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 16:40'!
emitCodeForValue: stack encoder: encoder

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition! !

!ReturnNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForReturn: encoder

	^expr sizeCodeForReturn: encoder! !

!ReturnNode methodsFor: 'code generation' stamp: 'eem 5/14/2008 14:52'!
sizeCodeForValue: encoder

	^expr sizeCodeForReturn: encoder! !

!ReturnNode methodsFor: 'converting'!
asReturnNode! !

!ReturnNode methodsFor: 'initialize-release'!
expr: e

	expr := e! !

!ReturnNode methodsFor: 'initialize-release'!
expr: e encoder: encoder sourceRange: range

	expr := e.
	encoder noteSourceRange: range forNode: self! !

!ReturnNode methodsFor: 'printing' stamp: 'yo 8/2/2004 17:21'!
expr

	^ expr.
! !

!ReturnNode methodsFor: 'printing' stamp: 'eem 5/6/2008 14:18'!
printOn: aStream indent: level

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printOn: aStream indent: level.
	expr printCommentOn: aStream indent: level! !

!ReturnNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:49'!
printWithClosureAnalysisOn: aStream indent: level

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printWithClosureAnalysisOn: aStream indent: level.
	expr printCommentOn: aStream indent: level! !

!ReturnNode methodsFor: 'testing'!
isReturnSelf

	^expr == NodeSelf! !

!ReturnNode methodsFor: 'testing'!
isSpecialConstant

	^expr isSpecialConstant! !

!ReturnNode methodsFor: 'testing'!
isVariableReference

	^expr isVariableReference! !

!ReturnNode methodsFor: 'visiting' stamp: 'eem 9/6/2009 12:35'!
accept: aVisitor
	^aVisitor visitReturnNode: self! !

!ReturnNode methodsFor: 'code generation (closures)' stamp: 'eem 7/24/2009 09:44'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Note we could do this:
		scopeBlock ~~ rootNode block ifTrue:
			[scopeBlock noteNonLocalReturn].
	 and pass up the flag in <BlockNode>>>analyseTempsWithin:rootNode:
	 which may be fast but will also give less information the debugger.
	 For now we consider clean blocks a premature optimization."
	self flag: 'consider clean blocks'.
	expr analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools! !

!ReturnNode methodsFor: '*Etoys-tiles' stamp: 'RAA 8/15/1999 19:31'!
explanation

	^'Exit this method returning the value of ',expr explanation
! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:10'!
visitAssignmentNode: anAssignmentNode
	"N.B.  since assigment happens after the value is evaluated the value is visited first."
	anAssignmentNode value accept: self.
	anAssignmentNode variable accept: self! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:13'!
visitBlockNode: aBlockNode
	aBlockNode statements do:
		[:statement| statement accept: self]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:14'!
visitBraceNode: aBraceNode
	aBraceNode elements do:
		[:element| element accept: self]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 16:13'!
visitCascadeNode: aCascadeNode
	aCascadeNode receiver accept: self.
	aCascadeNode messages do:
		[:message| self visitMessageNodeInCascade: message]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitCommentNode: aCommentNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitFieldNode: aFieldNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 2/9/2009 10:04'!
visitFutureNode: aFutureNode
	aFutureNode receiver accept: self.
	(aFutureNode originalSelector isKindOf: SelectorNode) ifTrue:
		[aFutureNode originalSelector accept: self]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitInstanceVariableNode: anInstanceVariableNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitLiteralNode: aLiteralNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitLiteralVariableNode: aLiteralVariableNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/5/2009 20:08'!
visitMessageNode: aMessageNode
	aMessageNode receiver accept: self.
	"receiver notNil ifTrue: ''receiver is nil for cascades''
		[receiver accept: self]."
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 2/9/2009 10:04'!
visitMessageNodeInCascade: aMessageNode
	"receiver is nil for cascades"
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:53'!
visitMethodNode: aMethodNode
	aMethodNode block accept: self! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitNewArrayNode: aNewArrayNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitRemoteTempVectorNode: aRemoteTempVectorNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:53'!
visitReturnNode: aReturnNode
	aReturnNode expr accept: self! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitSelectorNode: aSelectorNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:07'!
visitTempVariableNode: aTempVariableNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 8/13/2010 16:50'!
visitUndeclaredVariableNode: aVariableNode! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'eem 5/30/2008 10:32'!
visitVariableNode: aVariableNode! !


!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'initialize-release' stamp: 'eem 9/5/2009 21:03'!
initialize
	inOptimizedBlock := false! !

!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'accessing' stamp: 'eem 9/8/2008 14:30'!
readBeforeWritten
	^readBeforeWritten ifNil: [IdentitySet new]! !

!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'visiting' stamp: 'eem 9/10/2008 15:28'!
visitAssignmentNode: anAssignmentNode
	anAssignmentNode value accept: self.
	anAssignmentNode variable isTemp
		ifTrue:
			[written ifNil: [written := IdentitySet new].
			 written add: anAssignmentNode variable]
		ifFalse:
			[anAssignmentNode variable accept: self]! !

!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'visiting' stamp: 'eem 9/5/2009 22:09'!
visitBlockNode: aBlockNode
	| savedWritten |
	"If we're in the optimized block in one side of an optimized ifTrue:ifFalse: et al
	 leave it to the enclosing visitMessageNode: activation to handle merging written."
	inOptimizedBlock ifTrue:
		[^super visitBlockNode: aBlockNode].
	"If we're not then don't update written because without evaluating the guard(s)
	 we can't tell if the block is evaluated or not, and we must avoid false positives."
	savedWritten := written copy.
	super visitBlockNode: aBlockNode.
	written := savedWritten! !

!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'visiting' stamp: 'eem 9/5/2009 22:03'!
visitMessageNode: aMessageNode
	| savedWritten writtenPostFirstArm |
	(aMessageNode isOptimized
	 and: [#(ifTrue:ifFalse: ifFalse:ifTrue: ifNil:ifNotNil: ifNotNil:ifNil:) includes: aMessageNode selector key]) ifFalse:
		[^super visitMessageNode: aMessageNode].
	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	savedWritten := written copy.
	aMessageNode argumentsInEvaluationOrder
		do: [:argument|
			argument isBlockNode
				ifTrue: [| savedIOB |
					savedIOB := inOptimizedBlock.
					inOptimizedBlock := true.
					[argument accept: self]
						ensure: [inOptimizedBlock := savedIOB]]
				ifFalse: [argument accept: self]]
		separatedBy:
			[writtenPostFirstArm := written.
			 written := savedWritten].
	(written notNil
	 and: [writtenPostFirstArm notNil]) ifTrue:
		[written := written intersection: writtenPostFirstArm]! !

!OptimizedBlockLocalTempReadBeforeWrittenVisitor methodsFor: 'visiting' stamp: 'eem 9/8/2008 14:43'!
visitTempVariableNode: aTempVariableNode
	(aTempVariableNode isArg
	 or: [written notNil
		and: [written includes: aTempVariableNode]]) ifTrue:
		[^self].
	readBeforeWritten ifNil:
		[readBeforeWritten := IdentitySet new].
	readBeforeWritten add: aTempVariableNode! !


!ParseNodeEnumerator methodsFor: 'initialize-release' stamp: 'eem 7/20/2009 19:44'!
ofBlock: aBlock
	theBlock := aBlock! !

!ParseNodeEnumerator methodsFor: 'initialize-release' stamp: 'eem 8/31/2010 11:24'!
ofBlock: aBlock select: aSelectBlock
	theBlock := aBlock.
	theSelectBlock := aSelectBlock! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitAssignmentNode: anAssignmentNode
	(theSelectBlock isNil or: [theSelectBlock value: anAssignmentNode]) ifFalse:
		[^nil].
	theBlock value: anAssignmentNode.
	^super visitAssignmentNode: anAssignmentNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitBlockNode: aBlockNode
	(theSelectBlock isNil or: [theSelectBlock value: aBlockNode]) ifFalse:
		[^nil].
	theBlock value: aBlockNode.
	^super visitBlockNode: aBlockNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitBraceNode: aBraceNode
	(theSelectBlock isNil or: [theSelectBlock value: aBraceNode]) ifFalse:
		[^nil].
	theBlock value: aBraceNode.
	^super visitBraceNode: aBraceNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitCascadeNode: aCascadeNode
	(theSelectBlock isNil or: [theSelectBlock value: aCascadeNode]) ifFalse:
		[^nil].
	theBlock value: aCascadeNode.
	^super visitCascadeNode: aCascadeNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitCommentNode: aCommentNode
	(theSelectBlock isNil or: [theSelectBlock value: aCommentNode]) ifFalse:
		[^nil].
	theBlock value: aCommentNode.
	^super visitCommentNode: aCommentNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitFieldNode: aFieldNode
	(theSelectBlock isNil or: [theSelectBlock value: aFieldNode]) ifFalse:
		[^nil].
	theBlock value: aFieldNode.
	^super visitFieldNode: aFieldNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitFutureNode: aFutureNode
	(theSelectBlock isNil or: [theSelectBlock value: aFutureNode]) ifFalse:
		[^nil].
	theBlock value: aFutureNode.
	^super visitFutureNode: aFutureNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitInstanceVariableNode: anInstanceVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: anInstanceVariableNode]) ifFalse:
		[^nil].
	theBlock value: anInstanceVariableNode.
	^super visitInstanceVariableNode: anInstanceVariableNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitLiteralNode: aLiteralNode
	(theSelectBlock isNil or: [theSelectBlock value: aLiteralNode]) ifFalse:
		[^nil].
	theBlock value: aLiteralNode.
	^super visitLiteralNode: aLiteralNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitLiteralVariableNode: aLiteralVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aLiteralVariableNode]) ifFalse:
		[^nil].
	theBlock value: aLiteralVariableNode.
	^super visitLiteralVariableNode: aLiteralVariableNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitMessageNode: aMessageNode
	(theSelectBlock isNil or: [theSelectBlock value: aMessageNode]) ifFalse:
		[^nil].
	theBlock value: aMessageNode.
	^super visitMessageNode: aMessageNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitMessageNodeInCascade: aMessageNodeInCascade
	(theSelectBlock isNil or: [theSelectBlock value: aMessageNodeInCascade]) ifFalse:
		[^nil].
	theBlock value: aMessageNodeInCascade.
	^super visitMessageNodeInCascade: aMessageNodeInCascade! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitMethodNode: aMethodNode
	(theSelectBlock isNil or: [theSelectBlock value: aMethodNode]) ifFalse:
		[^nil].
	theBlock value: aMethodNode.
	^super visitMethodNode: aMethodNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitNewArrayNode: aNewArrayNode
	(theSelectBlock isNil or: [theSelectBlock value: aNewArrayNode]) ifFalse:
		[^nil].
	theBlock value: aNewArrayNode.
	^super visitNewArrayNode: aNewArrayNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitRemoteTempVectorNode: aRemoteTempVectorNode
	(theSelectBlock isNil or: [theSelectBlock value: aRemoteTempVectorNode]) ifFalse:
		[^nil].
	theBlock value: aRemoteTempVectorNode.
	^super visitRemoteTempVectorNode: aRemoteTempVectorNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitReturnNode: aReturnNode
	(theSelectBlock isNil or: [theSelectBlock value: aReturnNode]) ifFalse:
		[^nil].
	theBlock value: aReturnNode.
	^super visitReturnNode: aReturnNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitSelectorNode: aSelectorNode
	(theSelectBlock isNil or: [theSelectBlock value: aSelectorNode]) ifFalse:
		[^nil].
	theBlock value: aSelectorNode.
	^super visitSelectorNode: aSelectorNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitTempVariableNode: aTempVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aTempVariableNode]) ifFalse:
		[^nil].
	theBlock value: aTempVariableNode.
	^super visitTempVariableNode: aTempVariableNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'eem 8/31/2010 11:13'!
visitVariableNode: aVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aVariableNode]) ifFalse:
		[^nil].
	theBlock value: aVariableNode.
	^super visitVariableNode: aVariableNode! !


!ParseNodeEnumerator class methodsFor: 'instance creation' stamp: 'eem 7/20/2009 19:45'!
ofBlock: aBlock
	^self new ofBlock: aBlock! !

!ParseNodeEnumerator class methodsFor: 'instance creation' stamp: 'eem 8/31/2010 11:43'!
ofBlock: aBlock select: selectBlock
	^self new ofBlock: aBlock select: selectBlock! !


!VariableScopeFinder methodsFor: 'private' stamp: 'eem 8/12/2010 16:50'!
enclosingNodeFor: enumerator of: rootNode
	"Answer the minimum enclosing root node for aVariabe or nil if none.
	 If the variable is accessed in more than one subnode then the rootNode is the
	 enclosing node, otherwise it is which ever single subnode node that includes it, if any.
	 enumerator applies its argument to all relevant subnodes of rootNode."
	| enclosingNodeOrNil |
	enclosingNodeOrNil := nil.
	enumerator value:
		[:subnode|
		(subnode accept: self) ifNotNil:
			[:enclosingNode|
			enclosingNodeOrNil := enclosingNodeOrNil
										ifNil: [enclosingNode]
										ifNotNil: [rootNode]]].
	^enclosingNodeOrNil! !

!VariableScopeFinder methodsFor: 'initialize-release' stamp: 'eem 8/12/2010 16:18'!
ofVariable: aVariableNode
	theVariable := aVariableNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 16:51'!
visitAssignmentNode: anAssignmentNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then anAssignmentNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock
								value: anAssignmentNode value;
								value: anAssignmentNode variable]
		of: anAssignmentNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 16:52'!
visitBlockNode: aBlockNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one statement then aBlockNode is the
	 enclosing node, otherwise it is which ever single block node that includes it, if any."
	^(self enclosingNodeFor: [:aBlock| aBlockNode statements do: aBlock] of: aBlockNode) ifNotNil:
		[:aNode|
		aNode isBlockNode ifTrue: [aNode] ifFalse: [aBlockNode]]! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 16:52'!
visitBraceNode: aBraceNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aBraceNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock| aBraceNode elements do: aBlock]
		of: aBraceNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/16/2010 18:02'!
visitCascadeNode: aCascadeNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aCascadeNode receiver.
							aCascadeNode messages do:
								[:msg| msg argumentsInEvaluationOrder do: aBlock]]
		of: aCascadeNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitCommentNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitFieldNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 16:55'!
visitFutureNode: aFutureNode
	^aFutureNode receiver accept: self! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitInstanceVariableNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitLiteralNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 19:03'!
visitLiteralVariableNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 17:07'!
visitMessageNode: aMessageNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aMessageNode receiver.
							aMessageNode argumentsInEvaluationOrder do: aBlock]
		of: aMessageNode! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 17:00'!
visitMethodNode: aMethodNode
	^aMethodNode block accept: self! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitNewArrayNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitRemoteTempVectorNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 17:01'!
visitReturnNode: aReturnNode
	^aReturnNode expr accept: self! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitSelectorNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 18:43'!
visitTempVariableNode: aNode
	^nil! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/13/2010 16:50'!
visitUndeclaredVariableNode: aVariableNode
	^theVariable name = aVariableNode name ifTrue: [theVariable]! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'eem 8/12/2010 19:06'!
visitVariableNode: aVariableNode
	^nil! !


!ParseStack methodsFor: 'accessing'!
pop: n

	(position := position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']! !

!ParseStack methodsFor: 'accessing' stamp: 'eem 9/12/2008 10:31'!
position: n 
	(position := n) > length
		ifTrue: [length := position]! !

!ParseStack methodsFor: 'accessing'!
push: n

	(position := position + n) > length 
		ifTrue: [length := position]! !

!ParseStack methodsFor: 'accessing'!
size

	^length! !

!ParseStack methodsFor: 'initialize-release'!
init

	length := position := 0! !

!ParseStack methodsFor: 'printing'!
printOn: aStream
	
	super printOn: aStream.
	aStream nextPutAll: ' at '; print: position; nextPutAll: ' of '; print: length! !

!ParseStack methodsFor: 'results'!
position

	^position! !


!Scanner methodsFor: 'error handling' stamp: 'nice 4/2/2010 19:36'!
ambiguousSelector: aString inRange: anInterval
	"Compile with backward compatibility: $- is part of literal argument.."
	
	token := token asSymbol.
	^self! !

!Scanner methodsFor: 'error handling'!
notify: string 
	"Refer to the comment in Object|notify:." 
	self error: string! !

!Scanner methodsFor: 'error handling' stamp: 'nice 2/25/2010 02:56'!
notify: string at: posiiton
	"Parser compatible message"
	 
	^self notify: string ! !

!Scanner methodsFor: 'error handling'!
offEnd: aString 
	"Parser overrides this"

	^self notify: aString! !

!Scanner methodsFor: 'expression types'!
advance

	| prevToken |
	prevToken := token.
	self scanToken.
	^prevToken! !

!Scanner methodsFor: 'expression types' stamp: 'nice 8/13/2012 23:30'!
scanAllTokenPositionsInto: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments."

	| lastMark |
	lastMark := 1.
	[currentComment ifNotNil:
		[currentComment do:
			[:cmnt| | idx |
			 idx := source originalContents indexOfSubCollection: cmnt startingAt: lastMark.
			 (idx > 0 and: [idx < mark]) ifTrue:
				[aBlock value: idx - 1 value: (lastMark := idx + cmnt size)]].
		 currentComment := nil].
	mark ifNotNil:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue:
			[| savedMark |
			 savedMark := mark.
			 self scanToken.
			 token := token negated.
			 mark := savedMark].
		"Compensate for the fact that the parser uses two character lookahead.  Normally we must
		  remove the extra two characters.  But this mustn't happen for the last token at the end of stream."
		 aBlock
			value: mark
			value: source position - (aheadChar == DoItCharacter
				ifTrue: [hereChar == DoItCharacter
					ifTrue: [0]
					ifFalse: [1]]
				ifFalse: [2])].
	 (tokenType == #rightParenthesis
	  or: [tokenType == #doIt]) ifTrue:
		[^self].
	tokenType == #leftParenthesis
		ifTrue: 
			[self scanToken; scanAllTokenPositionsInto: aBlock]
		ifFalse: 
			[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
				ifTrue: 
					[self scanLitWord.
					 token == #true ifTrue: [token := true].
					 token == #false ifTrue: [token := false].
					 token == #nil ifTrue: [token := nil]]
				ifFalse:
					[(token == #- 
					  and: [(self typeTableAt: hereChar) == #xDigit])
						ifTrue: 
							[self scanToken.
							 token := token negated]]].
		self scanToken ] repeat! !

!Scanner methodsFor: 'expression types' stamp: 'ul 3/15/2011 04:02'!
scanLitByteVec
	| stream |
	stream := (ByteArray new: 16) writeStream.
	[ tokenType == #rightBracket or: [ tokenType == #doIt ] ] whileFalse: [
		tokenType == #word
			ifTrue: [ self scanLitWord ].
		(token isInteger and: [ token between: 0 and: 255 ])
			ifFalse: [ ^ self offEnd: '8-bit integer or right bracket expected' ].
		stream nextPut: token.
		self scanToken ].
	token := stream contents! !

!Scanner methodsFor: 'expression types' stamp: 'nice 9/6/2013 00:15'!
scanLitVec
	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]] whileFalse:
		[tokenType == #leftParenthesis
			ifTrue: 
				[self scanToken; scanLitVec]
			ifFalse: 
				[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
					ifTrue: 
						[self scanLitWord.
						token == #true ifTrue: [token := true].
						token == #false ifTrue: [token := false].
						token == #nil ifTrue: [token := nil]]
					ifFalse:
						[(token isCharacter and: [tokenType ~~ #character])
							ifTrue: [token := token asSymbol]
							ifFalse: [(token == #- 
								  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
									[self scanToken.
									 token := token negated]]]].
		s nextPut: token.
		self scanToken].
	token := s contents! !

!Scanner methodsFor: 'expression types' stamp: 'ul 4/3/2011 02:11'!
scanLitWord
	"Accumulate keywords and asSymbol the result."

	token := (String streamContents: [ :stream |
		stream nextPutAll: token.
		[ (self typeTableAt: hereChar) == #xLetter ] whileTrue: [
			self xLetter.
			stream nextPutAll: token ] ]) asSymbol! !

!Scanner methodsFor: 'expression types' stamp: 'ul 3/27/2011 22:17'!
scanStringStruct

	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]]
		whileFalse: 
			[tokenType == #leftParenthesis
				ifTrue: 
					[self scanToken; scanStringStruct]
				ifFalse: 
					[tokenType == #word ifFalse:
						[^self error: 'only words and parens allowed']].
			s nextPut: token.
			self scanToken].
	token := s contents! !

!Scanner methodsFor: 'expression types' stamp: 'nice 9/5/2013 23:28'!
scanToken

	[(tokenType := self typeTableAt: hereChar) == #xDelimiter]
		whileTrue: [self step].  "Skip delimiters fast, there almost always is one."
	mark := aheadChar == DoItCharacter
		ifTrue: [hereChar == DoItCharacter
			ifTrue: [source position + 1]
			ifFalse: [source position]]
		ifFalse: [source position - 1].
	(tokenType at: 1) == $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step "else just unique the first char"].
	^token! !

!Scanner methodsFor: 'expression types' stamp: 'ul 3/27/2011 23:31'!
step

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := DoItCharacter]
		ifFalse: [aheadChar := source next].
	^c! !

!Scanner methodsFor: 'initialize-release' stamp: 'ul 12/6/2009 19:57'!
initScannerForTokenization
	"Use a version of typeTable that doesn't raise xIllegal when enocuntering an _"
	| underscoreIndex |
	underscoreIndex := typeTable identityIndexOf: #xUnderscore ifAbsent: [^self].
	typeTable := typeTable copy.
	typeTable at: underscoreIndex put: #xUnderscoreForTokenization! !

!Scanner methodsFor: 'initialize-release' stamp: 'ul 3/27/2011 23:07'!
initialize

	super initialize.
	buffer := WriteStream on: (String new: 40).
	typeTable := TypeTable! !

!Scanner methodsFor: 'initialize-release'!
scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source := inputStream.
	self step.
	self step.
	self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/9/2012 14:32'!
typeTableAt: aCharacter
	^typeTable
		at: aCharacter charCode
		ifAbsent:
			[aCharacter == DoItCharacter
				ifTrue: [#doIt]
				ifFalse: [#xLetter]]! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 10/24/2011 18:14'!
xBinary

	| startOfToken |
	tokenType := #binary.
	startOfToken := mark.
	token := String with: self step.
	[(self typeTableAt: hereChar) == #xBinary or: [(self typeTableAt: hereChar) == #verticalBar]] whileTrue:
		[(hereChar == $- and: [(self typeTableAt: aheadChar) == #xDigit])
			ifTrue: [^self ambiguousSelector: (token , '-')
					inRange: (startOfToken to: source position - 1).].
		token := token, (String with: self step)].
	token := token asSymbol! !

!Scanner methodsFor: 'multi-character scans' stamp: 'ul 3/27/2011 23:32'!
xColon
	"Allow := for assignment"
	
	aheadChar == $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol! !

!Scanner methodsFor: 'multi-character scans'!
xDelimiter
	"Ignore blanks, etc."

	self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/9/2012 09:41'!
xDigit
	"Form a number."

	tokenType := #number.
	aheadChar == DoItCharacter
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := (SqNumberParser on: source)
		failBlock: [:errorString :position | self notify: errorString at:position];
		nextNumber.
	self step; step! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/22/2012 03:22'!
xDollar
	"Form a Character literal."

	aheadChar == DoItCharacter
		ifTrue:
			[mark := mark + 1. "Let the notification lie behind the dollar"
			^self offEnd: 'A Character was expected'].
	self step. "pass over $"
	token := self step.
	tokenType := #character! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/9/2012 09:43'!
xDoubleQuote
    "Collect a comment."

	buffer reset.
	self step.
	[ hereChar == $" ] whileFalse: [
		hereChar == DoItCharacter ifTrue: [
			^self offEnd: 'Unmatched comment quote' ].
		buffer nextPut: self step ].
	self step.
	(currentComment ifNil: [ 
		currentComment := OrderedCollection new ])
			add: buffer contents.
	self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 7/25/2010 12:23'!
xIllegal
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar charCode , ' ' , hereChar charCode storeStringHex , ')' at: mark! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/9/2012 13:57'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore and:[self allowUnderscoreSelectors]]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [DoItCharacter]
							ifFalse: [source next]].
	tokenType := (type == #xColon and: [aheadChar ~~ $=])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 9/6/2013 01:45'!
xLitQuote
	"Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'."
	| start |
	start := mark.
	self step. "litQuote"
	self scanToken.
	tokenType == #leftParenthesis
		ifTrue: [self scanToken; scanLitVec.
			mark := start + 1.
			tokenType == #doIt
				ifTrue: [self offEnd: 'Unmatched parenthesis']]
		ifFalse: [tokenType == #leftBracket
				ifTrue: [self scanToken; scanLitByteVec.
					mark := start + 1.
					tokenType == #doIt
						ifTrue: [self offEnd: 'Unmatched bracket']]
				ifFalse: [(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
						ifTrue: [self scanLitWord]
						ifFalse: [(tokenType == #string or: [ tokenType == #verticalBar ])
							ifTrue: [token := token asSymbol]
							ifFalse: [tokenType == #binary 
								ifFalse: [(token isCharacter and: [tokenType ~~ #character and: [self class prefAllowUnicharSymbol]])
									ifTrue: [token := token asSymbol]
									ifFalse: [self notify: 'Invalid literal character' at: start + 1]]]]]].
	mark := start.
	tokenType := #literal

	"#(Pen)
	#Pen
	#'Pen'
	"! !

!Scanner methodsFor: 'multi-character scans' stamp: 'nice 2/9/2012 09:39'!
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar == $' 
		and: [aheadChar == $' 
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: 
			[hereChar == DoItCharacter
				ifTrue: [^self offEnd: 'Unmatched string quote'].
			buffer nextPut: self step].
	self step.
	token := buffer contents.
	tokenType := #string! !

!Scanner methodsFor: 'multi-character scans' stamp: 'eem 11/16/2010 15:05'!
xUnderscore
	self allowUnderscoreAssignments ifTrue:[ | type |
		"Figure out if x _foo (no space between _ and foo) 
		should be a selector or assignment"
		(((type := self typeTableAt: aheadChar) == #xLetter
			or:[type == #xDigit or:[type == #xUnderscore]]) 
			and:[self allowUnderscoreSelectors]) ifFalse:[
				self step.
				tokenType := #leftArrow.
				^token := #'_'
		].
	].
	self allowUnderscoreSelectors ifTrue:[^self xLetter].
	^self xIllegal! !

!Scanner methodsFor: 'multi-character scans' stamp: 'eem 5/8/2008 17:01'!
xUnderscoreForTokenization
	self step.
	tokenType := #leftArrow.
	^token := #'_'! !

!Scanner methodsFor: 'public access' stamp: 'ul 3/27/2011 23:37'!
scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: (ReadStream on: stringOrArray asString).
	strm := WriteStream on: (Array new: 10).
	[tokenType == #doIt]
		whileFalse: 
			[tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"! !

!Scanner methodsFor: 'public access' stamp: 'ul 3/27/2011 22:16'!
scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: (ReadStream on: sourceString asString).
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll! !

!Scanner methodsFor: 'public access'!
scanStringStruct: textOrString 
	"The input is a string whose elements are identifiers and parenthesized
	 groups of identifiers.  Answer an array reflecting that structure, representing
	 each identifier by an uninterned string."

	self scan: (ReadStream on: textOrString asString).
	self scanStringStruct.
	^token

	"Scanner new scanStringStruct: 'a b (c d) (e f g)'"! !

!Scanner methodsFor: 'public access' stamp: 'eem 8/20/2008 20:55'!
scanTokenPositionsIn: textOrString into: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source := (ReadStream on: textOrString asString).
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: (String with: Character cr) with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]'"! !

!Scanner methodsFor: 'public access'!
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self scan: (ReadStream on: textOrString asString).
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"! !

!Scanner methodsFor: 'public access' stamp: 'ul 3/27/2011 23:39'!
typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings. This methiod accepts _ (underscore) as an assignment token
	 irrespective of whether the system prefers := as the assignment token."
	| s |
	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asString).
	s := WriteStream on: (Array new: 16).
	[tokenType == #doIt] whileFalse:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
			[self scanToken.
			 token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"! !

!Scanner methodsFor: 'private' stamp: 'ul 10/28/2010 00:41'!
allowUnderscoreAssignments
	"Query preference"
	^self class allowUnderscoreAsAssignment! !

!Scanner methodsFor: 'private' stamp: 'ar 3/12/2010 15:51'!
allowUnderscoreSelectors
	"Query preference"
	^self class prefAllowUnderscoreSelectors! !


!Scanner class methodsFor: 'testing' stamp: 'sd 3/30/2005 22:00'!
inviolateInstanceVariableNames
	"Answer a list of instance variable names not to be used.  (Place holder for real list)"
	^ #('thisContext' 'self')! !

!Scanner class methodsFor: 'testing' stamp: 'sd 3/30/2005 22:00'!
isLegalInstVarName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((self isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  aString) not]! !

!Scanner class methodsFor: 'testing' stamp: 'nice 11/19/2010 17:22'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."

	| i ascii type next last |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].

	"TypeTable should have been origined at 0 rather than 1 ..."
	ascii := (aSymbol at: 1) asciiValue.
	type := TypeTable at: ascii ifAbsent: [^false].
	type == #xLetter ifTrue: [
		next := last := nil.
		[i > 1]
				whileTrue: 
					[ascii := (aSymbol at: i) asciiValue.
					type := TypeTable at: ascii ifAbsent: [^false].
					(type == #xLetter or: [type == #xDigit or: [type == #xColon
							and: [
								next == nil
									ifTrue: [last := #xColon. true] 
									ifFalse: [last == #xColon and: [next ~~ #xDigit and: [next ~~ #xColon]]]]]])
						ifFalse: [^ false].
					next := type.
					i := i - 1].
			^ true].
	type == #xBinary ifTrue:
		[^(2 to: i) allSatisfy: [:j |
			ascii := (aSymbol at: j) asciiValue.
			(TypeTable at: ascii ifAbsent: []) == #xBinary]].
	type == #verticalBar ifTrue: [^i = 1].
	^false! !

!Scanner class methodsFor: 'testing' stamp: 'fbs 2/13/2006 22:33'!
wellFormedInstanceVariableNameFrom: aString
	"Answer a legal instance variable name, derived from aString"

	| cleansedString |
	cleansedString := aString select: [:ch | ch isDigit or: [ch isLetter]].
	(cleansedString isEmpty or: [cleansedString first isDigit])
		ifTrue: [cleansedString := 'a', cleansedString]
		ifFalse:	[cleansedString := cleansedString withFirstCharacterDownshifted].

	[self isLegalInstVarName: cleansedString] whileFalse:
		[cleansedString := cleansedString, 'x'].
	^ cleansedString

"Scanner wellFormedInstanceVariableNameFrom:  '234 xx\ Uml /ler42342380-4'"! !

!Scanner class methodsFor: 'initialization' stamp: 'nice 2/9/2012 14:54'!
initialize
	
	self initializeTypeTable.
	"The unicode ending with FFFE or FFFF are non characters and can be used by applications if they wish.
	We use last legal unicode 16r10FFFF to encode the end of source stream"
	"DoItCharacter := Character value: 16r10FFFF"
	
	"Use the Cuis (and older Squeak) convention."
	DoItCharacter := Character value: 26! !

!Scanner class methodsFor: 'initialization' stamp: 'nice 2/9/2012 14:59'!
initializeTypeTable
	"self initializeTypeTable"

	| newTable |
	newTable := Array new: 256 withAll: #xBinary. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.

	1 to: 255
		do: [:index |
			(Character value: index) isLetter
				ifTrue: [newTable at: index put: #xLetter]].

	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #xUnderscore.
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 6/10/2011 22:19'!
allowBlockArgumentAssignment
	"Accessor for the system-wide preference"
	
	<preference: 'Allow block argument assignment.'
		category: 'Compiler'
		description: 'If enabled, the compiler will allow assignment into block arguments.
This provides backward compatibility with the pre-closure compiler.'
		type: #Boolean>
	^AllowBlockArgumentAssignment ifNil: [ false ]! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 6/10/2011 22:20'!
allowBlockArgumentAssignment: aBoolean
	"Accessor for the system-wide preference"
	
	AllowBlockArgumentAssignment := aBoolean! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 10/28/2010 00:40'!
allowUnderscoreAsAssignment
	"Accessor for the system-wide preference"
	<preference: 'Allow underscore assignments'
		category: 'Compiler'
		description: 'When true, underscore can be used as assignment operator'
		type: #Boolean>
	^AllowUnderscoreAssignments ifNil:[true]! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 10/28/2010 00:40'!
allowUnderscoreAsAssignment: aBool
	"Accessor for the system-wide preference"
	AllowUnderscoreAssignments := aBool! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 10/28/2010 00:40'!
prefAllowUnderscoreAssignments
	"Accessor for the system-wide preference"
	
	self deprecated: 'Use #allowUnderscoreAsAssignment'.
	^self allowUnderscoreAsAssignment! !

!Scanner class methodsFor: 'preferences' stamp: 'ul 10/28/2010 00:41'!
prefAllowUnderscoreAssignments: aBool
	"Accessor for the system-wide preference"
	
	self deprecated: 'Use #allowUnderscoreAsAssignment:'.
	self allowUnderscoreAsAssignment: aBool! !

!Scanner class methodsFor: 'preferences' stamp: 'ar 3/12/2010 15:50'!
prefAllowUnderscoreSelectors
	"Accessor for the system-wide preference"
	<preference: 'Allow underscore selectors'
		category: 'Compiler'
		description: 'When true, underscore can be used in selectors and varibable names'
		type: #Boolean>
	^AllowUnderscoreSelectors ifNil:[false]! !

!Scanner class methodsFor: 'preferences' stamp: 'cwp 10/22/2012 00:33'!
prefAllowUnderscoreSelectors: aBool
	"Accessor for the system-wide preference"
	AllowUnderscoreSelectors := aBool.
	
	"Reinitialize String's tokenish character map"
	String initialize! !

!Scanner class methodsFor: 'preferences' stamp: 'nice 10/4/2011 21:08'!
prefAllowUnicharSymbol
	"Accessor for the system-wide preference"
	<preference: 'Allow symbols with unique character like #,'
		category: 'Compiler'
		description: 'When true, the historical syntax #, #; or #. is allowed.'
		type: #Boolean>
	^AllowUnicharSymbol ifNil: [false]! !

!Scanner class methodsFor: 'preferences' stamp: 'nice 10/4/2011 21:13'!
prefAllowUnicharSymbol: aBoolean
	"Accessor for the system-wide preference"
	AllowUnicharSymbol := aBoolean! !


!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:21'!
ambiguousSelector: aString inRange: anInterval
	| correctedSelector userSelection offset intervalWithOffset |
	
	self interactive ifFalse: [
		"In non interactive mode, compile with backward comapatibility: $- is part of literal argument"
		Transcript cr; store: encoder classEncoding; nextPutAll:#'>>';store: encoder selector; show: ' would send ' , token , '-'.
		^super ambiguousSelector: aString inRange: anInterval].
	
	"handle the text selection"
	userSelection := cue requestor selectionInterval.
	intervalWithOffset := anInterval first + requestorOffset to: anInterval last + requestorOffset.
	cue requestor selectFrom: intervalWithOffset first to: intervalWithOffset last.
	cue requestor select.

	"Build the menu with alternatives"
	correctedSelector := AmbiguousSelector 
			signalName: aString
			inRange: intervalWithOffset.
	correctedSelector ifNil: [^self fail].

	"Execute the selected action"
	offset := self substituteWord: correctedSelector wordInterval: intervalWithOffset offset: 0.
	cue requestor deselect.
	cue requestor selectInvisiblyFrom: userSelection first to: userSelection last + offset.
	token := (correctedSelector readStream upTo: Character space) asSymbol! !

!Parser methodsFor: 'error correction' stamp: 'cwp 10/15/2007 23:00'!
canDeclareClassVariable
	^encoder classEncoding ~~ UndefinedObject! !

!Parser methodsFor: 'error correction' stamp: 'nice 2/4/2011 22:10'!
canDeclareInstanceVariable
	^encoder classEncoding ~~ UndefinedObject! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:21'!
collectTemporaryDeclarationsFrom: methodNode
	| tempsMarks str |
	tempsMarks := OrderedCollection new.
	str := cue requestor text asString.
	methodNode accept: (ParseNodeEnumerator
		ofBlock: [ :aNode | 
			| mark |
			(aNode class canUnderstand: #tempsMark) 
				ifTrue: 
					[mark := aNode tempsMark.
					(mark notNil and: [ mark between: 1 and: str size ] and: [ (str at: mark) = $| ])
						ifTrue: [ tempsMarks addLast: aNode ]]]).
	(tempsMark notNil and: [ tempsMark between: 1 and: str size ] and: [ (str at: tempsMark) = $| ])
						ifTrue: [ tempsMarks addLast: self ].
	^ tempsMarks sorted: [ :a :b | a tempsMark > b tempsMark ]! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:20'!
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].

	userSelection := cue requestor selectionInterval.
	cue requestor selectFrom: spots first first to: spots last last.
	cue requestor select.

	correctSelector := UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	cue requestor deselect.
	cue requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:20'!
correctVariable: proposedVariable interval: spot 
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| binding userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: 
		[^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	"First check to see if the requestor knows anything about the variable"
	(binding := cue requestor bindingOf: proposedVariable)
		ifNotNil: [^encoder global: binding name: proposedVariable].
	userSelection := cue requestor selectionInterval.
	cue requestor selectFrom: spot first to: spot last.
	cue requestor select.

	"Build the menu with alternatives"
	action := UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	cue requestor deselect.
	cue requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value! !

!Parser methodsFor: 'error correction' stamp: 'eem 7/6/2010 15:07'!
declareClassVar: name
	| sym class |
	sym := name asSymbol.
	class := encoder classEncoding.
	class := class theNonMetaClass.		"not the metaclass"
	class addClassVarName: name.
	Smalltalk logChange: class definition.
	^ encoder global: (class classPool associationAt: sym)
			name: sym! !

!Parser methodsFor: 'error correction' stamp: 'eem 5/27/2009 09:27'!
declareGlobal: name
	| sym |
	sym := name asSymbol.
	^encoder
		global: (encoder environment
					at: sym put: nil;
					associationAt: sym)
		name: sym! !

!Parser methodsFor: 'error correction' stamp: 'eem 5/6/2010 21:14'!
declareInstVar: name
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarName: name.
	Smalltalk logChange: encoder classEncoding definition.
	^InstanceVariableNode new name: name index: encoder classEncoding instSize
		! !

!Parser methodsFor: 'error correction' stamp: 'eem 8/17/2010 16:35'!
declareTemp: name at: levelTag
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^(encoder bindUndeclaredTemp: name)
		tag: levelTag;
		yourself! !

!Parser methodsFor: 'error correction' stamp: 'eem 8/14/2010 19:34'!
declareTempAndPaste: name
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^encoder bindUndeclaredTemp: name! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:19'!
declareUndeclaredTemps: methodNode
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."

	| undeclared userSelection blocksToVars |
	(undeclared := encoder undeclaredTemps) isEmpty ifTrue:
		[^self].
	userSelection := cue requestor selectionInterval.
	blocksToVars := IdentityDictionary new.
	undeclared do:
		[:var|
		(blocksToVars
			at: (var tag == #method
					ifTrue: [methodNode block]
					ifFalse: [methodNode accept: (VariableScopeFinder new ofVariable: var)])
			ifAbsentPut: [SortedCollection new]) add: var name].
	(blocksToVars removeKey: methodNode block ifAbsent: []) ifNotNil:
		[:rootVars|
		rootVars do: [:varName| self pasteTempAtMethodLevel: varName]].
	(blocksToVars keys sorted: [:a :b| a tempsMark < b tempsMark]) do:
		[:block| | decl |
		decl := (blocksToVars at: block) reduce: [:a :b| a, ' ', b].
		block temporaries isEmpty
			ifTrue:
				[self substituteWord: ' | ', decl, ' |'
					wordInterval: (block tempsMark + 1 to: block tempsMark)
					offset: requestorOffset]
			ifFalse:
				[self substituteWord: decl, ' '
					wordInterval: (block tempsMark to: block tempsMark - 1)
					offset: requestorOffset]].
	cue requestor selectInvisiblyFrom: userSelection first to: userSelection last + requestorOffset.
	ReparseAfterSourceEditing signal! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 11:45'!
defineClass: className 
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| sym cat def d2 |
	sym := className asSymbol.
	cat := UIManager default request: 'Enter class category : ' initialAnswer: self encoder classEncoding theNonMetaClass category.
	cat
		ifEmpty: [cat := 'Unknown'].
	def := 'Object subclass: #' , sym , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , cat , ''''.
	d2 := UIManager default request: 'Edit class definition : ' initialAnswer: def.
	d2
		ifEmpty: [d2 := def].
	Compiler evaluate: d2.
	^ encoder
		global: (cue environment bindingOf: sym)
		name: sym! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:17'!
pasteTempAtMethodLevel: name
	| insertion delta theTextString characterBeforeMark |

	theTextString := cue requestor text string.
	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].
	(theTextString at: tempsMark) = $| ifTrue: [
  		"Paste it before the second vertical bar"
		insertion := name, ' '.
		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
		delta := 0.
	] ifFalse: [
		"No bars - insert some with CR, tab"
		insertion := '| ' , name , ' |',String cr.
		delta := 2.	"the bar and CR"
		characterBeforeMark = Character tab ifTrue: [
			insertion := insertion , String tab.
			delta := delta + 1.	"the tab"
		].
	].
	tempsMark := tempsMark +
		(self substituteWord: insertion
			wordInterval: (tempsMark to: tempsMark-1)
			offset: 0) - delta! !

!Parser methodsFor: 'error correction' stamp: 'cwp 10/15/2007 22:58'!
possibleVariablesFor: proposedVariable 
	^encoder possibleVariablesFor: proposedVariable! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:16'!
queryUndefined
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	cue requestor selectFrom: varStart to: varStart + varName size - 1; select.
	(UndefinedVariable name: varName) ifFalse: [^ self fail]! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:15'!
removeEmptyTempDeclarationsFrom: methodNode

	| sourceCode madeChanges tempsMarkHolder |
	sourceCode := cue requestor text asString.
	tempsMarkHolder := self collectTemporaryDeclarationsFrom: methodNode.
	madeChanges := false.
	tempsMarkHolder do: [ :currentBlock | | tempsMarkChar0 tempsMarkChar1 tempsMarkChar2 end start |
		tempsMarkChar0 := (sourceCode at: currentBlock tempsMark).
		tempsMarkChar1 := (sourceCode at: currentBlock tempsMark - 1).
		tempsMarkChar2 := (sourceCode at: currentBlock tempsMark - 2).
		tempsMarkChar0 = $| & tempsMarkChar1 = $| 
			ifTrue: 
				[ end := currentBlock tempsMark. 
				start := end - 1].
		tempsMarkChar0 = $| & tempsMarkChar1 = $  & tempsMarkChar2 = $| 
			ifTrue: 
				[ end := currentBlock tempsMark. 
				start := end - 2].
		
		start notNil & end notNil ifTrue: [
			| lineStart lineEnd |
			lineStart := 1 + (sourceCode 
				lastIndexOf: Character cr 
				startingAt: start - 1
				ifAbsent: [ 0 ]).
			lineEnd := sourceCode 
				indexOf: Character cr
				startingAt: end + 1
				ifAbsent: [ sourceCode size ].
			((sourceCode indexOfAnyOf: CharacterSet nonSeparators startingAt: lineStart) >= start 
				and: [ (sourceCode indexOfAnyOf: CharacterSet nonSeparators startingAt: end + 1) > lineEnd ]) ifTrue: [
					start := lineStart.
					end := lineEnd ].
			cue requestor correctFrom: start to: end with: ''.
			madeChanges := true.
			currentBlock tempsMark: nil ] ].
	madeChanges ifTrue: [ReparseAfterSourceEditing signal]! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:15'!
removeUnusedTemporaryNamed: temp from: str lookingAt: currentBlock movingTempMarksOf: someBlocks

	| start end |
	end := currentBlock tempsMark - 1.
	["Beginning at right temp marker..."
	start := end - temp size + 1.
	end < temp size or: [ (str at: start) = $| ]
		or: [ temp = (str copyFrom: start to: end) 
			and: [ ((str at: start - 1) = $| | (str at: start - 1) isSeparator) 
				& ((str at: end + 1) = $| | (str at: end + 1) isSeparator) ] ]]
		whileFalse: [ 
			"Search left for the unused temp"
			end := cue requestor nextTokenFrom: end direction: -1 ].
	(end < temp size or: [ (str at: start) = $| ])
		ifFalse: 
			[(str at: start - 1) = $ 
				ifTrue: [ start := start - 1 ].
			cue requestor correctFrom: start to: end with: ''.
			someBlocks do: [ :aBlock | aBlock tempsMark: aBlock tempsMark - (end - start + 1)].
			^true ].
	^false! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:14'!
removeUnusedTemps: methodNode
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| madeChanges tempsMarkHolder unusedTempNames tempMarkHoldersToChange |
	madeChanges := false.
	tempMarkHoldersToChange := OrderedCollection new.
	tempsMarkHolder := self collectTemporaryDeclarationsFrom: methodNode.
	unusedTempNames := encoder unusedTempNames select: 
		[ :temp | (encoder lookupVariable: temp ifAbsent: [ ]) isUndefTemp 
				and: [ self queriedUnusedTemporaries at: temp ifAbsentPut: [UnusedVariable name: temp] ]].
	tempsMarkHolder do: [ :currentBlock | 
		tempMarkHoldersToChange add: currentBlock.
		unusedTempNames do: 
			[ :temp |
			(self 
				removeUnusedTemporaryNamed: temp 
				from: cue requestor text asString 
				lookingAt: currentBlock
				movingTempMarksOf: tempMarkHoldersToChange) ifTrue: [ madeChanges := true ]]].
	madeChanges
		ifTrue: [ self removeEmptyTempDeclarationsFrom: methodNode.
			ReparseAfterSourceEditing signal ]! !

!Parser methodsFor: 'error correction'!
substituteSelector: selectorParts wordIntervals: spots
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset := 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset := self substituteWord: word wordInterval: interval offset: offset ]
! !

!Parser methodsFor: 'error correction' stamp: 'eem 9/5/2009 14:41'!
substituteVariable: each atInterval: anInterval 
	self 
		substituteWord: each
		wordInterval: anInterval
		offset: 0.
	^encoder encodeVariable: each! !

!Parser methodsFor: 'error correction' stamp: 'cwp 12/27/2012 10:14'!
substituteWord: correctWord wordInterval: spot offset: o
	"Substitute the correctSelector into the (presumed interactive) receiver.
	 Update requestorOffset based on the delta size and answer the updated offset."

	cue requestor correctFrom: spot first + o to: spot last + o with: correctWord.
	requestorOffset := requestorOffset + correctWord size - spot size.
	^o + correctWord size - spot size! !

!Parser methodsFor: 'error handling' stamp: 'eem 5/14/2008 13:34'!
addWarning: aString
	"ignored by the default compiler."! !

!Parser methodsFor: 'error handling' stamp: 'nice 2/22/2012 02:49'!
expected: aString 
	"Notify a problem at token 'here'."

	^ self notify: aString , ' expected' at: hereMark + requestorOffset! !

!Parser methodsFor: 'error handling' stamp: 'nice 2/26/2010 00:21'!
fail

	| exitBlock |
	(encoder == nil or: [encoder == self])
		ifFalse: [encoder release. encoder := nil]. "break cycle"
	exitBlock := failBlock.
	failBlock := nil.
	^exitBlock value! !

!Parser methodsFor: 'error handling' stamp: 'cwp 12/27/2012 10:19'!
interactive
	"Answer true if compilation is interactive"

	^ cue requestor notNil! !

!Parser methodsFor: 'error handling'!
notify: aString 
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset! !

!Parser methodsFor: 'error handling' stamp: 'nice 9/20/2013 02:00'!
notify: string at: location
	cue requestor isNil
		ifTrue: [| notification |
			(encoder == self or: [encoder isNil]) ifTrue: [^ self fail  "failure setting up syntax error"].
			(notification := SyntaxErrorNotification
					inClass: encoder classEncoding
					withCode: 
						(source contents asText
							copyReplaceFrom: location
							to: location - 1
							with: ((string , ' ->') asText allBold 
								addAttribute: TextColor red; yourself))
					doitFlag: doitFlag
					errorMessage: string
					location: location) signal.
			notification tryNewSourceIfAvailable]
		ifFalse: [cue requestor
					notify: string , ' ->'
					at: location
					in: source].
	^self fail! !

!Parser methodsFor: 'error handling' stamp: 'di 2/9/1999 15:43'!
offEnd: aString 
	"Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!!"

	requestorOffset == nil
		ifTrue: [^ self notify: aString at: mark]
		ifFalse: [^ self notify: aString at: mark + requestorOffset]
! !

!Parser methodsFor: 'expression types'!
argumentName

	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance! !

!Parser methodsFor: 'expression types' stamp: 'eem 6/2/2009 10:26'!
assignment: varNode
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true! !

!Parser methodsFor: 'expression types' stamp: 'eem 7/20/2009 12:09'!
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode variableNodes temporaryBlockVariables start |
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[variableNodes addLast: (encoder bindBlockArg: self argumentName within: blockNode)].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	temporaryBlockVariables := self temporaryBlockVariablesFor: blockNode.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporaries: temporaryBlockVariables.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]! !

!Parser methodsFor: 'expression types' stamp: 'eem 10/8/2012 13:40'!
braceExpression
	" { elements } => BraceNode."

	| elements locations loc more |
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression or right brace'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	(self match: #rightBrace)
		ifFalse: [^self expected: 'Period or right brace'].
	^true! !

!Parser methodsFor: 'expression types' stamp: 'eem 2/3/2011 09:50'!
cascade
	" {; message} => CascadeNode."

	| rcvr msgs |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	rcvr := parseNode cascadeReceiver.
	msgs := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := rcvr.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			msgs addLast: parseNode].
	parseNode := CascadeNode new receiver: rcvr messages: msgs! !

!Parser methodsFor: 'expression types' stamp: 'eem 2/6/2013 14:35'!
expression
	(hereType == #word
	 and: [tokenType == #leftArrow]) ifTrue:
		[^self assignment: self variable].
	self primaryExpression ifFalse:
		[^false].
	((self messagePart: 3 repeat: true)
	 and: [hereType == #semicolon]) ifTrue:
		[self cascade].
	^true! !

!Parser methodsFor: 'expression types' stamp: 'nice 10/24/2011 18:41'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := WriteStream on: (String new: 32).
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
			(level >= 2 and: [hereType == #verticalBar]) ifTrue: [self transformAVerticalBarIntoABinarySelector].
			(hereType == #binary and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asOctetString asSymbol.
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
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
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	repeat]
		whileTrue: [].
	^true! !

!Parser methodsFor: 'expression types' stamp: 'lw 5/26/2011 21:24'!
method: doit context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap blk prim temps messageComment methodNode |
	sap := self pattern: doit inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	doit ifFalse: [self pragmaSequence].
	temps := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doit ifFalse: [self pragmaSequence].
	prim := self pragmaPrimitives.
	self statements: #() innerBlock: doit.
	blk := parseNode.
	doit ifTrue: [blk returnLast]
		ifFalse: [blk returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temps
		block: blk
		encoder: encoder
		primitive: prim
		properties: properties.
	self interactive ifTrue:
		[self declareUndeclaredTemps: methodNode.
		 self removeUnusedTemps: methodNode].
	^methodNode! !

!Parser methodsFor: 'expression types' stamp: 'eem 5/29/2008 09:36'!
newMethodNode
	^self encoder methodNodeClass new! !

!Parser methodsFor: 'expression types' stamp: 'IgorStasenko 3/25/2011 14:16'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	| args selector |
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1}]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3}]].

	hereType == #word ifTrue: [^ {self advance asSymbol. {}. 1}].

	(hereType == #binary or: [hereType == #verticalBar]) ifTrue: 
		[selector := self advance asSymbol.
		args := Array with: (encoder bindArg: self argumentName).
		^ {selector. args. 2}].

	hereType == #keyword ifTrue: 
		[selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue:[
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ {selector contents asSymbol. args. 3}].
	^self expected: 'Message pattern'! !

!Parser methodsFor: 'expression types' stamp: 'ul 3/27/2011 15:33'!
primaryExpression 
	hereType == #word 
		ifTrue: 
			[parseNode := self variable.
			(parseNode isUndefTemp and: [self interactive])
				ifTrue: [self queryUndefined].
			parseNode nowHasRef.
			^ true].
	hereType == #leftBracket
		ifTrue: 
			[self advance.
			self blockExpression.
			^true].
	hereType == #leftBrace
		ifTrue: 
			[self braceExpression.
			^true].
	hereType == #leftParenthesis
		ifTrue: 
			[self advance.
			self expression ifFalse: [^self expected: 'expression'].
			(self match: #rightParenthesis)
				ifFalse: [^self expected: 'right parenthesis'].
			^true].
	(hereType == #string or: [hereType == #number or: [hereType == #literal or: [hereType == #character]]])
		ifTrue: 
			[parseNode := encoder encodeLiteral: self advance.
			^true].
	(here == #- and: [tokenType == #number and: [1 + hereEnd = mark]])
		ifTrue: 
			[self advance.
			parseNode := encoder encodeLiteral: self advance negated.
			^true].
	^false! !

!Parser methodsFor: 'expression types' stamp: 'eem 5/30/2008 11:51'!
statements: argNodes innerBlock: inner

	^self statements: argNodes innerBlock: inner blockNode: BlockNode new! !

!Parser methodsFor: 'expression types' stamp: 'eem 8/14/2012 18:49'!
statements: argNodes innerBlock: inner blockNode: theBlockNode

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	currentComment := nil.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						 stmts size = 0 ifTrue: 
							[stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true! !

!Parser methodsFor: 'expression types' stamp: 'cwp 12/27/2012 10:14'!
temporaries
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [cue requestor selectionInterval first]
								ifFalse: [1].
			^ #()].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [vars addLast: (encoder bindTemp: self advance)].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'
! !

!Parser methodsFor: 'expression types' stamp: 'cwp 12/27/2012 10:14'!
temporariesIn: methodSelector
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [cue requestor selectionInterval first]
								ifFalse: [1].
			^ #()].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [vars addLast: (encoder bindTemp: self advance in: methodSelector)].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'! !

!Parser methodsFor: 'expression types' stamp: 'eem 8/13/2010 15:57'!
temporaryBlockVariablesFor: aBlockNode
	"Scan and answer temporary block variables."

	| variables |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[aBlockNode tempsMark: prevMark + requestorOffset.
		 ^#()].

	variables := OrderedCollection new.
	[hereType == #word] whileTrue:
		[variables addLast: (encoder bindBlockTemp: self advance within: aBlockNode)].
	(self match: #verticalBar) ifFalse:
		[^self expected: 'Vertical bar'].
	aBlockNode tempsMark: prevMark + requestorOffset.
	^variables! !

!Parser methodsFor: 'expression types' stamp: 'di 12/4/1999 21:04'!
variable

	| varName varStart varEnd |
	varStart := self startOfNextToken + requestorOffset.
	varName := self advance.
	varEnd := self endOfLastToken + requestorOffset.
	^ encoder encodeVariable: varName
		sourceRange: (varStart to: varEnd)
		ifUnknown: [self correctVariable: varName interval: (varStart to: varEnd)]! !

!Parser methodsFor: 'pragmas' stamp: 'jcg 9/30/2009 00:20'!
addPragma: aPragma
	properties := self properties copyWith: aPragma! !

!Parser methodsFor: 'pragmas' stamp: 'eem 11/14/2012 23:51'!
pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number or: [ hereType == #character ] ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (Scanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'! !

!Parser methodsFor: 'pragmas' stamp: 'jcg 9/30/2009 00:20'!
pragmaPrimitives
	| primitives |
	self properties isEmpty ifTrue:
		[^0].
	primitives := properties pragmas select:
					[:pragma|
					self class primitivePragmaSelectors includes: pragma keyword].
	primitives isEmpty ifTrue:
		[^0].
	primitives size > 1 ifTrue:
		[^self notify: 'Ambigous primitives'].
	^self perform: primitives first keyword withArguments: primitives first arguments! !

!Parser methodsFor: 'pragmas' stamp: 'ul 6/17/2011 12:38'!
pragmaSequence
	"Parse a sequence of method pragmas."
	
	[
		(self matchToken: #<)
			ifFalse: [ ^ self ].
		self pragmaStatement.
		(self matchToken: #>)
			ifFalse: [ ^ self expected: '>' ] ] repeat! !

!Parser methodsFor: 'pragmas' stamp: 'mha 9/1/2009 16:32'!
pragmaStatement
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	" This is a ugly hack into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last = $: or: [ selector first isLetter not ])
			ifTrue: [ arguments add: (self pragmaLiteral: selector) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true! !

!Parser methodsFor: 'pragmas' stamp: 'jcg 9/30/2009 00:19'!
properties
	^ properties ifNil: [ properties := AdditionalMethodState new ]! !

!Parser methodsFor: 'primitives'!
allocateLiteral: lit
	encoder litIndex: lit! !

!Parser methodsFor: 'primitives' stamp: 'nice 9/6/2013 00:48'!
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library."
	| descriptorClass callType modifier retType externalName args argType module fn |
	descriptorClass := cue environment
		valueOf: #ExternalFunction 
		ifAbsent: [^ false].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil ifTrue:[^self expected:'return type'].
	"Parse function name or index"
	externalName := here.
	(self match: #string) 
		ifTrue:[externalName := externalName asSymbol]
		ifFalse:[(self match:#number) ifFalse:[^self expected:'function name or index']].
	(self match: #leftParenthesis) ifFalse:[^self expected:'argument list'].
	args := WriteStream on: Array new.
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

!Parser methodsFor: 'primitives' stamp: 'ar 12/2/1999 16:49'!
externalType: descriptorClass
	"Parse an return an external type"
	| xType |
	xType := descriptorClass atomicTypeNamed: here.
	xType == nil ifTrue:["Look up from class scope"
		Symbol hasInterned: here ifTrue:[:sym|
			xType := descriptorClass structTypeNamed: sym]].
	xType == nil ifTrue:[
		"Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	(self matchToken:#*)
		ifTrue:[^xType asPointerType]
		ifFalse:[^xType]! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:17'!
primitive: anIntegerOrString
	"Create indexed primitive."
	
	^self primitive: anIntegerOrString error: nil! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:21'!
primitive: anIntegerOrString error: errorCodeVariableOrNil
	"Create indexed primitive with optional error code."
	
	^anIntegerOrString isInteger
		ifTrue:
			[errorCodeVariableOrNil ifNotNil:
				[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
			 anIntegerOrString]
		ifFalse:
			[anIntegerOrString isString
				ifTrue: [self primitive: anIntegerOrString module: nil error: errorCodeVariableOrNil]
				ifFalse: [self expected: 'Indexed primitive']]! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:20'!
primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil
	"Create named primitive with optional error code."
	
	^self primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:21'!
primitive: aNameString module: aModuleStringOrNil
	"Create named primitive."
	
	^self primitive: aNameString module: aModuleStringOrNil error: nil! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:18'!
primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117! !

!Parser methodsFor: 'public access' stamp: 'nice 8/27/2010 20:54'!
encoder
	^encoder ifNil:
		[encoder := EncoderForV3PlusClosures new]! !

!Parser methodsFor: 'public access' stamp: 'jmv 3/2/2010 17:11'!
encoderClass: anEncoderClass
	encoder ifNotNil: [
		self error: 'encoder already set'].
	encoder := anEncoderClass new! !

!Parser methodsFor: 'public access' stamp: 'nice 9/19/2013 23:15'!
parse: sourceStreamOrString class: behavior

	^ self parse: sourceStreamOrString readStream class: behavior
		noPattern: false notifying: nil ifFail: [^nil]! !

!Parser methodsFor: 'public access' stamp: 'nice 9/19/2013 23:18'!
parse: sourceStream class: class noPattern: noPattern notifying: req ifFail: aBlock 
	| c |
	c := CompilationCue
			source: sourceStream
			class: class
			requestor: req.
	^ self 
		parseCue: c 
		noPattern: noPattern 
		ifFail: aBlock! !

!Parser methodsFor: 'public access' stamp: 'nice 9/20/2013 00:55'!
parseArgsAndTemps: aString 
        "Parse the argument, aString, answer nil if an error occurs. Otherwise, 
        answer an Array of strings (the argument names and temporary variable names)."

        aString == nil ifTrue: [^#()].
        doitFlag := false.               "Don't really know if a doit or not!!"
        ^self initPattern: aString
                return: [:pattern | (pattern at: 2) , (self temporariesIn: (pattern at: 1))]! !

!Parser methodsFor: 'public access' stamp: 'nice 9/23/2013 22:42'!
parseCue: aCue noPattern: noPattern ifFail: aBlock 
	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the CompilationCue to 
	 resolve variables. Errors in parsing are reported to the cue's requestor; 
	 otherwise aBlock is evaluated. The argument noPattern is a Boolean that is
	 true if the the sourceStream does not contain a method header (i.e., for DoIts)."

	| methNode repeatNeeded myStream s p subSelection |
	myStream := aCue sourceStream.
	[repeatNeeded := false.
	 p := myStream position.
	 s := myStream upToEnd.
	 myStream position: p.
	 subSelection := aCue requestor notNil and: [aCue requestor selectionInterval = (p + 1 to: p + s size)].
	 self encoder init: aCue notifying: self.
	 self init: myStream cue: aCue failBlock: [^ aBlock value].
	 doitFlag := noPattern.
	 failBlock:= aBlock.
	 [methNode := self method: noPattern context: cue context] 
		on: ReparseAfterSourceEditing 
		do:	[ :ex |
			repeatNeeded := true.
			properties := nil. "Avoid accumulating pragmas and primitives Number"
			myStream := ex newSource 
				ifNil: [subSelection
							ifTrue:
								[ReadStream
									on: cue requestor text string
									from: cue requestor selectionInterval first
									to: cue requestor selectionInterval last]
							ifFalse:
								[ReadStream on: cue requestor text string]]
				ifNotNil: [:src | myStream := src readStream]].
	 repeatNeeded] whileTrue:
		[encoder := self encoder class new].
	methNode sourceText: s.
	^methNode
! !

!Parser methodsFor: 'public access' stamp: 'nice 9/20/2013 00:51'!
parseMethodComment: aString setPattern: aBlock
	"Answer the method comment for the argument, aString. Evaluate aBlock 
	with the message pattern in the form #(selector, arguments, precedence)."

	self
		initPattern: aString
		return: aBlock.
	currentComment==nil
		ifTrue:	[^OrderedCollection new]
		ifFalse:	[^currentComment]! !

!Parser methodsFor: 'public access' stamp: 'nice 9/20/2013 00:51'!
parseParameterNames: aString 
	"Answer the parameter names for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self initScannerForTokenization.
	^self
		initPattern: aString
		return: [:pattern | pattern at: 2]! !

!Parser methodsFor: 'public access' stamp: 'eem 6/26/2014 12:35'!
parseSelector: aString 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self allowUnderscoreSelectors ifFalse: [self initScannerForTokenization].
	^self
		initPattern: aString
		return: [:pattern | pattern at: 1]! !

!Parser methodsFor: 'scanning' stamp: 'nice 2/9/2012 14:02'!
advance
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := source position - (aheadChar == DoItCharacter
		ifTrue: [hereChar == DoItCharacter
			ifTrue: [0]
			ifFalse: [1]]
		ifFalse: [2]).
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this! !

!Parser methodsFor: 'scanning' stamp: 'hmm 7/16/2001 19:23'!
endOfLastToken

	^ prevEnd ifNil: [mark]! !

!Parser methodsFor: 'scanning'!
match: type 
	"Answer with true if next tokens type matches."

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false! !

!Parser methodsFor: 'scanning' stamp: 'di 6/7/2000 08:44'!
matchReturn

	^ self match: #upArrow! !

!Parser methodsFor: 'scanning'!
matchToken: thing 
	"Matches the token, not its type."

	here = thing ifTrue: [self advance. ^true].
	^false! !

!Parser methodsFor: 'scanning'!
startOfNextToken
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark! !

!Parser methodsFor: 'scanning' stamp: 'nice 9/5/2013 23:06'!
transformAVerticalBarIntoABinarySelector
	"Transform a vertical bar into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0"
	here := '|'.
	hereType := #binary.
	[tokenType == #verticalBar and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , '|'.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].! !

!Parser methodsFor: 'temps'!
bindArg: name

	^ self bindTemp: name! !

!Parser methodsFor: 'temps'!
bindTemp: name

	^name! !

!Parser methodsFor: 'temps' stamp: 'eem 5/13/2008 12:17'!
bindTemp: name in: methodSelector

	^name! !

!Parser methodsFor: 'private'!
addComment

	parseNode ~~ nil
		ifTrue: 
			[parseNode comment: currentComment.
			currentComment := nil]! !

!Parser methodsFor: 'private' stamp: 'topa 3/15/2015 23:22'!
allowUnderscoreAssignments
	"Query class + preference"
	^ (encoder ifNotNil: [:e | 
		e == self 
			ifTrue: [nil]
			ifFalse: [e classEncoding allowUnderscoreAssignments]])
		ifNil: [super allowUnderscoreAssignments]! !

!Parser methodsFor: 'private' stamp: 'cwp 12/26/2012 23:37'!
init: sourceStream cue: aCue failBlock: aBlock

	self setCue: aCue.
	failBlock := aBlock.
	requestorOffset := 0.
	super scan: sourceStream.
	prevMark := hereMark := mark.
	self advance
! !

!Parser methodsFor: 'private' stamp: 'nice 9/20/2013 00:51'!
initPattern: aString return: aBlock

	| result |
	self
		init: (ReadStream on: aString asString)
		cue: (CompilationCue source: aString)
		failBlock: [^nil].
	encoder := self.
	result := aBlock value: (self pattern: false inContext: nil).
	encoder := failBlock := nil.  "break cycles"
	^result! !

!Parser methodsFor: 'private' stamp: 'nice 2/24/2013 22:38'!
setCue: aCue
	cue := aCue! !

!Parser methodsFor: 'accessing' stamp: 'lw 5/31/2011 17:35'!
queriedUnusedTemporaries

	queriedUnusedTemporaries ifNil: 
		[queriedUnusedTemporaries := Dictionary new].
	^queriedUnusedTemporaries! !

!Parser methodsFor: 'accessing' stamp: 'lw 5/27/2011 18:47'!
tempsMark
	^ tempsMark! !

!Parser methodsFor: 'accessing' stamp: 'lw 5/27/2011 18:47'!
tempsMark: aNumber
tempsMark := aNumber! !

!Parser methodsFor: '*45Deprecated' stamp: 'nice 12/13/2013 16:32'!
initPattern: aString notifying: req return: aBlock

	req
		ifNil:
			[self deprecated: 'Notifying nil is unnecessary, simply use #initPattern:return:'.
			^self initPattern: aString return: aBlock]
		ifNotNil:
			[| result |
			self deprecated: '#initPattern:return: was preferred because everybody was notifying nil, but you.
You could ask for re-integration of this message'.
			self
				init: (ReadStream on: aString asString)
				cue: (CompilationCue source: aString requestor: req)
				failBlock: [^nil].
			encoder := self.
			result := aBlock value: (self pattern: false inContext: nil).
			encoder := failBlock := nil.  "break cycles"
			^result]! !


!Parser class methodsFor: 'accessing' stamp: 'eem 12/1/2008 14:32'!
primitivePragmaSelectors
	"Answer the selectors of pragmas that specify VM primitives.
	 Needed for compile and decomple."
	^#(primitive:
		primitive:error:
		primitive:error:module:
		primitive:module:
		primitive:module:error:)! !


!ColoredCodeStream methodsFor: 'color/style' stamp: 'md 8/14/2005 17:33'!
colorTable
	"Answer the table to use to determine colors"

	^ colorTable ifNil: [colorTable := ST80ColorTable]! !

!ColoredCodeStream methodsFor: 'color/style' stamp: 'sw 5/20/2001 21:05'!
withColor: colorSymbol emphasis: emphasisSymbol do: aBlock
	"Evaluate the given block with the given color and style text attribute"

	^ self withAttributes: {TextColor color: (Color perform: colorSymbol).
							TextEmphasis perform: emphasisSymbol}
		do: aBlock! !

!ColoredCodeStream methodsFor: 'color/style' stamp: 'sw 5/20/2001 11:30'!
withStyleFor: elementType do: aBlock
	"Evaluate aBlock with appropriate emphasis and color for the given elementType"

	| colorAndStyle |
	colorAndStyle := self colorTable at: elementType.
	^ self withColor: colorAndStyle first emphasis: colorAndStyle second do: aBlock! !


!ColoredCodeStream class methodsFor: 'class initialization' stamp: 'wiz 9/12/2005 00:41'!
initialize
	"Initialize the colors that characterize the ST80 dialect"

	ST80ColorTable := IdentityDictionary new.
	#(	(temporaryVariable blue italic)
		(methodArgument blue normal)
		(methodSelector black bold)
		(blockArgument red normal)
		(comment brown normal)
		(variable magenta normal)
		(literal	orange normal)
		(keyword darkGray bold)
		(prefixKeyword veryDarkGray bold)
		(setOrReturn black bold)) do:
			[:aTriplet |
				ST80ColorTable at: aTriplet first put: aTriplet allButFirst]

"ColoredCodeStream initialize"! !

!ColoredCodeStream class methodsFor: 'instance creation' stamp: 'md 8/15/2005 11:00'!
contents: blockWithArg 
	"Evaluate blockWithArg on a DialectStream of the given description"

	| stream |
	stream := self on: (Text new: 400).
	blockWithArg value: stream.
	^ stream contents! !

Compiler initialize!
Decompiler initialize!
ParseNode initialize!
VariableNode initialize!
MessageNode initialize!
Scanner initialize!
ColoredCodeStream initialize!
