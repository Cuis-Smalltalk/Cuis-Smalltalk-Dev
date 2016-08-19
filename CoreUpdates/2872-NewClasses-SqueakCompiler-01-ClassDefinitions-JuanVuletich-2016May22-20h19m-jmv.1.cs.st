'From Cuis 4.2 of 25 July 2013 [latest update: #2767] on 22 May 2016 at 8:19:42.769868 pm'!
!classDefinition: #CompilationCue category: #'Compiler-Kernel'!
Object subclass: #CompilationCue
	instanceVariableNames: 'source context receiver class environment requestor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

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

!classDefinition: #CompiledMethodTrailer category: #'Kernel-Methods'!
Object subclass: #CompiledMethodTrailer
	instanceVariableNames: 'data encodedData kind size method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!CompiledMethodTrailer commentStamp: '<historical>' prior: 0!
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

!classDefinition: #CompiledMethodWithNode category: #'Compiler-Support'!
Object subclass: #CompiledMethodWithNode
	instanceVariableNames: 'node method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #Compiler category: #'Compiler-Kernel'!
Object subclass: #Compiler
	instanceVariableNames: 'parser cue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Compiler commentStamp: 'cwp 12/26/2012 23:17' prior: 0!
The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor (usually a kind of StringHolderController) is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode.!

!classDefinition: #SyntaxErrorNotification category: #'Compiler-Exceptions'!
Error subclass: #SyntaxErrorNotification
	instanceVariableNames: 'inClass code doitFlag errorMessage location newSource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!

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

!classDefinition: #ParserNotification category: #'Compiler-Exceptions'!
Notification subclass: #ParserNotification
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
!classDefinition: #AmbiguousSelector category: #'Compiler-Exceptions'!
ParserNotification subclass: #AmbiguousSelector
	instanceVariableNames: 'interval'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!

!AmbiguousSelector commentStamp: 'nice 2/23/2010 15:40' prior: 0!
An AmbiguousSelector is a notification produced by the Scanner/Parser/Compiler when encountering this ambiguous construct:

1@-2

Upper expression can be interpreted both
1 @ -2 (regular st-80 and former Squeak syntax, the minus is attached to the literal number)
1 @- 2 (extended binary selector, the minus sign is allowed at any position and thus part of the binary selector)
!

!classDefinition: #UndeclaredVariable category: #'Compiler-Exceptions'!
ParserNotification subclass: #UndeclaredVariable
	instanceVariableNames: 'parser interval'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
!classDefinition: #UndefinedVariable category: #'Compiler-Exceptions'!
ParserNotification subclass: #UndefinedVariable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
!classDefinition: #UnknownSelector category: #'Compiler-Exceptions'!
ParserNotification subclass: #UnknownSelector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
!classDefinition: #UnusedVariable category: #'Compiler-Exceptions'!
ParserNotification subclass: #UnusedVariable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Exceptions'!
!classDefinition: #ReparseAfterSourceEditing category: #'Compiler-Support'!
Notification subclass: #ReparseAfterSourceEditing
	instanceVariableNames: 'newSource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!ReparseAfterSourceEditing commentStamp: 'nice 9/18/2013 22:05' prior: 0!
A ReparseAfterSourceEditing is a Notification used to restart the syntax parsing phase of a compilation after a source code edition.

Instance Variables
	newSource:		<UndefinedObject | String | Text | Stream>

newSource
	- this is the new source code to be used for restarting compilation if non interactive

In case of interactive compilation, newSource variable is nil because source code edition is performed directly in the source code editor, and the new source code will be picked directly there by the compiler.

In case of non interactive compilation, source code edition typically occurs in a SyntaxError window popping up. But the compiler has no direct access to this object, so newSource has to be passed by our intermediate.
!

!classDefinition: #UndeclaredVariableReference category: #'Compiler-Support'!
Notification subclass: #UndeclaredVariableReference
	instanceVariableNames: 'parser varName varStart varEnd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #UndeclaredVariableWarning category: #'Compiler-Support'!
Warning subclass: #UndeclaredVariableWarning
	instanceVariableNames: 'name selector class'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #InstructionClient category: #'Kernel-Methods'!
Object subclass: #InstructionClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!InstructionClient commentStamp: 'md 4/8/2003 12:50' prior: 0!
My job is to make it easier to implement clients for InstructionStream. See InstVarRefLocator
as an example. !

!classDefinition: #BlockLocalTempCounter category: #'Compiler-Support'!
InstructionClient subclass: #BlockLocalTempCounter
	instanceVariableNames: 'stackPointer scanner blockEnd joinOffsets'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!BlockLocalTempCounter commentStamp: '<historical>' prior: 0!
I am a support class for the decompiler that is used to find the number of local temps in a block by finding out what the stack offset is at the end of a block.!

!classDefinition: #BlockStartLocator category: #'Kernel-Methods'!
InstructionClient subclass: #BlockStartLocator
	instanceVariableNames: 'nextJumpIsAroundBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!classDefinition: #ClosureExtractor category: #'Kernel-Methods'!
InstructionClient subclass: #ClosureExtractor
	instanceVariableNames: 'action scanner currentContext'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!ClosureExtractor commentStamp: 'mha 9/21/2010 11:16' prior: 0!
A ClosureExtractor is a utility class that is used to extract all BlockClosures from a CompiledMethod. It inherits from InstructionClient and understands only one single message, namely that corresponding to the push closure bytecode instruction. Being sent this message, a ClosureExtractor will create a BlockClosure instance and evaluate the block it holds as an instance variable with that closure as parameter.!

!classDefinition: #InstVarRefLocator category: #'Kernel-Methods'!
InstructionClient subclass: #InstVarRefLocator
	instanceVariableNames: 'bingo'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!InstVarRefLocator commentStamp: 'md 4/8/2003 12:50' prior: 0!
My job is to scan bytecodes for instance variable references.

BlockContext allInstances collect: [ :x |
	{x. x hasInstVarRef}
].!

!classDefinition: #InstructionPrinter category: #'Kernel-Methods'!
InstructionClient subclass: #InstructionPrinter
	instanceVariableNames: 'method scanner stream oldPC innerIndents indent printPC indentSpanOfFollowingJump'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!InstructionPrinter commentStamp: 'md 4/8/2003 12:47' prior: 0!
My instances can print the object code of a CompiledMethod in symbolic format. They print into an instance variable, stream, and uses oldPC to determine how many bytes to print in the listing. The variable method  is used to hold the method being printed.!

!classDefinition: #RelativeInstructionPrinter category: #'Kernel-Methods'!
InstructionPrinter subclass: #RelativeInstructionPrinter
	instanceVariableNames: 'printCode labels labelling'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack'
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

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

!classDefinition: #ParseNode category: #'Compiler-ParseNodes'!
Object subclass: #ParseNode
	instanceVariableNames: 'comment pc'
	classVariableNames: 'CodeBases CodeLimits LdFalse LdInstType LdLitIndType LdLitType LdMinus1 LdNil LdSelf LdSuper LdTempType LdThisContext LdTrue LoadLong NodeFalse NodeNil NodeSelf NodeSuper NodeThisContext NodeTrue Send SendPlus SendType StdLiterals StdSelectors StdVariables'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ParseNode commentStamp: '<historical>' prior: 0!
This superclass of most compiler/decompiler classes declares common class variables, default messages, and the code emitters for jumps. Some of the class variables are initialized here; the rest are initialized in class VariableNode.!

!classDefinition: #AssignmentNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #AssignmentNode
	instanceVariableNames: 'variable value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!AssignmentNode commentStamp: '<historical>' prior: 0!
AssignmentNode comment: 'I represent a (var_expr) construct.'!

!classDefinition: #BlockArgsNode category: #'Compiler-Syntax'!
ParseNode subclass: #BlockArgsNode
	instanceVariableNames: 'temporaries'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Syntax'!
!classDefinition: #BlockNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #BlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode temporaries optimized optimizedMessageNode actualScopeIfOptimized blockExtent remoteTempNode copiedValues closureCreationNode startOfLastStatement tempsMark'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!BlockNode commentStamp: '<historical>' prior: 0!
I represent a bracketed block with 0 or more arguments and 1 or more statements. If I am initialized with no statements, I create one. I have a flag to tell whether my last statement returns a value from the enclosing method. My last three fields remember data needed for code generation. I can emit for value in the usual way, in which case I create a literal method (actually a context remotely copied) to be evaluated by sending it value: at run time. Or I can emit code to be evaluated in line; this only happens at the top level of a method and in conditionals and while-loops, none of which have arguments.!

!classDefinition: #BraceNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #BraceNode
	instanceVariableNames: 'elements sourceLocations emitNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!BraceNode commentStamp: '<historical>' prior: 0!
Used for compiling and decompiling brace constructs.

These now compile into either a fast short form for 4 elements or less:
	Array braceWith: a with: b ... 
or a long form of indefinfite length:
	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.

The erstwhile brace assignment form is no longer supported.!

!classDefinition: #CascadeNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #CascadeNode
	instanceVariableNames: 'receiver messages'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!CascadeNode commentStamp: '<historical>' prior: 0!
The first message has the common receiver, the rest have receiver == nil, which signifies cascading.!

!classDefinition: #CommentNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #CommentNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #DecompilerConstructor category: #'Compiler-Support'!
ParseNode subclass: #DecompilerConstructor
	instanceVariableNames: 'method instVars nArgs literalValues tempVars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!DecompilerConstructor commentStamp: '<historical>' prior: 0!
I construct the node tree for a Decompiler.!

!classDefinition: #DecompilerConstructorForClosures category: #'Compiler-Support'!
DecompilerConstructor subclass: #DecompilerConstructorForClosures
	instanceVariableNames: 'tempNameCounter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Encoder commentStamp: 'cwp 12/26/2012 23:29' prior: 0!
I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.!

!classDefinition: #BytecodeEncoder category: #'Compiler-Kernel'!
Encoder subclass: #BytecodeEncoder
	instanceVariableNames: 'stream position rootNode blockExtentsToLocals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!BytecodeEncoder commentStamp: '<historical>' prior: 0!
I am an abstract superclass for different bytecode set encoders.  Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.!

!classDefinition: #EncoderForV3 category: #'Compiler-Kernel'!
BytecodeEncoder subclass: #EncoderForV3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

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

!classDefinition: #EncoderForV3PlusClosures category: #'Compiler-Kernel'!
EncoderForV3 subclass: #EncoderForV3PlusClosures
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

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

!classDefinition: #FutureNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #FutureNode
	instanceVariableNames: 'receiver originalSelector futureSelector futureDelta futureArgs effectNode valueNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

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

!classDefinition: #LeafNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #LeafNode
	instanceVariableNames: 'key code index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

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

!classDefinition: #LiteralNode category: #'Compiler-ParseNodes'!
LeafNode subclass: #LiteralNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!LiteralNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a literal string or number.!

!classDefinition: #SpecialLiteralNode category: #'Compiler-ParseNodes'!
LiteralNode subclass: #SpecialLiteralNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #SelectorNode category: #'Compiler-ParseNodes'!
LeafNode subclass: #SelectorNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!SelectorNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a selector.!

!classDefinition: #KeyWordNode category: #'Compiler-Syntax'!
SelectorNode subclass: #KeyWordNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Syntax'!

!KeyWordNode commentStamp: '<historical>' prior: 0!
I am a part of a selector.   #at:put: is owned by a SelectorNode, and #put: within it is owned by a KeyWordNode.!

!classDefinition: #SpecialSelectorNode category: #'Compiler-ParseNodes'!
SelectorNode subclass: #SpecialSelectorNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!SpecialSelectorNode commentStamp: 'eem 8/5/2014 20:18' prior: 0!
A SpecialSelectorNode is a subclass of SelectorNode that handles the special selectors, a high static and/or dynamic frequency set of selectors that are assigned their own bytecodes.  Special selectors both save space in the literal frame and allow an interpreter to implement these sends directly for certain classes of receiver and argument, for example the SmallIntegers, a technique known as static receiver prediction.!

!classDefinition: #VariableNode category: #'Compiler-ParseNodes'!
LeafNode subclass: #VariableNode
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!VariableNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a variable. Note that my name and key are different for pool variables: the key is the Object Reference.!

!classDefinition: #FieldNode category: #'Compiler-ParseNodes'!
VariableNode subclass: #FieldNode
	instanceVariableNames: 'fieldDef rcvrNode readNode writeNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!FieldNode commentStamp: '<historical>' prior: 0!
FileNode handles field access in Tweak, e.g. self fieldName := foo => self fieldName: foo.!

!classDefinition: #InstanceVariableNode category: #'Compiler-ParseNodes'!
VariableNode subclass: #InstanceVariableNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MaybeContextInstanceVariableNode category: #'Compiler-ParseNodes'!
InstanceVariableNode subclass: #MaybeContextInstanceVariableNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MaybeContextInstanceVariableNode commentStamp: '<historical>' prior: 0!
This class conspires to arrange that inst var access for contexts is done exclusively using the long-form instance variabl;e access bytecodes.  See InstructionStream class>>variablesAndOffsetsDo:.

A virtual machine can benefit in performance by organizing method and block activations using a  more conventional stack organization than by using first-class activation records (contexts).  But such a virtual machine is also cabable of hiding the stack and making it appear as if contexts are still used.  This means the system has better performance but still has all the benefits of first-class activation records.  To pull this off the VM needs to intercept any and all accesses to context objects so that it can make contexts function as proxy objects for stack frames.

Without help from the image such a virtual machine based on an interpreter would have to perform an expensive check on all instance variable accesses to determine if the instance variable was that of a context serving as a proxy for a stack frame.  A simple hack is to take advantage of the short and long forms of instance variable access bytecodes.  The BlueBook instruction set (and likely any bytecode set evolved from it) has short form bytecodes for fetching and storing the first few bytecodes (BlueBook fetch first 16, store first 8).  Contexts typically have at most 6 instance variables.  If we arrange to use the long-form bytecodes for all context inst var accesses then we only have to check for context inst var access in long-form bytecodes, and then only if the index is within the context inst var range.  This effectively makes the check free because on modern processors checking an index fetched from memory into a register against a constant costs far less than the memry read to fetch the index.!

!classDefinition: #LiteralVariableNode category: #'Compiler-ParseNodes'!
VariableNode subclass: #LiteralVariableNode
	instanceVariableNames: 'readNode writeNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #TempVariableNode category: #'Compiler-ParseNodes'!
VariableNode subclass: #TempVariableNode
	instanceVariableNames: 'argType hasRefs hasDefs scope definingScope readingScopes writingScopes remoteNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!TempVariableNode commentStamp: '<historical>' prior: 0!
I am a parse tree leaf representing a temporary variable!

!classDefinition: #RemoteTempVectorNode category: #'Compiler-ParseNodes'!
TempVariableNode subclass: #RemoteTempVectorNode
	instanceVariableNames: 'remoteTemps readNode writeNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!RemoteTempVectorNode commentStamp: '<historical>' prior: 0!
I am a node for a vector of remote temps, created to share temps between closures when those temps are written to in closures other than their defining ones.!

!classDefinition: #UndeclaredVariableNode category: #'Compiler-ParseNodes'!
VariableNode subclass: #UndeclaredVariableNode
	instanceVariableNames: 'tag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments'
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers StdTypers ThenFlag'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MessageNode commentStamp: '<historical>' prior: 0!
I represent a receiver and its message.
	
Precedence codes:
	1 unary
	2 binary
	3 keyword
	4 other
	
If special>0, I compile special code in-line instead of sending messages with literal methods as remotely copied contexts.!

!classDefinition: #MessageAsTempNode category: #'Compiler-ParseNodes'!
MessageNode subclass: #MessageAsTempNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MessageAsTempNode commentStamp: '<historical>' prior: 0!
This node represents accesses to temporary variables for do-its in the debugger.  Since they execute in another context, they must send a message to the original context to access the value of the temporary variable in that context.!

!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

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

!classDefinition: #MethodTempsNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MethodTempsNode
	instanceVariableNames: 'temporaries'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #NewArrayNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #NewArrayNode
	instanceVariableNames: 'numElements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!NewArrayNode commentStamp: '<historical>' prior: 0!
I represent a node for the genPushNewArray: opcode.!

!classDefinition: #ReturnNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #ReturnNode
	instanceVariableNames: 'expr'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ReturnNode commentStamp: '<historical>' prior: 0!
I represent an expression of the form ^expr.!

!classDefinition: #ParseNodeVisitor category: #'Compiler-Support'!
Object subclass: #ParseNodeVisitor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

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

!classDefinition: #OptimizedBlockLocalTempReadBeforeWrittenVisitor category: #'Compiler-Support'!
ParseNodeVisitor subclass: #OptimizedBlockLocalTempReadBeforeWrittenVisitor
	instanceVariableNames: 'inOptimizedBlock readBeforeWritten written'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!OptimizedBlockLocalTempReadBeforeWrittenVisitor commentStamp: '<historical>' prior: 0!
Answer the set of temporary variables that are read before they are written in the visited parse tree.  Used by the compiler to detect those block-local temporaries of blocks in optimized loops that require nilling to prevent a value from a previous iteration persisting into a subsequent one.!

!classDefinition: #ParseNodeEnumerator category: #'Compiler-Support'!
ParseNodeVisitor subclass: #ParseNodeEnumerator
	instanceVariableNames: 'theBlock theSelectBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

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

!classDefinition: #VariableScopeFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #VariableScopeFinder
	instanceVariableNames: 'theVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!VariableScopeFinder commentStamp: 'eem 8/14/2010 19:45' prior: 0!
A VariableScopeFinder is used to find the minimum enclosing scope of a variable in a method.  This is used when auto-declaring temporaries to find the smallest enclosing block in which to declare the temp.

Instance Variables
	theVariable:		<VariableNode>

theVariable
	- the varable whose scope is to be determined
!

!classDefinition: #ParseStack category: #'Compiler-Support'!
Object subclass: #ParseStack
	instanceVariableNames: 'position length'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!ParseStack commentStamp: '<historical>' prior: 0!
I keep track of the current and high position of the stack that will be needed by code being compiled.!

!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable'
	classVariableNames: 'AllowBlockArgumentAssignment AllowUnderscoreAssignments AllowUnderscoreSelectors AllowUnicharSymbol DoItCharacter TypeTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

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

!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder parseNode failBlock requestorOffset tempsMark doitFlag properties queriedUnusedTemporaries cue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Parser commentStamp: 'cwp 12/26/2012 23:34' prior: 0!
I parse Smalltalk syntax and create a MethodNode that is the root of the parse tree. I look one token ahead.!

!classDefinition: #ColoredCodeStream category: #'Compiler-Kernel'!
TextStream subclass: #ColoredCodeStream
	instanceVariableNames: 'dialect colorTable'
	classVariableNames: 'ST80ColorTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
