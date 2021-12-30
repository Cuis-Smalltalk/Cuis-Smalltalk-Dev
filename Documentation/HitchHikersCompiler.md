# THE HITCH HIKER'S GUIDE TO THE SMALLTALK COMPILER

by Vassili Bykov

Reprinted with permission by author

This article is about nuts and bolts of VisualWorks and Squeak compilers. The compiler is probably close to the bottom of the list of things one would want to mess with. If you do so often, your name must be Dan or Eliot or you work with one of them. In that case you might not learn much, if anything at all here.

The fun of VisualWorks and Squeak is you have as much access to the compiler as the implementers. If you are like me, you like to take things apart to see how they work. This article is a guided tour through implementation of some of compiler hacks I have done in the past. It is also intended to be a guided tour through the compiler. It began as a review of VisualWorks compiler, but ended up with bits and pieces of Squeak thrown in. VisualWorks and Squeak share a common ancestor and it is fun to look at similarities and differences accumulated over the years.

This is not about the compiler theory or building a compiler. We will not discuss grammars, bottom-up vs. top-down parsing, or shift/reduce conflicts. Of course it will not make you a compiler wizard unless you already are. What is the point to it then? Well, I thought of that analogy with a hitching a ride. There is a lot of things you do not get to do, but you do get to see places.

## The Babel Fish

First let us go through the terminology check list and review what it is that we are about to see.

The problem of translating a program text into a machine code may look dauntingly complex. Probably because it is. Following the divide and conquer principle, it is traditionally viewed as (at least) three phases: scanning, parsing, and code generation.

Let's say the compiler translates an expression

````Smalltalk
    foo := self bar
````
The expression is passed as either a string or a stream--so the compiler's view of it is "a character f, followed by two o's, followed by a space, followed by a colon...". From this point of view, it is hard to see the forest behind the trees, and it includes too much unnecessary information. For example, we do not care if it is space, several spaces, or a tab separating foo and :=.

The first phase, scanning or lexical analysis, eliminates such unnecessary information and lumps individual characters together into larger pieces: lexemes or tokens. After scanning, the example is seen as "a word foo followed by a colon-equal operator, followed by a word self, followed by a word bar".

The compiler now sees a stream of tokens but still no structure behind it. The process of finding the structure is called parsing.   Its result is a tree representing the breakdown of the source into syntactic elements. After parsing the example, the compiler would know that the input is "an assignment with a reference to variable foo on the left hand side and the result of the message ````bar```` sent to self on the right hand side".

The parse tree is the meaning of the original program. This final phase, code generation, rewrites the meaning as target machine code. We are getting a little ahead of ourselves (we will discuss the bytecode understood by the interpreter a little later), but here is an idea of what our example could translate to:

````Smalltalk
    push self
    send #bar
    store local 0
````
In the remainder of the section, we will have a look at where these compiler stages are implemented. In Squeak, the classes related to the compiler are grouped under the System-Compiler category. In VisualWorks, they are further categorized under four System-Compiler-* categories.

## Scanning

In both VisualWorks and Squeak, the lexical analyzer is the class Scanner. We just discussed lexical analysis as a separate compilation step, though usually it is not. In the actual implementation, Scanner provides a stream-like access to the source, supplying the parser with tokens one by one at the parser's request. Tokens are not represented as real objects. Scanning a token merely sets two instance variables in the Scanner: tokenType and token. tokenType is a Symbol indicating the type of the token just scanned, for example #keyword or #number. token variable contains an object--often a String, a Number, or a Character, that represents the value of the token. For example, after scanning the first word of the example, tokenType would contain a Symbol #word and token--a String 'foo'.

Even though the scanner does not actually convert the source into a sequence of tokens in a single shot, it may be convenient, as we explore how the compiler works, to see a complete sequence of tokens corresponding to certain compiler input. The following method (which works in both Squeak and VisualWorks--they do have a common ancestor!) converts a String of Smalltalk code into an array of tokenType and token value pairs.

````Smalltalk
    Scanner>> scanAll: aString
        | tokenStream |
        tokenStream := WriteStream on: (Array new: 10).
        self scan: (ReadStream on: aString).
        [tokenStream nextPut: (Array with: tokenType with: token).
        tokenType == #doIt] 
            whileFalse: [self scanToken].
        ^tokenStream contents
````
As an exercise, use this method to scan various code fragments. They do not have to be valid Smalltalk code: as we mentioned, the Scanner does not analyze the structure of the input.  Anything that looks like a word scans as a token of type #word, be it a reference to a local variable foo, a global variable Foo, a message selector in an expression "self foo", or a literal object reference like nil, true, or false. Only later compilation stages figure out the actual meaning and translation of these words.

**Cuis example**. How to scan a String.
````smalltalk
s _ Scanner new. 
s scanTokens: '1 + 2'.                      "=> #(1 #+ 2) "
````

## Parsing

The parser is implemented as class Parser in both VisualWorks and Squeak. It is a hand-coded recursive descent parser (we will see what it means in one of the examples), easy to understand and modify. It uses Scanner to read tokens of the program being compiled, one by one as they are needed. An unusual implementation feature is that Parser is a subclass of Scanner! Whatever the reason for this design decision was, it apparently originates in Smalltalk-80 since VisualWorks and Squeak share it.

The entry point to the parser, at least one shared by VisualWorks and Squeak, is the message ````parse:class:noPattern:context:notifying:ifFail:```` understood by instances of Parser. The result of this message, provided no errors were encountered during compilation, is a parse tree. The following example works in both VisualWorks and Squeak
````Smalltalk
    Parser new
        parse: (ReadStream on: '(Point new x: 10; y: 16r20) r')
        class: UndefinedObject
        noPattern: true
        context: nil
        notifying: nil
        ifFail: [^nil]
````
and returns the root of the resulting parse tree. Squeak's ObjectExplorer is especially nice to view the result.

Let us have a look at the arguments the Parser expects, because many of them play an active role in compiling or highlight important points.

The first argument of the message is a Stream open on the source to compile. The source is either a method for a class in the system or a "standalone" snippet of code to be evaluated (for example, in a workspace).

If the source is the method, the second argument is the class where the method is to be installed.  The class provides information about the variables--instance, class, and pool--accessible in the method. The third (noPattern:) argument is false in that case, to indicate that the parser should expect to find a message pattern (method header) at the beginning of the source.

If the source is a snippet of code, the third argument is true to indicate that there is no message pattern at the beginning of it. The class: argument depends on the tool used to evaluate code; we will see this in one of the examples.

The context: argument is used in the case when the code should be able to use variables within an existing block or method context (a debugger may want to use this possibility).

The last two arguments are used to handle errors. The notifier: argument is the error handler. In VisualWorks, this object is typically an instance of a subclass of CompilerErrorHanlder.  If an error is found in a program, the notifier is responsible for reporting it to the user in some way. One notifier might insert the text into the code view at the location of the error, while another would simply print an error message in the Transcript.

If the error encountered is fatal and compilation should be aborted, the compiler invokes the block passed as ifFail: argument. This block usually includes a return statement to return from the context that invoked the Parser in the first place, thus aborting compilation.

Together, the notifier and ifFail block form an exception handling framework of sorts. The compiler and its protocol predate any of the existing Smalltalk exception frameworks by more than a decade, this must be the reason for not using a "real" one.

If no errors occur, the message returns with the root node of the parse tree of the source code. The tree is built of instances of subclasses of ProgramNode (VisualWorks) or ParseNode (Squeak). In VisualWorks, all of these classes are categorized under System-Compiler-Program Objects.

**Cuis example**. How to parse a code String snippet.
````Smalltalk
p _ Parser new.
". explore the output of this message send (Cltr+Shift+i, in Linux) "
p parse:  (ReadStream on: '1 + 2 * 3')  
	class:  UndefinedObject 
	category:  nil
	noPattern: true
	doIt:  false
	context: nil  
	notifying: nil 
	ifFail: [^ nil].
````

## The Smalltalk Machine

At the final stage, the compiler produces code that corresponds to the parse tree it just built. Because the meaning of the code is represented by the parse tree nodes, the code is generated by the nodes themselves.

We will have a look at the code generation protocol when time comes. Right now, we are more concerned about the "machine" the code will run on: the interpreter and the code it understands.

The alpha and omega of the interpreter are invocation contexts. They are, at least conceptually, heap objects--instances of classes MethodContext and BlockContext. A context holds information relevant to execution of the current method: the CompiledMethod which contains the bytecode being executed, the receiver object, the values of method's temporary variables (the arguments are considered temporary variables, too), and the program counter. It also keeps track of the sender--the context from which the message that the current context processes was sent. The chain of contexts in memory linked by their sender fields roughly corresponds to the call stack of "conventional" programming languages.

The interpreter manipulates values in contexts when a program runs. Again, this is only a conceptual view.  In a real system, the interpreter (or the dynamic compiler like in VW) would not probably use heap objects to execute code. However, it would still provide this illusion through thisContext pseudovariable.

The values the interpreter uses when it executes the code are the program counter, the receiver, the method literals and temporary variables (literals reside in the CompiledMehod object, temporaries--in the current context). Message arguments are treated as temporaries (locals in VisualWorks terms). There are bytecodes to work with these values. Another important structure most bytecodes refer to is the stack. Again, this is not the kind of stack used in conventional languages to keep track of function return addresses. The stack used by the interpreter is part of a context, so each method invocation has its own little stack. It is like the data stack in Forth: it is used instead of registers to temporarily hold onto data. One cell of a stack can hold one object (that is, either an object reference or an immediate object like a SmallInteger).

We do not need a complete bytecode specification, but here is an overview of most basic instructions--to get a feel for what is available. Instruction names are not "official", partly to avoid unnecessary detail and complication, partly to not commit to a particular implementation, since we look at two systems with similar yet different bytecode sets.

Data manipulation:
````
push self - push the receiver of the current message on the stack.
push instance N - push the receiver's instance variable at index N.
push temp N (or push local N in VW) - push an object in temporary N of the current context.
   As mentioned before, arguments are temps too. For example, in a method of 2 arguments
   temps 0 and 1 are the message arguments, temps 2 and on are the temporary variables
   declared in the method.
push literal N - push an object at index N in the current method's literal frame
   (we will discuss literal frames when the time is right).
pop instance N - pop the object on top of the stack into the receiver's instance variable at index N.
pop temp N - pop the object on top of the stack into the context's temp N.
````
For an obvious reason, there is no pop self or pop literal N. Two extra stack-related operations are
````
dup - just like in Forth, push a copy of the current top of the stack--as we will see, handy to implement cascades.
pop - discard top of the stack
````
A program may combine these instructions to perform assignments. foo := self could be translated as
````
    push self
    pop temp 0
````
To encode message sends, a message selector is stored in the method's literal frame. A message ````send```` instruction
````Smalltlak
    send N
````
refers to the selector to send by index in the literal frame. Keeping this in mind, we will ignore the index and write message sends directly as send #foo. Message receiver and arguments are pushed on the stack before the send instruction executes (first the receiver, then the arguments in first-to-last order). After the send returns they are replaced with a single object: the returned value. For example, a code fragment foo := self bar: 1 with: #one could be translated into:
````
    push self
    push literal 0  "lit.0 holds a SmallInteger 1"
    push literal 1  "lit.1 holds a Symbol #one"
    send #bar:with:
    pop temp 1
````
As an optimization, implementations often provide special instructions to push popular objects such as true, false, nil, 0, 1, without storing them in a literal frame, so the actual code you would see if you try this example may be a little different.

Finally, control instructions, or branches... Are they needed at all? They would not be if messages like ifTrue: were real messages. Since for optimization reasons they are inlined, conditional and unconditional branches are needed to encode the flow of control in the code like in any assembly language program.

An unconditional branch passes control to another location in the method bytecode. The location is specified by offset. There are often several forms of same branch instruction, depending on how large is the offset. We are only concerned with the symbolic instruction form, so we will not care about those different formats.

A conditional branch expects to find a Boolean on top of the stack. It branches--or does not branch--depending on that value. It also removes that value from the stack regardless of the outcome of the test. Here is an example involving a condition:

````Smalltalk
    foo := self foo ifTrue: [self bar] ifFalse: [nil]
````
and a possible translation:
````
    push self
    send foo
    jump ifFalse L1
    push self
    send bar
    jump L2
  L1:
    push nil
  L2:
    pop temp 0
````
Finally, when a method completes, it is expected to leave its answer on the stack and execute a return instruction. Again, implementations may provide special instructions to push a frequently used return value such as self and return in a single instruction.

## How to See Things

To make sure examples in this article work, we need convenient access to methods' bytecodes and decompiled code.

Both in Squeak and in VisualWorks, when you select a method in a browser holding Shift down, the browser shows the method's decompiled code instead of the code from the source file.

In Squeak, there is a menu item Show Bytecodes in the code view menu. Selecting it shows the method's bytecodes instead of the source.

In VisualWorks with ProgrammingHacks loaded, there is an inspect item on the utilities menu of the method list view. Selecting that item opens an inspector on the selected CompiledMethod. A compiled method inspector can show the method's bytecodes.

**In Cuis**. Visit a method with the *Browser*, RightClick the method name and select *inspect CompiledMethod*. An *inspector* window 
will show up, on the left pane select *all bytecodes*.

Granted, this is hardly direct access. Could we rig the system to show the bytecodes when a method is selected with the Control key does, like decompiled code is displayed when the Shift key is down? Yes, we could.  Surprisingly, the check for Shift is in the CompiledMethod's (CompiledCode's, actually) method getSourceForUserIfNone: rather than in the browser. The first line of the method invokes the decompilation block when Shift is down. A single additional line:
````Smalltalk
    InputState default ctrlDown ifTrue: [^self symbolic].
````
would do the trick with displaying bytecode.

And now, on to the examples.

## Private Selectors (VisualWorks)

Squeak includes support for "private methods", with privacy enforced at compilation time. The idea of enforcement is simple: messages with selectors that begin with "pvt" can be sent only to self. An attempt to send such a message to anything but self is reported as a compilation error.

Regardless of the merits of this innovation, implementing it in VisualWorks makes a good example of a simple compiler modification. To get the grunt's work out of the way, we begin by adding a test method to Symbol to check whether it is a selector of a private message:

````Smalltalk
    Symbol>> isPrivateSelector
        self size > 3 ifFalse: [^false].
        1 to: 3 do: [:index | 
            (self at: index) = ('pvt' at: index) ifFalse: [^false]].
        ^true
````
Now let us look at the compiler. We begin by investigating how message sends are represented in parse trees. A little earlier we how to use the Parser to produce a parse tree from a string of source code. If we use it to build a tree for:
````Smalltalk
    self new: 10 withAll: Character space
````
the relevant portion of the result (best viewed using Squeak's ObjectExplorer) looks like the following:
````Smalltalk
  MessageNode
    selector: #new:withAll:
    receiver: a VariableNode {self}
    arguments:
      argument 1: LiteralNode {10}
      argument 2: MessageNode
                    selector: #space
                    receiver: a VariableNode {Character}
                    arguments: #()
````
The whole result is an instance of MethodNode. A Parser always produces a MethodNode, even when it compiles a snippet of code to be evaluated.

Obviously, MessageNode is what we are interested in. Instances of MessageNode represent message sends in the parsed code. The messages we want to reject would appear as MessageNodes with the instance variable selector containing a Symbol that answers true to the privacy test, and the receiver being anything but self. Checking the selector is easy using the method we added to Symbol. How can we test the second condition? As we see in the parse tree above, a reference to self is represented by a VariableNode, just like a reference to the "real" global variable Character. Perhaps we can add a method to VariableNode to test whether it represents a reference to self. A look at the VariableNode in the browser shows that such a method already exists (isSelf).

The only question remaining is when to run the test. Obviously, it should run when the compiler traverses the parse tree. This happens when the code is generated by asking each node to emit code. There is an alternative. The parse tree is also traversed when it is being built.  The MessageNode initialization method receiver:selector:arguments: is invoked for every new message send node about to be included into a parse tree.

Squeak performs the test at the parse time; we will do the same simply because we have not yet studied code generation.

The class to change is SimpleMessageNode, the Abstract superclass of MessageNode. It defines and manages the message send attributes we are interested in. The ensurePrivacy method must be defined before the other method is changed! The receiver:selector:arguments: method runs every time a message send is found in the code being compiled. If you did not provide the ensurePrivacy method first, the compiler would fail trying to compile anything with message sends--including the ensurePrivacy method itself! Welcome to compiler hacking. If the compiler is broken often you cannot fix it because, well, the compiler is broken...
````Smalltalk
    SimpleMessageNode>> ensurePrivacy
        (selector isPrivateSelector and: [receiver isSelf not]) 
            ifTrue: [self error: 'can only send this to self']

    SimpleMessageNode>> receiver: rcvr selector: sel arguments: args
        receiver := rcvr.
        selector := sel.
        arguments := args.
        precedence := nil.  "compute on demand"
        self ensurePrivacy
````
In this particular case, if we did change the receiver:selector:arguments: method first, we could still change the method back to its original definition, since the original method contained no message sends. We could then define ensurePrivacy and change the receiver:selector:arguments: again. Recovery like that is an exception rather than a rule, though.

The modification is complete and you can try it out.  You may notice one glitch related to error reporting. What is a MessageNode supposed to do if it decides to reject a message send? We use ````self error: ...```` which opens a debugger. This is not how compilation errors are usually reported. Most often, the error message is pasted right into the code view.

This is done by sending messages to a notifier object (remember that notifying: argument passed to the Parser?). In VisualWorks, this object is usually an instance of one of the subclasses of CompilerErrorHandler. The requestor is held on to by a Parser and is available through the Parser's messages under the error handling protocol. The problem is, in VisualWorks a MessageNode does not know the parser when the node is being initialized. This is different from Squeak, where one of the arguments of a MessageNode initialization method is an Encoder object. The Encoder knows how to properly report errors using the original notifier.

For this reason, the privacy test in VisualWorks should have actually been introduced into the code generation logic.

Finally, note that it would be trivial to relax the privacy check a little and allow private messages to be sent to super as well as to self. I bet there would be those who would say this would make sense, as well as those who would say it would not, as well as those who would say this whole privacy test thing is just a gimmick inspired by lesser languages.

## Compile Time Evaluation (VisualWorks)

The next tour stop is something found in Dolphin Smalltalk and IBM Smalltalk: compile-time evaluation. Frowned upon by many as an arcane feature with few credible applications, it is still an interesting compiler exercise. It would be nice to implement in Squeak since my implementation for VisualWorks exists and has been available from the UIUC archive for a while. Unfortunately, as we will see, implementing it in Squeak would require significant changes to both the Scanner and the Parser.

Compile-time evaluation is support for arbitrary literal objects in methods. In classic Smalltalk, you can specify some objects, such as Strings, directly in the code.  Here is how it works. For example, when the compiler sees
````Smalltalk
    "hello" size
````
it creates an instance of String with the proper content and stores the instance as one of the CompiledMethod's indexed variables. A Smalltalk VM can refer to objects in a CompiledMethod's indexed variables. The bytecode of the method would include instructions to fetch a reference from a certain indexed variable and send it the size message. These indexed variables are used by the compiler for various purposes, including storing literals and selectors of messages sent by the method, and are collectively known as the method's literal frame.

Any object could be stored in a literal frame, but Smalltalk syntax includes notation only for literal Numbers, Characters, Strings, Symbols ans Arrays. Compile-time evaluation is a mechanism to allow literals of other types. The expression to create a literal object is used in a method in place of the literal, wrapped in #(). The expression is evaluated when the method is compiled--hence the name of the feature--and the result is used as a literal object in place of the #() construct. For example, these two lines would be equivalent:
````Smalltalk
    'abc' size
    #(String with: $a with: $b with: $c) size
````
Compile-time evaluation can be used to include literal dictionaries in a method:
````Smalltalk
    errorStringFor: anInteger
        ^#(Dictionary new
            at: 1 put: 'File not found';
            at: 2 put: 'Not enough memory';
            yourself)
                at: anInteger ifAbsent: [^'Unknown error']
````
What would it take to implement something like that in VisualWorks or Squeak? We can probably reuse the existing mechanism that handles "classic" literal objects such as Strings. As we could see in the parse tree of self new: 10 withAll: Character space in the last section, the literal 10 was represented by an instance of LiteralNode. Experiments with other literal objects show that any literal is parsed as an instance of LiteralNode, with the actual object stored in the value variable of the node. This suggests a simple plan of attack. All we have to do is tweak the parser to recognize the #(...) thing, evaluate whatever is found between the parentheses, and produce a LiteralNode with the result of evaluation as its value. The code generator will take care of the rest.

This sounds simple enough but before we begin, let's check how #() scans. The scanAll: method we added to the Scanner will show that a sample expression, say
````Smalltalk
    #(Time now)
````
scans in VisualWorks as
````
    <literalQuote>
    <literalQuote>
    <leftParenthesis>
    <word 'Time'>
    <word 'now'>
    <rightParenthesis>
````
while the Squeak Scanner, on the other hand, scans it as a single token
````
    <literal #(#Time #now)>
````
Apparently there is a significant difference between VisualWorks and Squeak in handling things that begin with a hash. The general organization of the Scanner is the same in both dialects: a Scanner reads a character and looks it up in a dispatch table by its ASCII code. The table is stored in the TypeTable class variable. It holds selectors of Scanner methods used to process characters of different types. The table is initialized in the class-side initialize method. The method responsible for anything that starts with $# is xLitQuote.

In Squeak, this method fully processes literal Arrays and Symbols. The Parser receives a token of type #literal with the literal Array or Symbol as the token value. In contrast to that, VisualWorks scanner is lighter. It breaks the input into primitive chunks such as words and hash marks (#literalQuote tokens), and it is the Parser that is responsible for recognizing literal Arrays or Symbols. For example,

````Smalltalk
    #(foo)
````	
is scanned in VisualWorks as
````
    <literalQuote>
    <leftParenthesis>
    <word 'foo'>
    <rightParenthesis>
````
to be converted by the Parser into a LiteralNode with value #(#foo), while in Squeak it leaves the Scanner as a token
````
    <literal #(foo)>
````
Now you see why implementations for Squeak and VisualWorks would be very different. We will proceed with the VisualWorks one. By the time we are done, it will be clear why the balance of Parser vs. Scanner responsibilities chosen in VisualWorks makes the implementation much easier.

It was already mentioned that the Parser uses recursive descent approach. Details and theory are available in any compiler textbook. For our purposes, understanding the general idea is enough. Essentially, the parser is a group of methods "parallel" to the rules of the language grammar. There is a method corresponding to each rule of the language grammar. The method checks the next token on the input stream (the lookahead) to see which of the possible alternatives of the rule to follow. Methods also include actions to build a parse tree.

This is best shown in an example. Here is a fragment of a somewhat simplified Smalltalk grammar:
````Smalltalk
    method ::= pattern temporaries body
    temporaries ::= '|' variable-name* '|' OR <empty string>
    variable-name ::= word
    body ::= statement*
    pattern ::= ...
````
A recursive descent parser (without the actions to build a parse tree) would look something like
````Smalltalk
    parseMethod
        self pattern; temporaries; body
    temporaries
        self lookahead == #verticalBar ifTrue:
            [self scan.
            [self lookahead == #word]
                whileTrue: [self variableName].
            self lookahead == #verticalBar ifFalse:
                [self syntaxError: 'Variable name expected'].
            self scan].
    variableName
        self addMethodTemp: self lookaheadValue.
        self scan
    ...
````
Parsing begins in the parseMethod method. Since, according to the grammar, method has only one possible expansion: a pattern followed by temporaries followed by body, parseMethod simply invokes the methods for these constructs in sequence. We ignore pattern in this discussion because it can be fairly involved--suppose it parsed successfully. The parser now expects to see a list of temporary variables, so temporaries method is called. According to the grammar, the list could be an empty string--that is, missing. The temporaries method has to decide which is the current case. Since body cannot begin with a vertical bar (this is not visible in the grammar fragment above, but we know it), the lookahead can be a vertical bar only if the temporaries declaration is present. The method checks the lookahead to choose the alternative to follow.

In short, the parser predicts what to expect further in the input based on the current token and the tokens seen so far. There are limitations to this parsing scheme, but it is easy to understand and implement by hand.

In VisualWorks, methods involved in recursive descent are under expression types-* protocols, and the entry point is the ````method:context:```` method. The method responsible for parsing things such as arrays, symbols and strings is constant. To make our modification, we will need to change this method to recognize the #() construct. The method is essentially a long case statement. Closer to the end we can see a number of tests for various things that can begin with a hash mark. (It is interesting to note that in VW 3.x, there is a test for #leftBrace--a qualified name literal, part of namespace support introduced in VW 5i which apparently has been in the works since before 3.x).

For our purposes, we add a case to recognize a second hash mark, shown in bold below. We can make this change immediately, without breaking the compiler. Even though compileTimeEval method is not yet in the system, the ````compileTimeEval```` message is only sent if we actually parse a #() construct.
````Smalltalk
  Parser>>constant
    ...
    tokenType == #leftBrace
        ifTrue:
            [self scanToken.
            self qualifiedNameLiteral.
            ^true].
    tokenType == #literalQuote "must be #(expr)"
        ifTrue:
            [self scanToken.
            tokenType == #leftParenthesis
                ifTrue:
                    [self compileTimeEval. "slurps (, expr, and )"
                    ^true].
            ^self expected: 'parenthesized eval-when-compile expression'].
    ^self expected: 'word, binary, keyword, #, (, or ['
````
We also fix the error message to mention a second hash mark as one of the things expected after a hash mark. (Again, note that VisualWorks implementors forgot to do that for left brace).

And now the heart of implementation: the compileTimeEval method. When the method is entered, both hash marks have already been scanned and the lookahead token is the opening parenthesis. Here is a summary of what the method should do to make compile-time evaluation work:

Read the token stream up to the matching closing parenthesis.

Parse and evaluate the tokens.

Build a LiteralNode with the evaluation result as the value.

We will follow implementation step by step, gradually filling in the blanks.

We begin with the last task, building the literal node, because it is the easiest. There is no need to do that. As the comment to the constant method explains, the method is expected to leave the constant object in the parseNode variable. Apparently, it is wrapped into a LiteralNode later by the caller of the constant method, so our method simply sets that variable:
````Smalltalk
    compileTimeEval
        ...read, parse, and evaluate...
        parseNode := ...evaluation result...
````
Reading tokens up to a closing parenthesis sounds simple enough--just read them one by one--but there is a catch. First, it should be a matching parenthesis. There may be other, nested, pairs of parentheses before it. Second, a collection of tokens, even after we scan in correctly as far as nested parentheses go, is useless to us. We need to parse and evaluate it, but the Parser has no interface to parse a pre-scanned sequence of tokens!  It would not be easy to add, too! (If you just  thought of constructing an object holding onto a sequence of tokens and simulating a Scanner by returning them one by one, pat yourself on the shoulder for having a perfect OO intuition). The problem is, in both Squeak and VisualWorks Parser is a subclass of Scanner--so a Parser and a Scanner is actually one object! Because of this design glitch, we cannot plug in a different Scanner and are stuck with a Parser that can only read character streams.

We can get through this, and easily. Think what the input looks like after we skip over the two hash marks of a #() expression. If the original source was #(Time now), the Parser/Scanner now looks at (Time now). Of course--this is just a parenthesized expression, or a primaryExpression in terms of the grammar the Parser understands! If we ask the Parser to parse a primaryExpression, it will gladly gobble everything up to the matching closing parenthesis and hand us the parse tree back.  Here is a second approximation of our method:
````Smalltalk
    compileTimeEval
        self primaryExpression.
        "parseNode now holds the parse tree of the #() body"
        parseNode := ...evaluate the current parseNode...
````
This is exactly why this is much easier to implement in VisualWorks than in Squeak. In Squeak, we would have to extend the scanner to read the input stream up the matching closing parenthesis, properly handling parentheses nesting, and then try to somehow parse that token sequence. While not impossible (nothing is impossible in Smalltalk, right?), that certainly would not be as easy as what we just did.

Now the next problem: evaluating the code. We do it often enough in Smalltalk, but now we have a parse tree to evaluate rather than a string. There is no interface to evaluate a parse tree. We will have to generate code from it and then evaluate that code.

First, generating code. The executable object in Smalltalk is a CompiledMethod, and this is what we have to produce. A CompiledMethod is always built from a full parse tree of a method (that is, one with a MethodNode as the root). In compileTimeEval, after having called primaryExpression, we only have a parse tree for the parenthesized expression. We need to build a full method parse tree out of the partial one we have. Parse trees are normally built by a special object, ProgramNodeBuilder, stored in the builder instance variable of the Parser. Browsing and borrowing around the system to figure out how to use it, we come up with the next approximation of our method:
````Smalltalk
    compileTimeEval
        | methodTree |
        self primaryExpression.
        methodTree := builder 
            newMethodSelector: #DoIt
            primitive: nil
            block: (builder newBlockBody: (builder newReturnValue: parseNode)).
        parseNode := ...build and execute a CompiledMethod from methodTree...
````
So we have a full method parse tree. Now we need a CompiledMethod built from it. The method will be "free": it will not reside in a method dictionary of any class in the system. There would be no way to execute such a method playing by regular Smalltalk rules, but VisualWorks allows us to execute such free methods.

The final important issue to consider is the scope used when we build a CompiledMethod. Any code is compiled within the scope of a certain class and executes as a message sent to an instance of that class. The scope determines what variables are accessible from the code. The code evaluated in the workspace is compiled in the scope of UndefinedObject. It executes as a message sent to nil.  (If you have never thought of it this way, evaluate self in a workspace and see what the result is. What is even more fun, investigate what happens if you evaluate an expression self DoIt in a Squeak workspace and why). Likewise, the code evaluated in a browser is compiled in scope of the class selected in the browser, so it can refer to class variables. The code evaluated in an inspector is compiled within the class of the object being inspected and is executed as a message sent to the object being inspected--so the code can use self and all of the object's instance variables.

A reasonable scope for compile-time evaluated code is the class where the method containing the code is defined. For example, #(self name) in an instance-side method of class Foo would capture a literal reference to a symbol #Foo.

To be able to evaluate the compile-time expression in that scope, the Parser needs to know the class where the method it now parses is supposed to be installed. In VW versions prior to 3.0, this information was not available. My older implementation in the UIUC archive adds targetClass instance variable to Parser. In version 3.0, VisualWorks implementors themselves added targetClass to Parser so that modification is no longer necessary. The (almost) final version of our method is:
````Smalltalk
    compileTimeEval
        | methodTree |
        self primaryExpression.
        methodTree := builder 
            newMethodSelector: #DoIt
            primitive: nil
            block: (builder newBlockBody: (builder newReturnValue: parseNode)).
        parseNode := targetClass performMethod:
            (Compiler new compile: methodTree in: targetClass class)
````
As a finishing touch, we need to take error handling into account. Evaluating the expression can result in an error. To report it gracefully, the parser needs to catch all exceptions that occur during evaluation. It also needs to save the start position of the expression in the source code. The position is used in the error notification protocol--so that, for example, a controller of a code view knows where to insert the error message.
````Smalltalk
    compileTimeEval
        | exprStart methodTree |
        exprStart := mark.
        self primaryExpression.
        methodTree := builder 
                newMethodSelector: #DoIt
                primitive: nil
                block: (builder newBlockBody: (builder newReturnValue: parseNode)).
        [parseNode := targetClass performMethod: 
                (Compiler new compile: methodTree in: targetClass class)]
            on: Error
            do: [:ex | self notify: 'evaluation error: ', ex errorString at: exprStart]
````
## (Almost) Inlined ifNil: (VisualWorks)

We just used the compile:in: method of a Compiler to produce a CompiledMethod from a parse tree. Now we will look in detail at what happens at that stage. In VisualWorks, the first thing that happens is macroexpansion.

Macros are not standard part of Smalltalk but VisualWorks does include an internal macro facility of sorts. It is used to replace some of the message sends with other nodes. Typically, the replacement nodes are special nodes like ConditionalNode or LoopNode which generate code in specialized way. For example, a MessageNode representing a send of ````to:do:```` message is replaced with another node (ArithmeticLoopNode). The replacement node generates an optimized inlined code for the looping construct to avoid the overhead of creating a block object and sending a message to it at each iteration. This kind of expansion introduces nodes into the parse tree that could not have been introduced otherwise. Another style of using the same facility would be replacing a message send with a group of other nodes, those that could have been written by hand. This is what macros are used for in some other languages.

A good example of that application is implementing ifNil:. The obvious implementation are two methods in Object and UndefinedObject:
````Smalltalk
  Object>>ifNil: aBlock
    ^self

  UndefinedObject>>ifNil: aBlock
    ^aBlock value
````
An alternative implementation could expand ifNil: into an equivalent form with an explicit nil comparison.
````Smalltalk
    ^foo ifNil: ['default']
````
would be replaced with
````Smalltalk
    ^nil == foo ifTrue: ['default'] ifFalse: [foo]
````
There is a catch though. Expanding the original like that result in code that can evaluate the receiver of ifNil: twice--which would matter if the receiver were an expression with side effects rather than just a variable reference. We could have used another expansion scheme. Those with a feel for closures would instantly come up with a scheme such as:
````Smalltalk
    ([:tmp | nil == value
        ifTrue: ['default']
        ifFalse: [tmp]]
            value: self stuffWithSideEffects)
````
This essentially introduces a local temporary variable--local in the sense we do not have to muck with the MethodNode somewhere up the parse tree to add that variable.

We won't actually use this scheme, though. For this exercise, it is more interesting to do a partial implementation. We will expand side effect-free expressions, and leave those with potential side effects untouched. Later, we will consider a "full" implementation that will generate bytecode in a special way to guarantee that the receiver expression is evaluated only once.

Macroexpansion is done on and by MessageNodes. Prior to emitting code, code generation methods in MessageNode ask the node to produce an expansion node (or a subtree of nodes). If expansion does occur, it is the expansion that generates the code. If no expansion occurs (the transformation method answers nil), the original node generates a plain vanilla message send. The expansion is only conceptual: the parse tree is not actually modified. The original message nodes that were expanded are still part of the tree. They hold onto their expansions and delegate code generation to them.

Expansion occurs if the selector of the MessageNode is found in MacroSelectors dictionary of MessageNode. The dictionary maps selectors that should be expanded to methods of MessageNode that perform the expansion.

In our case, we want to transform an expression like:
````Smalltalk
    <receiver> ifNil: <a block>
````
into
````Smalltalk
    nil == <receiver> ifTrue: <a block> ifFalse: [<receiver>]
````	
which means the macro transformer for ifNil: should build a parse tree of this shape (again, there is nothing like browsing around to figure out such things):
````Smalltalk
  ConditionalNode
    condition:
      MessageNode
        receiver: nil
        selector: #==
        argument: <the original receiver>
    trueBlock: <the block supplied to ifNil:>
    falseBlock: <block with the original receiver>
````
We should do it only if the receiver is side effect-free, and if the argument is a literal block. Testing for side effects appears to be very easy (yes, this is part of the reason I took this as an example). Testing protocol of ProgramNode includes hasEffect message. A MessageNode has a method to check that a certain argument is a literal block of a given number of arguments. This is all we need to know to write a macro transformer:
````Smalltalk
    transformIfNil
        "MacroSelectors at: #ifNil: put: #transformIfNil"
        ^((self testLiteralBlock: 0 at: 1)
               and: [self receiver hasEffect not])
            ifTrue:
                [ConditionalNode new
                    sourcePosition: self sourcePosition;
                    condition: (MessageNode new
                        receiver: (LiteralNode new value: nil)
                        selector: #==
                        arguments: (Array with: self receiver))
                    trueBlock: arguments first
                    falseBlock: (BlockNode new body: self receiver)
                    from: self]
            ifFalse: [nil]
````
The code required to register the transformer is in the method comment. If we packaged this modification as a parcel, registration could be done as a post-load action, with a "symmetrical" pre-unload action to unregister the transformer before unloading the parcel. Of course, the "regular" ifNil: methods in Object and UndefinedObject would go into the parcel as well to support the non-expanded cases.

To see that the code actually works, add a method like the following to your favourite sandbox class:
````Smalltalk
    foo: anObject
        ^anObject ifNil: [3 + 4]
````
Look at the decompiled code of that method. It should look like:
````Smalltalk
    foo: t1 
        nil == t1 ifTrue: [^3 + 4].
        ^t1
````
Change the method so that the receiver of ````ifNil:```` is an expression with side effects (a message send). The decompiled code will then be similar to the original source.

## Assertions (Squeak)

Now let us implement a fancy assertion mechanism. Assertions are trivial to implement as a message ````assert```` sent to a block, with the assert method defined either as a real test or as a no-op . We will implement a fancier version, to satisfy the following requirements.

The use of assertions should impose zero overhead. When the assertion preference is off, compiled methods should contain no traces of assertion code.

When assertions are on, assertion code is inlined in the containing method.

When the assertion setting is changed, all methods containing assertions are automatically recompiled.

Assertion failure signals an AssertionFailedError exception.

A few years ago I implemented a framework similar to the one I just described in VisualWorks 2.0. It used a less reasonable syntax (self assert: [...]), and has to be updated to work with VisualWorks 3.0 and later. It substitutes a special kind of program node, AssertionNode, in place of the original ````assert:```` message send. AssertionNode takes care of generating (or not generating) the inlined assertion code.

We will implement a similar facility in Squeak. Squeak handles special selectors in a way different from VisualWorks, which makes the exercise more interesting.  Instead of the already familiar parse tree transformation we will get a chance to actually play with code generation.

First, let us have a look how MessageNode works in Squeak.  Instead of the familiar MacroSelectors dictionary found in VW, we see five class variables in Squeak: MacroEmitters, MacroPrinters, MacroSelectors, MacroSizers, MacroTransformers. All of them hold an Array.  As you remember, in VisualWorks MessageNode MacroSelectors is a dictionary that maps selectors of messages that should, or could, be handled specially to selectors of macro transformer methods.

The Squeak approach is similar but different. MacroSelectors is an array which holds selectors of messages that may require special handling. During initialization, a MessageNode looks up its selector in that array. If it finds the selector, it remembers its index as a value of instance variable special. Otherwise, the value of special is set to zero to indicate that no special handling is required.

As far as code generation goes, MessageNode is responsible for everything. Unlike one in VisualWorks, it does not build expansion nodes to delegate special code generation to them.  Code generation is controlled by the value of special instance variable. If it is zero, a regular message send is produced. If it is not, the value of special is used as an index into one of the four other Macro* arrays to find the selector of a method responsible for a particular code generation step. For example, #ifFalse: is a special selector. It is found at index 2 in MacroSelectors. The method of MessageNode responsible for preparing the node for code generation is transformIfFalse:, found at index 2 in MacroTransformers. The method responsible for producing bytecode is ````emitIf:on:value: ````, found at index 2 in MacroEmitters.

There are three steps of code generation in Squeak: transformation, sizing, and emitting.

At the transformation step--entered through the transform: method--a MessageNode determines if the message send actually needs special treatment and prepares for it. For example, if the message is ````ifTrue:```` but the argument is a variable rather than a literal block, the node should generate a regular message send. In such cases, transformation method should return false.  If that happens, special is reset to zero and for the rest of code generation everything works as it would for a regular message send.

Suppose a message send does need special treatment.  A transformation method answers true to indicate that.  Before returning, though, it usually prepares and remembers any helper objects to assist it in code generation. For example, ````transformToDo:```` message creates AssignmentNodes to generate code that initializes and increments the loop variable. A MessageNode follows an ad-hoc approach to preserve these objects.   Most transformation methods collect all helper objects into an array and stuff that array into the arguments instance variable (replacing or adding to the actual message send arguments). Sizing and emitting methods expect to find them there.

Sizing is a preparatory step for code generation. At that point, a node determines the size of the bytecode it will generate and any of the control branches. This information is needed to correctly generate forward branches.

Here a sketch of how an ifTrue:ifFalse: is translated to illustrate why sizing is necessary.
````
    <condition code>
    jump ifFalse to L1
    <trueBlock code>
    jump to L2
  L1:
    <falseBlock code>
  L2: ...
````
To generate branches in the above code, the node needs to know the size of the trueBlock to produce the first jump ifFalse branch around it on false condition, as well as the size of the falseBlock to produce an unconditional jump to branch from the end of the trueBlock to the end of the statement. A sizing method of ifTrue:ifFalse: in MessageNode calculates sizes of the condition test, the true block, and the false block. It remembers sizes of the blocks in its sizes instance variable because it will need them later, and returns the total size of the whole statement--the sum of the sizes of the condition test, both conditional blocks, and all branch instructions involved.

There are two sizing methods in ProgramNodes: sizeForEffect: and sizeForValue:. The difference is important. Any Smalltalk expression has a value, though that value is sometimes ignored.   For that reason, each program node can generate two kinds of code: one for-value--the bytecode that executes and leaves the value of the expression at the top of the stack; the other for-effect--so the stack is the same after executing the code as it was before. Often, the code for-effect is same as code for-value followed by a pop. The two sizing methods are responsible for calculating the size for each kind of code.

In case of a special selector, MacroSizers dispatch table is used to invoke a method to calculate the size.   There is just one sizing method for each special selector, with last argument indicating whether the code is to be for-effect or for-value.

Finally, when it is time to generate code, a message ````emitForEffect:on:```` or ````emitForValue:on:````  is sent to a node. The difference is same as above. For example, a plain vanilla message send ````self foo:```` 1 would compile for value as
````
  pushSelf
  push 1
  send: foo:
````
and for effect as
````
  pushSelf
  push 1
  send #foo:
  pop
````
As usual, MacroEmitters dispatch table is used to select a method to emit special case code. The arguments to a macro emitter are two objects: the stack and the bytecode stream. The stream is the bytecode stream we write instructions to. The stack is an instance of ParseStack that measures the maximum stack depth a method may need. (The maximum stack depth is required to build a proper CompiledMethod). Without going into too much detail, when you emit a pop instruction, you should send pop: 1 to the stack, when you emit a push instruction, you should send push: 1.

Armed with this knowledge, let's get to work. In a nutshell, given an expression
````
    [<expr1>.
    <expr2>] assert
````
we want to emit code (assuming we want to emit code at all) equivalent to
````
    <expr1>.
    <expr2> ifFalse: [AssertionFailedError signal]
````
We will add a class variable IncludeAsserts to MessageNode to keep track of whether we should inline or omit assertions.

The transformation is possible only if the receiver is a literal block. Otherwise we should compile assert as a real message send (assert is used as a message for purposes other than assertions, and we don't want to break those). Of course, it is an error if assert is sent to a literal block, but the block expects one or more arguments. Here is how we can write the transformer:
````Smalltalk
  transformAssert: encoder
    (receiver isMemberOf: BlockNode) ifFalse: [^false].
    receiver numberOfArguments = 0 ifFalse:
        [^encoder notify: 'Receiver must be a 0-argument literal block'].
    arguments := Array with:
        (MessageNode new
            receiver: (encoder encodeVariable: 'AssertionFailedError')
            selector: #signal
            arguments: #()
            precedence: 3
            from: encoder).
    ^true
````
We manufacture and store a MessageNode corresponding to a message send ````AssertionFailedError signal````. We will later use it to generate code. In the already mentioned ad-hoc style, we stuff it in the arguments variable as the only element of the array.  (It would probably be no harm to store the node itself, but it goes too much against the variable name which suggests a collection).

Next we write the emitter.  (It is easier to write a sizer afterwards, when we know sizes of what parts of the code emitter needs to generate branches).
````Smalltalk
  emitAssert: stack on: stream value: forValue
    IncludeAsserts == false ifTrue: [^self].
    receiver emitForEvaluatedValue: stack on: stream.
    self emitBranchOn: true dist: sizes first pop: stack on: stream.
    arguments first emitForEffect: stack on: stream
````
The first line is simple.  If we choose not to include assertions, we bail out immediately and end up generating no code at all. The condition IncludeAsserts == false makes any object an acceptable a truth value--most importantly nil, the value of an uninitialized variable. This saves the trouble of changing the class initializer.

If we decide to inline the code, we start by emitting the code to compute the condition. We need the condition (the receiver) to produce code for-value. Sending ````emitForValue:on: ```` to the condition (a block) would be wrong, though--the result would be equivalent to writing
````Smalltalk
    [...] ifFalse: [...]
````
Instead, we should "unwrap" the block and generate the code for its content. To do that, we send ````emitForEvaluatedValue:on: ```` to the BlockNode (because "messages" like ifTrue: need to unwrap blocks as well, BlockNode conveniently provides this method).

Next we want to generate code to signal AssertionFailedSignal, preceded by a branch that will take us around that signaling code when the value on top of the stack (the value of the condition) is true. To generate a branch, we need to know the size of the signaling code. We take a mental node to calculate and store that size in the sizes Array in the sizing method, and generate the branch using the supposedly calculated size. The branch kicks in if the value on top of the stack is true (first argument) and jumps "sizes first" bytes ahead, presumably just far enough to get past the exception signaling code.

Finally, on the third line we delegate to the node for AssertionFailedError signal built during transformation to generate the exception signaling code.

Now look back at the emitter. Can you notice something missing?

In principle, we should always emit code of the type requested by the forValue argument. The code we emit for an assertion is always for-effect. The first part, the condition code, leaves a single boolean value on the stack. The branch instruction consumes that value regardless of whether it is true or false. After the dust settles, nothing is left on the stack.

Should we provide a for-value code generator? If we decide to, what is the value of assert supposed to be, considering it might be compiled as a test or as a no-op?

We could settle on some reasonably featureless object as a value, but I prefer more a radical approach. Let us declare that assert has no meaningful value and expressions like
````Smalltalk
    foo := [self check] assert
````
should be detected by the compiler and reported as illegal. How hard would it be to implement such checking?

All that is needed is for the node to complain if the forValue argument is true. The only problem is, the node does not know the right object to complain to when the emitting method runs. Errors are supposed to be detected earlier and reported using the encoder object. Encoder is available at the transformation and sizing stages as one of the arguments.

At the transformation stage, we do not know yet if the code is to be for-effect or for-value. The best (and the only) place for the test is then the sizing method.

The sizing method is responsible for one more thing. Remember cross-referencing. If we simply emit code like we did above, methods that use assert will not be detected as senders of assert because regardless of whether assertions are inlined or omitted, the ````assert```` message is never sent.

This is very easy to fix: senders are detected by the Symbols they have in their literal frames. If we artificially insert a Symbol #assert in the literal frame of a method, the method will be found as a sender of assert even though it does not really send that message.

We will place another symbol into the literal frame as well: #inlinedAssertionMarker. When the assertion setting changes, we will search for methods referencing that symbol to find all methods that include inlined assertions to recompile them.  Searching for #assert would find us more than we need: #assert is also used in some other parts of the system.

Here, finally, is the sizing method:
````Smalltalk
  sizeAssert: encoder value: forValue
    forValue ifTrue:
        [^encoder notify: 'Assertions can be used for effect only'].
    encoder 
        litIndex: #assert;
        litIndex: #inlinedAssertionMarker.
    IncludeAsserts == false ifTrue: [^0].
    sizes := Array with: (arguments first sizeForEffect: encoder).
    ^(receiver sizeForEvaluatedValue: encoder)
        + (self sizeBranchOn: true dist: sizes first)
        + sizes first
````
The encoder is responsible for keeping track of literals, so we use it to add literals to the method. ````litIndex:```` message answers the index of the object in the literal frame. We are not interested in it because we put the symbols there only as markers. There is nothing unexpected about the size calculation itself. The method returns the grand total size of code we generate, including the condition, the signaler, and the branch instruction between them (we send a special message to compute the size of the branch because the size of a branch can depend on branch condition and the distance to the target).

We are almost done. If we now change MessageNode class>>initialize method to put #assert at the end of MacroSelectors, and selectors of methods we've just listed at the end of their respective dispatch tables, the special handling for assert should kick in. Of course, we also need to provide the AssertionFailedError class.

A method like
````Smalltalk
  foo: n
    [n > 0] assert.
    ^3 + 4
````
should now compile fine and work as expected (including signaling AssertionFailedError when the argument is negative, with an option to resume execution). Decompiling the code (hold down Shift as you select the method in a browser) will show
````Smalltalk
  foo: n
    n > 0 ifFalse: [AssertionFailedError signal].
    ^3 + 4
````
Change the value of IncludeAsserts class variable to false, recompile the method (we will eventually have automatic recompilation) and look at the code--it will be
````Smalltalk
  foo: n
    ^3 + 4
````
Search for senders of assert--the method will be there. Victory? Almost. Try writing a method that sends assert to a variable.  Such send is compiled as a regular message.  Try to decompile the method--boom!  Even though we mostly talk about the compiler here, now is the time to say a few words about the decompiler.

The Decompiler scans bytecodes of a CompiledMethod looking for specific patterns and reconstructs a parse tree that could have been used to produce that bytecode. It is not necessarily identical to the one that was actually used, but it has same meaning. The parse tree is then asked to print itself, producing readable decompiled Smalltalk source code.

This is what MacroPrinters are about. They provide a hook to nicely format control structures. Interestingly, we don't really need one for inlined assertions themselves. When assertions are inlined, the decompiled parse tree does not have a node for an ````assert```` message send at all.  It is when ````assert```` is a real message send, the decompiled tree contains a MessageNode for it.  A MacroPrinter for assert is called and our half-baked version breaks because we have not provided one. So let's do it.  The printer in our case should simply print the message send without any special formatting.  The receiver has already been printed by the time the printer is called.
````Smalltalk
  printAssertOn: aStream indent: level

    self 
        printKeywords: selector key
        arguments: arguments
        on: aStream
        indent: level
````
The rest of the implementation are technical issues: class-side accessors in MessageNode to query and change the value of IncludeAsserts flag and find and recompile all methods in the system with #inlinedAssertionMarker. Nothing special there--see the complete implementation.

## (Really) Inlined ifNil: (VisualWorks)

We just got our feet wet doing simple code generation. That generating, however, could have been done--and has been done in the VisualWorks version of Assert--at the macroexpander level. All we did was replacing something that looked like
````Smalltalk
  [<foo>] assert
````
with
````Smalltalk
  <foo> ifFalse: [AssertionFailedError signal]
````
This is a purely syntactic substitution. Even in Squeak with less of a "formal" macro facility than in VisualWorks, we could have constructed the parse tree for the whole "... ifFalse: [AssertionFailedError signal]" expression, stored it in the assert MessageNode, and delegated code generation to it. We followed another implementation route simply to illustrate key issues related to code generation in Squeak.

This final example reimplements the inlined ifNil:.  We follow a different approach, to inline ifNil: always, regardless of whether code is side effect-free or not. Implementation will generate non-trivial bytecode--the kind we could not have produced using macroexpander.

Initially I planned this to be a Squeak example (just to end up with a more fair VW/Squeak example ratio). At the time of the writing, Andrew C. Greenberg independently started working on an inlined ifNil: for Squeak. By the time this article is available, his implementation will probably be available as well. To not duplicate the effort, this will be another VisualWorks example. One the positive side, this gives us a chance to look at interesting differences between code generation in Squeak and VisualWorks.

The reason we wanted to detect code with side effects in the original ifNil: implementation was the possibility to evaluate it twice in the expansion code.  We used built-in capability of ProgramNodes to detect side effects, and gave up on the transformation if that was the case. Unfortunately, side effect detection is not (and cannot) be very smart.  Literal expressions and variable references have no side effect, anything else--including and first of all message sends--is assumed to have one. This is a very pessimistic but the only feasible approach. Something like
````Smalltalk
    self name ifNil: [self defaultName]
````
would not be transformed by our simple ifNil: transformer. This prevents many potential uses of ifNil: from being inlined.

An obvious expansion scheme that avoids multiple evaluation would be:
````Smalltalk
    | tmp |
    tmp := <expr1>.
    tmp == nil ifTrue: [<expr2>] ifFalse: [tmp]
````
It is possible to manipulate the parse tree introduce an artificial temporary variable into a method (or into one of the method's blocks), but a much simpler and more elegant solution is to generate ifNil: so that the value of the receiver expression is temporarily saved on the stack.  Here is a sketch of what the generated code should look like:
````
    <for-value code of expr1>   ; value is now on the stack
    dup
    push nil
    send #==                    ; value and aBoolean are on the stack
    jump ifFalse to L1          ; branch consumes aBoolean
    pop
    <for-value code of expr2>
L1:                             ; at this point either the <expr1>
    ...                         ; value or <expr2> value is on the stack
````
We will reimplement ifNil: following this approach. As usual in VisualWorks, we begin by defining a class to represent the special selector node, IfNilNode, and add a transformation method to MessageNode. The method reads
````Smalltalk
  transformIfNil
    "MacroSelectors at: #ifNil: put: #transformIfNil"
    ^(self testLiteralBlock: 0 at: 1)
        ifTrue:
            [IfNilNode new
                baseNode: receiver
                alternativeNode: arguments first]
        ifFalse: [nil]
````
Nothing unusual here. IfNilNode remembers parse trees for the the receiver and the argument of the original ifNil: message.

Code generation in VisualWorks is similar to one in Squeak--the code is generated by the nodes of the parse tree.  Each node understands two messages: emitValue: and emitEffect:. Infrastructure to  support code generation, however, is better developed in VisualWorks. One immediately visible feature is that nodes do not worry about calculating code size to generate branches.

Code generation methods accept just one argument: codeStream, an instance of CodeStream.  A code stream is a "smart" code generator. It holds a bytecode stream and knows the encoding of various VM instructions. It keeps track of the method stack usage (so there is no need for a separate stack object as there is in Squeak).  It also keeps track of locations within the code to help generate branches. It uses a special object called CodeLabel to keep track of logical locations in code. Targets of branch instructions are specified through labels rather than using explicit offsets. Labels are created by the CodeStream.

````labelHere```` message sent to a CodeStream answers a label marking the current location in the code. The label can later be used to generate a branch jumping back to the current location.

````newLabel```` answers a "future label" representing a yet undefined location for forward references. A forward label can be used like an already defined label can. It makes forward branches possible. When the location the label represents is reached, a message ````define:```` is sent to the code stream with the label as the argument:
````Smalltalk
    aCodeStream define: aLabel
````
The code stream defines the label to represent the current location in the code and backpatches the already emitted instructions which used the just defined label. In other words, CodeLabels play the role of an assembler's symbol table. This is a much easier to use mechanism than explicit code size calculation of Squeak.

Here is the implementation of ````emitValue: ```` method of IfNilNode.
````Smalltalk
  emitValue: codeStream
    | exit |
    exit := codeStream newLabel.
    baseNode emitValue: codeStream.
    codeStream 
        dupFirst; "now two values on the stack"
        pushConstant: nil;
        send: #== numArgs: 1;
        putBranchTo: exit if: false; "leaves one copy on the stack"
        pop.
    alternativeNode emitValue: codeStream.
    codeStream define: exit
````
It is a straightforward implementation of the code generation sketch given above. It uses the exit label to define a forward branch around the alternative code. We do not provide a separate ````emitEffect:````. The default ````emitEffect:```` uses ````emitValue:```` to emit for-value code and then adds a pop to it. This is good enough.

As a side note, you can find three instructions in VisualWorks with dup in the name: dupFirst, dupNext and dupLast. In terms of "plain" stack operations, however, dupFirst works as "dup", dupNext works as "pop; dup", dupLast, quite surprisingly, works simply as "pop". They are used to encode Smalltalk cascades.  A cascade
````Smalltalk
  expr
    foo;
    bar;
    baz
````
is encoded as (comments on the right show the contents of the stack after execution of each instruction; stack top is on the right)
````
  <for-value code for expr>  ; <value of expr>
  dupFirst                   ; <value of expr> <value of expr>
  send foo                   ; <value of expr> <value of foo send>
  dupNext                    ; <value of expr> <value of expr>
  send bar                   ; <value of expr> <value of bar send>
  dupLast                    ; <value of expr>
  send baz                   ; <value of baz send>
````
The reason these three instructions are used instead of combinations of  dup and pop is probably to make it easier for the Decompiler to recognize cascades.

Speaking of decompilation. Our inlined ifNil: should already work, provided IfNilNode includes mundane things such as instance variables to hold onto baseNode and alternativeNode, and transformIfNil: has been registered in MacroSelectors of MessageNode as a transformer for ifNil:. We can look at bytecodes of a CompiledMethod with an inlined ifNil: to see that the code is correct. The problem is, we cannot use the Decompiler on such methods.  It is written to recognize certain bytecode patterns. The pattern generated by IfNilNode is unlike anything else, and this confuses the Decompiler. (It seems to think this code is the beginning of a message cascade, because of the dupFirst instruction).

A perfect implementation should have included a Decompiler fix to recognize this possible new pattern and correctly translate it to a parse tree node. Because this is just an example, we will ignore the decompilation problem altogether. (In fact, those concerned with issues of decompilation can find it interesting to read the Decompiler class comment and the description of mustBeSimple instance variable in the ByteCodeStream class comment).
