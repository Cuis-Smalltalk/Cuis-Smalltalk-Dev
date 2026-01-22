'From Cuis7.7 [latest update: #7805] on 22 January 2026 at 12:40:13 pm'!

!Object methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
yourself
	"Answer self."! !

!Object methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	"Append to the argument aStream a sequence of characters that is an expression 
	whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'! !

!Object methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'! !

!Object methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'! !

!Object methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class! !

!Object methodsFor: 'updating' stamp: 'ST-80 31/May/1983 09:10:35'!
changed
	"Receiver changed in a general way. Inform all of the observers by sending
	them an #update: message.
	Note: In Smalltalk-80 observers were called 'dependents'."

	self changed: self! !

!Object methodsFor: 'updating' stamp: 'ST-80 31/May/1983 09:10:35'!
update: aspectSymbol
	"Receive a change notice from an object we are observing. 
	The default behavior is to do nothing.
	Subclasses should act appropriately on the aspects they are interested in."

	^self! !


!Object class methodsFor: 'documentation' stamp: 'ST-80 31/May/1983 09:10:35'!
whatIsAPrimitive
	"Some messages in the system are responded to primitively. A primitive   
	response is performed directly by the interpreter rather than by evaluating   
	expressions in a method. The methods for these messages indicate the   
	presence of a primitive response by including <primitive: xx> before the   
	first expression in the method.   
	  
	Primitives exist for several reasons. Certain basic or 'primitive' 
	operations cannot be performed in any other way. Smalltalk without 
	primitives can move values from one variable to another, but cannot add two 
	SmallIntegers together. Many methods for arithmetic and comparison 
	between numbers are primitives. Some primitives allow Smalltalk to 
	communicate with I/O devices such as the disk, the display, and the keyboard. 
	Some primitives exist only to make the system run faster; each does the same 
	thing as a certain Smalltalk method, and its implementation as a primitive is 
	optional.  
	  
	When the Smalltalk interpreter begins to execute a method which specifies a 
	primitive response, it tries to perform the primitive action and to return a 
	result. If the routine in the interpreter for this primitive is successful, 
	it will return a value and the expressions in the method will not be evaluated. 
	If the primitive routine is not successful, the primitive 'fails', and the 
	Smalltalk expressions in the method are executed instead. These 
	expressions are evaluated as though the primitive routine had not been 
	called.  
	  
	The Smalltalk code that is evaluated when a primitive fails usually 
	anticipates why that primitive might fail. If the primitive is optional, the 
	expressions in the method do exactly what the primitive would have done (See 
	Number @). If the primitive only works on certain classes of arguments, the 
	Smalltalk code tries to coerce the argument or appeals to a superclass to find 
	a more general way of doing the operation (see SmallInteger +). If the 
	primitive is never supposed to fail, the expressions signal an error (see 
	SmallInteger asFloat).  
	  
	Each method that specifies a primitive has a comment in it. If the primitive is 
	optional, the comment will say 'Optional'. An optional primitive that is not 
	implemented always fails, and the Smalltalk expressions do the work 
	instead.  
	 
	If a primitive is not optional, the comment will say, 'Essential'. Some 
	methods will have the comment, 'No Lookup'. See Object 
	howToModifyPrimitives for an explanation of special selectors which are 
	not looked up.  
	  
	For the primitives for +, -, *, and bitShift: in SmallInteger, and truncated 
	in Float, the primitive constructs and returns a 16-bit 
	LargePositiveInteger when the result warrants it. Returning 16-bit 
	LargePositiveIntegers from these primitives instead of failing is 
	optional in the same sense that the LargePositiveInteger arithmetic 
	primitives are optional. The comments in the SmallInteger primitives say, 
	'Fails if result is not a SmallInteger', even though the implementor has the 
	option to construct a LargePositiveInteger. For further information on 
	primitives, see the 'Primitive Methods' part of the chapter on the formal 
	specification of the interpreter in the Smalltalk book."

	self error: 'comment only'! !


!Compiler methodsFor: 'public access' stamp: 'ST-80 31/May/1983 09:10:35'!
format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the incoming text, and then print the parse tree to yield the answer, a string containing the original code in standard format."

	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode := self format: sourceStream noPattern: false ifFail: [^nil].
	^aNode decompileString! !


!Scanner methodsFor: 'error handling' stamp: 'ST-80 31/May/1983 09:10:35'!
notify: string 
	self error: string! !

!Scanner methodsFor: 'error handling' stamp: 'ST-80 31/May/1983 09:10:35'!
offEnd: aString 
	"Parser overrides this"

	^self notify: aString! !

!Scanner methodsFor: 'multi-character scans' stamp: 'ST-80 31/May/1983 09:10:35'!
xDelimiter
	"ignore blanks etc."

	self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'ST-80 31/May/1983 09:10:35'!
xDollar
	"form a Character literal"

	self step. "pass over $"
	token := self step.
	tokenType := #number "really should be Char, but rest of compiler doesn't know"! !


!Parser methodsFor: 'expression types' stamp: 'ST-80 31/May/1983 09:10:35'!
argumentName
	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance! !

!Parser methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
match: type 
	"Answer with true if next tokens type matches"

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false! !

!Parser methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
matchToken: thing 
	"matches the token, not its type"

	here = thing ifTrue: [self advance. ^true].
	^false! !

!Parser methodsFor: 'temporaries' stamp: 'ST-80 31/May/1983 09:10:35'!
bindTemp: name 
	^name! !


!ParseNode methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asReturnNode
	^ReturnNode new expr: self! !

!ParseNode methodsFor: 'encoding' stamp: 'ST-80 31/May/1983 09:10:35'!
encodeSelector: selector 
	^nil! !

!ParseNode methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop"

	super printOn: aStream! !

!ParseNode methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream indent: level precedence: p 
	self printOn: aStream indent: level! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
canCascade
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isArg
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isReturnSelf
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isVariableReference
	^false! !

!ParseNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
comment
	^comment! !

!ParseNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
comment: newComment 
	"self halt."

	comment := newComment! !


!Encoder methodsFor: 'encoding' stamp: 'ST-80 31/May/1983 09:10:35'!
cantStoreInto: varName
	^ StdVariables includesKey: varName! !

!Encoder methodsFor: 'error handling' stamp: 'ST-80 31/May/1983 09:10:35'!
notify: string 
	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false! !

!Encoder methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
noteSuper
	supered := true! !

!Encoder methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
release
	requestor := nil! !

!Encoder methodsFor: 'temporaries' stamp: 'ST-80 31/May/1983 09:10:35'!
maxTemp
	^nTemps! !


!AssignmentNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
variable: aVariable value: expression 
	variable := aVariable.
	value := expression! !


!BlockNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
arguments: argNodes 
	"decompile"

	arguments := argNodes! !

!BlockNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
numberOfArguments
	^arguments size! !

!BlockNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
returnLast
	self returns
		ifFalse: 
			[returns := true.
			statements at: statements size put: statements last asReturnNode]! !

!BlockNode methodsFor: 'code generation' stamp: 'ST-80 31/May/1983 09:10:35'!
code
	^statements first code! !

!BlockNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isJust: node 
	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]! !

!BlockNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
returns
	^returns or: [statements last isReturningIf]! !


!MethodNode methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
tempNames
	^encoder tempNames! !


!LeafNode methodsFor: 'code generation' stamp: 'ST-80 31/May/1983 09:10:35'!
code
	^code! !

!LeafNode methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
key
	^key! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
key: object code: byte 
	key := object.
	code := byte! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
name: ignored key: object code: byte 
	key := object.
	code := byte! !


!VariableNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isVariableReference
	^true! !

!VariableNode methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream indent: level 
	aStream nextPutAll: name! !


!MessageNode methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
precedence
	^precedence! !

!MessageNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range 
	"compile"

	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !

!MessageNode methodsFor: 'expression types' stamp: 'ST-80 31/May/1983 09:10:35'!
cascadeReceiver
	"nil out rcvr (to indicate cascade) and return what it had been"

	| rcvr |
	rcvr := receiver.
	receiver := nil.
	^rcvr! !


!ReturnNode methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asReturnNode! !

!ReturnNode methodsFor: 'code generation' stamp: 'ST-80 31/May/1983 09:10:35'!
code
	^expr code! !

!ReturnNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isReturnSelf
	^expr == NodeSelf! !

!ReturnNode methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isVariableReference
	^expr isVariableReference! !

!ReturnNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
expr: e 
	expr := e! !

!ReturnNode methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
expr: e encoder: encoder sourceRange: range 
	expr := e.
	encoder noteSourceRange: range forNode: self! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeAnyLiteral: value
	^LiteralNode new
		key: value
		index: 0
		type: LdLitType! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeArguments: args block: block
	^block arguments: args! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeAssignTo: variable value: expression
	^AssignmentNode new variable: variable value: expression! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeCascadedMessage: selector arguments: arguments
	^self
		codeMessage: nil
		selector: selector
		arguments: arguments! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeSelector: sel code: code
	^SelectorNode new key: sel code: code! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeSuper
	^NodeSuper! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 31/May/1983 09:10:35'!
codeThisContext
	^NodeThisContext! !

!DecompilerConstructor methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
method: aMethod class: aClass literals: literals
	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals! !


!ParseStack methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
pop: n 
	(position := position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']! !

!ParseStack methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
push: n 
	(position := position + n) > length 
		ifTrue: [length := position]! !

!ParseStack methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
size
	^length! !

!ParseStack methodsFor: 'results' stamp: 'ST-80 31/May/1983 09:10:35'!
position
	^position! !

!ParseStack methodsFor: 'initialize-release' stamp: 'ST-80 31/May/1983 09:10:35'!
init
	length := position := 0! !


!Form methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bits
	"Answer the receiver's Bitmap containing its bits."
	^bits! !

!Form methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bits: aBitmap 
	"Reset the Bitmap containing the receiver's bits."

	bits := aBitmap! !

!Form methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
offset: aPoint 
	offset := aPoint! !


!Form class methodsFor: 'mode constants' stamp: 'ST-80 31/May/1983 09:10:35'!
and
	"Answer the integer denoting the logical 'and' combination rule."
	^1! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 31/May/1983 09:10:35'!
erase
	"Answer the integer denoting mode erase."
	^4! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 31/May/1983 09:10:35'!
over
	"Answer the integer denoting mode over."
	^3! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 31/May/1983 09:10:35'!
reverse
	"Answer the integer denoting mode reverse."
	^6! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 31/May/1983 09:10:35'!
under
	"Answer the integer denoting mode under."
	^7! !


!Cursor methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	self storeOn: aStream base: 2! !


!Cursor class methodsFor: 'current cursor' stamp: 'ST-80 31/May/1983 09:10:35'!
currentCursor
	"Answer the instance of Cursor that is the one currently displayed."
	^CurrentCursor! !


!BitBlt methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
destOrigin: aPoint 
	"Set the destination coordinates to be those of aPoint."
	destX := aPoint x.
	destY := aPoint y! !

!BitBlt methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
destRect: aRectangle 
	"Set the destination coordinates to be those of aRectangle top left and
	the width and height of the receiver to be the width and height of aRectangle."
	destX := aRectangle left.
	destY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height! !


!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bottom
	"Answer the position of the receiver's bottom horizontal line."
	^corner y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bottomCenter
	"Answer the point at the center of the bottom horizontal line of the receiver."
	^self center x @ self bottom! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
bottomRight
	"Answer the point at the right edge of the bottom horizontal line of the receiver."
	^corner! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
center
	"Answer the point at the center of the receiver."
	^self topLeft + self bottomRight // 2! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
corner
	"Answer the point at the bottom right corner of the receiver."
	^corner! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
extent
	"Answer with a rectangle with origin 0@0 and corner the receiver's 
	width @ the receiver's height."
	^corner - origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
height
	"Answer the height of the receiver."
	^corner y - origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
left
	"Answer the position of the receiver's left vertical line."
	^origin x! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
leftCenter
	"Answer the point at the center of the receiver's left vertical line."
	^self left @ self center y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
origin
	"Answer the point at the top left corner of the receiver."
	^origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
right
	"Answer the position of the receiver's right vertical line."
	^corner x! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
rightCenter
	"Answer the point at the center of the receiver's right vertical line."
	^self right @ self center y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
top
	"Answer the position of the receiver's top horizontal line."
	^origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
topCenter
	"Answer the point at the center of the receiver's top horizontal line."
	^self center x @ self top! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
topLeft
	"Answer the point at the top left corner of the receiver's top horizontal line."
	^origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
topRight
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
width
	"Answer the width of the receiver."
	^corner x - origin x! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'ST-80 31/May/1983 09:10:35'!
insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint 
	"Answer a Rectangle that is inset from the receiver by a given amount in the 
	origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint! !

!Rectangle methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
containsPoint: aPoint 
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]! !

!Rectangle methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream! !


!CharacterBlock methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
stringIndex
	"Answer the position of the receiver in the string it indexes."
	^stringIndex! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
< aCharacterBlock 
	"Answer whether the string index of the receiver precedes that of aCharacterBlock."
	^stringIndex < aCharacterBlock stringIndex! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
<= aCharacterBlock 
	"Answer whether the string index of the receiver does not come after that of
	aCharacterBlock."
	^(self > aCharacterBlock) not! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
> aCharacterBlock 
	"Answer whether the string index of the receiver comes after that of
	aCharacterBlock."
	^aCharacterBlock < self! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
>= aCharacterBlock 
	"Answer whether the string index of the receiver does not precede that of
	aCharacterBlock."
	^(self < aCharacterBlock) not! !


!CompositionScanner methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
rightX
	"Meaningful only when a line has just been composed -- refers to the line most recently composed.  This is a subtrefuge to allow for easy resizing of a composition rectangle to the width of the maximum line.  Useful only when there is only one line in the form or when each line is terminated by a carriage return.  Handy for sizing menus and lists."

	^spaceX! !


!Date class methodsFor: 'general inquiries' stamp: 'ST-80 31/May/1983 09:10:35'!
dateAndTimeNow
	"Answer an array with first element Date today and second element Time now."

	^Time dateAndTimeNow! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'ST-80 31/May/1983 09:10:35'!
classVarNames
	"Answer a Set of the receiver's class variable names.  Since the receiver does
	not retain knowledge of class variables, the method fakes it by creating an empty set."

	^Set new! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'ST-80 31/May/1983 09:10:35'!
instanceCount
	"Answer the number of instances of the receiver that are currently in use."

	| count |
	count := 0.
	self allInstancesDo: [:x | count := count + 1].
	^count! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 31/May/1983 09:10:35'!
compileAll
	^ self compileAllFrom: self! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 31/May/1983 09:10:35'!
compilerClass
	"Return a compiler class appropriate for source methods of this class."

	^Compiler! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 31/May/1983 09:10:35'!
decompile: selector 
	"Find the compiled code associated with the argument, selector, as a message selector
	in the receiver's method dictionary and decompile it.  Answer the resulting source
	code as a string.  Create an error if the selector is not in the receiver's method
	dictionary."

	^self decompilerClass new decompile: selector in: self! !

!Behavior methodsFor: 'creating method dictionary' stamp: 'ST-80 31/May/1983 09:10:35'!
methodDictionary: aDictionary 
	"Store the argument, aDictionary, as the method dictionary of the receiver."

	methodDict := aDictionary! !


!ClassDescription methodsFor: 'compiling' stamp: 'ST-80 31/May/1983 09:10:35'!
compile: code classified: heading 
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary under the classification 
	indicated by the second argument, heading. nil is to be notified if an error occurs.
	The argument code is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyAll: selArray from: class 
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under -as yet not classified-"

	self copyAll: selArray
		from: class
		classified: nil! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyAll: selArray from: class classified: cat 
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under the third argument, cat."

	selArray do: 
		[:s | self copy: s
				from: class
				classified: cat]! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyAllCategoriesFrom: aClass 
	"Specify that the categories of messages for the receiver include all of those found
	in the class, aClass.  Install each of the messages found in these categories into the
	method dictionary of the receiver, classified under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyCategory: cat from: aClass classified: newCat 
	"Specify that one of the categories of messages for the receiver is the third argument,
	newCat.  Copy each message found in the category cat in class aClass into this
	new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'ST-80 31/May/1983 09:10:35'!
fileOutOn: aFileStream 
	"File a description of the receiver on aFileStream."

	self fileOutOn: aFileStream
		moveSource: false
		toFile: 0! !

!ClassDescription methodsFor: 'instance variables' stamp: 'ST-80 31/May/1983 09:10:35'!
addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility! !

!ClassDescription methodsFor: 'organization' stamp: 'ST-80 31/May/1983 09:10:35'!
category
	"Answer the system organization category for the receiver."

	^SystemOrganization categoryOfElement: self name! !

!ClassDescription methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorCategoryName
	self error: 'Category name must be a String'! !


!Metaclass methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
soleInstance
	"The receiver has only one instance.  Answer it."

	^thisClass! !

!Metaclass methodsFor: 'instance variables' stamp: 'ST-80 31/May/1983 09:10:35'!
addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString := aString.
	self instVarNames do: [:aString2 | fullString := aString2 , ' ' , fullString].
	self instanceVariableNames: fullString! !

!Metaclass methodsFor: 'instance variables' stamp: 'ST-80 31/May/1983 09:10:35'!
removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."


	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray := self instVarNames copyWithout: aString.
	newString := ''.
	newArray do: [:aString2 | newString := aString2 , ' ' , newString].
	self instanceVariableNames: newString! !


!Collection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject 
	"Include newObject as one of the receiver's elements.  Answer newObject.
	This message should not be sent to instances of subclasses of ArrayedCollection."

	self subclassResponsibility! !

!Collection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements.  Answer
	aCollection."

	aCollection do: [:each | self add: each].
	^aCollection! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  Collect the 
	resulting values into a collection that is like the receiver.  Answer the new 
	collection. "

	| newCollection |
	newCollection := self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
inject: thisValue into: binaryBlock 
	"Accumulate a running value associated with evaluating the argument,
	binaryBlock, with the current value and the receiver as block arguments. 
	The initial value is the value of the argument, thisValue.
		For instance, to sum a collection, use: 
			collection inject: 0 into: [:subTotal :next | subTotal + next]."

	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue! !

!Collection methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!Collection methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Remove oldObject as one of the receiver's elements.  If several of the
	elements are equal to oldObject, only one is removed. If no element is equal to
	oldObject, answer the result of evaluating anExceptionBlock.  Otherwise,
	answer the argument, oldObject.

	SequenceableCollections can not respond to this message."

	self subclassResponsibility! !

!Collection methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
removeAll: aCollection 
	"Remove each element of aCollection from the receiver.  If successful for each,
	answer aCollection."

	aCollection do: [:each | self remove: each].
	^aCollection! !

!Collection methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally! !

!Collection methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
emptyCheck
	self isEmpty ifTrue: [self errorEmptyCollection]! !


!Bag methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
at: index 
	self errorNotKeyed! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
at: index put: anObject
	self errorNotKeyed! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing count."
	| counts |
	counts := SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^ counts! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
sortedElements
	"Answer with a collection of elements with counts, sorted by element."
	| elements |
	elements := SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^ elements! !

!Bag methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject 
	^self add: newObject withOccurrences: 1! !

!Bag methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
do: aBlock 
	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]! !

!Bag methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
includes: anObject 
	^contents includesKey: anObject! !

!Bag methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
occurrencesOf: anObject 
	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
indexOfSubCollection: aSubCollection startingAt: anIndex 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal the rest of
	the elements of aSubCollection.  Begin the search at element anIndex of the 
	receiver.  If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
findLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index := self size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, starting
	with the last element and taking each in sequence up to the first.  For
	SequenceableCollections, this is the reverse of the enumeration in do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: oldObject ifAbsent: anExceptionBlock 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement! !

!SequenceableCollection methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorOutOfBounds
	self error: 'indices are out of bounds'! !


!RunArray methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
values
	"Answer the values in the receiver."
	^values! !

!RunArray methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyReplaceFrom: start to: stop with: replacement 
	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)! !

!RunArray methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)! !

!RunArray methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
runs
	^runs! !


!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)! !

!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
runs: newRuns values: newValues 
	"Answer a new instance of RunArray with runs and values specified
	by the arguments."

	| instance |
	instance := self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance! !


!Interval methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'! !

!Interval methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
first
	^start! !

!Interval methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject 
	"Adding to an Interval is not allowed."
	self shouldNotImplement! !

!Interval methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: newObject 
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'! !

!Interval methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	"This is possible because we know numbers store and print the same"

	self printOn: aStream! !

!Interval methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
species
	^Array! !


!OrderedCollection methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
size
	^lastIndex - firstIndex + 1! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject 
	^self addLast: newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject after: oldObject 
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just succeeding oldObject.  Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject before: oldObject 
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just preceding oldObject.  Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
addAllFirst: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the receiver.	
	Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection! !

!OrderedCollection methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorNoSuchElement
	self error: 'attempt to index non-existent element in an ordered collection'! !


!ArrayedCollection methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject
	self shouldNotImplement! !

!ArrayedCollection methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!ArrayedCollection methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
defaultElement
	^nil! !

!ArrayedCollection methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
storeElementsFrom: firstIndex to: lastIndex on: aStream 
	| noneYet defaultElement arrayElement |
	noneYet := true.
	defaultElement := self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement := self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet := false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer a new instance of me, containing the four arguments as the elements."

	| newCollection |
	newCollection := self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection! !


!ByteArray methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
defaultElement
	^0! !


!LinkedList methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
first
	"Answer the first link;  create an error if the receiver is empty."

	self emptyCheck.
	^firstLink! !

!LinkedList methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
last
	"Answer the last link;  create an error if the receiver is empty."

	self emptyCheck.
	^lastLink! !

!LinkedList methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isEmpty
	^firstLink == nil! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: aLink 
	"Add aLink to the end of the receiver's list."

	^self addLast: aLink! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
addFirst: aLink 
	"Add aLink to the beginning of the receiver's list."

	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
addLast: aLink 
	"Add aLink to the end of the receiver's list."

	self isEmpty
		ifTrue: [firstLink := aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink! !

!LinkedList methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
removeFirst
	"Remove the first element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using the sequence addFirst:/removeFirst
	causes the receiver to behave as a stack; using addLast:/removeFirst causes the
	receiver to behave as a queue."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [firstLink := oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
removeLast
	"Remove the receiver's last element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using addLast:/removeLast causes the
	receiver to behave as a stack; using addFirst:/removeLast causes the receiver to
	behave as a queue."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [aLink := firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink := aLink nextLink].
				 aLink nextLink: nil.
				 lastLink := aLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
do: aBlock
	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink := aLink nextLink]! !


!CharacterSequence methodsFor: 'adding' stamp: 'ST-80 31/May/1983 09:10:35'!
add: newObject
	self shouldNotImplement! !

!CharacterSequence methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asString
	"Answer the receiver itself."
	^self! !

!CharacterSequence methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !


!String methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
isLiteral
	^true! !


!String class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: size withAll: value
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value! !

!String class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: firstChar with: secondChar
	"Answer a new instance of me, containing firstChar and secondChar.
	String with: $a
	String with: $á with: Character euro
	"

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstChar.
	newCollection at: 2 put: secondChar.
	^newCollection! !

!String class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !


!Text methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
at: index 
	^string at: index! !

!Text methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
string
	"Answer the string representation of the receiver."
	^string! !

!Text methodsFor: 'copying' stamp: 'ST-80 31/May/1983 09:10:35'!
copyFrom: start to: stop 
	"Answer with a copied subrange of this text"

	| realStart realStop |
	stop > self size
		ifTrue: [realStop := self size]		"handle selection at end of string"
		ifFalse: [realStop := stop].
	start < 1
		ifTrue: [realStart := 1]			"handle selection before start of string"
		ifFalse: [realStart := start].
	^Text 
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)! !

!Text methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asNumber
	"Answer the number created by interpreting the receiver as the textual
	representation of a number."

	^string asNumber! !

!Text methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asString
	"Answer a String representation of the textual receiver."
	^string! !

!Text methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asText	
	"Answer the receiver itself."
	^self! !

!Text methodsFor: 'emphasis' stamp: 'ST-80 31/May/1983 09:10:35'!
runLengthFor: characterIndex 
	"Answer the count of characters remaining in run beginning with 
	characterIndex."

	^runs runLengthAt: characterIndex! !

!Text methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)! !

!Text methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
runs
	^runs! !


!Text class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: stringSize 
	^self fromString: (String new: stringSize)! !

!Text class methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
string: aString runs: anArray  
	^self basicNew setString: aString setRuns: anArray! !


!Dictionary methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
keyAtValue: value 
	"Answer the key whose value equals the argument, value.  If there is none,
	cause an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]! !

!Dictionary methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count := 0.
	self do: [:each | anObject = each ifTrue: [count := count + 1]].
	^count! !

!Dictionary methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: anObject 
	self shouldNotImplement! !

!Dictionary methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: anObject ifAbsent: exceptionBlock 
	self shouldNotImplement! !

!Dictionary methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]! !

!Dictionary methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
errorValueNotFound
	self error: 'value not found'! !


!SystemDictionary methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
exitToDebugger
	"Enter the machine language debugger, if one exists.  Essential.  See Object 
	documentation whatIsAPrimitive. "

	<primitive: 114>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
quitPrimitive
	"Exit to another operating system on the host machine, if one exists.  All 
	state changes in the object space since the last snapshot are lost.  Essential.  
	See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed! !


!SharedQueue methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
nextPut: value 
	"Send value through the receiver.  If a Process has been suspended waiting to 
	receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition := writePosition + 1].
	readSynch signal.
	^value! !

!SharedQueue methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
size
	"Answer the number of objects that have been sent through the
	receiver and not yet received by anyone."

	^writePosition - readPosition! !

!SharedQueue methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isEmpty
	"Answer whether any objects have been sent through the receiver
	and not yet received by anyone."

	^readPosition = writePosition! !


!Stream methodsFor: 'reading' stamp: 'ST-80 31/May/1983 09:10:35'!
contents
	"Answer the contents of the receiver."
	self subclassResponsibility! !


!Stream class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new
	self error: 'Streams are created with on: and with:'! !


!PositionableStream methodsFor: 'reading' stamp: 'ST-80 31/May/1983 09:10:35'!
peekFor: anObject 
	"Answer false and do not move the position if self next ~= anObject or if the
	receiver is at the end. Answer true and increment position if self next = anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject := self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position := position - 1.
	^false! !

!PositionableStream methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
positionError
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'! !


!ReadStream methodsFor: 'writing' stamp: 'ST-80 31/May/1983 09:10:35'!
nextPut: anObject 
	self shouldNotImplement! !

!ReadStream methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	| len |
	collection := aCollection.
	readLimit :=  lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]! !


!ReadStream class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	"Answer with a new instance streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex! !


!WriteStream methodsFor: 'character writing' stamp: 'ST-80 31/May/1983 09:10:35'!
tab
	"Append a tab character to the receiver."

	self nextPut: Character tab! !

!WriteStream methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
size
	^readLimit := readLimit max: position! !

!WriteStream methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
store: anObject 
	"Have anObject print on me for rereading."

	anObject storeOn: self! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
blockReturnTop
	"Print the ReturnTopOfStack bytecode."
	self print: 'blockReturn'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
doDup
	"Print the Duplicate Top of Stack bytecode."
	self print: 'dup'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
doPop
	"Print the Remove Top of Stack bytecode."
	self print: 'pop'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
methodReturnConstant: value 
	"Print the Return Constant bytecode."
	self print: 'return: ' , value printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
methodReturnReceiver
	"Print the Return Self bytecode."
	self print: 'returnSelf'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
methodReturnTop
	"Print the Return Top of Stack bytecode."
	self print: 'returnTop'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
popIntoLiteralVariable: anAssociation 
	"Print the Removes the Top of the Stack and Stores it into a Literal Variable
	bytecode."
	self print: 'popIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
popIntoTemporaryVariable: offset 
	"Print the Removes the Top of the Stack and Stores it into a Temporary Variable
	bytecode."
	self print: 'popIntoTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushActiveContext
	"Print the Push the Active Context on the Top of its Own Stack bytecode."
	self print: 'pushThisContext: '! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushLiteralVariable: anAssociation
	"Print the Push the Contents of anAssociation on the Top of the Stack bytecode."
	self print: 'pushLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushReceiver
	"Print the Push the Active Context's Receiver on the Top of the Stack bytecode."
	self print: 'self'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushTemporaryVariable: offset
	"Print the Push the Contents of the Temporary Variable whose Index is the
	argument, offset, on the Top of the Stack bytecode."
	self print: 'pushTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
storeIntoLiteralVariable: anAssociation 
	"Print the Store the Top of the Stack into a Literal Variable of the Method bytecode."
	self print: 'storeIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
storeIntoTemporaryVariable: offset 
	"Print the Store the Top of the Stack into a Temporary Variable of the Method
	bytecode."
	self print: 'storeIntoTemp: ' , offset printString! !


!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 31/May/1983 09:10:35'!
followingByte
	"Answer the following bytecode."
	^self method at: pc + 1! !

!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 31/May/1983 09:10:35'!
method
	"Answer the compiled method that supplies the receiver's bytecodes."

	^sender		"method access when used alone (not as part of a context)"! !

!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 31/May/1983 09:10:35'!
nextByte
	"Answer the next bytecode."
	^self method at: pc! !

!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 31/May/1983 09:10:35'!
pc
	"Answer the index of the next bytecode."

	^pc! !

!InstructionStream methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
method: method pc: startpc 
	sender := method. 
	"allows this class to stand alone as a method scanner"
	pc := startpc! !


!InstructionStream class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
on: method 
	"Answer a new InstructionStream on the argument, method."

	^self new method: method pc: method initialPC! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
blockReturnTop
	"No action needed"! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
methodReturnConstant: value
	self pushConstant: value; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
methodReturnReceiver
	self pushReceiver; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
popIntoLiteralVariable: value
	self pushLiteralVariable: value; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
popIntoReceiverVariable: offset
	self pushReceiverVariable: offset; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushActiveContext
	stack addLast: constructor codeThisContext! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushConstant: value
	| node |
	node := value == true ifTrue: [constTable at: 2]
		ifFalse: [value == false ifTrue: [constTable at: 3]
		ifFalse: [value == nil ifTrue: [constTable at: 4]
		ifFalse: [constructor codeAnyLiteral: value]]].
	stack addLast: node! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushLiteralVariable: assoc
	stack addLast: (constructor codeAnyLitInd: assoc)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushReceiver
	stack addLast: (constTable at: 1)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
storeIntoLiteralVariable: assoc
	self pushLiteralVariable: assoc; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
storeIntoReceiverVariable: offset
	self pushReceiverVariable: offset; doStore: stack! !

!Decompiler methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
popTo: oldPos
	| t |
	t := Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t! !


!ContextPart methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
home
	"Answer the context in which the receiver was defined."

	self subclassResponsibility! !

!ContextPart methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
receiver
	"Answer the receiver of the message that created this context."

	self subclassResponsibility! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
doDup
	"Simulates the action of a 'duplicate top of stack' bytecode."

	self push: self top! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
doPop
	"Simulates the action of a 'remove top of stack' bytecode."

	self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
jump: distance 
	"Simulates the action of a 'unconditional jump' bytecode whose
	offset is the argument, distance."

	pc := pc + distance! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushActiveContext
	"Simulates the action of bytecode that pushes the the active
	context on the top of its own stack."

	self push: self! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushConstant: value 
	"Simulates the action of bytecode that pushes the constant, value, on
	the top of the stack."

	self push: value! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 31/May/1983 09:10:35'!
pushReceiver
	"Simulates the action of bytecode that pushes the the active
	context's receiver on the top of the stack."

	self push: self receiver! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 31/May/1983 09:10:35'!
depthBelow: aContext
	"Answer how many calls between this and aContext."
	| this depth |
	this := self.
	depth := 0.
	[this == aContext or: [this == nil]]
		whileFalse:
			[this := this sender.
			depth := depth + 1].
	^depth! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 31/May/1983 09:10:35'!
hasSender: context 
	"Answer true if the receiver is strictly above context on the stack."

	| s |
	self == context ifTrue: [^false].
	s := sender.
	[s == nil]
		whileFalse: 
			[s == context ifTrue: [^true].
			s := s sender].
	^false! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 31/May/1983 09:10:35'!
releaseTo: caller 
	"Remove information from the receiver and the contexts on its 
	sender chain up to caller in order to break circularities."

	| c s |
	c := self.
	[c == nil or: [c == caller]]
		whileFalse: 
			[s := c sender.
			c singleRelease.
			c := s]! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 31/May/1983 09:10:35'!
stack 
	"Answer an array of the contexts on the receiver's sender chain."
	^self stackOfSize: 9999! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 31/May/1983 09:10:35'!
swapSender: coroutine 
	"Replace the receiver's sender with coroutine and answer the receiver's previous sender.
	For use in coroutining."

	| oldSender |
	oldSender := sender.
	sender := coroutine.
	^oldSender! !

!ContextPart methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
top
	"Answer the top of the receiver's stack."

	^self at: stackp! !

!ContextPart methodsFor: 'system simulation' stamp: 'ST-80 31/May/1983 09:10:35'!
step
	"Simulate the execution of the receiver's next bytecode.
	Answer the context that would be the active context
	after this bytecode."

	^self interpretNextInstructionFor: self! !

!ContextPart methodsFor: 'temporaries' stamp: 'ST-80 31/May/1983 09:10:35'!
tempAt: index
	"Answer the value of the temporary variable whose index is the argument, index."
	self subclassResponsibility! !

!ContextPart methodsFor: 'temporaries' stamp: 'ST-80 31/May/1983 09:10:35'!
tempAt: index put: value 
	"Store the argument, value, as the temporary variable whose
	index is the argument, index."
	self subclassResponsibility! !


!ContextPart class methodsFor: 'examples' stamp: 'ST-80 31/May/1983 09:10:35'!
runSimulated: aBlock
	"The simulator is a group of methods in class ContextPart which do what the 
	Smalltalk interpreter does.  They execute Smalltalk bytecodes.  By adding code 
	to the simulator, you could take statistics on the running of Smalltalk methods.
	See also trace: callStatistics: and instructionStatistics: for sample uses"

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored]

	"ContextPart runSimulated: [Pen new defaultNib: 5; go: 100]"! !


!MethodContext methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
removeSelf
	"Nil the receiver pointer and answer the former value."

	| tempSelf |
	tempSelf := receiver.
	receiver := nil.
	^tempSelf! !


!Message methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
arguments
	"Answer the arguments of the receiver."
	^args! !

!Message methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
setSelector: aSymbol arguments: anArray 
	selector := aSymbol.
	args := anArray! !


!Message class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
selector: aSymbol
	"Answer an instance of me with unary selector, aSymbol."

	^self new setSelector: aSymbol arguments: (Array new: 0)! !

!Message class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
selector: aSymbol argument: anObject 
	"Answer an instance of me whose selector is aSymbol and single argument
	is anObject."

	^self new setSelector: aSymbol arguments: (Array with: anObject)! !

!Message class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
selector: aSymbol arguments: anArray 
	"Answer an instance of me with selector, aSymbol, and arguments,
	anArray."

	^self new setSelector: aSymbol arguments: anArray! !


!FileList methodsFor: 'file name list' stamp: 'ST-80 31/May/1983 09:10:35'!
fileName
	^ fileName! !


!Inspector methodsFor: 'doIt/accept/explain' stamp: 'ST-80 31/May/1983 09:10:35'!
doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."
	^object! !


!Switch methodsFor: 'state' stamp: 'ST-80 31/May/1983 09:10:35'!
isOff
	"Answer whether the receiver is set off or not."
	^on not! !

!Switch methodsFor: 'state' stamp: 'ST-80 31/May/1983 09:10:35'!
isOn
	"Answer whether the receiver is set on or not."
	^on! !

!Switch methodsFor: 'state' stamp: 'ST-80 31/May/1983 09:10:35'!
switch
	"Change the state of the receiver from 'on' to 'off' or from 'off' to 'on' (see 
	Switch|turnOn, Switch|turnOff)."

	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]! !

!Switch methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
initializeOff
	on := false. 
	onAction := nil.
	offAction := nil! !

!Switch methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
initializeOn
	on := true. 
	onAction := nil.
	offAction := nil! !


!Switch class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^self newOff! !

!Switch class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
newOff
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^super new initializeOff! !

!Switch class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
newOn
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'on'."

	^super new initializeOn! !


!Boolean methodsFor: 'logical operations' stamp: 'ST-80 31/May/1983 09:10:35'!
eqv: aBoolean 
	"Answer true if the receiver is equivalent to aBoolean."

	^self == aBoolean! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- if the receiver is true, answer the value of
	the argument, alternativeBlock; otherwise answer false without evaluating the
	argument."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: alternativeBlock 
	"If the receiver is true (i.e., the condition is true), then the value is the true
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Same as ifTrue:ifFalse:"
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: alternativeBlock 
	"If the receiver is false (i.e., the condition is false), then the value is the false
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"If the receiver is true (i.e., the condition is true), then answer the value of the
	argument trueAlternativeBlock.  If the receiver is false, answer the result of
	evaluating the argument falseAlternativeBlock.  If the receiver is a nonBoolean
	then create an error message.  Execution does not actually reach here because the
	expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- if the receiver is false, answer the value of
	the argument, alternativeBlock; otherwise answer true without evaluating the
	argument."
	self subclassResponsibility! !

!Boolean methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream
	self printOn: aStream! !


!Boolean class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new
	self error: 'You may not create any more Booleans - this is two-valued logic'! !


!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self! !

!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: alternativeBlock 
	"Since the condition is false, answer the value of the false alternative, 
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."

	^nil! !

!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- answer value of alternativeBlock."

	^alternativeBlock value! !


!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."

	^alternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: alternativeBlock 
	"Since the condition is true, the value is the true alternative, which is nil. 
	Execution does not actually reach here because the expression is compiled 
	in-line."

	^nil! !

!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually 
	reach here because the expression is compiled in-line."

	^alternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer with the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 31/May/1983 09:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self! !


!Number methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
* aNumber 
	"Answer the result of multiplying the receiver by aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
+ aNumber 
	"Answer the sum of the receiver and aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
- aNumber 
	"Answer the difference between the receiver and aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
/ aNumber 
	"Answer the result of dividing receiver by aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'mathematical functions' stamp: 'ST-80 31/May/1983 09:10:35'!
exp
	"Answer the exponential of the receiver as a floating point number."
	^self asFloat exp! !

!Number methodsFor: 'mathematical functions' stamp: 'ST-80 31/May/1983 09:10:35'!
squared
	"Answer the receiver multipled by itself."
	^self * self! !

!Number methodsFor: 'truncation and round off' stamp: 'ST-80 31/May/1983 09:10:35'!
truncated
	"Answer an integer nearest the receiver toward zero."
	^self quo: 1! !

!Number methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
@ y 
	"Answer a new Point whose x value is the receiver and whose y value is the 
	argument.  Optional.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 18>
	^Point x: self y: y! !

!Number methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asPoint
	"Answer a new Point with the receiver as both coordinates; 
	often used to supply the same value in two dimensions, as with
	symmetrical gridding or scaling."

	^self @ self! !

!Number methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
degreesToRadians
	"The receiver is assumed to represent degrees.  Answer the
	conversion to radians."
	^self asFloat degreesToRadians! !

!Number methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
radiansToDegrees
	"The receiver is assumed to represent radians.  Answer the
	conversion to degrees."
	^self asFloat radiansToDegrees! !

!Number methodsFor: 'intervals' stamp: 'ST-80 31/May/1983 09:10:35'!
to: stop
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by 1."

	^Interval from: self to: stop by: 1! !

!Number methodsFor: 'intervals' stamp: 'ST-80 31/May/1983 09:10:35'!
to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by step."

	^Interval from: self to: stop by: step! !

!Number methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
even
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0! !

!Number methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
odd
	"Answer whether the receiver is an odd number."
	^self even == false! !

!Number methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0! !


!Float methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asFloat
	"Answer with the receiver itself."
	^self! !

!Float methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
degreesToRadians
	^self * RadiansPerDegree! !

!Float methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
radiansToDegrees
	^self / RadiansPerDegree! !

!Float methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
integerPart
	"Answer with a new Float whose value is the receiver's truncated value."

	^self - self fractionPart! !


!Float class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
readFrom: aStream 
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream) asFloat! !


!Fraction methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
denominator
	^denominator! !

!Fraction methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
negated
	^Fraction numerator: numerator negated denominator: denominator! !

!Fraction methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
numerator
	^numerator! !

!Fraction methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asFraction	
	"Answer with the receiver itself."
	^self! !

!Fraction methodsFor: 'truncation and round off' stamp: 'ST-80 31/May/1983 09:10:35'!
truncated
	^numerator quo: denominator! !

!Fraction methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
reduced
	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd := numerator gcd: denominator.
	numer := numerator // gcd.
	denom := denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom! !


!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 31/May/1983 09:10:35'!
allMask: mask 
	"Treat the argument as a bit mask.  Answer true if all of the
	bits that are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)! !

!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 31/May/1983 09:10:35'!
anyMask: mask 
	"Treat the argument as a bit mask.  Answer true if any of the
	bits that are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)! !

!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 31/May/1983 09:10:35'!
noMask: mask 
	"Treat the argument as a bit mask.  Answer true if none of the bits
	that are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask)! !

!Integer methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asFraction
	"Answer with a Fraction that represents the receiver."

	^Fraction numerator: self denominator: 1! !

!Integer methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983 09:10:35'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by
	the receiver."

	| count |
	count := 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count := count + 1]! !

!Integer methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
isLiteral
	^true! !

!Integer methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
even
	^((self digitAt: 1) bitAnd: 1) = 0! !

!Integer methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
growby: n 
	^self growto: self digitLength + n! !

!Integer methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
growto: n 
	^self copyto: (self species new: n)! !


!Integer class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: length neg: neg 
	"Answer an instance of a large integer whose size is length.  neg is a flag determining
	whether the integer is negative or not."

	neg 
		ifTrue: [^LargeNegativeInteger new: length]
		ifFalse: [^LargePositiveInteger new: length]! !


!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
abs! !

!LargePositiveInteger methodsFor: 'system primitives' stamp: 'ST-80 31/May/1983 09:10:35'!
digitAt: index put: value 
	"Store the second argument (value) in the indexable field of the receiver 
	indicated by index.  Fail if the value is negative or is larger than 255.  Fail if the 
	index is not an Integer or is out of bounds.  Answer with the value that was 
	stored.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value! !

!LargePositiveInteger methodsFor: 'system primitives' stamp: 'ST-80 31/May/1983 09:10:35'!
digitLength
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed! !


!LargeNegativeInteger methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
abs
	^self negated! !

!LargeNegativeInteger methodsFor: 'arithmetic' stamp: 'ST-80 31/May/1983 09:10:35'!
negated
	^self copyto: (LargePositiveInteger new: self digitLength)! !


!SmallInteger methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
odd
	^(self bitAnd: 1) = 1! !


!MessageTally methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
< aMessageTally 
	^tally > aMessageTally tally! !

!MessageTally methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
> aMessageTally 
	^tally < aMessageTally tally! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
class: aClass method: aMethod 
	class := aClass.
	method := aMethod.
	tally := 0.
	receivers := Array new: 0! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
method
	^method! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
primitives: anInteger 
	tally := anInteger.
	receivers := nil! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
tally
	^tally! !


!UndefinedObject methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
storeOn: aStream 
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: 'dependents access' stamp: 'ST-80 31/May/1983 09:10:35'!
addDependent: ignored
	self error: 'Nil should not have dependents'! !


!UndefinedObject class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new
	self error: 'You may not create any more undefined objects--use nil'! !


!Delay methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
resumptionTime
	"Answer the value of the system's millisecondClock at which the receiver's 
	suspended Process will resume."

	^resumptionTime! !


!InputSensor methodsFor: 'keyboard' stamp: 'ST-80 31/May/1983 09:10:35'!
flushKeyboard
	"Remove all characters from the keyboard buffer."

	[self keyboardPressed]
		whileTrue: [self keyboard]! !

!InputSensor methodsFor: 'mouse' stamp: 'ST-80 31/May/1983 09:10:35'!
mousePoint
	"Answer a Point indicating the coordinates of the current mouse location."
	^self primMousePt! !

!InputSensor methodsFor: 'mouse' stamp: 'ST-80 31/May/1983 09:10:35'!
waitClickButton
	"Wait for the user to click (press and then release) any mouse button and then
	answer with the current location of the cursor."
	self waitButton.
	^self waitNoButton! !


!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
activePriority
	"Answer the priority level of the currently running Process."

	^activeProcess priority! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
activeProcess
	"Answer the currently running Process."

	^activeProcess! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
highestPriority
	"Answer the number of priority levels currently available for use."

	^quiescentProcessLists size! !

!ProcessorScheduler methodsFor: 'removing' stamp: 'ST-80 31/May/1983 09:10:35'!
remove: aProcess ifAbsent: aBlock 
	"Remove aProcess from the list on which it is waiting for the processor. If 
	it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 31/May/1983 09:10:35'!
suspendFirstAt: aPriority 
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 31/May/1983 09:10:35'!
suspendFirstAt: aPriority ifNone: noneBlock 
	"Suspend the first Process that is waiting to run with priority aPriority.  
	If no Process is waiting, evaluate noneBlock"

	| aList |
	aList := quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 31/May/1983 09:10:35'!
terminateActive
	"Terminate the process that is currently running."

	activeProcess terminate! !


!ProcessorScheduler class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new
	"New instances of ProcessorScheduler should not be created."

	self error:
'New ProcessSchedulers should not be created since
the integrity of the system depends on a unique scheduler'! !


!Character methodsFor: 'printing' stamp: 'ST-80 31/May/1983 09:10:35'!
isLiteral
	^true! !

!Character methodsFor: 'testing' stamp: 'ST-80 31/May/1983 09:10:35'!
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."
	^self isLetter or: [self isDigit]! !


!Link methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
nextLink
	"Answer the Link to which the receiver points."
	^nextLink! !

!Link methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
nextLink: aLink 
	"Store the argument, as the Link to which the receiver refers."
	^nextLink := aLink! !


!Process methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
priority
	"Answer the priority of the receiver."
	^priority! !

!Process methodsFor: 'private' stamp: 'ST-80 31/May/1983 09:10:35'!
suspendedContext: aContext 
	suspendedContext := aContext! !

!Process methodsFor: 'changing suspended state' stamp: 'ST-80 31/May/1983 09:10:35'!
install: aContext 
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext := aContext! !


!Point methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
x
	"Answer the x coordinate."
	^x! !

!Point methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
y
	"Answer the y coordinate."
	^y! !

!Point methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
< aPoint 
	"Answer whether the receiver is 'above and to the left' of aPoint."
	^x < aPoint x and: [y < aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
<= aPoint 
	"Answer whether the receiver is 'neither below nor to the right' of aPoint."

	^x <= aPoint x and: [y <= aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
> aPoint 
	"Answer whether the receiver is 'below and to the right' of aPoint."

	^x > aPoint x and: [y > aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 31/May/1983 09:10:35'!
>= aPoint 
	"Answer whether the receiver is 'neither above nor to the left' of aPoint."

	^x >= aPoint x and: [y >= aPoint y]! !

!Point methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
asPoint
	"Answer the receiver itself."
	^self! !

!Point methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
corner: aPoint 
	"Answer a new Rectangle whose origin is the receiver and whose corner is aPoint.
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self corner: aPoint! !

!Point methodsFor: 'converting' stamp: 'ST-80 31/May/1983 09:10:35'!
extent: aPoint 
	"Answer a new Rectangle whose origin is the receiver and whose extent is aPoint. 
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self extent: aPoint! !

!Point methodsFor: 'point functions' stamp: 'ST-80 31/May/1983 09:10:35'!
dist: aPoint 
	"Answer the distance between aPoint and the receiver."

	^(aPoint - self) r! !


!RemoteString methodsFor: 'accessing' stamp: 'ST-80 31/May/1983 09:10:35'!
sourceFileNumber
	"Answer the index of the file on which the string is stored."
	^sourceFileNumber! !


!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
newFileNumber: sourceIndex position: anInteger 
	"Answer a new instance of RemoteString for file indexed by sourceIndex,
	at the position anInteger.  Assumes that the string is already stored
	on the file and the instance will be used to access it."

	^self new fileNumber: sourceIndex position: anInteger! !

!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
newString: aString onFileNumber: sourceIndex 
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file and creates the remote reference."

	^self new string: aString onFileNumber: sourceIndex! !

!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
newString: aString onFileNumber: sourceIndex toFile: aFileStream
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file, aFileStream, and creates the remote
	reference.  Assumes that the index corresponds properly to aFileStream."

	^self new string: aString onFileNumber: sourceIndex toFile: aFileStream! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
labels: aString
	"Answer an instance of me whose items are in aString."
	^self labels: aString lines: nil! !


!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent: removeAllActions)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor finalizationRegistry finalize mourn retryWithGC:until: toFinalizeSend:to: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('pinning' isPinned pin unpin)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeString isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('morphic' activeHand flash runningWorld)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError:signalContext: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('profiler support' profilerFriendlyCall:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('updating' changed update:)
!


!Inspector reorganize!
('accessing' baseFieldList fieldList object: printedSelectionIndices selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty: bindingNamesDo: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('stepping' stepAt:)
('testing' is: shouldAssumeSelectionIsPrinted)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel doItContext editorClassFor: objectSpecificMenuSpecFrom: textStylerClassFor:)
('private' changedSelectionIndex classDefinitionChangedFrom:to:oldClassDefinition: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
('binding' bindingOf:)
('doIt/accept/explain' doItReceiver)
!


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend suspendAndUnblock terminate)
('simulate execution' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: isSimulatingAnotherProcess isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString storeOn:)
('private' resolvePendingQuickReturns suspendAndReleaseCriticalSection suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debugContext:title: debugWithTitle: effectiveProcess evaluate:onBehalfOf:)
('signaling' signalException:)
('test support' suspendPrimitivelyOrFail)
('testing' isActiveProcess isBlocked isInExternalCode isReady isRunning isSuspended isTerminated isTheLastPreempted)
('process state' environmentAt:ifAbsent: environmentAt:ifAbsentPut: environmentAt:put: environmentRemoveKey:ifAbsent:)
('changing suspended state' install: restartTopWith:)
!

