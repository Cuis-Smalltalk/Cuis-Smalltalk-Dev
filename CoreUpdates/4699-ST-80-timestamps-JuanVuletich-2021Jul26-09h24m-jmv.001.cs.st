'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 9:34:59 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
peekFor: anObject 
	"Answer false and do not move the position if self next ~= anObject or if the
	receiver is at the end. Answer true and increment position if self next = anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject _ self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position _ position - 1.
	^false! !

!PositionableStream methodsFor: 'positioning' stamp: 'ST-80 5/31/1983 9:10:35'!
position
	"Answer the current position of accessing the stream."
	^position! !

!PositionableStream methodsFor: 'positioning' stamp: 'ST-80 5/31/1983 9:10:35'!
position: anInteger 
	"Set position to anInteger as long as anInteger is within the bounds of the 
	receiver's contents.  If it is not, cause an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position _ anInteger]
		ifFalse: [self positionError]! !

!PositionableStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection 
	collection _ aCollection.
	readLimit _ aCollection size.
	position _ 0.
	self reset! !


!PositionableStream class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection 
	"Answer a new instance of me, streaming over aCollection."

	^self basicNew on: aCollection! !

!PositionableStream class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	"Answer a new instance of me, streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew on: (aCollection copyFrom: firstIndex to: lastIndex)! !


!ReadStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
nextPut: anObject 
	self shouldNotImplement! !

!ReadStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	| len |
	collection _ aCollection.
	readLimit _  lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]! !


!WriteStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contents
	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: position! !

!WriteStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
next
	self shouldNotImplement! !

!WriteStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	^readLimit _ readLimit max: position! !

!WriteStream methodsFor: 'positioning' stamp: 'ST-80 5/31/1983 9:10:35'!
position: anInteger 
	readLimit _ readLimit max: position.
	super position: anInteger! !

!WriteStream methodsFor: 'positioning' stamp: 'ST-80 5/31/1983 9:10:35'!
reset
	readLimit _ readLimit max: position.
	position _ 0! !


!FileStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
next
	(position >= readLimit and: [self atEnd])
		ifTrue: [^nil]
		ifFalse: [^collection at: (position _ position + 1)]! !


!Link methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
nextLink
	"Answer the Link to which the receiver points."
	^nextLink! !

!Link methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
nextLink: aLink 
	"Store the argument, as the Link to which the receiver refers."
	^nextLink _ aLink! !


!Process methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
priority
	"Answer the priority of the receiver."
	^priority! !

!Process methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
suspendedContext: aContext 
	suspendedContext _ aContext! !


!Process class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
forContext: aContext priority: anInteger 
	"Answer an instance of me that has suspended aContext at priority anInteger."

	| newProcess |
	newProcess _ self new.
	newProcess suspendedContext: aContext.
	newProcess priority: anInteger.
	^newProcess! !


!Compiler methodsFor: 'public access' stamp: 'ST-80 5/31/1983 9:10:35'!
format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the incoming text, and then print the parse tree to yield the answer, a string containing the original code in standard format."

	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode _ self format: sourceStream noPattern: false ifFail: [^nil].
	^aNode decompileString! !


!Scanner methodsFor: 'error handling' stamp: 'ST-80 5/31/1983 9:10:35'!
notify: string 
	self error: string! !

!Scanner methodsFor: 'multi-character scans' stamp: 'ST-80 5/31/1983 9:10:35'!
xDelimiter
	"ignore blanks etc."

	self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'ST-80 5/31/1983 9:10:35'!
xDollar
	"form a Character literal"

	self step. "pass over $"
	token _ self step.
	tokenType _ #number "really should be Char, but rest of compiler doesn't know"! !


!Parser methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
addComment
	parseNode ~~ nil
		ifTrue: 
			[parseNode comment: currentComment.
			currentComment _ nil]! !

!Parser methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
initPattern: aString notifying: req return: aBlock
	| result |
	self
		init: (ReadStream on: aString asString)
		notifying: req
		failBlock: [^nil].
	encoder _ self.
	result _ aBlock value: (self pattern: false inContext: nil).
	encoder _ failBlock _ nil.  "break cycles"
	^result! !

!Parser methodsFor: 'expression types' stamp: 'ST-80 5/31/1983 9:10:35'!
argumentName
	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance! !

!Parser methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
match: type 
	"Answer with true if next tokens type matches"

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false! !

!Parser methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
matchToken: thing 
	"matches the token, not its type"

	here = thing ifTrue: [self advance. ^true].
	^false! !

!Parser methodsFor: 'temporaries' stamp: 'ST-80 5/31/1983 9:10:35'!
bindTemp: name 
	^name! !


!ParseNode methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asReturnNode
	^ReturnNode new expr: self! !

!ParseNode methodsFor: 'encoding' stamp: 'ST-80 5/31/1983 9:10:35'!
encodeSelector: selector 
	^nil! !

!ParseNode methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop"

	super printOn: aStream! !

!ParseNode methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream indent: level precedence: p 
	self printOn: aStream indent: level! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
canCascade
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isArg
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isReturnSelf
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isReturningIf
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isVariableReference
	^false! !

!ParseNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
comment
	^comment! !

!ParseNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
comment: newComment 
	"self halt."

	comment _ newComment! !


!Encoder methodsFor: 'encoding' stamp: 'ST-80 5/31/1983 9:10:35'!
cantStoreInto: varName
	^ StdVariables includesKey: varName! !

!Encoder methodsFor: 'error handling' stamp: 'ST-80 5/31/1983 9:10:35'!
notify: string 
	| req |
	requestor == nil
		ifFalse: 
			[req _ requestor.
			self release.
			req notify: string].
	^false! !

!Encoder methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
noteSuper
	supered _ true! !

!Encoder methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
release
	requestor _ nil! !

!Encoder methodsFor: 'temporaries' stamp: 'ST-80 5/31/1983 9:10:35'!
maxTemp
	^nTemps! !


!AssignmentNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
variable: aVariable value: expression 
	variable _ aVariable.
	value _ expression! !


!BlockNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
arguments: argNodes 
	"decompile"

	arguments _ argNodes! !

!BlockNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
numberOfArguments
	^arguments size! !

!BlockNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
returnLast
	self returns
		ifFalse: 
			[returns _ true.
			statements at: statements size put: statements last asReturnNode]! !

!BlockNode methodsFor: 'code generation' stamp: 'ST-80 5/31/1983 9:10:35'!
code
	^statements first code! !

!BlockNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isJust: node 
	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]! !

!BlockNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
returns
	^returns or: [statements last isReturningIf]! !


!MethodNode methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
tempNames
	^encoder tempNames! !


!LeafNode methodsFor: 'code generation' stamp: 'ST-80 5/31/1983 9:10:35'!
code
	^code! !

!LeafNode methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
key
	^key! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
key: object code: byte 
	key _ object.
	code _ byte! !

!LeafNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
name: ignored key: object code: byte 
	key _ object.
	code _ byte! !


!VariableNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isVariableReference
	^true! !

!VariableNode methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream indent: level 
	aStream nextPutAll: name! !


!MessageNode methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
precedence
	^precedence! !

!MessageNode methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
transform: encoder 
	special = 0 ifTrue: [^false].
	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue: 
			[^true]
		ifFalse: 
			[special _ 0. ^false]! !

!MessageNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range 
	"compile"

	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !

!MessageNode methodsFor: 'expression types' stamp: 'ST-80 5/31/1983 9:10:35'!
cascadeReceiver
	"nil out rcvr (to indicate cascade) and return what it had been"

	| rcvr |
	rcvr _ receiver.
	receiver _ nil.
	^rcvr! !


!ReturnNode methodsFor: 'code generation' stamp: 'ST-80 5/31/1983 9:10:35'!
code
	^expr code! !

!ReturnNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isReturnSelf
	^expr == NodeSelf! !

!ReturnNode methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isVariableReference
	^expr isVariableReference! !

!ReturnNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
expr: e 
	expr _ e! !

!ReturnNode methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
expr: e encoder: encoder sourceRange: range 
	expr _ e.
	encoder noteSourceRange: range forNode: self! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeAnyLitInd: association
	^VariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeAnyLiteral: value
	^LiteralNode new
		key: value
		index: 0
		type: LdLitType! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeAnySelector: selector
	^SelectorNode new
		key: selector
		index: 0
		type: SendType! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeArguments: args block: block
	^block arguments: args! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeAssignTo: variable value: expression
	^AssignmentNode new variable: variable value: expression! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeCascadedMessage: selector arguments: arguments
	^self
		codeMessage: nil
		selector: selector
		arguments: arguments! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeSelector: sel code: code
	^SelectorNode new key: sel code: code! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeSuper
	^NodeSuper! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ST-80 5/31/1983 9:10:35'!
codeThisContext
	^NodeThisContext! !

!DecompilerConstructor methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
method: aMethod class: aClass literals: literals
	method _ aMethod.
	instVars _ aClass allInstVarNames.
	nArgs _ method numArgs.
	literalValues _ literals! !


!ParseStack methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
pop: n 
	(position _ position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']! !

!ParseStack methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
push: n 
	(position _ position + n) > length 
		ifTrue: [length _ position]! !

!ParseStack methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	^length! !

!ParseStack methodsFor: 'results' stamp: 'ST-80 5/31/1983 9:10:35'!
position
	^position! !

!ParseStack methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
init
	length _ position _ 0! !


!RemoteString methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
sourceFileNumber
	"Answer the index of the file on which the string is stored."
	^sourceFileNumber! !


!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
newFileNumber: sourceIndex position: anInteger 
	"Answer a new instance of RemoteString for file indexed by sourceIndex,
	at the position anInteger.  Assumes that the string is already stored
	on the file and the instance will be used to access it."

	^self new fileNumber: sourceIndex position: anInteger! !

!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
newString: aString onFileNumber: sourceIndex 
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file and creates the remote reference."

	^self new string: aString onFileNumber: sourceIndex! !

!RemoteString class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
newString: aString onFileNumber: sourceIndex toFile: aFileStream
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file, aFileStream, and creates the remote
	reference.  Assumes that the index corresponds properly to aFileStream."

	^self new string: aString onFileNumber: sourceIndex toFile: aFileStream! !


!Form methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bits
	"Answer the receiver's Bitmap containing its bits."
	^bits! !

!Form methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
offset: aPoint 
	offset _ aPoint! !


!Form class methodsFor: 'mode constants' stamp: 'ST-80 5/31/1983 9:10:35'!
and
	"Answer the integer denoting the logical 'and' combination rule."
	^1! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 5/31/1983 9:10:35'!
erase
	"Answer the integer denoting mode erase."
	^4! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 5/31/1983 9:10:35'!
over
	"Answer the integer denoting mode over."
	^3! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 5/31/1983 9:10:35'!
reverse
	"Answer the integer denoting mode reverse."
	^6! !

!Form class methodsFor: 'mode constants' stamp: 'ST-80 5/31/1983 9:10:35'!
under
	"Answer the integer denoting mode under."
	^7! !


!Cursor methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	self storeOn: aStream base: 2! !


!Cursor class methodsFor: 'current cursor' stamp: 'ST-80 5/31/1983 9:10:35'!
currentCursor
	"Answer the instance of Cursor that is the one currently displayed."
	^CurrentCursor! !


!DisplayScreen methodsFor: 'displaying' stamp: 'ST-80 5/31/1983 9:10:35'!
beDisplay
	"Tell the interpreter to use the receiver as the current display image.  Fail if the 
	form is too wide to fit on the physical display.  Essential.  See Object 
	documentation whatIsAPrimitive."

	<primitive: 102>
	self primitiveFailed! !


!BitBlt methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
destOrigin: aPoint 
	"Set the destination coordinates to be those of aPoint."
	destX _ aPoint x.
	destY _ aPoint y! !

!BitBlt methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
destRect: aRectangle 
	"Set the destination coordinates to be those of aRectangle top left and
	the width and height of the receiver to be the width and height of aRectangle."
	destX _ aRectangle left.
	destY _ aRectangle top.
	width _ aRectangle width.
	height _ aRectangle height! !


!Point methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
x
	"Answer the x coordinate."
	^x! !

!Point methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
y
	"Answer the y coordinate."
	^y! !

!Point methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aPoint 
	"Answer whether the receiver is 'above and to the left' of aPoint."
	^x < aPoint x and: [y < aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
<= aPoint 
	"Answer whether the receiver is 'neither below nor to the right' of aPoint."

	^x <= aPoint x and: [y <= aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
> aPoint 
	"Answer whether the receiver is 'below and to the right' of aPoint."

	^x > aPoint x and: [y > aPoint y]! !

!Point methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
>= aPoint 
	"Answer whether the receiver is 'neither above nor to the left' of aPoint."

	^x >= aPoint x and: [y >= aPoint y]! !

!Point methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asPoint
	"Answer the receiver itself."
	^self! !

!Point methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
corner: aPoint 
	"Answer a new Rectangle whose origin is the receiver and whose corner is aPoint.
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self corner: aPoint! !

!Point methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
extent: aPoint 
	"Answer a new Rectangle whose origin is the receiver and whose extent is aPoint. 
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self extent: aPoint! !


!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bottom
	"Answer the position of the receiver's bottom horizontal line."
	^corner y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bottomCenter
	"Answer the point at the center of the bottom horizontal line of the receiver."
	^self center x @ self bottom! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bottomRight
	"Answer the point at the right edge of the bottom horizontal line of the receiver."
	^corner! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
center
	"Answer the point at the center of the receiver."
	^self topLeft + self bottomRight // 2! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
corner
	"Answer the point at the bottom right corner of the receiver."
	^corner! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
extent
	"Answer with a rectangle with origin 0@0 and corner the receiver's 
	width @ the receiver's height."
	^corner - origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
height
	"Answer the height of the receiver."
	^corner y - origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
left
	"Answer the position of the receiver's left vertical line."
	^origin x! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
leftCenter
	"Answer the point at the center of the receiver's left vertical line."
	^self left @ self center y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
origin
	"Answer the point at the top left corner of the receiver."
	^origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
right
	"Answer the position of the receiver's right vertical line."
	^corner x! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
rightCenter
	"Answer the point at the center of the receiver's right vertical line."
	^self right @ self center y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
top
	"Answer the position of the receiver's top horizontal line."
	^origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
topCenter
	"Answer the point at the center of the receiver's top horizontal line."
	^self center x @ self top! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
topLeft
	"Answer the point at the top left corner of the receiver's top horizontal line."
	^origin! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
topRight
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y! !

!Rectangle methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
width
	"Answer the width of the receiver."
	^corner x - origin x! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'ST-80 5/31/1983 9:10:35'!
insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint 
	"Answer a Rectangle that is inset from the receiver by a given amount in the 
	origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint! !

!Rectangle methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream! !


!CharacterBlock methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
stringIndex
	"Answer the position of the receiver in the string it indexes."
	^stringIndex! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aCharacterBlock 
	"Answer whether the string index of the receiver precedes that of aCharacterBlock."
	^stringIndex < aCharacterBlock stringIndex! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
<= aCharacterBlock 
	"Answer whether the string index of the receiver does not come after that of
	aCharacterBlock."
	^(self > aCharacterBlock) not! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
> aCharacterBlock 
	"Answer whether the string index of the receiver comes after that of
	aCharacterBlock."
	^aCharacterBlock < self! !

!CharacterBlock methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
>= aCharacterBlock 
	"Answer whether the string index of the receiver does not precede that of
	aCharacterBlock."
	^(self < aCharacterBlock) not! !


!StrikeFont methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
glyphs
	"Answer a Form containing the bits representing the characters of the receiver."
	^glyphs! !

!StrikeFont methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
name: aString
	"Set the receiver's name."
	name _ aString.! !

!StrikeFont methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
subscript
	"Answer an integer that is the further vertical offset relative to the
	baseline for positioning characters as subscripts."
	^subscript! !

!StrikeFont methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
superscript
	"Answer an integer that is the further vertical offset relative to the
	baseline for positioning characters as superscripts."
	^superscript! !

!StrikeFont methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
xTable
	"Answer an array of the left x-coordinate of characters in glyphs."
	^xTable! !


!CompositionScanner methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
rightX
	"Meaningful only when a line has just been composed -- refers to the line most recently composed.  This is a subtrefuge to allow for easy resizing of a composition rectangle to the width of the maximum line.  Useful only when there is only one line in the form or when each line is terminated by a carriage return.  Handy for sizing menus and lists."

	^spaceX! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
labels: aString
	"Answer an instance of me whose items are in aString."
	^self labels: aString lines: nil! !


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:doIt:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('interactive checks' performInteractiveChecks:)
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
!


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('comment')
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRange:basedOn: expandRange:basedOnChildRange: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBacktickNode isBlockNode isBraceNode isCascadeNode isComplex isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('accessing' comment comment:)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: completeSourceRangesBasedOn: criteriaToSortSourceRangeBetween:and: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: parseNodesPathAt:using:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables temps:literals:class:)
('release')
('initialize-release' noteSuper release)
('temporaries' maxTemp)
!


!AssignmentNode reorganize!
('code generation' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value:from: variable:value:from:sourceRange:)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: isAssignmentNode isAssignmentToTemporary isComplex)
('visiting' accept:)
('initialize-release' variable:value:)
!


!LeafNode reorganize!
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation' code emitCodeForLoad:forValue:encoder: reserve: sizeCodeForLoad:forValue:)
('private' code:type:)
('code generation (new scheme)' emitCodeForEffect:encoder: sizeCodeForEffect: sizeCodeForValue:)
('accessing' key key:)
('initialization' key:index:type:)
('testing' equivalentTo:)
('initialize-release' key:code: name:key:code:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval originalArguments originalReceiver receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend toDoIncrement: toDoLimit:)
('cascading' receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: compare:with: hasEquivalentArgumentsWith: hasEquivalentReceiverWith: ifNilReceiver pvtCheckForPvtSelector: transform:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
('initialize-release' receiver:selector:arguments:precedence:from:sourceRange:)
('expression types' cascadeReceiver)
!


!ReturnNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('converting' asReturnNode)
('code generation' code)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForValue:encoder: sizeCodeForReturn: sizeCodeForValue:)
('printing' expr printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' equivalentTo: isImplicitSelfReturnIn: isReturn isReturnSelf isSpecialConstant isVariableReference)
('initialization')
('initialize-release' expr: expr:encoder:sourceRange:)
!


!DecompilerConstructor reorganize!
('visiting' accept:)
('constructor' codeAnyLitInd: codeAnyLiteral: codeAnySelector: codeArguments:block: codeArguments:temps:block: codeAssignTo:value: codeBlock:returns: codeBrace: codeCascade:messages: codeCascadedMessage:arguments: codeConstants codeEmptyBlock codeInst: codeMessage:selector:arguments: codeMethod:block:tempVars:primitive:class: codeRemoteTemp:remoteTemps: codeSelector:code: codeSuper codeTemp: codeTemp:named: codeThisContext decodeIfNilWithReceiver:selector:arguments: decodeLiteralVariableValueDereferenceWithReceiver:selector:arguments:)
('initialization')
('initialize-release' method:class:literals:)
!


!ParseStack reorganize!
('accessing' pop: position: push: size)
('results' position)
('printing' printOn:)
('initialization')
('initialize-release' init)
!


!RemoteString class reorganize!
('instance creation' newFileNumber:position: newString:onFileNumber: newString:onFileNumber:toFile:)
!


!Form reorganize!
('accessing' bits bits: bitsSize center depth extent getCanvas height nativeDepth offset offset: pixelsPerWord size width wordsPerLine)
('analyzing' cgForPixelValue:orNot: dominantColor innerPixelRectFor:orNot: isAnyPixel: pixelCompare:with:at: primCountBits tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width: border:width:borderHeight:fillColor: border:width:fillColor: border:width:rule:fillColor: border:widthRectangle:rule:fillColor: borderWidth:borderHeight:fillColor:)
('coloring' fill:fillColor: fillBlack fillBlack: fillColor: fillGray fillGray: fillShape:fillColor: fillShape:fillColor:at: fillWhite fillWhite: fillWithColor: reverse reverse: reverse:fillColor:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: colormapIfNeededForGray8bpp colormapIfNeededFrom: mapColor:to: maskingMap newColorMap pixelValueFor:)
('converting' as8BitColorForm as8BitStandardPaletteColorForm asColorFormOfDepth: asCursorForm asForm asFormAutoRange asFormOfDepth: asGrayForm asGrayForm: asGrayScale asGrayScaleAndTransparent asGrayScaleAndTransparent: convertAlphaToZeroValueTransparency icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: postCopy)
('display box access' boundingBox)
('displaying' displayAutoRangeAt: displayAutoRangeAt:zoom: displayOn:at:clippingBox:rule: paintBits:at:translucent:)
('displaying-generic' display displayAt: displayOn: displayOn:at: displayOn:at:rule:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: storeBitsOn:base: storeOn: storeOn:base: storeSmall1BitBitsOn: storeSmall1BitOn: writeAttributesOn: writeBMPfileNamed: writeFileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnFileNamed:)
('filling' eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock:)
('image manipulation' smear:distance: tallyPixelValues)
('other' divideByAlpha fixAlpha makeAllPixelsAlpha: makeAllPixelsOpaque makeAllPixelsOpaqueBlack preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: colorInterpolatedAt: i:j: i:j:put: isTransparentAt: pixelValueAt: pixelValueAt:put: primPixelValueAtX:y:)
('scaling, rotation' flippedBy: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotatedBy:smoothing: rotatedByDegrees: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent)
('transitions' pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' copyFromByteArray2:to: copyFromByteArray: fillSmall1BitWith: hackBits64: hackBits: initFromArray: setExtent:depth: setExtent:depth:bits:)
('comparing' = hash)
('initialization' fromDisplay: swapEndianness)
('encoding' addDeltasFrom:)
!


!DisplayScreen reorganize!
('displaying' beDisplay flash: flash:with: forceDamageToScreen:)
('other' deferUpdates: forceDisplayUpdate forceToScreen forceToScreen: fullScreenMode: newDepth: primitiveDeferUpdates: restore restoreAfter: supportedDisplayDepths supportsDisplayDepth:)
('disk I/O' objectForDataStream:)
('private' findAnyDisplayDepth findAnyDisplayDepthIfNone: newDepthNoRestore: primRetryShowRectLeft:right:top:bottom: primShowRectLeft:right:top:bottom: setExtent:depth:)
('copying' copy:from:in:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: eraseShape: fill:rule:fillColor: fillFromXYColorBlock: fillShape:fillColor:at:)
('initialization' initialize)
!

