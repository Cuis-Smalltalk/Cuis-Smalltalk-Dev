'From Cuis7.5 [latest update: #7452] on 28 August 2025 at 11:45:22 am'!

!ProtoObject methodsFor: 'testing' library: #MinimalKernel01 stamp: 'md 11/24/1999 19:26'!
isNil
	"Coerces nil to true and everything else to false."

	^false! !

!ProtoObject methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 8/27/2009 13:23'!
notNil
	"Coerces nil to false and everything else to true."

	^true! !

!ProtoObject methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'md 11/24/1999 19:27'!
== anObject 
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class!! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed! !

!ProtoObject methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'md 11/24/1999 19:27'!
~~ anObject
	"Answer whether the receiver and the argument are not the same object 
	(do not have the same object pointer)."

	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]! !

!ProtoObject methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 16:19:39'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by immediate classes such as
	SmallInteger, Character and SmallFloat64.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed! !

!ProtoObject methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 4/29/2025 16:17:57'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger.
	This implementation is appropriate for minimal bootstrapped images.
	Regular images will automatically use the implementation in Object (if present)."

	{ self } elementsExchangeIdentityWith: { otherObject }! !

!ProtoObject methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 5/29/2025 09:15:37'!
nextInstance
	"Answer the next instance after the receiver in the enumeration of all instances of this class.
	Fails if all instances have been enumerated.
	See Object>>#someInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.

	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 78>
	^nil! !

!ProtoObject methodsFor: 'initialization' library: #MinimalKernel01 stamp: 'md 11/18/2003 10:33'!
initialize
	"Subclasses should redefine this method to perform initializations on instance creation"! !

!ProtoObject methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 5/15/2025 14:19:33'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver.
	"
	self class isMeta
		ifFalse: [ aStream nextPutAll: 'an instance of ' ]
		ifTrue: [ aStream nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: aStream.! !

!ProtoObject methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'jmv 5/19/2025 18:24:23'!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| exception context count |

	"Use the MEssageNotUnderstood exception if present"
	exception := Exception deepSubclassNamed: 'MessageNotUnderstood'.
	exception notNil ifTrue:
		[ ^ exception new 
			message: aMessage;
			receiver: self;
			signal ].

	"Or do the best we can in a bare bones system"
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.
	self printOn: StdIOWriteStream stdout.
	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !

!ProtoObject methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'jmv 4/29/2025 16:46:57'!
error: aString 
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"

	| context count |
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Error: ';
		nextPutAll: aString; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.
	Smalltalk quitPrimitive: 128.! !

!ProtoObject methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'jmv 4/29/2025 16:50:28'!
warning: aString
	"May be redefined."
	"
	7  warning: 'El totón!!'
	"
	StdIOWriteStream stdout
		nextPutAll: 'Warning: ';
		nextPutAll: aString; newLine;
		flush.! !


!Object methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'yo 6/29/2004 11:39'!
at: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [self errorSubscriptBounds: index]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'yo 6/29/2004 13:08'!
at: index put: value 
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0	! !

!Object methodsFor: 'class membership' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
class
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed! !

!Object methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
= anObject 
	"Answer true if the receiver and the argument represent the same object 
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject! !

!Object methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
~= anObject 
	"Answer true if the receiver and the argument do not represent the same 
	object and false otherwise."

	^self = anObject == false! !

!Object methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'Squeak1.2 6/29/1997 10:40:55'!
hash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	May be overridden, and should be overridden in any classes that define = "

	^ self identityHash! !

!Object methodsFor: 'copying' library: #MinimalKernel01 stamp: 'ajh 8/18/2001 21:25'!
copy
	"Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy."

	^self shallowCopy postCopy! !

!Object methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 9/7/2022 09:58:22'!
postCopy
	"self is a shallow copy, subclasses should copy fields as necessary to complete the full copy.
	Also clear any ivars that should be initialized lazyly."

	^ self! !

!Object methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jm 11/14/97 11:08'!
shallowCopy
	"Answer a copy of the receiver which shares the receiver's instance variables."
	| class newObject index |
	<primitive: 148>
	class := self class.
	class isVariable
		ifTrue: 
			[index := self basicSize.
			newObject := class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index).
					index := index - 1]]
		ifFalse: [newObject := class basicNew].
	index := class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index).
			index := index - 1].
	^ newObject! !

!Object methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
shouldNotImplement
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'! !

!Object methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'reThink 3/12/2001 18:14'!
value

	^self! !

!Object methodsFor: 'message handling' library: #MinimalKernel01 stamp: 'di 3/26/1999 07:52'!
perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)! !

!Object methodsFor: 'message handling' library: #MinimalKernel01 stamp: 'di 3/26/1999 07:52'!
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)! !

!Object methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 12/31/1969 21:17:51'!
instVarAt: index put: anObject
	"Primitive. Store a value into a fixed variable in an object. The numbering of the
	 variables corresponds to the named instance variables, followed by the indexed
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed
	 variable. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim74: index put: anObject ].
	self primitiveFailed! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/29/2015 14:57'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ar 7/9/1999 18:18'!
isBehavior
	"Return true if the receiver is a behavior.
	Note: Do not override in any class except behavior."
	^false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/6/2022 10:20:29'!
isByteString
	"Overridden to return true in String, natch"
	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/9/2024 12:18:01'!
isCharacter
	"Overridden to return true in Character."
	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isInteger
	"Overridden to return true in Integer."

	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'rhi 8/12/2003 09:52'!
isInterval

	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/3/2010 22:35'!
isMethodProperties
	^false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isNumber
	"Overridden to return true in Number, natch"
	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 12/7/2022 09:09:02'!
isString
	"Overridden to return true in String, UnicodeString."
	^ false! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'md 4/30/2003 15:30'!
isSymbol
	^ false ! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 12/7/2022 09:16:18'!
isUnicodeString
	"Overridden to return true in UnicodeString, natch"
	^ false! !

!Object methodsFor: 'associating' library: #MinimalKernel01 stamp: 'HAW 9/9/2019 08:57:04'!
-> anObject
	"Answer an Association between self and anObject"

	^Association key: self value: anObject! !


!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'HAW 3/4/2019 15:19:06'!
definesInstanceVariableNamed: anInstanceVariableName

	^self instVarNames includes: anInstanceVariableName! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
format
	"Answer an Integer that encodes the kinds and numbers of variables of instances
	of the receiver."

	^format! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 15:26:02'!
instSize
	"Answer the number of named instance variables
	(as opposed to indexed variables) of the receiver.
	Class format is
		<5 bits inst spec><16 bits inst size>"

	^ format bitAnd: 16rFFFF! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 2/18/2020 19:37'!
instSpec
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (Context BlockClosure AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable	(DoubleWordArray et al)
		10-11	= 32-bit indexable	(WordArray et al)			(includes one odd bit, unused in 32-bit instances)
		12-15	= 16-bit indexable	(DoubleByteArray et al)		(includes two odd bits, one unused in 32-bit instances)
		16-23	= 8-bit indexable	(ByteArray et al)			(includes three odd bits, one unused in 32-bit instances)
		24-31	= compiled code	(CompiledCode et al)		(includes three odd bits, one unused in 32-bit instances)

	 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
	 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
	 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
	 has format 18 in 32 bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
	 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6).
	 Formats 24-31 are for compiled code which is a combination of pointers and bytes.  The number of pointers is
	 determined by literal count field of the method header, which is the first field of the object and must be a SmallInteger. 
	 The literal count field occupies the least significant 15 bits of the method header, allowing up to 32,767 pointer fields,
	 not including the header."
	^(format bitShift: -16) bitAnd: 16r1F! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 2/28/2010 22:50'!
instVarNames
	"Answer an Array of the instance variable names. Behaviors must make 
	up fake local instance variable names because Behaviors have instance 
	variables for the purpose of compiling methods, but these are not named 
	instance variables."

	| mySize superSize |
	mySize := self instSize.
	superSize := 
		superclass
			ifNil: [0]
			ifNotNil: [superclass instSize].
	mySize = superSize ifTrue: [^#()].	
	^(superSize + 1 to: mySize) collect: [:i | 'inst' , i printString]! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'rca 7/26/2000 16:53'!
name
	"Answer a String that is the name of the receiver."
	^'a subclass of ', superclass name! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'HAW 6/28/2020 10:57:39'!
theNonMetaClass

	self subclassResponsibility ! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 15:32:35'!
typeOfClass
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^(#(normal
		normal
		variable
		variable
		weak
		ephemeron
		nil
		immediate
		nil
		longs
		words				nil
		shorts				nil nil nil
		bytes				nil nil nil nil nil nil nil
		compiledMethod	nil nil nil nil nil nil nil)
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ar 7/9/1999 18:18'!
isBehavior
	"Return true if the receiver is a behavior"
	^true! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 15:28:45'!
isBits
	"Answer whether the receiver contains just bits (not pointers).
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 7! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 7/5/2024 15:06:58'!
isBytes
	"Answer whether the receiver's instances have indexed 8-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 16! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'eem 12/1/2014 11:46'!
isCompiledMethodClass
	"Answer whether the receiver has compiled method instances that mix pointers and bytes."
	^self instSpec >= 24! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 7/5/2024 15:07:05'!
isDoubleBytes
	"Answer whether the receiver's instances have indexed 16-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 12! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 7/5/2024 15:07:09'!
isDoubleWords
	"Answer whether the receiver's instances have indexed 64-bit integer slots.
	If true, #isBits is also true.
	 The class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 9! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'eem 3/3/2014 17:23'!
isEphemeronClass
	"Answer whether the receiver has ephemeral instance variables.  The garbage collector will
	 fire (queue for finalization) any ephemeron whose first instance variable is not referenced
	 other than from the transitive closure of references from ephemerons. Hence referring to
	 an object from the first inst var of an ephemeron will cause the ephemeron to fire when
	 the rest of the system does not refer to the object and that object is ready to be collected.
	 Since references from the remaining inst vars of an ephemeron will not prevent the ephemeron
	 from firing, ephemerons may act as the associations in weak dictionaries such that the value
	 (e.g. properties attached to the key) will not prevent firing when the key is no longer referenced
	 other than from ephemerons.  Ephemerons can therefore be used to implement instance-based
	 pre-mortem finalization."
	^self instSpec = 5! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
isFixed
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'eem 3/3/2014 17:23'!
isImmediateClass
	"Answer whether the receiver has immediate instances.  Immediate instances
	 store their value in their object pointer, not in an object body.  Hence immediates
	 take no space and are immutable.  The immediates are distinguished by tag bits
	 in the pointer. They include SmallIntegers and Characters.  Hence in the 32-bit
	 system SmallIntegers are 31-bit signed integers and Characters are 30-bit
	 unsigned character codes."
	^self instSpec = 7! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ab 3/12/2003 17:44'!
isMeta
	^ false! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
isPointers
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 16:57:28'!
isVariable
	"Answer whether the receiver has indexable variables.
	 Class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	(instSpec between: 2 and: 4) ifTrue: [ ^true ].
	^instSpec >= 9! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ar 3/21/98 02:36'!
isWeak
	"Answer whether the receiver has contains weak references."
	^ self instSpec = 4! !

!Behavior methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 7/5/2024 15:07:13'!
isWords
	"Answer whether the receiver's instances have indexed 32-bit integer slots.
	If true, #isBits is also true.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 10! !

!Behavior methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream! !

!Behavior methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'eem 8/13/2014 06:32'!
basicNew
	"Primitive. Answer an instance of the receiver (which is a class) with no 
	 indexable variables. Fail if the class is indexable. Essential. See Object 
	 documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run
	 before the method is activated.  Check that space was low and retry
	 via handleFailingBasicNew if so."

	<primitive: 70 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingBasicNew].
	self isVariable ifTrue: [^self basicNew: 0].
	self primitiveFailed! !

!Behavior methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'topa 10/7/2015 20:41'!
basicNew: sizeRequested
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 71 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingBasicNew: sizeRequested].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self primitiveFailed! !

!Behavior methodsFor: 'instance creation' library: #MinimalKernel01 stamp: ''!
new
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	^ self basicNew initialize
! !

!Behavior methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'sd 5/20/2004 11:20'!
new: sizeRequested 
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  ! !

!Behavior methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
allSubclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDo: 
		[:cl | 
		aBlock value: cl.
		cl allSubclassesDo: aBlock]! !

!Behavior methodsFor: 'class hierarchy' library: #MinimalKernel01 stamp: 'jmv 5/19/2025 18:21:34'!
deepSubclassNamed: aName
	"Or nil"
	self allSubclassesDo: [ :cls | cls name = aName ifTrue: [ ^cls ]].
	^nil.! !

!Behavior methodsFor: 'class hierarchy' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
superclass
	"Answer the receiver's superclass, a Class."

	^superclass! !

!Behavior methodsFor: 'instances' library: #MinimalKernel01 stamp: 'eem 3/10/2014 19:04'!
allInstancesOrNil
	"Answer all instances of the receiver, or nil if the primitive
	 fails, which it may be due to being out of memory."
	<primitive: 177>
	^nil! !

!Behavior methodsFor: 'instances' library: #MinimalKernel01 stamp: 'jmv 5/29/2025 09:15:16'!
someInstance
	"Answer the first instance of this receiver.
	Fails if there are none.
	See Object>>#nextInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.

	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^nil! !

!Behavior methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 16:08:35'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  If the VM is using
	 this implementation then classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.
	 There are 22 bits of classTable index and 22 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 175>
	self primitiveFailed.! !

!Behavior methodsFor: 'constants' library: #MinimalKernel01 stamp: 'eem 2/27/2025 10:21:52'!
maxInstSize
	"Answer the maximum number of named instance variables
	 (as opposed to indexed variables) of the receiver.
	 The class format is
		<5 bits inst spec><16 bits inst size>"
	^16rFFFF! !


!ClassDescription methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 11:14'!
instVarNames
	"Answer an Array of the receiver's instance variable names."

	^instanceVariables ifNil: [ #() ]! !

!ClassDescription methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/23/2011 18:11'!
theMetaClass
	"Sent to a class or metaclass, always return the metaclass"

	^self class! !

!ClassDescription methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^self! !

!ClassDescription methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/4/2023 18:58:11'!
printNameOn: aStream
	aStream nextPutAll: 'a subclass of '.
	superclass notNil
		ifTrue: [ superclass printNameOn: aStream ]
		ifFalse: [ aStream nextPutAll: 'nil' ]! !

!ClassDescription methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: self name! !


!Class methodsFor: 'class variables' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 11:09'!
classPool
	"Answer the dictionary of class variables."

	^classPool
		ifNil: [Dictionary new]! !

!Class methodsFor: 'pool variables' library: #MinimalKernel01 stamp: 'jmv 9/13/2023 18:06:41'!
sharedPools
	"Answer a Set of the pool dictionaries declared in the receiver."

	^sharedPools ifNil: [#()]! !

!Class methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 11:09'!
name
	"Answer the name of the receiver."

	^name ifNil: [super name]! !

!Class methodsFor: 'accesing class hierarchy' library: #MinimalKernel01 stamp: 'jmv 2/28/2010 23:16'!
subclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	subclasses
		ifNotNil: [subclasses do: aBlock]! !

!Class methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/4/2023 18:51:34'!
printNameOn: aStream
	name notNil
		ifTrue: [ aStream nextPutAll: name ]
		ifFalse: [ super printNameOn: aStream ]! !


!Metaclass methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 10/24/2023 10:07:27'!
allClassInstVarNames
	"Similar to #allInstVarNames, but don't include class instance variables used by the system itself,
	only those defined as class side #instanceVariableNames: "

	^(superclass notNil and: [ superclass isMeta ])
		ifTrue: [superclass allClassInstVarNames , self instVarNames]
		ifFalse: [self instVarNames copy]	"Guarantee a copy is answered."! !

!Metaclass methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 11:39'!
name
	"Answer a String that is the name of the receiver, either 'Metaclass' or 
	the name of the receiver's class followed by ' class'."

	^thisClass
		ifNil: [ 'a Metaclass']
		ifNotNil: [ thisClass name , ' class']! !

!Metaclass methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/23/2011 18:12'!
theMetaClass
	"Sent to a class or metaclass, always return the metaclass"

	^self! !

!Metaclass methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^thisClass! !

!Metaclass methodsFor: 'class hierarchy' library: #MinimalKernel01 stamp: 'ar 7/9/1999 14:11'!
subclassesDo: aBlock
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	thisClass subclassesDo:[:aSubclass|
		"The following test is for Class class which has to exclude
		the Metaclasses being subclasses of Class."
		aSubclass isMeta ifFalse:[aBlock value: aSubclass class]].! !

!Metaclass methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
isMeta
	^ true! !

!Metaclass methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/4/2023 19:05:02'!
printNameOn: aStream
	thisClass notNil
		ifTrue: [
			thisClass printNameOn: aStream.
			aStream nextPutAll: ' class']
		ifFalse: [ aStream nextPutAll: 'a Metaclass']! !

!Metaclass methodsFor: 'class variables' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
classPool
	"Answer the dictionary of class variables."

	^thisClass classPool! !


!Collection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^tally! !

!Collection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'sma 4/30/2000 11:17'!
allSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	self do: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true! !

!Collection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'di 11/21/1999 20:14'!
anySatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false"

	self do:[:item | (aBlock value: item) ifTrue: [^ true]].
	^ false! !

!Collection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'len 5/1/2022 13:25:40'!
count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count := 0.
	self do: [:each| (aBlock value: each) ifTrue: [count := count + 1]].
	^ count! !

!Collection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
reject: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to false.  Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]! !

!Collection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to true.  Answer the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!Collection methodsFor: 'private' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 12:08:53'!
species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."
	"Redefined here just for reference. See inheritance.
	#collect: avoids using #species in String, when there are non-Character objects
	#select: and #copy avoid using it in SortedCollection"

	^ self class! !

!Collection methodsFor: 'testing' library: #MinimalKernel01 stamp: 'sma 5/12/2000 14:07'!
includes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:each | each = anObject]! !

!Collection methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jpb 8/2/2019 23:59:42'!
is: aSymbol
	"Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Collection = aSymbol or: [ super is: aSymbol ]! !

!Collection methodsFor: 'testing' library: #MinimalKernel01 stamp: 'di 11/6/1998 09:16'!
isSequenceable
	^ false! !

!Collection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 8/9/2016 09:35:07'!
= other
	"Default implementation. Usually redefined in subclasses."

	^self == other! !

!Collection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/12/2019 18:08:54'!
emptyCollectionHash
	^self species hash! !

!Collection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/12/2019 18:09:13'!
hash
	"A default hash function for any collection.  Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash := self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self do: [ :elem | hash := hash bitXor: elem hash]].
	^ hash bitXor: self size hash
! !


!SequenceableCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
indexOf: anElement 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ls 8/18/1998 00:36'!
indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	^self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock! !

!SequenceableCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ul 9/12/2010 04:34'!
indexOf: anElement startingAt: start
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer 0."

	^self indexOf: anElement startingAt: start ifAbsent: 0! !

!SequenceableCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.1 9/23/1996 20:34:59'!
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	start to: self size do:
		[:i | (self at: i) = anElement ifTrue: [^ i]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver.  
	Answer the receiver itself.
	Use copyReplaceFrom:to:with: for insertion/deletion which may alter the 
	size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1! !

!SequenceableCollection methodsFor: 'converting' library: #MinimalKernel01 stamp: 'jmv 3/7/2015 22:28'!
asByteArray
	"Answer a ByteArray whose elements are the elements of the receiver.
	Optimized implementation."

	^ ByteArray withAll: self! !

!SequenceableCollection methodsFor: 'copying' library: #MinimalKernel01 stamp: 'di 1/16/98 16:40'!
, otherCollection 
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 printStringBase: 16) copyFrom: 4 to: 6) , ' boy!!'
"! !

!SequenceableCollection methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:13:49'!
copyBlank: requestedSize
	"Answer an instance like me, of size n, but without content.
	All elements may be nil, zero or some other default value if needed."

	^self species new: requestedSize! !

!SequenceableCollection methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:14:06'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1 max: 0.
	^(self copyBlank: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:14:40'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self copyBlank: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection! !

!SequenceableCollection methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:15:40'!
grownTo: newSize

	| grownCollection |
	grownCollection := self copyBlank: newSize.
	grownCollection replaceFrom: 1 to: self size with: self startingAt: 1.
	^grownCollection! !

!SequenceableCollection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'jmv 9/16/2014 09:32'!
do: aBlock 
	"Refer to the comment in Collection >> #do:
	Note: Subclasses need to redefine either #do: or #size (or both!!).
	"
	1 to: self size do: [ :index |
		aBlock value: (self at: index) ]! !

!SequenceableCollection methodsFor: 'enumerating' library: #MinimalKernel01 stamp: 'jmv 2/19/2015 10:50'!
select: aBlock 
	"Refer to the comment in Collection>>select: ."
	^self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			(aBlock value: (self at: index))
				ifTrue: [ strm nextPut: (self at: index) ]]]! !

!SequenceableCollection methodsFor: 'testing' library: #MinimalKernel01 stamp: 'sma 5/12/2000 14:08'!
includes: anObject
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0! !

!SequenceableCollection methodsFor: 'testing' library: #MinimalKernel01 stamp: 'di 11/6/1998 09:35'!
isSequenceable
	^ true! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 2/19/2015 20:36'!
= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	"#( 1 2 3) = (1 to: 3) -> false"
	otherCollection isInterval ifTrue: [ ^false ].
	^ self hasEqualElements: otherCollection! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 12/27/2018 18:20:09'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size := self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 16:08:53'!
hash
	"Subclasses might use other methods.
	However #hashQuick is suggested for very large collections."
	^ self hashQuick! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/14/2022 14:50:31'!
hashQuick
	^self hashQuick: self species hash! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/14/2022 14:50:11'!
hashQuick: initialHash
	| hash size step |

	size := self size.
	hash := (initialHash + size hash) hashMultiply.
	step := size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem := self at: i.
		elem == self ifFalse: [
			hash := (hash + elem hash) hashMultiply]].
	^hash! !

!SequenceableCollection methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/19/2025 10:48:21'!
is: otherCollection subcollectionAt: index
	"Answer whether the receiver includes otherCollection as a subcollection at position index."

	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := otherCollection size.
	sequenceSize = 0 ifTrue: [ ^true ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :i |
		(self at: index+i-1) = (otherCollection at: i) ifFalse: [ ^false ]].
	^true! !

!SequenceableCollection methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'jmv 8/5/2022 11:41:43'!
asStreamResult
	"See senders.
	Also see #thatCanHoldCodePoints"
	^self! !

!SequenceableCollection methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'sma 5/12/2000 12:51'!
readStream
	^ ReadStream on: self! !

!SequenceableCollection methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'jmv 7/7/2022 10:37:44'!
thatCanBeModified
	"By default, SequenceableCollections can handle #at:put:"

	^self! !

!SequenceableCollection methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'sma 5/12/2000 12:52'!
writeStream
	^ WriteStream on: self! !


!SequenceableCollection class methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'jmv 1/18/2011 14:06'!
streamContents: blockWithArg
	^self streamContents: blockWithArg estimatedSize: 100! !

!SequenceableCollection class methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:30:56'!
streamContents: blockWithArg estimatedSize: estimatedSize

	^self streamContents: blockWithArg on: (self new: estimatedSize)! !

!SequenceableCollection class methodsFor: 'stream creation' library: #MinimalKernel01 stamp: 'jmv 2/7/2024 16:28:16'!
streamContents: blockWithArg on: aCollection

	| stream originalContents |
	
	stream := aCollection writeStream.
	blockWithArg value: stream.
	originalContents := stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents asStreamResult ]
		ifFalse: [ stream contents ]! !


!ArrayedCollection methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !


!ArrayedCollection class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 9/29/2019 12:52:40'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements! !

!ArrayedCollection class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 2/5/2024 17:43:28'!
new: sizeRequested
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  ! !

!ArrayedCollection class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 9/29/2019 12:47:53'!
numElements
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0! !

!ArrayedCollection class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 3/7/2015 23:10'!
withAll: aSequenceableCollection
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection! !

!ArrayedCollection class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 5/12/2025 12:09:36'!
withAll: aSequenceableCollection with: finalElement
	"Create a new collection containing all the elements from aSequenceableCollection."

	| answer |
	answer := (self new: aSequenceableCollection size + 1)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection.
	answer at: answer size put: finalElement.
	^answer! !


!Array methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 4/9/2016 19:32'!
is: aSymbol
	^ aSymbol == #Array or: [ super is: aSymbol ]! !

!Array methodsFor: 'system prinitives' library: #MinimalKernel01 stamp: 'len 5/15/2019 16:43:02'!
elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

	<primitive: 128 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[| maxRequired |
		 "In Spur, two-way become may involve making each pair of objects into a forwarder into a copy of the other.
		 So if become fails with #'insufficient object memory', garbage collect, and if necessary, grow memory."
		 maxRequired := (self sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0])
						+ (otherArray sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0]).
		 (Smalltalk garbageCollectMost < maxRequired
		  and: [Smalltalk garbageCollect < maxRequired]) ifTrue:
			[Smalltalk growMemoryByAtLeast: maxRequired].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed! !

!Array methodsFor: 'system prinitives' library: #MinimalKernel01 stamp: 'jmv 1/13/2017 09:57:31'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !


!ByteArray methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 9/11/2023 17:58:16'!
indexOf: anInteger startingAt: start ifAbsent: aBlock

	| ans |
	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	ans := ByteArray indexOfByte: anInteger inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!ByteArray methodsFor: 'converting' library: #MinimalKernel01 stamp: 'ar 11/2/1998 00:51'!
asByteArray
	^self! !

!ByteArray methodsFor: 'converting' library: #MinimalKernel01 stamp: 'Squeak1.2 6/29/1997 10:40:55'!
asString
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self! !

!ByteArray methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 5/26/2022 16:04:32'!
is: aSymbol
	^#ByteArray = aSymbol or: [ super is: aSymbol ]! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 16:40:25'!
bitAt: bitIndex
	"Answer the bit (0 or 1) at a bit index.
	This way, the receiver behaves as a BitArray.
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8."
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	^ self bitAt: index bitPosition: bitPosition! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 16:40:44'!
bitAt: bitIndex put: aBit
	"Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8.
	#[1 0 0 ] bitAt: 1
	#[0 1  0 ] bitAt: 9
	#[0 0 128 ] bitAt: 24
	"
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	self bitAt: index bitPosition: bitPosition put: aBit! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/17/2019 08:51:43'!
bitBooleanAt: bitIndex
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	^ (self bitAt: bitIndex) = 1! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/17/2019 08:52:49'!
bitBooleanAt: bitIndex put: aBoolean
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	self bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:27:44'!
float32At: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self uint32At: index bigEndian: bool.
	^ Float fromIEEE32Bit: w! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:28:17'!
float32At: index put: value bigEndian: bool 
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w := value asIEEE32BitWord.
	self uint32At: index put: w bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:27:48'!
float64At: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self uint32At: index bigEndian: bool.
	w2 := self uint32At: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:28:21'!
float64At: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self uint32At: index put: w1 bigEndian: bool.
	self uint32At: index + 4 put: w2 bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/6/2015 23:16'!
int16At: index bigEndian: bigEndian
	"Return a 16-bit signed integer quantity starting from the given byte index"

	| result |
	result := bigEndian
		ifFalse: [ ((self at: index + 1) bitShift: 8) + (self at: index) ]
		ifTrue: [ ((self at: index) bitShift: 8) + (self at: index + 1) ].
	result < 16r8000 ifTrue: [ ^result ].
	^result - 16r10000! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/6/2015 23:16'!
int16At: index put: value bigEndian: bigEndian
	"Store a 16-bit signed integer quantity starting from the given byte index"
	
	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r10000 ].
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (unsignedValue bitShift: -8);
			at: index put: (unsignedValue bitAnd: 16rFF).
		^value ].
	self
		at: index put: (unsignedValue bitShift: -8);
		at: index + 1 put: (unsignedValue bitAnd: 16rFF).
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/9/2015 20:28'!
int32At: index bigEndian: bigEndian
	"Return a 32-bit integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	
	| byte result |
	bigEndian ifFalse: [
		(byte := self at: index + 3) <= 16r7F ifTrue: [ "Is the result non-negative?"
			byte <= 16r3F ifTrue: [
				^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
			^(LargePositiveInteger new: 4)
				replaceFrom: 1
					to: 4
					with: self
					startingAt: index;
				normalize ].
		"Negative"
		byte >= 16rC0 ifTrue: [
			^-1 - (((((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index) bitXor: 16rFF)) ].
		(result := LargeNegativeInteger new: 4)
			digitAt: 4 put: ((self at: index + 3) bitXor: 16rFF);
			digitAt: 3 put: ((self at: index + 2) bitXor: 16rFF);
			digitAt: 2 put: ((self at: index + 1) bitXor: 16rFF).
		(byte := ((self at: index) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
			^result
				digitAt: 1 put: byte;
				normalize ].
		^result
			digitAt: 1 put: 16rFF;
			- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this." ].
	(byte := self at: index) <= 16r7F ifTrue: [ "Is the result non-negative?"
		byte <= 16r3F ifTrue: [
			^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3) ].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize ].
	"Negative"
	16rC0 <= byte ifTrue: [
		^-1 - (((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index + 3) bitXor: 16rFF)) ].
	(result := LargeNegativeInteger new: 4)
		digitAt: 4 put: (byte bitXor: 16rFF);
		digitAt: 3 put: ((self at: index + 1) bitXor: 16rFF);
		digitAt: 2 put: ((self at: index + 2) bitXor: 16rFF).
	(byte := ((self at: index + 3) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
		^result
			digitAt: 1 put: byte;
			normalize ].
	^result 
		digitAt: 1 put: 16rFF;
		- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this."! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/6/2015 17:13'!
int32At: index put: value bigEndian: bigEndian
	"Store a 32-bit signed integer quantity starting from the given byte index"
	
	| v v2 |
	value isLarge ifTrue: [
		bigEndian ifFalse: [
			value positive ifTrue: [
				self 
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			v := 0.
			[ v <= 3 and: [ (v2 := ((value digitAt: v + 1) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
				self at: index + v put: 0.
				v := v + 1 ].
			self at: index + v put: v2.
			v := v + 1.
			[ v <= 3 ] whileTrue: [
				self at: index + v put: ((value digitAt: (v := v + 1)) bitXor: 16rFF) ].
			^value ].
		value positive ifTrue: [
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index + 3 put: (value digitAt: 1).
			^value ].
		v := 3.
		[ 0 <= v and: [ (v2 := ((value digitAt: 4 - v) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
			self at: index + v put: 0.
			v := v - 1 ].
		self at: index + v put: v2.
		[ 0 <= (v := v - 1) ] whileTrue: [
			self at: index + v put: ((value digitAt: 4 - v) bitXor: 16rFF) ].
		^value ].
	v := value bitShift: -24.
	0 <= (v := (v bitAnd: 16r7F) - (v bitAnd: 16r80)) ifFalse: [
		v := v + 16r100 ].
	bigEndian ifFalse: [
		self 
			at: index put: (value bitAnd: 16rFF);
			at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
			at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
			at: index + 3 put: v.
		^value ].
	self
		at: index put: v;
		at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
		at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
		at: index + 3 put: (value bitAnd: 16rFF).
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:27:16'!
int64At: index bigEndian: bigEndian
	"Return a 64-bit signed integer quantity starting from the given byte index."

	| value |
	value := self uint64At: index bigEndian: bigEndian.
	value digitLength < 8 ifTrue: [ ^value ].
	(value digitAt: 8) < 16r80 ifTrue: [ ^value ].
	^value - 16r10000000000000000! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:27:27'!
int64At: index put: value bigEndian: bigEndian
	"Store a 64-bit signed integer quantity starting from the given byte index."
	
	^self
		uint64At: index
		put: (value negative
			ifFalse: [ value ]
			ifTrue: [ value + 16r10000000000000000 ])
		bigEndian: bigEndian! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 6/5/2024 10:00:06'!
int8At: index bigEndian: dummyArgument
	"Return a 8-bit signed integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| result |
	result := self at: index.
	result < 16r80 ifTrue: [ ^result ].
	^result - 16r100! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 6/5/2024 10:00:50'!
int8At: index put: value bigEndian: dummyArgument
	"Store a 8-bit signed integer quantity at the given byte index
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r100 ].
	self at: index put: unsignedValue.
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/3/2015 01:33'!
uint16At: index bigEndian: bigEndian
	"Return a 16-bit unsigned integer quantity starting from the given byte index"

	bigEndian ifFalse: [ ^((self at: index + 1) bitShift: 8) + (self at: index) ].
	^((self at: index) bitShift: 8) + (self at: index + 1)
	! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/3/2015 15:29'!
uint16At: index put: value bigEndian: bigEndian
	"Store a 16-bit unsigned integer quantity starting from the given byte index"
	
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (value bitShift: -8);
			at: index put: (value bitAnd: 16rFF).
		^value ].
	self
		at: index put: (value bitShift: -8);
		at: index+1 put: (value bitAnd: 16rFF).
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'eem 2/22/2016 09:20'!
uint32At: index bigEndian: bigEndian
	"Return a 32-bit unsigned integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	| byte |
	bigEndian ifTrue:
		[((byte := self at: index) <= 16r3F
		 or: [SmallInteger maxVal >  1073741823]) ifTrue:
			[^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize].
	((byte := self at: index + 3) <= 16r3F
	 or: [SmallInteger maxVal >  1073741823]) ifTrue:
		[^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)].
	^(LargePositiveInteger new: 4)
		replaceFrom: 1 to: 4 with: self startingAt: index;
		normalize! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/3/2015 01:33'!
uint32At: index put: value bigEndian: bigEndian
	"Store a 32-bit unsigned integer quantity starting from the given byte index"
	
	value isLarge
		ifTrue: [
			bigEndian ifFalse: [
				self
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index +3 put: (value digitAt: 1) ]
		ifFalse: [
			bigEndian ifFalse: [
				self 
					at: index put: (value bitAnd: 16rFF);
					at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
					at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
					at: index + 3 put: (value bitShift: -24).
				^value ].
			self 
				at: index put: (value bitShift: -24);
				at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
				at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
				at: index + 3 put: (value bitAnd: 16rFF) ].
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'eem 2/22/2016 09:36'!
uint64At: index bigEndian: bigEndian
	"Return a 64-bit unsigned integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"

	| byte |
	SmallInteger maxVal >  1073741823 ifTrue:
		[bigEndian
			ifTrue: "64-bit SmallIntegers have a 3 bit tag and a sign bit, so the most positive value has 16rF as its top byte."
				[(byte := self at: index) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)) bitShift: 8)
						+ (self at: index + 4) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)]]
			ifFalse:
				[(byte := self at: index + 7) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 4)) bitShift: 8)
						+ (self at: index + 3) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)]]].
	bigEndian ifFalse: [
		(byte := self at: index + 7) = 0 ifFalse: [
			^(LargePositiveInteger new: 8)
				replaceFrom: 1 to: 8 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 6) = 0 ifFalse: [
			^(LargePositiveInteger new: 7)
				replaceFrom: 1 to: 7 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 5) = 0 ifFalse: [
			^(LargePositiveInteger new: 6)
				replaceFrom: 1 to: 6 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 4) = 0 ifFalse: [
			^(LargePositiveInteger new: 5)
				replaceFrom: 1 to: 5 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 3) <= 16r3F ifFalse: [
			^(LargePositiveInteger new: 4)
				replaceFrom: 1 to: 4 with: self startingAt: index;
				normalize ].
		^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
	(byte := self at: index) = 0 ifFalse: [
		^(LargePositiveInteger new: 8)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: (self at: index + 1);
			digitAt: 8 put: byte;
			normalize ].
	(byte := self at: index + 1) = 0 ifFalse: [	
		^(LargePositiveInteger new: 7)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: byte;
			normalize ].
	(byte := self at: index + 2) = 0 ifFalse: [	
		^(LargePositiveInteger new: 6)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: byte;
			normalize ].
	(byte := self at: index + 3) = 0 ifFalse: [	
		^(LargePositiveInteger new: 5)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: byte;
			normalize ].		
	(byte := self at: index + 4) <= 16r3F ifFalse: [
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: byte;
			normalize ].
	^(((byte bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'ul 9/9/2015 20:18'!
uint64At: index put: value bigEndian: bigEndian
	"Store a 64-bit unsigned integer quantity starting from the given byte index"
	
	| i j |
	value isLarge ifTrue: [
		i := value digitLength.
		bigEndian ifFalse: [
			self
				replaceFrom: index
					to: index + i - 1
					with: value 
					startingAt: 1;
				replaceFrom: index + i
					to: index + 7
					with: #[0 0 0 0 0 0 0 0]
					startingAt: 1.
			^value ].
		j := index + 8.
		i <= 7 ifTrue: [
			self
				replaceFrom: index
				to: j - i - 1
				with: #[0 0 0 0 0 0 0 0]
				startingAt: 1 ].
		[ 1 <= i ] whileTrue: [
			self at: j - i put: (value digitAt: i).
			i := i - 1 ].
		^value ].
	bigEndian ifFalse: [
		j := index - 1.
		i := value.
		[ 1 <= i ] whileTrue: [
			self at: (j := j + 1) put: (i bitAnd: 16rFF).
			i := i bitShift: -8 ].
		self replaceFrom: j + 1
			to: index + 7
			with: #[0 0 0 0 0 0 0 0]
			startingAt: 1.
		^value ].
	j := index + 8.
	i := value.
	[ 1 <= i ] whileTrue: [
		self at: (j := j - 1) put: (i bitAnd: 16rFF).
		i := i bitShift: -8 ].
	self replaceFrom: index
		to: j - 1
		with: #[0 0 0 0 0 0 0 0]
		startingAt: 1.
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 6/5/2024 10:00:58'!
uint8At: index bigEndian: dummyArgument
	"Return a 8-bit unsigned integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index! !

!ByteArray methodsFor: 'access - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 6/5/2024 10:01:02'!
uint8At: index put: value bigEndian: dummyArgument
	"Store a 8-bit unsigned integer quantity at the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index put: value! !

!ByteArray methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/26/2022 15:06:03'!
= another
	"Make good use of String primitives"

	self == another ifTrue: [ ^ true ].
	self species == another species ifFalse: [^ false].
	^String is: self equalTo: another ! !

!ByteArray methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/14/2022 14:52:04'!
hash
	"#hash is implemented, because #= is implemented"
	self size > ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: ByteArray hash ].
	^self class
		hashBytes: self
		startingWith: ByteArray hash! !

!ByteArray methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 1/13/2017 09:57:38'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !


!ByteArray class methodsFor: 'byte based hash' library: #MinimalKernel01 stamp: 'jmv 6/27/2022 15:45:37'!
hashBytes: aByteArray startingWith: speciesHash
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	byteArraySize := aByteArray size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [ :pos |
		hash := hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash! !

!ByteArray class methodsFor: 'byte based hash' library: #MinimalKernel01 stamp: 'jmv 6/14/2022 14:45:55'!
minSizeForHashQuick
	"Answer the size above which we prefer using the #hashQuick method"

	^256! !

!ByteArray class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 1/6/2024 15:50:15'!
findFirstInBytes: aByteArrayOrByteString inSet: aByteSetByteArray startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>

	aByteSetByteArray size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aByteArrayOrByteString size.
	[ i <= stringSize and: [ (aByteSetByteArray at: (aByteArrayOrByteString basicAt: i) +1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !

!ByteArray class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 9/11/2023 17:57:34'!
indexOfByte: anInteger inBytes: aByteArrayOrByteString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	start > 0 ifFalse: [ ^ 0 ].
	stringSize := aByteArrayOrByteString size.
	start to: stringSize do: [:pos |
		(aByteArrayOrByteString basicAt: pos) = anInteger ifTrue: [^ pos]].
	^ 0
! !


!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 11/29/2008 11:38'!
methodClass
	"answer the class that I am installed in"
	^(self literalAt: self numLiterals) value.! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/4/2024 15:38:13'!
numLiterals
	"Answer the number of literals used by the receiver."

	^ self header bitAnd: 16r7FFF! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'HAW 1/7/2019 15:49:22'!
selector
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	^self 
		withPropertiesDo: [ :properties | properties selector ] 
		ifSelector: [ :selector | selector ]
! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 7/4/2023 16:00:46'!
withPropertiesDo: withBlock ifSelector: notBlock
	"Evaluate withBlock with our AdditionalMethodState,
	or evaluate notBlock with our selector
		(if we don't hold any Pragmas or properties in an AdditionalMethodState)"

	| penultimalLiteral |
	penultimalLiteral := self penultimateLiteral.
	^penultimalLiteral isMethodProperties 
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]! !

!CompiledMethod methodsFor: 'converting' library: #MinimalKernel01 stamp: 'ul 12/8/2010 03:24'!
asString

	^self printString! !

!CompiledMethod methodsFor: 'literals' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
header
	"Answer the word containing the information about the form of the
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1! !

!CompiledMethod methodsFor: 'literals' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
literalAt: index 
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1! !

!CompiledMethod methodsFor: 'literals' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
objectAt: index 
	"Answer with the method header (if index=1) or a literal (if index >1) from the 
	receiver.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed! !

!CompiledMethod methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 5/14/2025 10:40:39'!
printClassAndSelectorOn: aStream

	aStream
		print: self methodClass;
		nextPutAll: '>>'.
	self selector storeOn: aStream! !

!CompiledMethod methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 5/14/2025 10:35:47'!
printOn: aStream
	"Overrides method inherited from the byte arrayed collection."

	aStream nextPut: $(.
	self printClassAndSelectorOn: aStream.
	aStream space; nextPut: $"; nextPut: $a; space.
	aStream nextPutAll: self class name.
	aStream nextPut: $:; print: self size.
	aStream nextPut: $(; print: self identityHash; nextPut: $); nextPut: $"; nextPut: $)! !

!CompiledMethod methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #CompiledMethod or: [ super is: aSymbol ]! !

!CompiledMethod methodsFor: 'private' library: #MinimalKernel01 stamp: 'eem 11/29/2008 11:10'!
penultimateLiteral
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	^(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex]
		ifFalse: [nil]! !


!Semaphore methodsFor: 'communication' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
signal
	"Send a signal through the receiver. If one or more processes have been 
	suspended trying to receive a signal, allow the first one to proceed. If no 
	process is waiting, remember the excess signal. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"! !

!Semaphore methodsFor: 'communication' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
wait
	"The active Process must receive a signal through the receiver before 
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See  
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals := excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"! !

!Semaphore methodsFor: 'mutual exclusion' library: #MinimalKernel01 stamp: 'jmv 2/23/2010 14:27'!
critical: mutuallyExcludedBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in
	the process of running the critical: message. If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."
	
	| caught |
	"We need to catch eventual interruptions very carefully. 
	The naive approach of just doing, e.g.,:
		self wait.
		aBlock ensure:[self signal].
	will fail if the active process gets terminated while in the wait.
	However, the equally naive:
		[self wait.
		aBlock value] ensure:[self signal].
	will fail too, since the active process may get interrupted while
	entering the ensured block and leave the semaphore signaled twice.
	To avoid both problems we make use of the fact that interrupts only
	occur on sends (or backward jumps) and use an assignment (bytecode)
	right before we go into the wait primitive (which is not a real send and
	therefore not interruptable either)."

	caught := false.
	^[
		caught := true.
		self wait.
		mutuallyExcludedBlock value
	] ensure: [ caught ifTrue: [self signal] ]
! !

!Semaphore methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'sma 4/22/2000 18:48'!
= anObject
	^ self == anObject! !

!Semaphore methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'sma 4/22/2000 18:48'!
hash
	^ self identityHash! !

!Semaphore methodsFor: 'initialize-release' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0! !

!Semaphore methodsFor: 'private' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 12:31:53'!
species
	"So we are never equal an Array"

	^ self class! !


!Semaphore class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
forMutualExclusion
	"Answer a new instance of me that contains a single signal.  
	This new instance can now be used for mutual exclusion (see the 
	critical: message to Semaphore)."

	^self new signal! !

!Semaphore class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Answer a new instance of Semaphore that contains no signals."

	^self basicNew initSignals! !


!CharacterSequence methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 6/11/2022 19:15:43'!
indexOf: aCharacter

	^self indexOf: aCharacter startingAt: 1! !

!CharacterSequence methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
string
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self! !

!CharacterSequence methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 7/25/2024 10:38:14'!
= another
	"Note: Behavior the same for String, Symbol, UnicodeString, UnicodeSymbol and Text,
	both as receiver and as argument, if they hold same Characters."

	self subclassResponsibility! !

!CharacterSequence methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 7/25/2024 10:36:22'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self subclassResponsibility! !

!CharacterSequence methodsFor: 'printing' library: #MinimalKernel01 stamp: 'sma 6/1/2000 09:48'!
printOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."

	self storeOn: aStream! !

!CharacterSequence methodsFor: 'printing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x := self at: i).
		x == $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !

!CharacterSequence methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/6/2022 10:54:40'!
isString
	^ true! !


!CharacterSequence class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 9/29/2019 12:52:40'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements! !

!CharacterSequence class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 9/29/2019 12:47:53'!
numElements
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0! !


!String methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 12:18:20'!
at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character codePoint: (super at: index)! !

!String methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/9/2024 12:12:34'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	
			aCharacter codePoint > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !

!String methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 09:21:57'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	"String can only hold ASCII + Latin1"
	aCharacter codePoint >255 ifTrue: [ ^ aBlock value ].
	ans := ByteArray indexOfByte: aCharacter codePoint inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 1/13/2017 09:58:04'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!String methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !

!String methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 12/7/2022 09:07:39'!
= aString
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isByteString ifFalse: [
		^aString = self ].

	^String is: self equalTo: aString.! !

!String methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 12/7/2022 09:07:58'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self size >  ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: String hash ].
	^ByteArray
		hashBytes: self
		startingWith: String hash! !

!String methodsFor: 'copying' library: #MinimalKernel01 stamp: 'KenD 9/4/2016 16:50:54'!
, aStringOrText
	^ aStringOrText appendToString: self! !

!String methodsFor: 'copying' library: #MinimalKernel01 stamp: 'KenD 9/4/2016 16:21:53'!
appendToString: aString

	| stringSize |
	stringSize := aString size.
	^ aString copyReplaceFrom: stringSize + 1
		 	    to: stringSize
		   	    with: self! !

!String methodsFor: 'copying' library: #MinimalKernel01 stamp: 'jmv 4/16/2023 17:37:20'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| rep |
	rep := replacementCollection string.
	rep isUnicodeString ifTrue: [
		^self asUnicodeString copyReplaceFrom: start to: stop with: rep  ].
	^super copyReplaceFrom: start to: stop with: rep.! !

!String methodsFor: 'converting' library: #MinimalKernel01 stamp: 'jmv 8/8/2024 17:11:12'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin-1 (ISO 8859-1)
	Answer a ByteArray with a valid UTF-8 sequence."

	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		self do: [ :c |
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: c codePoint ]].! !

!String methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jpb 8/3/2019 00:01:22'!
is: aSymbol
	"Note: Senders might prefer #isString for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#String = aSymbol or: [ super is: aSymbol]! !

!String methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/9/2022 15:01:48'!
isAscii
	"Return true when I am pure 7-bit ASCII.
	This means, for example, that my bytes are also my UTF-8 representation."

	^ String isAscii: self! !

!String methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/6/2022 10:21:36'!
isByteString
	^ true! !


!String class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 2/4/2024 21:05:06'!
new: sizeRequested 
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  ! !

!String class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
with: aCharacter 
	"Answer a new instance of me, containing only aCharacter.
	String with: $a
	String with: $á
	String with: Character euro
	"

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: aCharacter.
	^newCollection! !

!String class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 6/27/2022 15:40:39'!
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>

	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 := order at: (string1 basicAt: i) + 1.
		c2 := order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
! !

!String class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 7/7/2024 15:41:39'!
is: aStringOrByteArray equalTo: another
	"Take advantage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: self caseSensitiveOrder) = 2! !

!String class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 1/6/2024 15:50:03'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	| nonAsciiBytesSetBA |
	nonAsciiBytesSetBA := `(ByteArray new: 128), ((ByteArray new: 128) + 1)`.
	^ (ByteArray
		findFirstInBytes: aStringOrByteArray
		inSet: nonAsciiBytesSetBA
		startingAt: 1)
			= 0.! !

!String class methodsFor: 'static state' library: #MinimalKernel01 stamp: 'jmv 7/25/2024 15:53:47'!
caseSensitiveOrder
	^`String basicCaseSensitiveOrder`.! !


!Symbol methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
at: anInteger put: anObject 
	"you can not modify the receiver."

	self errorNoModification! !

!Symbol methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	self errorNoModification! !

!Symbol methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 4/30/2025 12:51:16'!
storeOn: aStream 

	aStream nextPut: $#.
	(Symbol isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !

!Symbol methodsFor: 'testing' library: #MinimalKernel01 stamp: 'md 4/30/2003 15:31'!
isSymbol
	^ true ! !

!Symbol methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/3/2022 10:34:38'!
= another

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another isSymbol ifTrue: [
		^ false].  "Was not ==, therefore can't be = "

	"Otherwise use string =..."
	^ super = another! !

!Symbol methodsFor: 'copying' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy
	"Answer with me, because Symbols are unique."! !

!Symbol methodsFor: 'private' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
species
	^String! !


!Symbol class methodsFor: 'services' library: #MinimalKernel01 stamp: 'jmv 4/30/2025 12:51:07'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| char |
	aSymbol size = 0 ifTrue: [^ false].
	aSymbol = ':' ifTrue: [ ^true ].
	aSymbol = '|' ifTrue: [ ^true ].
	(aSymbol allSatisfy: [ :c | Character binarySelectorCharacters includes: c ])
		ifTrue: [ ^true ].
	char := aSymbol at: 1.
	(char isLetter or: [ char = $_ ]) ifTrue: [
		(aSymbol allSatisfy: [ :c |
			c isLetter or: [ c = $_ or: [ c isDigit or: [ c = $: ]]]])
				ifTrue: [ ^true ]].		
	^false! !


!SystemDictionary methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/3/2012 18:04'!
printOn: aStream
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]! !

!SystemDictionary methodsFor: 'snapshot and quit' library: #MinimalKernel01 stamp: 'jmv 3/27/2015 17:52'!
quitPrimitive: exitCode
	"Primitive. Exit with exitCode to another operating system on the host machine, if one exists.
	All state changes in the object space since the last snapshot are lost.
	Ignore exitCode if it's not supported by the VM."

	<primitive: 113>
	self quitPrimitive! !

!SystemDictionary methodsFor: 'vm parameters' library: #MinimalKernel01 stamp: 'jmv 4/25/2024 11:26:11'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers.
	 Answer with the current value of that register.  Primitive fails if parameterIndex is invalid or has no corresponding register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	byte size of old-space (read-only)
		2	byte size of young-space (read-only)
		3	byte size of object memory (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	Scavenging GCs
		10	total milliseconds in scavenging GCs since startup (read-only) since startup (read-only)
		11	tenures of surving objects since startup/last write (read-write)
		12	event trace mask; if 1 << eventType is set in the mask then event received by primGetNextEvent: will be printed to stderr.
		13	VM ticker start microseconds (Croquet/QwaqVM only)
		14	VM ticker count (Croquet/QwaqVM only)
		15	VM ticker call count (Croquet/QwaqVM only)
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in full GC compaction since startup (a portion of parameter 8)
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which to shrink object memory (read-write)
		25	ammount to grow by when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	bytes allocated in total since start-up or reset (read-write)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use (Cog Stack VM only, otherwise nil)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		44	size of eden, in bytes (Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		46	size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties stored in the image header (that instruct the VM) as an integer encoding an array of bit flags. 
			Bit 0: if set, tells the VM that the image's Process class has threadId as its 5th inst var 'threadAffinity' 
					(after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods. This is useful when profiling a system to set the number
					of literals beyond which CompiledMethods are not jitted.
			Bit 2: if set, preempting does not yield. If not set, preempting a process puts it at the back of the run queue, yielding to all other
					process of the same priority. This was the Smalltalk-80 behavior, but it makes sharing structures between processes of 
					the same priority unnecesarily more complex.
			Bit 3: in a VM that supports muilt-threaded FFI, if set, the Window system will only be accessed from the first VM thread:
					'noThreadingOfGUIThread'
			Bit 4: in a Spur vm, if set, new finalization enabled. Enable for images with Ephemeron support.
					causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: If set, wheel events will be delivered as such and not mapped to arrow key events.
					If unset, create fake keyboard events for wheel events. 
			Bit 6: See #doMixedArithmetic:
				If set, arithmetic primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If unset, the VM will make the conversion, meaning that `SmallInteger maxVal asFloat = SmallInteger maxVal` could evaluate to false.
			Bit 7: if set use the unix epoch for dates and times in files (FilePlugin / FileAttributesPlugin) / or is it about UTC???- If set UTC seconds from epoch, rather than local seconds
			Bit 8: if set upscaleDisplayIfHighDPI
		49	the size of the external semaphore table (read-write; Cog VMs only)
		50-51 reserved for VM parameters that persist in the image (such as eden above)
		52	root (remembered) table maximum size (read-only)
		53	the number of oldSpace segments (Spur only, otherwise nil)
		54	total size of free old space (Spur only, otherwise nil)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of forceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only; Cog VMs only)
		61	number of stack page divorces since startup (read-only; Cog VMs only)
		62	number of machine code zone compactions since startup (read-only; Cog VMs only)
		63	milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
		64	current number of machine code methods (read-only; Cog VMs only)		
		65	In newer Cog VMs a set of flags describing VM features,
				if non-zero bit 0 implies multiple bytecode set support;
				if non-zero bit 1 implies read-only object support;
				if non-zero bit 2 implies the VM suffers from using an ITIMER heartbeat (if 0 it has a thread that provides the heartbeat)
			(read-only; Cog VMs only; nil in older Cog VMs, a boolean answering multiple bytecode support in not so old Cog VMs)
				if non-zero bit 3 implies the VM supports cross-platform BIT_IDENTICAL_FLOATING_POINT arithmetic
				if non-zero bit 4 implies the VM can catch exceptions in FFI calls and answer them as primitive failures
				if non-zero bit 5 implies the VM's suspend primitive backs up the process to before the wait if it was waiting on a condition variable
		66	the byte size of a stack page in the stack zone  (read-only; Cog VMs only)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur VMs only).
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		70	the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
		71	the value of VM_PROXY_MINOR (the interpreterProxy minor version number)
		72	total milliseconds in full GCs Mark phase since startup (read-only)
		73	total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74	maximum pause time due to segment allocation
		75	whether arithmetic primitives will do mixed type arithmetic; if false they fail for different receiver and argument types
		76	the minimum unused headroom in all stack pages; Cog VMs only"

	<primitive: 254>
	(parameterIndex isInteger
	 and: [parameterIndex > 0]) ifTrue:
		[^nil].
	self primitiveFailed! !

!SystemDictionary methodsFor: 'vm parameters' library: #MinimalKernel01 stamp: 'jmv 11/9/2023 17:23:56'!
wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	Smalltalk wordSize
	"

	^self vmParameterAt: 40! !


!Stream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
next
	"Answer the next object accessible by the receiver."

	self subclassResponsibility! !

!Stream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection! !

!Stream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'djp 7/21/1998 11:40'!
print: anObject
	anObject printOn: self! !

!Stream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 12/9/2013 17:52'!
binary
	"Compatibility. If possible, store and retrieve bytes."
	self useBytes! !

!Stream methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	"Return true if the receiver responds to the stream protocol (if that's what's asked)"
	^ aSymbol == #Stream or: [ super is: aSymbol ]! !

!Stream methodsFor: 'nonhomogeneous accessing' library: #MinimalKernel01 stamp: 'jmv 9/5/2023 08:29:24'!
nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length."

	| bytes length |

	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^bytes asString! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 20:26:41'!
nextFloat32BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ])
			readStream nextFloat32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ])
			readStream nextFloat32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes float32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 20:28:46'!
nextFloat32Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ]) hex 'DB0F4940'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ]) hex '40490FDB'
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes float32At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/9/2025 11:59:30'!
nextFloat64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ])
			readStream nextFloat64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ])
			readStream nextFloat64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ (bytes float64At: 1 bigEndian: bigEndian)
		+ 0.0 "Convert to SmallFloat64 if appropriate"! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:23:04'!
nextFloat64Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes float64At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:34:51'!
nextInt16BigEndian: bigEndian
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) 
			readStream nextInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) 
			readStream nextInt16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes int16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:35:32'!
nextInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes int16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:36:17'!
nextInt32BigEndian: bigEndian
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) 
			readStream nextInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) 
			readStream nextInt32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes int32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:36:30'!
nextInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes int32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/8/2023 17:37:04'!
nextInt64BigEndian: bigEndian
	"Answer the next signed, 64-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: false ]) 
			readStream nextInt64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: true ]) 
			readStream nextInt64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ bytes int64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/8/2023 17:37:55'!
nextInt64Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 64-bit integer on this (binary) stream.

	(16r10000000000000000-1234567890123456789) hex '16rEEDDEF0B82167EEB' 

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt64Put: -1234567890123456789 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes int64At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:37:24'!
nextUint16BigEndian: bigEndian
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) 
			readStream nextUint16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) 
			readStream nextUint16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes uint16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:37:38'!
nextUint16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes uint16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:38:20'!
nextUint32BigEndian: bigEndian
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) 
			readStream nextUint32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) 
			readStream nextUint32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes uint32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/5/2023 17:38:44'!
nextUint32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes uint32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/8/2023 17:36:39'!
nextUint64BigEndian: bigEndian
	"Answer the next unsigned, 64-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: false ]) 
			readStream nextUint64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: true ]) 
			readStream nextUint64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ bytes uint64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' library: #MinimalKernel01 stamp: 'jmv 5/8/2023 17:36:03'!
nextUint64Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 64-bit integer on this (binary) stream.

	1234567890123456789 hex '16r112210F47DE98115' 

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint64Put: 1234567890123456789 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes uint64At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !


!PositionableStream methodsFor: 'positioning' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
position
	"Answer the current position of accessing the stream."
	^position! !

!PositionableStream methodsFor: 'positioning' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
position: anInteger 
	"Set position to anInteger as long as anInteger is within the bounds of the 
	receiver's contents.  If it is not, cause an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]! !

!PositionableStream methodsFor: 'positioning' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0! !

!PositionableStream methodsFor: 'private' library: #MinimalKernel01 stamp: 'jmv 5/22/2022 18:43:45'!
on: aCollection
	"Initialize the stream to read over all of aCollection.
	The collection is considered to have useful content."

	collection := aCollection.
	readLimit := aCollection size.
	position := 0.
	self reset.! !

!PositionableStream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'nice 11/22/2009 18:10'!
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	| newArray |
	newArray := self collectionSpecies new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray! !

!PositionableStream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 13:10'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do: [ :i |
		(obj := self next) ifNil: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		aCollection at: startIndex+i put: obj].
	^aCollection! !

!PositionableStream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ar 12/23/1999 14:53'!
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.! !

!PositionableStream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection! !

!PositionableStream methodsFor: 'mode' library: #MinimalKernel01 stamp: 'jmv 9/12/2023 12:30:15'!
useBytes
	collection class = ByteArray ifFalse: [
		self error: 'Can not #useBytes' ]! !


!PositionableStream class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection 
	"Answer a new instance of me, streaming over aCollection."

	^self basicNew on: aCollection! !


!ReadStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 09:59:57'!
next
	"Answer the next object in the Stream represented by the receiver."

	^position >= readLimit
		ifFalse: [collection at: (position := position + 1)]! !

!ReadStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'ls 8/16/1998 00:46'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
! !

!ReadStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'ar 12/23/1999 15:01'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]! !

!ReadStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'jmv 5/28/2022 17:16:40'!
nextPutAll: aCollection
	self shouldNotImplement! !


!WriteStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'jmv 5/31/2022 15:27:30'!
contents
	readLimit := readLimit max: position.
	^(collection copyFrom: 1 to: position) asStreamResult! !

!WriteStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
next
	self shouldNotImplement! !

!WriteStream methodsFor: 'reading' library: #MinimalKernel01 stamp: 'jmv 5/30/2022 10:57:04'!
next: n
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n! !

!WriteStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'jmv 5/30/2022 10:58:13'!
next: n into: aCollection startingAt: startIndex
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n into: aCollection startingAt: startIndex! !

!WriteStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 11:58:03'!
nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isCharacter and: [anObject codePoint > 255 and: [collection canHoldAnyCodePoint not]]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position := position + 1.
			^collection at: position put: anObject].! !

!WriteStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 11:55:05'!
nextPutAll: aCollection

	| newEnd |
	(aCollection canHoldAnyCodePoint and: [collection canHoldAnyCodePoint not]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	(self isCompatibleWithContents: aCollection)
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd := position + aCollection size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position := newEnd.! !

!WriteStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !

!WriteStream methodsFor: 'positioning' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
position: anInteger 
	readLimit := readLimit max: position.
	super position: anInteger! !

!WriteStream methodsFor: 'positioning' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
reset
	readLimit := readLimit max: position.
	position := 0! !

!WriteStream methodsFor: 'private' library: #MinimalKernel01 stamp: 'jmv 6/6/2022 10:33:40'!
isCompatibleWithContents: aCollection

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isByteString and: [ (collection is: #Text) or: [ collection is: #ArrayOfCharactersAndCodePoints ]])
		ifTrue: [ ^ true ].

	^ false! !

!WriteStream methodsFor: 'private' library: #MinimalKernel01 stamp: 'jmv 7/7/2022 10:44:06'!
on: aCollection
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless (most likely nil).
	Overwrite them.
	In the case of ReadWriteStream, for read operations, stream appears empty until stuff is added."

	super on: aCollection thatCanBeModified.
	readLimit := 0.
	writeLimit := aCollection size! !


!StdIOWriteStream methodsFor: 'streaming' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 10:19:51'!
flush
	"Flush pending changes"
	^self primFlush: fileID! !

!StdIOWriteStream methodsFor: 'streaming' library: #MinimalKernel01 stamp: 'jmv 4/21/2022 14:10:29'!
nextPut: aCharacter
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: (String with: aCharacter).
	^aCharacter! !

!StdIOWriteStream methodsFor: 'streaming' library: #MinimalKernel01 stamp: 'jmv 10/26/2022 15:55:49'!
nextPutAll: aStringOrUTF8Bytes
	"Write the given String  or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes := aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: 'Лорем ипсум долор сит амет, дебет салутанди меи еи. Ан хис граеце утамур долорум, лорем принципес молестиае дуо ин. Еи мовет ностро оптион нам, еос епицури омиттантур ут. Ут мунере лаборе интеллегебат усу, еа сед дебет луптатум, видит иллуд иудицо яуо ут.'; flush
"! !

!StdIOWriteStream methodsFor: 'character writing' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 10:48:18'!
newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter! !

!StdIOWriteStream methodsFor: 'character writing' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 11:08:02'!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !

!StdIOWriteStream methodsFor: 'open / close' library: #MinimalKernel01 stamp: 'jmv 9/16/2019 13:47:35'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID := aFileID.
	name := streamName.! !

!StdIOWriteStream methodsFor: 'primitives' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 10:20:17'!
primFlush: id
	"Flush pending changes to the disk"

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>! !

!StdIOWriteStream methodsFor: 'primitives' library: #MinimalKernel01 stamp: 'jmv 4/29/2025 17:01:32'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self error: 'StdIOWriteStream [', name, '] write failed'.! !

!StdIOWriteStream methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 09:35:56'!
printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !


!StdIOWriteStream class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 11:46:37'!
stdout
	StdOut ifNil: [
		StdOut := self basicNew.
		StdOut 
			openOnHandle: self stdoutHandle 
			name: 'stdout' ].
	^StdOut! !

!StdIOWriteStream class methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 09:26:09'!
stdioHandles
" answer handles: #(stdin stdout stderr) "
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed! !

!StdIOWriteStream class methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 11/17/2016 09:26:12'!
stdoutHandle

	^ self stdioHandles at: 2! !


!AdditionalMethodState methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 7/4/2023 16:01:43'!
isMethodProperties
	"AdditionalMethodState holds
		Pragmas
		Method Properties (in the form of Associations)
	if any CompiledMethod needs them."

	^true! !

!AdditionalMethodState methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'md 2/16/2006 17:50'!
selector
	^selector! !


!BlockClosure methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 5/24/2008 11:21'!
outerContext
	^outerContext! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'eem 10/29/2024 15:16:35'!
value
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 201>

	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'eem 10/29/2024 15:14:58'!
value: firstArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the argument and copied values to the activation
	 as its argument and copied temps. Primitive. Essential."
	<primitive: 202>
	
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'eem 10/29/2024 15:16:47'!
value: firstArg value: secondArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 203>

	numArgs ~= 2 ifTrue:
		[self numArgsError: 2].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'eem 10/29/2024 15:15:51'!
value: firstArg value: secondArg value: thirdArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 204>
	
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'eem 8/22/2008 14:21'!
valueNoContextSwitch
	"An exact copy of BlockClosure>>value except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 221>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed! !

!BlockClosure methodsFor: 'evaluating' library: #MinimalKernel01 stamp: 'jmv 8/29/2010 18:33'!
valueWithPossibleArgument: anArg 
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg"

	numArgs = 0 ifTrue: [ ^self value ].
	^self value: anArg! !

!BlockClosure methodsFor: 'exceptions' library: #MinimalKernel01 stamp: 'ar 12/4/2009 18:30'!
ensure: aBlock
	"Evaluate a termination block after evaluating the receiver, regardless of
	 whether the receiver's evaluation completes.  N.B.  This method is *not*
	 implemented as a primitive.  Primitive 198 always fails.  The VM uses prim
	 198 in a context's method as the mark for an ensure:/ifCurtailed: activation."

	| complete returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	complete ifNil:[
		complete := true.
		aBlock value.
	].
	^ returnValue! !

!BlockClosure methodsFor: 'printing' library: #MinimalKernel01 stamp: 'eem 5/24/2008 11:23'!
printOn: aStream
	aStream nextPutAll: '[closure] in '.
	outerContext printOn: aStream! !


!ContextPart methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 6/27/2008 15:55'!
at: index
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!ContextPart methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 6/27/2008 15:57'!
at: index put: value
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!ContextPart methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 6/27/2008 15:56'!
basicAt: index
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!ContextPart methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 6/27/2008 15:57'!
basicAt: index put: value
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!ContextPart methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 9/17/2014 17:13'!
basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed! !

!ContextPart methodsFor: 'debugger access' library: #MinimalKernel01 stamp: 'eem 11/27/2014 13:11'!
methodClass 
	"Answer the class in which the receiver's method was found."
	
	^self method methodClass ifNil: [self objectClass: self receiver].! !

!ContextPart methodsFor: 'debugger access' library: #MinimalKernel01 stamp: 'md 2/17/2006 18:47'!
selector
	"Answer the selector of the method that created the receiver."

	^self method selector ifNil: [self method defaultSelector].! !

!ContextPart methodsFor: 'debugger access' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
sender
	"Answer the context that sent the message that created the receiver."

	^sender! !

!ContextPart methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/4/2023 18:56:01'!
printOn: aStream 
	| selector class mclass |
	self method
		ifNil: [^ super printOn: aStream].
	class := self receiver class.
	mclass := self methodClass.
	selector := self selector ifNil: [ self method defaultSelector].
	class printNameOn: aStream.
	mclass == class 
		ifFalse: [
			aStream nextPut: $(.
			mclass printNameOn: aStream.
			aStream nextPut: $)].
	aStream nextPutAll: '>>'.
	aStream nextPutAll: selector.
	selector == #doesNotUnderstand: ifTrue: [
		aStream space.
		(self tempAt: 1) selector printOn: aStream.
	].
! !


!MethodContext methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
method
	^method! !

!MethodContext methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 3/1/2010 11:40'!
outerContext
	"Answer the context within which the receiver is nested."

	^ closureOrNil
		ifNotNil: [closureOrNil outerContext]! !

!MethodContext methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
receiver
	^receiver! !

!MethodContext methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'eem 8/20/2008 09:28'!
tempAt: index 
	"Answer the value of the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at: primitive to give latitude to the
	 VM in context management."

	<primitive: 210>
	^self at: index! !

!MethodContext methodsFor: 'printing' library: #MinimalKernel01 stamp: 'eem 5/27/2008 17:23'!
printOn: aStream

	self outerContext
		ifNil: [super printOn: aStream]
		ifNotNil:
			[:outerContext|
			 aStream nextPutAll: '[] in '.
			 outerContext printOn: aStream]! !


!Message methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
selector
	"Answer the selector of the receiver."
	^selector! !


!Boolean methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
& aBoolean 
	"Evaluating conjunction -- Evaluate the argument.  Then answer true if both the
	receiver and the argument are true."
	self subclassResponsibility! !

!Boolean methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- Evaluate the argument.  Then answer true if
	either the receiver or the argument is true."
	self subclassResponsibility! !

!Boolean methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation-- answer true if the receiver is false, answer false if the receiver is true."
	self subclassResponsibility! !

!Boolean methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jpb 8/3/2019 00:11:00'!
is: aSymbol
	^#Boolean = aSymbol or: [ super is: aSymbol ]! !

!Boolean methodsFor: 'copying' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."! !


!False methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer false since receiver is false."

	^self! !

!False methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean! !

!False methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation -- answer true since the receiver is false."

	^true! !

!False methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	"Print false."
	aStream nextPutAll: 'false'! !


!True methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer alternativeObject since receiver is true."

	^alternativeObject! !

!True methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self! !

!True methodsFor: 'logical operations' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation--answer false since the receiver is true."

	^false! !

!True methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'true'! !


!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
< aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
<= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than or equal to the argument. Otherwise answer false."

	^(self > aMagnitude) not! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
> aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is greater than the argument. Otherwise answer false."

	^aMagnitude < self! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'nice 12/31/2008 04:06'!
>= aMagnitude 
	"Answer whether the receiver is greater than or equal to the argument."

	^aMagnitude <= self! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max,
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
hash
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
max: aMagnitude 
	"Answer the receiver or the argument, whichever has the greater magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !

!Magnitude methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
min: aMagnitude 
	"Answer the receiver or the argument, whichever has the lesser magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !


!Number methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'jmv 11/11/2014 10:23'!
// aNumber
	"Integer division with truncation toward negative infinity. 
	9//4 = 2
	-9//4 = -3
	-0.9//0.4 = -3
	#\\ answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) floor! !

!Number methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'jmv 5/12/2022 16:59:55'!
\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\ -4 = -3
	0.9\\0.4 = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - (self // divisor * divisor)

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"! !

!Number methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
abs
	"Answer a Number that is the absolute value (positive magnitude) of the receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]! !

!Number methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
negated
	"Answer a Number that is the negation of the receiver."
	^0 - self! !

!Number methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 11:41:36'!
= aNumber
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility! !

!Number methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 11:41:27'!
hash
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility! !

!Number methodsFor: 'mathematical functions' library: #MinimalKernel01 stamp: 'jmv 8/2/2019 11:47:20'!
raisedToInteger: exponent
	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	Maybe further discussion is required on this topic."
	
	| bitProbe result |

	exponent negative ifTrue: [^self raisedToNegativeInteger: exponent ].
	exponent = 0 ifTrue: [^ self class one].
	exponent = 1 ifTrue: [^ self].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 	[
		(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
		bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
			whileTrue: [
				result := result * result].
	^result! !

!Number methodsFor: 'truncation and round off' library: #MinimalKernel01 stamp: 'wiz 5/29/2011 17:54'!
ceiling
	"Answer the integer nearest the receiver toward  infinity."

	| truncation |
	truncation := self truncated.
	self <= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation + 1]! !

!Number methodsFor: 'truncation and round off' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation := self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]! !

!Number methodsFor: 'converting' library: #MinimalKernel01 stamp: 'di 11/6/1998 13:44'!
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with a Integer, convert us and evaluate exprBlock."
	^ self subclassResponsibility! !

!Number methodsFor: 'converting' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
asInteger
	"Answer an integer nearest the receiver toward zero."
	^self truncated! !

!Number methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jpb 8/2/2019 23:58:01'!
is: aSymbol
	"Note: Senders might prefer #isNumber for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Number = aSymbol or: [ super is: aSymbol]! !

!Number methodsFor: 'testing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isNumber
	^ true! !

!Number methodsFor: 'testing' library: #MinimalKernel01 stamp:''!
isZero
	^self = 0! !

!Number methodsFor: 'testing' library: #MinimalKernel01 stamp: 'di 4/23/1998 11:18'!
negative
	"Answer whether the receiver is mathematically negative."

	^ self < 0! !

!Number methodsFor: 'intervals' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
to: stop do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue := self.
	[nextValue <= stop]
		whileTrue: 
			[aBlock value: nextValue.
			nextValue := nextValue + 1]! !

!Number methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 1/5/2014 19:55'!
printString

	^String streamContents: [:str | self printOn: str base: 10]! !


!Number class methodsFor: 'constants' library: #MinimalKernel01 stamp: 'GabrielOmarCotelli 5/23/2009 20:46'!
one

	^1! !


!Integer methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'jmv 12/24/2011 08:43'!
* aNumber
	"Refer to the comment in Number * " 
	aNumber isInteger ifTrue: [
		^ self
			digitMultiply: aNumber
			neg: (self negative xor: aNumber negative) ].
	^ aNumber adaptToInteger: self andSend: #*! !

!Integer methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'di 11/6/1998 13:59'!
+ aNumber
	"Refer to the comment in Number + "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ (self digitAdd: aNumber) normalize]
			ifFalse: [^ self digitSubtract: aNumber]].
	^ aNumber adaptToInteger: self andSend: #+! !

!Integer methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'di 11/6/1998 13:59'!
- aNumber
	"Refer to the comment in Number - "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ self digitSubtract: aNumber]
			ifFalse: [^ (self digitAdd: aNumber) normalize]].
	^ aNumber adaptToInteger: self andSend: #-! !

!Integer methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'jmv 8/1/2019 17:26:46'!
// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #// argument: aNumber].
	aNumber isInteger ifFalse: [ ^super // aNumber ].
	self = 0 ifTrue: [^ 0].
	q := self quo: aNumber.
	"Refer to the comment in Number>>#//."
	^(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
			ifTrue: [q - 1"Truncate towards minus infinity."]
			ifFalse: [q]! !

!Integer methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'sqr 1/26/2014 13:38'!
highBit
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to 
	get the highest bit of the magnitude."
	self < 0 ifTrue: [^ self error: 'highBit is not defined for negative integers'].
	^ self highBitOfMagnitude! !

!Integer methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'jmv 12/4/2018 11:59:58'!
highBitOfMagnitude
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Receiver has to be positive!!"

	| shifted bitNo |
	shifted := self < 0 ifTrue: [0 - self] ifFalse: [self].
	bitNo := 0.
	[shifted < 65536]
		whileFalse: 
			[shifted := shifted bitShift: -16.
			bitNo := bitNo + 16].
	shifted < 256
		ifFalse: 
			[shifted := shifted bitShift: -8.
			bitNo := bitNo + 8].
		
	"The high bits table can be obtained with:
	(1 to: 8) inject: #[0] into: [:highBits :rank | highBits , (highBits collect: [:e | rank])]."
	^bitNo + ( #[0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] at: shifted + 1)! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'di 11/6/1998 14:00'!
< aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) > 0]
						ifFalse: [^ (self digitCompare: aNumber) < 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andSend: #<! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 9/1/2010 10:19'!
<= aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) >= 0]
						ifFalse: [^ (self digitCompare: aNumber) <= 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andSend: #<=! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 5/27/2014 11:04'!
= aNumber

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue: [
		aNumber class == self class ifFalse: [ ^false ].
		^ (self digitCompare: aNumber) = 0].
	^ aNumber adaptToInteger: self andSend: #=! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'di 11/6/1998 14:00'!
> aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) < 0]
						ifFalse: [^(self digitCompare: aNumber) > 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andSend: #>! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 9/1/2010 10:19'!
>= aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) <= 0]
						ifFalse: [^(self digitCompare: aNumber) >= 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andSend: #>=! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 1/15/2025 09:57:42'!
hash
	"Hash is reimplemented because = is implemented.
	Siimply answering self as #hash has bad performance for the following:
	| s |
	s := (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images...
	See Float>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply! !

!Integer methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 1/15/2025 09:56:58'!
hashMultiply
	self subclassResponsibility! !

!Integer methodsFor: 'converting' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
asInteger
	"Answer with the receiver itself."

	^self! !

!Integer methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jpb 8/2/2019 23:58:43'!
is: aSymbol
	"Note: Senders might prefer #isInteger for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Integer = aSymbol or: [ super is: aSymbol ]! !

!Integer methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
isInteger
	^true! !

!Integer methodsFor: 'truncation and round off' library: #MinimalKernel01 stamp: 'sqr 12/21/2013 17:55'!
ceiling 
	"Refer to the comment in Number|ceiling."

	^self! !

!Integer methodsFor: 'truncation and round off' library: #MinimalKernel01 stamp: 'sqr 12/21/2013 17:56'!
floor 
	"Refer to the comment in Number|floor."

	^self! !


!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'di 2/1/1999 21:29'!
* aNumber 
	"Primitive. Multiply the receiver by the argument and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 9>
	^ super * aNumber! !

!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'di 2/1/1999 21:31'!
+ aNumber 
	"Primitive. Add the receiver to the argument and answer with the result
	if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 1>
	^ super + aNumber! !

!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
- aNumber 
	"Primitive. Subtract the argument from the receiver and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation
	whatIsAPrimitive."

	<primitive: 2>
	^super - aNumber! !

!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'tk 11/30/2001 11:55'!
// aNumber 
	"Primitive. Divide the receiver by the argument and answer with the
	result. Round the result down towards negative infinity to make it a
	whole integer. Fail if the argument is 0 or is not a SmallInteger.
	Essential. No Lookup. See Object documentation whatIsAPrimitive. "

	<primitive: 12>
	^ super // aNumber 	"Do with quo: if primitive fails"! !

!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'tk 11/30/2001 11:53'!
\\ aNumber 
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided by
	the argument Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 11>
	^ super \\ aNumber 	"will use // to compute it if primitive fails"! !

!SmallInteger methodsFor: 'arithmetic' library: #MinimalKernel01 stamp: 'jmv 6/25/2019 18:01:48'!
quo: aNumber 
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards zero to make it a whole integer. 
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object 
	documentation whatIsAPrimitive."
	<primitive: 13>
	aNumber = 0 ifTrue: [^ZeroDivide new signalReceiver: self selector: #quo: argument: aNumber ].
	(aNumber isMemberOf: SmallInteger)
		ifFalse: [^ super quo: aNumber].
	(aNumber = -1 and: [self = self class minVal])
		ifTrue: ["result is aLargeInteger" ^ self negated].
	self primitiveFailed! !

!SmallInteger methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'jmv 4/24/2016 01:40'!
bitAnd: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	self >= 0 ifTrue: [^ arg bitAnd: self].
	^ arg < 0
		ifTrue: [(arg bitInvert bitOr: self bitInvert) bitInvert]
		ifFalse: [arg bitClear: self bitInvert]! !

!SmallInteger methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'di 4/30/1998 10:33'!
bitOr: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 15>
	self >= 0 ifTrue: [^ arg bitOr: self].
	^ arg < 0
		ifTrue: [(self bitInvert bitAnd: arg bitInvert) bitInvert]
		ifFalse: [(self bitInvert bitClear: arg) bitInvert]! !

!SmallInteger methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'mir 9/25/2008 15:18'!
bitShift: arg 
	"Primitive. Answer an Integer whose value is the receiver's value shifted
	left by the number of bits indicated by the argument. Negative arguments
	shift right. The receiver is interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 17>
	self >= 0 ifTrue: [^ super bitShift: arg].
	^ arg >= 0
		ifTrue: [(self negated bitShift: arg) negated]
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].! !

!SmallInteger methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'mir 9/25/2008 15:18'!
bitXor: arg 
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 16>
	self >= 0 ifTrue: [^ arg bitXor: self].
	^ arg < 0
		ifTrue: [self bitInvert bitXor: arg bitInvert]
		ifFalse: [(self bitInvert bitXor: arg) bitInvert].! !

!SmallInteger methodsFor: 'bit manipulation' library: #MinimalKernel01 stamp: 'sqr 1/26/2014 13:48'!
lowBit
	" Answer the index of the low order one bit.
		2r00101000 lowBit       (Answers: 4)
		2r-00101000 lowBit      (Answers: 4)"

	self = 0 ifTrue: [^0].
	^(self bitXor: self - 1) highBit! !

!SmallInteger methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
even
	^(self bitAnd: 1) = 0! !

!SmallInteger methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ul 4/2/2016 20:19'!
decimalDigitLength
	"Answer the number of digits printed out in base 10.
	 Note that this only works for positive SmallIntegers up to 63-bits."
	
	self <= 99999999 "8" ifTrue: [
		self <= 9999 "4" ifTrue: [
			self <= 99 "2" ifTrue: [
				self <= 9 "1" ifTrue: [ ^1].
				^2 ].
			self <= 999 "3" ifTrue: [ ^3].
			^4 ].
		self <= 999999 "6" ifTrue: [
			self <= 99999 "5" ifTrue: [ ^5 ].
			^6 ].
		self <= 9999999 "7" ifTrue: [ ^7 ].
		^8 ].
	self <= 1073741823 "10" ifTrue: [ "This is here only to avoid LargeInteger comparisons in 32-bit VMs"
		self <= 999999999 "9" ifTrue: [ ^9 ].
		^10 ].
	self <= 999999999999999 "15" ifTrue: [
		self <= 9999999999999 "13" ifTrue: [
			self <= 99999999999 "11" ifTrue: [
				self <= 9999999999 "10" ifTrue: [ ^10 ].
				^11 ].
			self <= 999999999999 "12" ifTrue: [ ^12 ].
			^13 ].
		self <= 99999999999999 "14" ifTrue: [ ^14 ].
		^15 ].
	self <= 99999999999999999 "17" ifTrue: [
		self <= 9999999999999999 "16" ifTrue: [ ^16 ].
		^17 ].
	self <= 999999999999999999 "18" ifTrue: [ ^18 ].
	^19 "(1 << 60 - 1) asString size"! !

!SmallInteger methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 7/6/2023 11:28:41'!
printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len := self decimalDigitLength.
	result := ByteArray new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result at: i put: 48 + (integer - (next * 10)).
		integer := next ].
	^result asString! !

!SmallInteger methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'tk 5/14/1999 20:54'!
nextInstance
	"SmallIntegers can't be enumerated this way.  There are a finite number of them from from (SmallInteger minVal) to (SmallInteger maxVal), but you'll have to enumerate them yourself with:
	(SmallInteger minVal) to: (SmallInteger maxVal) do: [:integer | <your code here>].
	"

	self shouldNotImplement ! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
< aNumber 
	"Compare the receiver with the argument and answer with true if the receiver is less 
	than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
<= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is less 
	than or equal to the argument.  Otherwise answer false.  Fail if the argument is 
	not a SmallInteger.  Optional.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
> aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
>= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than or equal to the argument.  Otherwise answer false.  Fail if the 
	argument is not a SmallInteger.  Optional.  No Lookup.  See Object 
	documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
~= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is not 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 8>
	^super ~= aNumber! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'SqR 8/3/2000 13:29'!
hashMultiply
	"Multiply by 1664525, take lower 28 bits, do not use LargeIntegers (not even in 32 bit images)"
	| low |

	low := self bitAnd: 16383.
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))
			bitAnd: 16r0FFFFFFF! !

!SmallInteger methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'Squeak1.2 6/29/1997 10:40:55'!
identityHash

	^self! !

!SmallInteger methodsFor: 'copying' library: #MinimalKernel01 stamp: 'sqr 12/21/2013 17:49'!
shallowCopy

	^self! !


!LookupKey methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
key
	"Answer the lookup key of the receiver."
	^key! !

!LookupKey methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
key: anObject 
	"Store the argument, anObject, as the lookup key of the receiver."
	key := anObject! !

!LookupKey methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
< aLookupKey 
	^key < aLookupKey key! !

!LookupKey methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 8/9/2016 10:01:30'!
= aLookupKey

	self == aLookupKey ifTrue: [ ^ true ].
	self species == aLookupKey species
		ifFalse: [ ^false ].

	^ key = aLookupKey key! !

!LookupKey methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'Squeak1.0 9/20/1996 10:50:05'!
hash
	"Hash is reimplemented because = is implemented."

	^key hash! !

!LookupKey methodsFor: 'printing' library: #MinimalKernel01 stamp: 'jmv 9/2/2012 21:43'!
printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPut: $)! !


!LookupKey class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
key: aKey 
	"Answer a new instance of me with the argument as the lookup up."
	^self new key: aKey! !


!Association methodsFor: 'printing' library: #MinimalKernel01 stamp: 'sqr 12/20/2013 18:24'!
printOn: aStream

	aStream
		print: self key;
		nextPutAll: ' -> ';
		print: self value! !

!Association methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	value := anObject! !

!Association methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
value
	"Answer the value of the receiver."
	^value! !

!Association methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."
	value := anObject! !

!Association methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 8/9/2016 10:01:44'!
= anAssociation

	self == anAssociation ifTrue: [ ^ true ].
	^ super = anAssociation and: [value = anAssociation value]! !

!Association methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 6/11/2019 21:18:46'!
hash
	"Hash is reimplemented because = is implemented."

	^ key hash! !


!Association class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
key: newKey value: newValue
	"Answer a new instance of me with the arguments as the key and
	value of the association."
	^(super key: newKey) value: newValue! !


!UndefinedObject methodsFor: 'printing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
isNil
	^true! !

!UndefinedObject methodsFor: 'testing' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
notNil
	^false! !

!UndefinedObject methodsFor: 'copying' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy
	"Only one instance of UndefinedObject should ever be made, so answer 
	with self."! !


!Character methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 4/29/2025 10:41:03'!
hash
	"Hash is reimplemented because #hash for long Strings relies on it, and especially on being the same as the codePoint's for the ASCII range. Also see the comment at Integer>>#hash.
	| longString |
	longString := (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars!! ' ].
	self assert: longString asByteString hash = longString asUnicodeString hash.
	self assert: longString asByteString = longString asUnicodeString.
	"
	^self codePoint hash! !

!Character methodsFor: 'comparing' library: #MinimalKernel01 stamp: 'jmv 9/25/2023 16:46:30'!
identityHash
	"Answer the receiver's character code."
	<primitive: 171>
	^self primitiveFailed! !

!Character methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 6/6/2022 10:40:24'!
isCharacter
	^true! !

!Character methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 12:37:08'!
isDigit
	"Answer whether the receiver is a digit."

	^ self codePoint between: 48 and: 57! !

!Character methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 16:55:13'!
isLetter
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLetter
	(Character codePoint: 16r042F) isLetter
	(Character codePoint: 16r0431) isLetter
	"
	^self isLowercase or: [ self isUppercase ]! !

!Character methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 5/6/2025 14:39:55'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLowercase
	(Character codePoint: 16r042F) isLowercase
	(Character codePoint: 16r0431) isLowercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	LowercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 97 and: 122 ].
		
	i := self codePoint + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.! !

!Character methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jmv 5/6/2025 14:40:28'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(Character codePoint: 16r22C7) isUppercase
	(Character codePoint: 16r042F) isUppercase
	(Character codePoint: 16r0431) isUppercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	UppercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 65 and: 90 ].

	i := self codePoint + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.! !

!Character methodsFor: 'accesing' library: #MinimalKernel01 stamp: 'jmv 12/1/2024 11:32:58'!
codePoint
	"Code points are the numbers assigned by the Unicode Consortium to every character in every writing system."
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $€ codePoint hex = '16r20AC'.
	"
	<primitive: 171>
	^self primitiveFailed! !

!Character methodsFor: 'copying' library: #MinimalKernel01 stamp: 'eem 2/5/2014 12:53'!
shallowCopy
	"Answer the receiver, because Characters are unique."
	^self! !


!Character class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 4/30/2025 12:42:37'!
binarySelectorCharacters
	^ '!!%&+-*/\,<=>?@~'! !

!Character class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 12:13:35'!
codePoint: anInteger
	"Answer the Character whose value is anInteger.
	
	self assert: (Character codePoint: 16r41) == $A.
	self assert: (Character codePoint: 16r20AC) == $€.
	"

	<primitive: 170>
	^self primitiveFailed! !

!Character class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'jmv 1/8/2024 12:16:02'!
digitValue: x 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self codePoint: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])! !

!Character class methodsFor: 'instance creation' library: #MinimalKernel01 stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Creating new characters is not allowed."

	self error: 'cannot create new characters'! !

!Character class methodsFor: 'accessing untypeable characters' library: #MinimalKernel01 stamp: 'jmv 5/17/2024 09:11:50'!
lf
	"Answer the Character representing a linefeed."

	^`Character codePoint: 10`! !

!Character class methodsFor: 'accessing untypeable characters' library: #MinimalKernel01 stamp: 'jmv 8/28/2013 10:28'!
newLineCharacter
	"Answer the Character representing a newLine, that is, a linefeed.
	This should be the only method in the system that knows about this detail.
	Everybody else should eventually ask us."

	^self lf! !

!Character class methodsFor: 'accessing untypeable characters' library: #MinimalKernel01 stamp: 'jmv 5/17/2024 09:15:46'!
space
	"Answer the Character representing a space."

	^ `Character codePoint: 32`! !


!Point methodsFor: 'copying' library: #MinimalKernel01 stamp: 'pb 10/29/2016 18:18:07'!
shallowCopy
	"Immutable"
	^ self.! !


!ProtoObject reorganize!
('[mk] initialization')
('[mk] testing')
('[mk] comparing')
('[mk] printing')
('[mk] error handling')
('[mk] system primitives')
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isNil isObject notNil)
('comparing' == ~~ identityHash scaledIdentityHash)
('system primitives' become: cannotInterpret: nextInstance nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryPrimitive:withArgs:)
('tracing' pointsTo:)
('initialization' initialize)
('printing' printOn:)
('error handling' doesNotUnderstand: error: warning:)
!


!Object reorganize!
('[mkObjectSerialization] object serialization' comeFullyUpOnReload: readDataFrom:size:)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
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
('object serialization' convertToCurrentVersion:refStream: objectForDataStream: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeString isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand flash runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
('associating' ->)
!


!Behavior reorganize!
('[mk] accessing')
('[mk] testing')
('[mk] printing')
('[mk] instance creation')
('[mk] class hierarchy')
('[mk] instances')
('[mk] enumerating')
('[mk] comparing')
('[mk] constants')
('[mkClassAndMethodCreation] initialization' superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' binding bindingOf: classBindingOf:)
('[mkClassAndMethodCreation] method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict methodDict:)
('[mkClassAndMethodCreation] private' flushCache)
('[mkClassAndMethodCreation] instances and variables' maxAccessibleInstSize)
('[mkClassAndMethodCreation] copying' postCopy)
('[mkClassAndMethodCreation] testing class hierarchy' includesBehavior: inheritsFrom:)
('accessing' classDepth decompilerClass definesInstanceVariableNamed: format instSize instSpec instVarNames name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteClassesCollection removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferExplicitlyTo: addMethodsTo:thatReferTo: addTo:methodsThatReferInHierarchyTo: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: stampAt:)
('system-support' allCallsOn)
('release' obsolete)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
('class hierarchy' deepSubclassNamed: superclass)
('instances' allInstancesOrNil someInstance)
('comparing' identityHash)
('constants' maxInstSize)
!


!ClassDescription reorganize!
('[mk] accessing')
('[mk] printing')
('[mkClassAndMethodCreation] initialization' organization: setInstVarNames: superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' localBindingOf:)
('accessing' classVersion comment comment: comment:stamp: hasComment instVarNames smalltalkComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inCategory:methodTimeStamp:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:libraryName:notifying: compile:classified:withStamp:libraryName:notifying:logSource: compile:classified:withStamp:notifying: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:library:stamp: methodsFor:library:stamp:prior: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on:withStamp:libraryName:priorMethod: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying: newInstanceFrom:variable:size:map:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!Class reorganize!
('[mk] accessing')
('[mk] accesing class hierarchy')
('[mk] printing')
('[mk] class variables')
('[mk] pool variables')
('[mkClassAndMethodCreation] initialization' addSubclass: declareClassVariables: setName: superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' binding bindingOf: classVarBindingOf: localBindingOf:)
('[mkClassAndMethodCreation] copying' copy)
('testing' hasMethods isObsolete)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classPool classPool: classPoolFrom: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' compileAllFrom: possibleVariablesFor:continuedFrom:)
('subclass creation' ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('subclass creation-convenience' subclass:instanceVariableNames:classVariableNames:poolDictionaries:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutAlphabetically fileOutHierarchy fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' hasSubclasses removeSubclass: subclasses superclass:)
('private' spaceUsed)
('initialization' declareClassVarString: declareSharedPoolString: declareSharedPools: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('SUnit tests' testCaseClass)
('enumerating' allSubclassesSortedDo: subclassesSortedDo:)
('accessing' name)
('accesing class hierarchy' subclassesDo:)
('printing' printNameOn:)
!


!Metaclass reorganize!
('[mk] accessing')
('[mk] testing')
('[mk] class hierarchy')
('[mk] printing')
('[mk] class variables')
('[mkClassAndMethodCreation] instance creation' new)
('[mkClassAndMethodCreation] accessing' bindingOf: localBindingOf:)
('[mkClassAndMethodCreation] copying' postCopy)
('accessing' allClassInstVarNames allInstances category name soleInstance theMetaClass theNonMetaClass)
('instance variables' addInstVarName: removeInstVarName:)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo:)
('compiling' acceptsLoggingOfCompilation possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isMeta isObsolete)
('enumerating' allInstancesDo:)
('private')
('initialization' adoptInstance:from: instanceVariableNames:)
('SUnit tests' testCaseClass)
('accessing variables' allClassVarNames)
('printing' printNameOn:)
('class variables' classPool)
!


!Collection reorganize!
('[mk] accessing')
('[mk] enumerating')
('[mk] testing')
('[mk] comparing')
('[mk] private')
('[mkHashedCollections] enumerating' associationsDo:)
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: reject:thenCollect: reject:thenDo: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('inspecting' hasContentsInExplorer)
('comparing' = emptyCollectionHash hash)
!


!SequenceableCollection reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] converting')
('[mk] copying')
('[mk] enumerating')
('[mk] testing')
('[mk] stream creation')
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('converting' @ asByteArray asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' , copyAfter: copyAfterLast: copyBlank: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: grownTo: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('inspecting' customizeExplorerContents explorerContents)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds: isSequenceable)
('gui' do:displayingProgress:)
('math functions' += -=)
('assertions' assertIsInBounds:)
('sorting' sorted:)
('comparing' = hasEqualElements: hash hashQuick hashQuick: is:subcollectionAt:)
('stream creation' asStreamResult readStream thatCanBeModified writeStream)
!


!SequenceableCollection class reorganize!
('[mk] stream creation')
('stream creation' streamContents: streamContents:estimatedSize: streamContents:limitedTo: streamContents:on: writeStream)
!


!ArrayedCollection reorganize!
('[mk] accessing')
('adding' add:)
('printing' printNameOn: storeOn:)
('private' defaultElement storeElementsFrom:to:on:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('objects from disk' byteSize restoreEndianness writeOn:)
('inserting' insert:shiftingRightAt:)
('testing' isEmpty)
('inspecting' explorerContents)
('math functions' derivative incrementFraction integral)
('accessing' size)
!


!ArrayedCollection class reorganize!
('[mk] instance creation')
('instance creation' instancesMustBeOfSpecificSize new new: new:from: new:withAll: newFrom: newFromStream: numElements with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: with:withAll: withAll: withAll:with:)
('stream creation' writeStream)
('aux' bytesPerBasicElement)
!


!Array reorganize!
('[mk] system prinitives')
('[mk] testing')
('enumerating' collect:thenSelect: select:thenCollect:)
('comparing' literalEqual:)
('converting' asArray elementsForwardIdentityTo: elementsForwardIdentityTo:copyHash: evalStrings)
('printing' isLiteral printAsBraceFormOn: printAsLiteralOn: printOn: storeAsBraceFormOn: storeOn:)
('private' hasLiteralSuchThat:)
('accessing' atWrap: atWrap:put:)
('testing' identityIncludes: is: isArray shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('literals' allLiteralsDo:)
('system prinitives' elementsExchangeIdentityWith: replaceFrom:to:with:startingAt:)
!


!ByteArray reorganize!
('[mk] access - Numeric Types')
('[mk] accessing')
('[mk] converting')
('[mk] testing')
('[mk] comparing')
('[mk] system primitives')
('accessing' atAllPut: bitSize indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent:)
('converting' asByteArray asHex asString asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' is: isLiteral)
('initialization' readHexFrom:)
('access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At:bigEndian: int8At:put:bigEndian: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At:bigEndian: uint8At:put:bigEndian:)
('comparing' = hash)
('system primitives' replaceFrom:to:with:startingAt:)
!


!ByteArray class reorganize!
('[mk] byte based hash')
('[mk] services')
('instance creation' newBits: newFrom: readHexFrom:)
('stream creation' streamUtf8Contents:)
('byte based hash' hashBytes:startingWith: minSizeForHashQuick)
('services' findFirstInBytes:inSet:startingAt: indexOfByte:inBytes:startingAt:)
!


!CompiledMethod reorganize!
('[mk] accessing')
('[mk] literals')
('[mk] printing')
('[mk] converting')
('[mk] testing')
('[mk] private')
('[mkObjectSerialization] object serialization' readDataFrom:size:)
('[mkClassAndMethodCreation] accessing' endPC initialPC methodClass:)
('[mkClassAndMethodCreation] literals' literalAt:put: objectAt:put:)
('[mkClassAndMethodCreation] private' flushCache penultimateLiteral: voidCogVMState)
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass flag frameSize methodClass methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: setSignFlag signFlag trailer withPropertiesDo:ifSelector:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: storeDataOn: zapSourcePointer)
('initialization' copyWithBytecodes: copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: header headerDescription implicitLiteralsDo: indexOfLiteral: literalAt: literalStrings literals literalsDo: objectAt: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasLiteralExplicitly: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' penultimateLiteral)
!


!Semaphore reorganize!
('[mk] communication')
('[mk] mutual exclusion')
('[mk] comparing')
('[mk] initialize-release')
('[mk] private')
('communication' signal wait waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('testing' isSignaled)
('initialization' terminateProcess)
('printing' printOn:)
('comparing' = hash)
('initialize-release' initSignals)
('private' species)
!


!Semaphore class reorganize!
('[mk] instance creation')
('instance creation' forMutualExclusion new)
!


!CharacterSequence reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] printing')
('[mk] testing')
('[mkSymbol] converting' asSymbol)
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= = > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: hash is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn: printOn: storeOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators isString)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
!


!CharacterSequence class reorganize!
('[mk] instance creation')
('[mkSymbol] testing' representsSymbols)
('instance creation' fromUtf8Bytes: new new:withAll: numElements readFrom:)
('fileman-constants' pathSeparators)
!


!String reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] copying')
('[mk] converting')
('[mk] testing')
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: replaceFrom:to:with:startingAt: size utf8BytesSize)
('comparing' < = caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString: copyReplaceFrom:to:with: copyWith:)
('converting' asByteString asByteStringIfAscii asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldAnyCodePoint translateToLowercase translateToUppercase uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldAnyCodePoint is: isAscii isByteString isEmpty isInCanonicalCompositionForm isIntegerDenotation)
('paragraph support' encompassLine: encompassParagraph:)
!


!String class reorganize!
('[mk] instance creation')
('[mk] services')
('[mk] static state')
('[mkSymbol] system' symbolClassToUse)
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new: new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with: with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compare:with:collated: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: is:equalTo: isAscii: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder caseSensitiveOrder lowercasingTable tokenishTable uppercasingTable)
!


!Symbol reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] testing')
('[mk] printing')
('[mk] copying')
('[mk] private')
('[mkSymbol] converting' asSymbol)
('[mkSymbol] private' errorNoModification flushCache string:)
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
('comparing' =)
('copying' shallowCopy)
('private' species)
!


!Symbol class reorganize!
('[mk] services')
('[mkSymbol] instance creation' intern: lookup:)
('[mkSymbol] class initialization' initialize)
('[mkSymbol] services' estimatedCount)
('[mkSymbol] testing' representsSymbols)
('[mkSymbol] private' accessProtect basicFromString:)
('access' allSymbolTablesDo: allSymbols allSymbolsSelect: findInterned: selectorsContaining: selectorsMatching: symbolCount thatStartsCaseSensitive:)
('instance creation' newFrom: readFrom:)
('private' rehash)
('services' hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: isLiteralSymbol: possibleSelectorsFor:)
!


!SystemDictionary reorganize!
('[mk] vm parameters')
('[mk] snapshot and quit')
('[mk] printing')
('[mkHashedCollections] globals' at:put: flushClassNameCache)
('[mkHashedCollections] system attributes' maxIdentityHash)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' cachedSystemAttribute: getSystemAttribute: listBuiltinModule: listLoadedModule: platformName primGetSystemAttribute:)
('[mkVMnImage] special objects' specialObjectsArray)
('[mkClassAndMethodCreation] miscelaneous' voidCogVMState)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 undeclaredReferences verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allImplementorsOf:subTo: allImplementorsOf:superTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithSourceString:matchCase:scopeClass:includeSubclasses:includeSuperclasses: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSelect:localTo: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs: saveAs:clearAllClassState: saveImage saveRollingReleaseAndQuit saveStableRelease send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing - main' browse browseFull: browseFull:selector: browseHierarchy: browseHierarchy:selector: browserWindowClass findAndBrowseClass findInSourceCode)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllClasses browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOf:subTo: browseAllImplementorsOf:superTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:localTo: browseAllSelect:name:autoHighlight: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToNumericConstants browseEqEqSentToSmallIntegerConstants browseEqSentToCharacterConstants browseEqSentToNumericConstants browseEqSentToSmallIntegerConstants browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithSourceString:in: browseMethodsWithSourceString:scopeClass:includeSubclasses:includeSuperclasses: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive)
('Closure measurements' browseMethodsWithEmptyClosures)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' checkIfAlreadyRunningOrStoppedNoExit displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments: processFinalCommandLineOption:optionArgument:raiseExceptions: processInitialCommandLineOption:optionArgument: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - recover user changes' warnAndOfferLastUserChanges)
('startup - restore lost changes' isQuitNoSaveRecord: isQuitRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put: wordSize)
!


!Stream reorganize!
('[mk] reading')
('[mk] writing')
('[mk] accessing')
('[mk] normalized access')
('[mk] nonhomogeneous accessing')
('[mk] testing')
('reading' basicNext contents next next: readOnly)
('writing' flush next:put: nextPut: nextPut:when: nextPutAll: nextPutAll:asCommaSeparated: nextPutAll:when: print:)
('accessing' binary localName nextMatchAll: openReadOnly)
('testing' atEnd closed is: isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print:fractionDigits: print:when: printOn:)
('FileStream compatibility' close)
('nonhomogeneous accessing' nextNumber nextString nextStringPut: nextUnicodeString nextUnicodeStringPut:)
('normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
!


!PositionableStream reorganize!
('[mk] accessing')
('[mk] positioning')
('[mk] private')
('[mk] mode')
('reading' back contents contentsOfEntireFile crLfNextLine next:into: nextAvailable: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('writing' next:putAll: next:putAll:startingAt:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('gui' untilEnd:displayingProgress:)
('accessing' next: next:into:startingAt: nextInto: originalContents)
('mode' useBytes)
!


!PositionableStream class reorganize!
('[mk] instance creation')
('instance creation' on:)
!


!ReadStream reorganize!
('[mk] reading')
('[mk] writing')
('reading' next next: next:into:startingAt: readInto:startingAt:count: readStream size upTo:delimiterIsTerminator: upToEnd)
('writing' next:putAll:startingAt: nextPut: nextPutAll:)
('accessing')
('private' on:from:to:)
('file stream compatibility' localName)
('testing' isReadOnly)
!


!WriteStream reorganize!
('[mk] reading')
('[mk] writing')
('[mk] positioning')
('[mk] private')
('accessing' size)
('reading' contents next next: nextWordsInto: peek upTo:delimiterIsTerminator:)
('writing' next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextPutAllString:withAttributes: space)
('positioning' position: reset resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: isCompatibleWithContents: on: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!StdIOWriteStream reorganize!
('[mk] streaming')
('[mk] character writing')
('[mk] open / close')
('[mk] primitives')
('[mk] printing')
('streaming' flush nextPut: nextPutAll:)
('character writing' newLine space)
('open / close' openOnHandle:name:)
('primitives' primFlush: primWrite:from:startingAt:count:)
('printing' printOn:)
!


!StdIOWriteStream class reorganize!
('[mk] instance creation')
('[mk] accessing')
('instance creation' stderr stdout)
('accessing' stderrHandle stdioHandles stdoutHandle)
('cached state access' releaseClassCachedState)
!


!AdditionalMethodState reorganize!
('[mk] testing')
('[mk] accessing')
('testing' analogousCodeTo: hasAtLeastTheSamePropertiesAs: hasLiteralSuchThat: includes: includesKey: isEmpty isMethodProperties notEmpty)
('accessing' at: at:ifAbsent: at:ifAbsentPut: at:put: keysAndValuesDo: pragmas properties removeKey:ifAbsent: selector selector: setMethod:)
('copying' copyWith: copyWithout:)
('properties' includesProperty: propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: removeKey:)
('decompiling' method:)
('literals' allLiteralsDo:)
!


!BlockClosure reorganize!
('[mk] accessing')
('[mk] evaluating')
('[mk] exceptions')
('[mk] printing')
('accessing' argumentCount capturedValues copiedValueAt: endpc home isBlock method numArgs numCopiedValues outerContext outerContextsDo: receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('evaluating' bench durationToRun ifError: microsecondsToRun millisecondsToRun millisecondsToRunWithoutGC timeToRun value value: value:value: value:value:value: value:value:value:value: valueAt: valueNoContextSwitch valueNoContextSwitch: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do:)
('printing' decompile printOn: printStack: storeOn:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessForDebugging newProcessWith:)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueAndWaitWhileUnwinding: valueEnsured valueUnpreemptively)
('testing' hasNonLocalReturn isCleanClosure isClosure isDead pushesSelf readsReceiverIvar sendsToSuper writesReceiverIvar)
('error handing' handles: numArgsError:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('debugger access' sender)
('objects from disk' asSerializable objectForDataStream:)
('services' timeProfile)
('initialization' outerContext:startpc:numArgs:copiedValues:)
('system simulation' simulateValueWithArguments:caller:)
('lambda calculus' curried withFirstArg:)
!


!ContextPart reorganize!
('[mk] accessing')
('[mk] debugger access')
('[mk] printing')
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doNop doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: stackPtr stackp: stepToCalleeOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndStop: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext runUntilReturnFrom: unwindTo: unwindTo:safely:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
!


!MethodContext reorganize!
('[mk] accessing')
('[mk] printing')
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasNonLocalReturn home isExecutingBlock method methodReturnContext outerContext outerContextsDo: receiver removeSelf tempAt: tempAt:put:)
('closure support' asContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop callPrimitive: pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd:)
('private' aboutToReturn:through: cannotReturn: endpc instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
!


!Message reorganize!
('[mk] accessing')
('accessing' argument arguments fullName keywords lookupClass numArgs selector sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn:with: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!


!Boolean reorganize!
('[mk] logical operations')
('[mk] testing')
('[mk] copying')
('logical operations' & | eqv: not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
('testing' is:)
('copying' shallowCopy)
!


!False reorganize!
('[mk] logical operations')
('[mk] printing')
('logical operations' & | not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('user interface support' asMenuItemTextPrefix)
('printing' printOn:)
!


!True reorganize!
('[mk] logical operations')
('[mk] printing')
('logical operations' & | not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('user interface support' asMenuItemTextPrefix)
('printing' printOn:)
!


!Magnitude reorganize!
('[mk] comparing')
('comparing' < <= = > >= between:and: hash max: min: min:max:)
!


!Number reorganize!
('[mk] arithmetic')
('[mk] comparing')
('[mk] mathematical functions')
('[mk] truncation and round off')
('[mk] converting')
('[mk] testing')
('[mk] intervals')
('[mk] printing')
('[mkFloat] converting' adaptToFloat:andSend:)
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('comparing' % = hash)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
!


!Number class reorganize!
('[mk] constants')
('instance creation' readFrom: readRemainderOf:from:base:withSign:)
('constants' one zero)
!


!Integer reorganize!
('[mk] arithmetic')
('[mk] comparing')
('[mk] truncation and round off')
('[mk] converting')
('[mk] bit manipulation')
('[mk] testing')
('arithmetic' * + - / // alignedTo: crossSumBase: gcd: quo: reciprocalModulo: reciprocalModulo2: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash hashMultiply)
('converting' adaptToFraction:andSend: asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat moduloTwoPiAsFloat)
('enumerating' timesRepeat:)
('inspecting' customizeExplorerContents explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog floorLog: ifMultipleOf2And5Do:otherwise: lcm: log log: log2 nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToFraction: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printAsLiteralOn: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even is: isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: ceiling floor normalize roundedHAFZ roundedHTE truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!SmallInteger reorganize!
('[mk] arithmetic')
('[mk] comparing')
('[mk] bit manipulation')
('[mk] testing')
('[mk] printing')
('[mk] system primitives')
('[mk] copying')
('[mkFloat] converting' asFloat)
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed lowBit)
('testing' even isLarge odd)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
('comparing' < <= = > >= ~= hashMultiply identityHash)
('copying' shallowCopy)
!


!LookupKey reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] printing')
('accessing' canAssign key key: name)
('testing' isSpecialReadBinding isVariableBinding)
('bindings' beBindingOfType:announcing: beReadOnlyBinding beReadOnlyBindingAnnouncing: beReadWriteBinding beReadWriteBindingAnnouncing: recompileBindingsAnnouncing:)
('literals' allLiteralsDo:)
('comparing' < = hash)
('printing' printOn:)
!


!LookupKey class reorganize!
('[mk] instance creation')
('instance creation' key:)
!


!Association reorganize!
('[mk] accessing')
('[mk] comparing')
('[mk] printing')
('printing' printOn: storeOn:)
('objects from disk' objectForDataStream:)
('testing' analogousCodeTo: isSpecialWriteBinding literalEqual:)
('accessing' key:value: value value:)
('comparing' = hash)
!


!Association class reorganize!
('[mk] instance creation')
('instance creation' key:value:)
!


!UndefinedObject reorganize!
('[mk] testing')
('[mk] printing')
('[mk] copying')
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral isNil notNil)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
('copying' shallowCopy)
!


!Character reorganize!
('[mk] accesing')
('[mk] comparing')
('[mk] testing')
('[mk] copying')
('accessing' asciiValue digitValue)
('comparing' < <= > >= collatesBefore: collatingCategory hash identityHash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isAscii isCharacter isDiacriticalMark isDigit isLetter isLineSeparator isLowercase isNullCharacter isSeparator isSymbolLike isUnaccented isUppercase isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting: tokenish)
('printing' hex isLiteral printAsLiteralOn: printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
('accesing' codePoint)
('copying' shallowCopy)
!


!Character class reorganize!
('[mk] instance creation')
('[mk] accessing untypeable characters')
('class initialization' accentedLetters initialize initializeNamedCharactersMap uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' binarySelectorCharacters codePoint: digitValue: new separators value:)
('accessing untypeable characters' backQuote backspace cr delete doubleQuote escape home insert lf newLineCharacter newPage noBreakSpace null pageDown pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)
('converting' evaluate:withUtf16WordsOf: evaluate:withUtf8BytesOf: evaluateOnce:withUtf8BytesOf: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOf:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
('services' canonicalCompositionOf:and: canonicalDecomposition:on: combiningClass: isCombiningStarter: isDiacriticalMark:)
('constants' namedCharactersMap)
('private' constantNameFor: constantNames)
!


!Point reorganize!
('[mk] copying')
('accessing' x y)
('arithmetic' * + - / // \\ abs)
('comparing' < <= = > >= between:and: hash max: min: min:max:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asPoint corner: extent: isPoint rect:)
('geometry' barycentricCoordinatesInTriangle:with:with:into:ifDegenerate: isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with:)
('interpolating' interpolateTo:at:)
('point functions' crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors insideTriangle:with:with: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized onLineFrom:to: onLineFrom:to:within: quadrantOf: squaredDistanceTo: transposed)
('polar coordinates' degrees magnitude r rho theta)
('printing' printOn: printStringFractionDigits: storeOn:)
('private - initialization' initializePvtX:y:)
('transforming' adhereTo: inverseRotatedBy:about: negated rotatedBy: scaledBy: translatedBy:)
('truncation and round off' ceiling floor rounded roundedHAFZ roundedToEven truncateTo: truncated)
('testing' isIntegerPoint isZero)
('copying' shallowCopy)
!

