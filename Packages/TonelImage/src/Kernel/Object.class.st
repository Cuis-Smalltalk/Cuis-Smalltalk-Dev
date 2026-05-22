"
Object is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass of Object) and its subclasses.

Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here.

Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes.

Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as isMorph.
"
Class {
	#name : 'Object',
	#superclass : 'ProtoObject',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'object serialization' }
Object class >> createFrom: aSmartRefStream size: varsOnDisk version: instVarList [
	"Create an instance of me so objects on the disk can be read in.  Tricky part is computing the size if variable.  Inst vars will be filled in later.  "

	^ self isVariable
		ifFalse: [self basicNew]
		ifTrue: ["instVarList is names of old class's inst vars plus a version number" 
				self basicNew: (varsOnDisk - (instVarList size - 1))]

]

{ #category : 'documentation' }
Object class >> howToModifyPrimitives [
	"You are allowed to write methods which specify primitives, but please use 
	caution.  If you make a subclass of a class which contains a primitive method, 
	the subclass inherits the primitive.  The message which is implemented 
	primitively may be overridden in the subclass (E.g., see at:put: in String's 
	subclass Symbol).  The primitive behavior can be invoked using super (see 
	Symbol string:). 
	 
	A class which attempts to mimic the behavior of another class without being 
	its subclass may or may not be able to use the primitives of the original class.  
	In general, if the instance variables read or written by a primitive have the 
	same meanings and are in the same fields in both classes, the primitive will 
	work.  

	For certain frequently used 'special selectors', the compiler emits a 
	send-special-selector bytecode instead of a send-message bytecode.  
	Special selectors were created because they offer two advantages.  Code 
	which sends special selectors compiles into fewer bytes than normal.  For 
	some pairs of receiver classes and special selectors, the interpreter jumps 
	directly to a primitive routine without looking up the method in the class.  
	This is much faster than a normal message lookup. 
	 
	A selector which is a special selector solely in order to save space has a 
	normal behavior.  Methods whose selectors are special in order to 
	gain speed contain the comment, 'No Lookup'.  When the interpreter 
	encounters a send-special-selector bytecode, it checks the class of the 
	receiver and the selector.  If the class-selector pair is a no-lookup pair, 
	then the interpreter swiftly jumps to the routine which implements the 
	corresponding primitive.  (A special selector whose receiver is not of the 
	right class to make a no-lookup pair, is looked up normally).  The pairs are 
	listed below.  No-lookup methods contain a primitive number specification, 
	<primitive: xx>, which is redundant.  Since the method is not normally looked 
	up, deleting the primitive number specification cannot prevent this 
	primitive from running.  If a no-lookup primitive fails, the method is looked 
	up normally, and the expressions in it are executed. 
	 
	No Lookup pairs of (class, selector) 
	 
	SmallInteger with any of			+ - * /  \\  bitOr: bitShift: bitAnd:  // 
	SmallInteger with any of			=  ~=  >  <  >=  <= 
	Any class with						== 
	Any class with 						@ 
	Point with either of				x y 
	BlockClosure with either of 		value value:
	"

	self error: 'comment only'
]

{ #category : 'class initialization' }
Object class >> initialize [
	"Object initialize"
]

{ #category : 'instance creation' }
Object class >> initializedInstance [
	^ self new
]

{ #category : 'compiling' }
Object class >> isOkToAddMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToAddMethod: selector isMeta: isMeta
]

{ #category : 'compiling' }
Object class >> isOkToChangeMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToChangeMethod: selector isMeta: isMeta
]

{ #category : 'compiling' }
Object class >> isOkToRemoveMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed
	This implementor is only for convenience, as the method is meant to be redefined in class side as needed."
	^super isOkToRemoveMethod: selector isMeta: isMeta
]

{ #category : 'instance creation' }
Object class >> newFrom: aSimilarObject [
	"Create an object that has similar contents to aSimilarObject.
	If the classes have any instance varaibles with the same names, copy them across.
	If this is bad for a class, override this method."

	^ (self isVariable
		ifTrue: [self basicNew: aSimilarObject basicSize]
		ifFalse: [self basicNew]
	  ) copySameFrom: aSimilarObject
]

{ #category : 'package support' }
Object class >> package [
	"
	Object package
	AtomMorph package
	
	AtomMorph package ifNotNil: [ :pck | pck fullFileName ]
	"
	^ CodePackage packageOfClass: self ifNone: nil
]

{ #category : 'instance creation' }
Object class >> readFrom: aStream [
	"Create an object based on the contents of aStream."

	| object ok |
	ok := (aStream is: #Stream) or: [ aStream isString ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object := Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object
]

{ #category : 'instance creation' }
Object class >> unStream: aByteArray [
	^ ReferenceStream unStream: aByteArray
]

{ #category : 'documentation' }
Object class >> whatIsAPrimitive [
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

	self error: 'comment only'
]

{ #category : 'windowColor' }
Object class >> windowColor [
	^ Theme current defaultWindowColor
]

{ #category : 'associating' }
Object >> -> anObject [
	"Answer an Association between self and anObject"

	^Association key: self value: anObject
]

{ #category : 'comparing' }
Object >> = anObject  [
	"Answer true if the receiver and the argument represent the same object 
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject
]

{ #category : 'comparing' }
Object >> ~= anObject  [
	"Answer true if the receiver and the argument do not represent the same 
	object and false otherwise."

	^self = anObject == false
]

{ #category : 'finalization' }
Object >> actAsExecutor [
	"Prepare the receiver to act as executor for any resources associated with it"
	self breakDependents 
]

{ #category : 'events-accessing' }
Object >> actionForEvent: anEventSelector [
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	^self actionMap ifNotNil: [ :map |
		map
			at: anEventSelector asSymbol
			ifAbsent: nil]
]

{ #category : 'events-accessing' }
Object >> actionMap [

	^ActiveModel actionMapFor: self
]

{ #category : 'events-accessing' }
Object >> actionSequenceForEvent: anEventSelector [

	^self actionMap
		ifNotNil: [ :map |
			(map
				at: anEventSelector asSymbol
				ifAbsent: [ WeakActionSequence new ])
					asActionSequence ]
		ifNil: [ WeakActionSequence new ]
]

{ #category : 'events-accessing' }
Object >> actionsDo: aBlock [

	self actionMap ifNotNil: [ :map |
		map do: aBlock ]
]

{ #category : 'morphic' }
Object >> activeHand [
	"Answer a hand for the morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!"

	^self runningWorld ifNotNil: [ :w | w activeHand ]
]

{ #category : 'converting' }
Object >> adaptToFloat: rcvr andSend: selector [
	"If no method has been provided for adapting an object to a Float,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
]

{ #category : 'converting' }
Object >> adaptToFraction: rcvr andSend: selector [
	"If no method has been provided for adapting an object to a Fraction,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
]

{ #category : 'converting' }
Object >> adaptToInteger: rcvr andSend: selector [
	"If no method has been provided for adapting an object to a Integer,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
]

{ #category : 'dependency mechanism' }
Object >> addDependent: anObserver [
	"Make the given object one of the receiver's observers (dependents).

	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.

	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."

	self
		when: #changed:
		send: #update:
		to: anObserver.
	^anObserver.
]

{ #category : 'accessing' }
Object >> addInstanceVarNamed: aName withValue: aValue [
	"Add an instance variable named aName and give it value aValue"
	self class addInstVarName: aName asPlainString.
	self instVarAt: self class instSize put: aValue
]

{ #category : 'literals' }
Object >> allLiteralsDo: aBlock [
	"Enumerate all literals in the receiver. Note that subclasses may override this to expose the literals they hold such as Array or Pragma. Also note that CompiledCode, being the typical entry point for such an operation, will rather call #allLiteralsDo:#scanForSpecial: on all code literals (and regular literals) to avoid expensive byte-code scanning if possible."

	self flag: #discuss. "mt: Better implement this only on Symbol, Array, Number, Binding, etc. instead of Object?"
	aBlock value: self.
]

{ #category : 'literals' }
Object >> allLiteralsDo: aBlock scanForSpecial: aBoolean [
	"Backstop. Avoid bytecode scanning in CompiledCode and below. See #hasLiteral:scanForSpecial:."
	
	self allLiteralsDo: aBlock.
]

{ #category : 'message handling' }
Object >> argumentName [
	| name |
	name := self argumentNameSufix.
	^name article, name
]

{ #category : 'message handling' }
Object >> argumentNameSufix [

	^self class isMeta ifTrue: ['Class'] ifFalse: [self class name]
]

{ #category : 'converting' }
Object >> as: aSimilarClass [
	"Answer an object of class aSimilarClass that has similar contents to the receiver."

	^ self class == aSimilarClass ifTrue: [self] ifFalse: [aSimilarClass newFrom: self]
]

{ #category : 'converting' }
Object >> asString [
	"Answer a string that represents the receiver.
	Don't include extra quotes for Strings.

	This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray,  the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired).

	It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day.

	In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability.

	In any case, in your code, if possible, use a more specific method.

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printString 
]

{ #category : 'error handling' }
Object >> assert: aBlock [
	"Throw an assertion error if aBlock does not evaluates to true."

	aBlock value ifFalse: [AssertionFailure signal: 'Assertion failed']
]

{ #category : 'error handling' }
Object >> assert: aBlock description: aStringOrBlock [
    "Throw an assertion error if aBlock does not evaluates to true."

    aBlock value 
        ifFalse: [ AssertionFailure signal: aStringOrBlock value ]
]

{ #category : 'accessing' }
Object >> at: index  [
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
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
Object >> at: index put: value  [
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
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'user interface support' }
Object >> autoCompleterClassFor: textGetter [
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil
]

{ #category : 'accessing' }
Object >> basicAt: index  [
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
Object >> basicAt: index put: value  [
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
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'inspecting' }
Object >> basicInspect [
	"Create and schedule an Inspector in which the user can examine the 
	receiver's variables. This method should not be overriden."

	Inspector openOn: self
]

{ #category : 'accessing' }
Object >> basicSize [
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0	
]

{ #category : 'system primitives' }
Object >> become: otherObject  [
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				| contextReceiver |
				contextReceiver := context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					| selector selfMethod otherObjectMethod |
					selector := context method selector.
					selfMethod := self class lookupSelector: selector.
					otherObjectMethod := otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						| errorMessage |
						errorMessage := contextReceiver class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#become: aborted.' print.
							Error signal: errorMessage, ' #become: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }
]

{ #category : 'system primitives' }
Object >> becomeForward: otherObject  [
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:
	"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					| selector oldMethod newMethod |
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						| errorMessage |
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward: aborted.' print.
							Error signal: errorMessage, ' #becomeForward: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }
]

{ #category : 'system primitives' }
Object >> becomeForward: otherObject copyHash: copyHash [
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:copyHash:
	"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					| selector oldMethod newMethod |
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						| errorMessage |
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward:copyHash: aborted.' print.
							Error signal: errorMessage, ' #becomeForward:copyHash: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash
]

{ #category : 'binding' }
Object >> bindingOf: aString [
	^nil
]

{ #category : 'dependency mechanism' }
Object >> breakDependents [
	"Remove all of the receiver's observers (dependents)."

	self removeActionsForEvent: #changed:
]

{ #category : 'user interface' }
Object >> browseClassHierarchy [
	| targetClass |
	targetClass := self class isMeta
				ifTrue: [self class theNonMetaClass]
				ifFalse: [self class ].
	Smalltalk browseHierarchy: targetClass.
]

{ #category : 'error handling' }
Object >> caseError [
	"Report an error from an in-line or explicit case statement."

	self error: 'Case not found, and no otherwise clause'
]

{ #category : 'casing' }
Object >> caseOf: aBlockAssociationCollection [
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z := {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z := {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"
]

{ #category : 'casing' }
Object >> caseOf: aBlockAssociationCollection otherwise: aBlock [
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, answer the result
	 of evaluating aBlock."

	aBlockAssociationCollection associationsDo:
		[:assoc | (assoc key value = self) ifTrue: [^assoc value value]].
	^ aBlock value

"| z | z := {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"| z | z := {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
]

{ #category : 'dependency mechanism' }
Object >> changed [
	"Receiver changed in a general way. Inform all of the observers by sending
	them an #update: message.
	Note: In Smalltalk-80 observers were called 'dependents'."

	self changed: self
]

{ #category : 'dependency mechanism' }
Object >> changed: aspectSymbol [
	"Receiver changed. The change is denoted by the argument. 
	Usually the argument is a Symbol that is part of the observer's change 
	protocol. Inform all of the observers.
	Note: In Smalltalk-80 observers were called 'dependents'."

	self 
		triggerEvent: #changed:
		with: aspectSymbol
]

{ #category : 'class membership' }
Object >> class [
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed
]

{ #category : 'system primitives' }
Object >> className [
	"Answer a string characterizing the receiver's class, for use in list views for example"

	^ self class name asPlainString
]

{ #category : 'debugging-halt' }
Object >> clearHaltOnce [
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: false
]

{ #category : 'object serialization' }
Object >> comeFullyUpOnReload: smartRefStream [
	"Normally this read-in object is exactly what we want to store. 7/26/96 tk"

	^ self
]

{ #category : 'converting' }
Object >> complexContents [

	^self
]

{ #category : 'user interface' }
Object >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"nil confirm: 'Are you hungry?'"

	^ PopUpMenu confirm: queryString
]

{ #category : 'user interface' }
Object >> confirm: aString orCancel: cancelBlock [
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	^ PopUpMenu confirm: aString orCancel: cancelBlock
]

{ #category : 'object serialization' }
Object >> convertToCurrentVersion: varDict refStream: smartRefStrm [

	"subclasses should implement if they wish to convert old instances to modern ones"
]

{ #category : 'copying' }
Object >> copy [
	"Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy."

	^self shallowCopy postCopy
]

{ #category : 'converting' }
Object >> copyAs: aSimilarClass [
	"Answer an object of class aSimilarClass that has similar contents to the receiver.
	Always answer a new object, even if of same class as receiver"

	^ aSimilarClass newFrom: self
]

{ #category : 'copying' }
Object >> copyForClipboard [
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."
	^self copy
]

{ #category : 'copying' }
Object >> copyFrom: anotherObject [
	"Copy to myself all instance variables I have in common with anotherObject.  This is dangerous because it ignores an object's control over its own inst vars.  "

	| mine his |
	<primitive: 168>
	mine := self class allInstVarNames.
	his := anotherObject class allInstVarNames.
	1 to: (mine size min: his size) do: [:ind |
		(mine at: ind) = (his at: ind) ifTrue: [
			self instVarAt: ind put: (anotherObject instVarAt: ind)]].
	self class isVariable & anotherObject class isVariable ifTrue: [
		1 to: (self basicSize min: anotherObject basicSize) do: [:ind |
			self basicAt: ind put: (anotherObject basicAt: ind)]].
]

{ #category : 'copying' }
Object >> copySameFrom: otherObject [
	"Copy to myself all instance variables named the same in otherObject.
	This ignores otherObject's control over its own inst vars."

	| myInstVars otherInstVars |
	myInstVars := self class allInstVarNames.
	otherInstVars := otherObject class allInstVarNames.
	myInstVars withIndexDo: [:each :index | | match |
		(match := otherInstVars indexOf: each) > 0 ifTrue:
			[self instVarAt: index put: (otherObject instVarAt: match)]].
	1 to: (self basicSize min: otherObject basicSize) do: [:i |
		self basicAt: i put: (otherObject basicAt: i)].

]

{ #category : 'inspecting' }
Object >> copyToClipboard [

	Clipboard storeObject: self
]

{ #category : 'inspecting' }
Object >> customizeExplorerContents [

	^ false.

]

{ #category : 'error handling' }
Object >> deny: aBoolean [

	self assert: aBoolean not
]

{ #category : 'private' }
Object >> deprecatedMethod [
	"Warn that this method is deprecated and should not be used"

	'========' print.
	thisContext sender print.
	'--------------' print.
	'This method is deprecated. It will be removed from the system. Please change this and any other related senders.' print.
	'--------------' print.
	thisContext sender printStack: 6.
	'========' print.
]

{ #category : 'message handling' }
Object >> disableCode: aBlock [
	"Use this method instead of commenting temporarily disabled code.
	This way, it will be accessible with senders, references to variables, etc.
	Besides, you'll avoid problems when the code to disable has comments!"
]

{ #category : 'printing' }
Object >> displayStringOrText [
	"To be used in the UI. Answer might be an instance of Text if appropriate.
	Don't include extra quotes for Strings.
	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^self printString
]

{ #category : 'error handling' }
Object >> doesNotUnderstand: aMessage  [
	 "Handle the fact that there was an attempt to send the given
	  message to the receiver but the receiver does not understand
	  this message (typically sent from the machine when a message
	  is sent to the receiver and no method is defined for that selector).

	 Raise the MessageNotUnderstood signal.  If it is caught, answer
	 the result supplied by the exception handler.  If it is not caught,
	 answer the result of resending the message within a guard for
	 infinite recursion. This allows, for example, the programmer to
	 implement the method and continue."

	"Testing: (3 activeProcess)"

	| exception resumeValue |
	(exception := MessageNotUnderstood new)
		message: aMessage;
		receiver: self.
	resumeValue := exception signal.
	^exception reachedDefaultHandler "i.e. exception was not caught..."
		ifTrue:
			[[aMessage sentTo: self]
				on: MessageNotUnderstood
				do: [:ex|
					(self == ex receiver
					and: [aMessage hasIdenticalContentsAs: ex message]) ifFalse:
						[ex pass].
					self error: 'infinite recursion in doesNotUnderstand:']]
		ifFalse: [resumeValue]
]

{ #category : 'user interface support' }
Object >> editorClassFor: textGetter [
	"Enable any object to be the textProvider for a PluggableTextModel"
	^TextEditor
]

{ #category : 'error handling' }
Object >> error: aString  [
	"Throw a generic Error exception."

	^Error new signal: aString
]

{ #category : 'private' }
Object >> errorDescriptionForSubcriptBounds: index [

	^'subscript is out of bounds: ' , index printString
]

{ #category : 'private' }
Object >> errorImproperStore [
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'
]

{ #category : 'private' }
Object >> errorNonIntegerIndex [
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'
]

{ #category : 'private' }
Object >> errorNotIndexable [
	"Create an error notification that the receiver is not indexable."

	self error: ('Instances of {1} are not indexable' format: {self class name})
]

{ #category : 'private' }
Object >> errorSubscriptBounds: index  [
	"Create an error notification that an improper integer was used as an index."

	self error: (self errorDescriptionForSubcriptBounds: index)
]

{ #category : 'message handling' }
Object >> executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver with no args"

	<primitive: 189>
	^ self withArgs: #() executeMethod: compiledMethod
]

{ #category : 'inspecting' }
Object >> explore [

	ObjectExplorerWindow
		open: (ObjectExplorer new rootObject: self)
		label: nil
]

{ #category : 'finalization' }
Object >> finalizationRegistry [
	"Answer the finalization registry associated with the receiver."
	^FinalizationRegistry default
]

{ #category : 'finalization' }
Object >> finalize [
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	^self
]

{ #category : 'morphic' }
Object >> flash [
	"Do nothing."

]

{ #category : 'printing' }
Object >> fullPrintString [
	"Answer a String whose characters are a description of the receiver."

	^ String streamContents: [:s | self printOn: s]
]

{ #category : 'debugging-halt' }
Object >> halt [
	"This is the typical message to use for inserting breakpoints during 
	debugging. It behaves like halt:, but does not call on halt: in order to 
	avoid putting this message on the stack. Halt is especially useful when 
	the breakpoint message is an arbitrary one."

	Halt signal
]

{ #category : 'debugging-halt' }
Object >> halt: aString  [
	"This is the typical message to use for inserting breakpoints during 
	debugging. It creates and schedules a Notifier with the argument, 
	aString, as the label."
	
	Halt new signal: aString
]

{ #category : 'debugging-halt' }
Object >> haltOnce [
	"Halt unless we have already done it once."
	
	self haltOnceEnabled ifTrue: [
		self clearHaltOnce.
		^ self halt
	]
]

{ #category : 'debugging-halt' }
Object >> haltOnceEnabled [

	^ Smalltalk
		at: #HaltOnce
		ifAbsent: [false]
]

{ #category : 'error handling' }
Object >> handles: exception [
	"This method exists to break an endless loop in Exception>>findHandlerFrom: if the exception
is invalid"
	^false
]

{ #category : 'events-accessing' }
Object >> hasActionForEvent: anEventSelector [
    "Answer true if there is an action associated with anEventSelector"

    ^(self actionForEvent: anEventSelector) notNil
]

{ #category : 'inspecting' }
Object >> hasContentsInExplorer [

	^self basicSize > 0 or: [self class allInstVarNames isEmpty not]

]

{ #category : 'literals' }
Object >> hasLiteral: literal [

	^ self
		hasLiteralSuchThat: [:lit | lit literalEqual: literal]
		scanForSpecial: false
]

{ #category : 'literals' }
Object >> hasLiteralSuchThat: testBlock [
	"Also see #hasLiteral:. Do not scan for implicit (i.e. special) literals by default to get a faster response."

	^ self hasLiteralSuchThat: testBlock scanForSpecial: false
]

{ #category : 'literals' }
Object >> hasLiteralSuchThat: testBlock scanForSpecial: aBoolean [
	"Backstop. Avoid bytecode scanning in CompiledCode and below. See #hasLiteral:scanForSpecial:."

	self
		allLiteralsDo: [:literal | (testBlock value: literal) ifTrue: [^ true]]
		scanForSpecial: aBoolean.
	
	^ false
]

{ #category : 'comparing' }
Object >> hash [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	May be overridden, and should be overridden in any classes that define = "

	^ self identityHash
]

{ #category : 'tracing' }
Object >> inboundPointers [
"Answers a collection of all objects in the system that point to myself"

	^ self inboundPointersExcluding: #()
]

{ #category : 'tracing' }
Object >> inboundPointersExcluding: objectsToExclude [
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts."

	| allObjectsToExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.

	allObjectsToExclude := { thisContext }, objectsToExclude.
	^Smalltalk allObjects select: [ :object |
		((object statePointsTo: self) or: 			[object class == self])
			and: [(allObjectsToExclude statePointsTo: object) not]
		]
]

{ #category : 'user interface' }
Object >> inform: aString [
	"Display a message for the user to read and then dismiss. 6/9/96 sw"

	aString isEmptyOrNil ifFalse: [PopUpMenu inform: aString]
]

{ #category : 'translation support' }
Object >> inline: inlineFlag [
	"For translation only; noop when running in Smalltalk."
]

{ #category : 'inspecting' }
Object >> inspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	self inspectorClass openOn: self
]

{ #category : 'inspecting' }
Object >> inspectWithLabel: aLabel [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	self inspectorClass openOn: self withLabel: aLabel
]

{ #category : 'inspecting' }
Object >> inspectorClass [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ Inspector
]

{ #category : 'system primitives' }
Object >> instVarAt: index [
	"Primitive. Answer a fixed variable in an object. The numbering of the variables
	 corresponds to the named instance variables, followed by the indexed instance
	 variables. Fail if the index is not an Integer or is not the index of a fixed variable.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim73: index ].
	self primitiveFailed
]

{ #category : 'system primitives' }
Object >> instVarAt: index put: anObject [
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
	self primitiveFailed
]

{ #category : 'system primitives' }
Object >> instVarAtPrim73: index [
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	variables corresponds to the named instance variables. Fail if the index 
	is not an Integer or is not the index of a fixed variable. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize
]

{ #category : 'system primitives' }
Object >> instVarAtPrim74: anInteger put: anObject [
	"Primitive. Store a value into a fixed variable in the receiver. The 
	numbering of the variables corresponds to the named instance variables. 
	Fail if the index is not an Integer or is not the index of a fixed variable. 
	Answer the value stored as the result. Using this message violates the 
	principle that each object has sovereign control over the storing of 
	values into its instance variables. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject
]

{ #category : 'system primitives' }
Object >> instVarNamed: aString [
	"Return the value of the instance variable in me with that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asPlainString)



]

{ #category : 'system primitives' }
Object >> instVarNamed: aString put: aValue [
	"Store into the value of the instance variable in me of that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asPlainString) put: aValue

]

{ #category : 'testing' }
Object >> is: aSymbol [
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!
	aSymbol is usually a class name (starting with uppercase) or a protocol conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
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
	^false
]

{ #category : 'testing' }
Object >> isArray [
	^false
]

{ #category : 'testing' }
Object >> isBehavior [
	"Return true if the receiver is a behavior.
	Note: Do not override in any class except behavior."
	^false
]

{ #category : 'testing' }
Object >> isBlock [

	^ false
]

{ #category : 'testing' }
Object >> isByteString [
	"Overridden to return true in String, natch"
	^ false
]

{ #category : 'testing' }
Object >> isCharacter [
	"Overridden to return true in Character."
	^ false
]

{ #category : 'testing' }
Object >> isClosure [
	^false
]

{ #category : 'testing' }
Object >> isCollection [
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^false
]

{ #category : 'testing' }
Object >> isCompiledCode [

	^ false
]

{ #category : 'testing' }
Object >> isComplex [
	"Answer true if receiver is a Complex number. False by default."
	^ false
]

{ #category : 'testing' }
Object >> isContext [

	^false 
]

{ #category : 'testing' }
Object >> isFloat [
	"Overridden to return true in Float, natch"
	^ false
]

{ #category : 'testing' }
Object >> isFloatOrFloatComplex [
	"Overridden to return true in Float and Complex"
	^ false
]

{ #category : 'testing' }
Object >> isFraction [
	"Answer true if the receiver is a Fraction."

	^ false
]

{ #category : 'testing' }
Object >> isInteger [
	"Overridden to return true in Integer."

	^ false
]

{ #category : 'testing' }
Object >> isInterval [

	^ false
]

{ #category : 'class membership' }
Object >> isKindOf: aClass  [
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]
]

{ #category : 'printing' }
Object >> isLiteral [
	"Answer whether the receiver has a literal text form recognized by the compiler, and
	 preferrably typeable by a human user. The literal form must be provided by #storeOn:"

	^false
]

{ #category : 'class membership' }
Object >> isMemberOf: aClass  [
	"Answer whether the receiver is an instance of the class, aClass."

	^self class == aClass
]

{ #category : 'testing' }
Object >> isMethodProperties [
	^false
]

{ #category : 'testing' }
Object >> isNumber [
	"Overridden to return true in Number, natch"
	^ false
]

{ #category : 'testing' }
Object >> isObject [
	^true
]

{ #category : 'pinning' }
Object >> isPinned [
	"Answer if the receiver is pinned.  The VM's garbage collector routinely moves
	 objects as it reclaims and compacts memory.  But it can also pin an object so
	 that it will not be moved, which can make it easier to pass objects out through
	 the FFI."
	<primitive: 183 error: ec>
	^self primitiveFailed
]

{ #category : 'testing' }
Object >> isPoint [
	"Overridden to return true in Point."

	^ false
]

{ #category : 'testing' }
Object >> isPseudoContext [
	^false
]

{ #category : 'testing' }
Object >> isString [
	"Overridden to return true in String, UnicodeString."
	^ false
]

{ #category : 'testing' }
Object >> isSymbol [
	^ false 
]

{ #category : 'testing' }
Object >> isUnicodeString [
	"Overridden to return true in UnicodeString, natch"
	^ false
]

{ #category : 'testing' }
Object >> isVariableBinding [
	"Return true if I represent a literal variable binding"
	^false
	
]

{ #category : 'comparing' }
Object >> literalEqual: other [

	^ self class == other class and: [self = other]
]

{ #category : 'printing' }
Object >> longPrintOn: aStream [
	"Append to the argument, aStream, the names and values of all 
	of the receiver's instance variables."

	self class allInstVarNames withIndexDo: [ :title :index |
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 print: (self instVarAt: index);
		 newLine]
]

{ #category : 'printing' }
Object >> longPrintOn: aStream limitedTo: sizeLimit indent: indent [
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printTextLimitedTo: (sizeLimit - 3 - title size max: 1));
		 newLine ]
]

{ #category : 'printing' }
Object >> longPrintString [
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str := String streamContents: [:aStream | self longPrintOn: aStream].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]
]

{ #category : 'printing' }
Object >> longPrintStringLimitedTo: aLimitValue [
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str := String streamContents: [:aStream | self longPrintOn: aStream limitedTo: aLimitValue indent: 0].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]
]

{ #category : 'printing' }
Object >> longPrintText [
	"Answer a Text with a description of the receiver."
	
	^ (Text streamContents: [:aStream | self longPrintOn: aStream])
		ifEmpty: [self printText, String newLineString] "Objects without inst vars should return something"
]

{ #category : 'printing' }
Object >> longPrintTextLimitedTo: anInteger [
	"Answer a Text with a description of the receiver. limited to anInteger characters."

	^ (Text streamContents: [:aStream | self longPrintOn: aStream limitedTo: anInteger indent: 0])
		ifEmpty: [self printText, String newLineString] "Objects without inst vars should return something"
]

{ #category : 'finalization' }
Object >> mourn [
	"Sent by the new Finalization mechanism.
	All weak objects (instances of weak classes) and Ephemerons may be mourned if they point to an object that needs to be collected.

	This default implementation does nothing. To override in specific subclasses."
]

{ #category : 'converting' }
Object >> mustBeBoolean [
	"Catches attempts to test truth of non-Booleans.  This message is sent from the VM.  The sending context is rewound to just before the jump causing this exception."

	^ self mustBeBooleanIn: thisContext sender
]

{ #category : 'converting' }
Object >> mustBeBooleanIn: context [
	"context is the where the non-boolean error occurred. Rewind context to before jump then raise error."

	| proceedValue |
	context skipBackBeforeJump.
	proceedValue := NonBooleanReceiver new
		object: self;
		signal: 'proceed for truth.'.
	^ proceedValue ~~ false
]

{ #category : 'testing' }
Object >> name [
	"Answer a name for the receiver.  This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems.  By default, we let the object just print itself out..  "

	^ self printString
]

{ #category : 'printing' }
Object >> nominallyUnsent: aSelectorSymbol [
	"From within the body of a method which is not formally sent within the system, but which you intend to have remain in the system (for potential manual invocation, or for documentation, or perhaps because it's sent by commented-out-code that you anticipate uncommenting out someday, send this message, with the selector itself as the argument.

This will serve two purposes:

	(1)  The method will not be returned by searches for unsent selectors (because it, in a manner of speaking, sends itself).
	(2)	You can locate all such methods by browsing senders of #nominallyUnsent:"
	"(jmv) Correction: send the unsent symbol from elsewhere. If it is sent from whithin itself, it will still appear as unsent."

	false ifTrue: [self flag: #nominallyUnsent:]    "So that this method itself will appear to be sent"

]

{ #category : 'error handling' }
Object >> notYetImplemented [
	NotYetImplemented signal
]

{ #category : 'error handling' }
Object >> notify: aString  [
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed."

	Warning signal: aString

	"
	nil notify: 'confirmation message'
	"
]

{ #category : 'error handling' }
Object >> notify: aString at: location [
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed. Subclasses can
	override this and insert an error message at location within aString."

	self notify: aString

	"nil notify: 'confirmation message' at: 12"
]

{ #category : 'object serialization' }
Object >> objectForDataStream: refStrm [
    "Return an object to store on an external data stream."

    ^ self
]

{ #category : 'tracing' }
Object >> outboundPointers [
"Answers a list of all objects I am causing not to be garbage-collected"

	| collection |
	collection := OrderedCollection new.
	self outboundPointersDo: [:ea | collection add: ea].
	^ collection
]

{ #category : 'tracing' }
Object >> outboundPointersDo: aBlock [
"do aBlock for every object I point to, exactly how the garbage collector would. Adapted from PointerFinder >> #followObject:"

	aBlock value: self class.
	1 to: self class instSize do: [:i | aBlock value: (self instVarAt: i)].
	1 to: self basicSize do: [:i | aBlock value: (self basicAt: i)].
]

{ #category : 'message handling' }
Object >> perform: aSymbol  [
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)
]

{ #category : 'message handling' }
Object >> perform: aSymbol with: anObject  [
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)
]

{ #category : 'message handling' }
Object >> perform: aSymbol with: firstObject with: secondObject  [
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)
]

{ #category : 'message handling' }
Object >> perform: aSymbol with: firstObject with: secondObject with: thirdObject  [
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject)
]

{ #category : 'message handling' }
Object >> perform: aSymbol with: firstObject with: secondObject with: thirdObject with: fourth [
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject with: fourth)
]

{ #category : 'message handling' }
Object >> perform: selector withArguments: argArray  [
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 84 error: errorCode>
	self error: errorCode.
]

{ #category : 'message handling' }
Object >> perform: selector withArguments: argArray inSuperclass: lookupClass [
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100 error: errorCode>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self error: errorCode.
]

{ #category : 'message handling' }
Object >> perform: selector withPossiblyWrongSizedArguments: argArray [
	"Fix arguments size if needed"

	| arguments numArgs |
	numArgs := selector numArgs.
	numArgs = argArray size
		ifTrue: [
			arguments := argArray ]
		ifFalse: [
			arguments := Array new: numArgs.
			arguments fillWith: argArray ].
	^ self perform: selector withArguments: arguments 
]

{ #category : 'pinning' }
Object >> pin [
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is pinned, and answers whether it was already pinned."
	"Note: Some VMs might not support pinning (for instance, SqueakJS on October 2023).
	If the primitive fails, answer nil.
	Senders must check for nil return value and act accordingly."

	^self setPinned: true
]

{ #category : 'copying' }
Object >> postCopy [
	"self is a shallow copy, subclasses should copy fields as necessary to complete the full copy.
	Also clear any ivars that should be initialized lazyly."

	^ self
]

{ #category : 'system primitives' }
Object >> primitiveChangeClassTo: anObject [
	"Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have.
	Note: The primitive will fail in most cases that you think might work. This is mostly because of because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for two of the reasons mentioned above. Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3).
	The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use."

	<primitive: 115>
	self primitiveFailed
]

{ #category : 'private' }
Object >> primitiveError: aString signalContext: signalContext [
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| emergencyEvaluator lines r context i |
	r := `10@10` extent: (Display extent -100 min: `1700@5000`).
	lines := r height // FontFamily defaultLineSpacing.
	emergencyEvaluator := Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	
	context := signalContext.
	i := 1.
	[context notNil and: [i < (30 min: lines - 15)]] whileTrue: [
		i := i + 1.
		emergencyEvaluator
			print: context;
			newLine.
		context := context sender ].
	
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint.
]

{ #category : 'error handling' }
Object >> primitiveFail [
	"#primitiveFail may be invoked by certain methods whose code is translated in C.
	In such a case #primitiveFail and not #primitiveFailed should be invoked.
	The reason is that this code is translated to C by VMMaker. #primitiveFail is implemented in Interpreter of VMMaker."

	^ self primitiveFailed
]

{ #category : 'error handling' }
Object >> primitiveFailed [
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self primitiveFailed: thisContext sender selector
]

{ #category : 'error handling' }
Object >> primitiveFailed: selector [
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self error: selector asPlainString, ' failed'
]

{ #category : 'printing' }
Object >> print [
	Transcript show: self printString; newLine
]

{ #category : 'printing' }
Object >> printAs: streamType limitedTo: limit [
	"Answer an instance of streamType whose characters describe the receiver.
	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString := streamType streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '[..]'
]

{ #category : 'printing' }
Object >> printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver.
	String streamContents: [ :strm | Object new printOn: strm ]
	"
	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title.
]

{ #category : 'printing' }
Object >> printString [
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 50000
]

{ #category : 'printing' }
Object >> printStringLimitedTo: limit [
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit
]

{ #category : 'printing' }
Object >> printText [

	^ self printTextLimitedTo: 50000
]

{ #category : 'printing' }
Object >> printTextLimitedTo: limit [
	"Answer a Text whose characters are a description of the receiver."

	^self printAs: Text limitedTo: limit
]

{ #category : 'printing' }
Object >> printWithClosureAnalysisOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	aStream
		nextPutAll: self class name withArticle
]

{ #category : 'profiler support' }
Object >> profilerFriendlyCall: aBlock [
	"In AndreasSystemProfiler, usually primitives are reported as children of the wrong node.
	The reason is that while the primitive is recorded, it is only taken into account at next suspension point.
	
	For long running primitives, and maybe only while profiling, this method provides a workaround.
	Evaluate and compare

		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	Also see #runProfilerProcess, and this more complex example (you might want to make it use #profilerFriendlyCall:)
		AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]

	Keep in mind there is a performance penaly each time this method is called.
	Consider doing it only for long-running primitives, or only while profiling code.
	"

	"The apparently useless loop is to ensure we have a suspension point in this method, after block evaluation.
	Suspension points are activation of message sends and bytecode back jumps."
	| primResult primWasCalled |
	primWasCalled := false.
	[ primWasCalled ] whileFalse: [
		primResult := aBlock value.
		primWasCalled := true ].
	^primResult
]

{ #category : 'accessing' }
Object >> rawBasicAt: index  [
	"A verbatim copy of #basicAt:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
Object >> rawBasicAt: index put: value  [
	"A verbatim copy of #basicAt:put:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
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
		ifTrue: [^self rawBasicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'object serialization' }
Object >> readDataFrom: aDataStream size: varsOnDisk [
	"Fill in the fields of self based on the contents of aDataStream.  Return self.
	 Read in the instance-variables written by Object>>storeDataOn:.
	 NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it.
	 Allow aDataStream to have fewer inst vars.  See SmartRefStream."
	| cntInstVars cntIndexedVars |

	cntInstVars := self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars := varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars := 0.
				cntInstVars := varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	1 to: cntInstVars do:
		[:i | self instVarAt: i put: aDataStream next].
	1 to: cntIndexedVars do:
		[:i | self basicAt: i put: aDataStream next].
	"Total number read MUST be equal to varsOnDisk!"
	^ self	"If we ever return something other than self, fix calls 
			on (super readDataFrom: aDataStream size: anInteger)"
]

{ #category : 'events-removing' }
Object >> releaseActionMap [
	"Modifying ActionMaps is kinda dangerous.
	It is an instance of EphemeronIdentityDictionary.
	Its integrity is protected by a Mutex, meaning that nested accesses from the same process could
	break it, leading to system crash if not done very carefully.
	As this is public protocol, and in any case EphemeronIdentityDictionary are automatically cleared
	when keys are collected, the risk is not worth it.
	"
	"ActiveModel releaseActionMapFor: self."
	self removeAllActions.
]

{ #category : 'object serialization' }
Object >> releaseCachedState [
	"Some subclasses might"
]

{ #category : 'events-removing' }
Object >> removeAction: anAction [
forEvent: anEventSelector

    self
        removeActionsSatisfying: [:action | action = anAction]
        forEvent: anEventSelector
]

{ #category : 'events-removing' }
Object >> removeActionsForEvent: anEventSelector [

	| map |
	map := self actionMap.
	map ifNotNil: [
		map removeKey: anEventSelector asSymbol ifAbsent: nil.
		"No need. ActionMaps is an EphemeronIdentityDictionary. Keys are automatically removed when collectable.
		Besides, this is dangerous for instance, if called from an iteration of ActionMaps."
		"map isEmpty
			ifTrue: [ self releaseActionMap ]"
		]
]

{ #category : 'events-removing' }
Object >> removeActionsSatisfying: aBlock [

	self actionMap ifNotNil: [ :map |
		map keys do: [ :eachEventSelector |
			self
   				removeActionsSatisfying: aBlock
				forEvent: eachEventSelector ]]
]

{ #category : 'events-removing' }
Object >> removeActionsSatisfying: aOneArgBlock forEvent: anEventSelector [
	self
		setActionSequence:
			((self actionSequenceForEvent: anEventSelector)
				reject: aOneArgBlock)
		forEvent: anEventSelector
]

{ #category : 'events-removing' }
Object >> removeActionsWithReceiver: anObject [

	self actionMap ifNotNil: [ :map |
		map keys do: [ :eachEventSelector |
			self
   				removeActionsSatisfying: [:anAction | anAction receiver == anObject]
				forEvent: eachEventSelector ]]
]

{ #category : 'events-removing' }
Object >> removeActionsWithReceiver: anObject [
forEvent: anEventSelector

    self
        removeActionsSatisfying:
            [:anAction |
            anAction receiver == anObject]
        forEvent: anEventSelector
]

{ #category : 'events-removing' }
Object >> removeAllActions [
	"Empty our #actionMap.
	Use this method when done with some object holding dependents. Do not call #releaseActionMap.
	Actually, usually you don't need to call any of these. Our #actionMap is cleared autonatically when
	we are collected,	as ActionMaps is an EphemeronIdentityDictionary.
	"
	self removeActionsSatisfying: [ :any | true ]
]

{ #category : 'dependency mechanism' }
Object >> removeDependent: anObject [
	"Remove the given object as one of the receiver's observers (dependents)."

	self 
		removeActionsWithReceiver: anObject
		forEvent: #changed:.
	^ anObject
]

{ #category : 'testing' }
Object >> renameTo: newName [
	"If the receiver has an inherent idea about its own name, it should take action here.  Any object that might be pointed to in the References dictionary might get this message sent to it upon reload"
]

{ #category : 'user interface' }
Object >> request: queryString do: acceptBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: '' verifying: [:aString| true] do: acceptBlock orCancel: nil
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer [
	"Modal. This method returns when an answer is provided."

	^ self request: queryString initialAnswer: defaultAnswer orCancel: ['']
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer do: acceptBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: acceptBlock orCancel: nil
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer do: acceptBlock orCancel: cancelBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: acceptBlock orCancel: cancelBlock
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock [
	"Modal. This method returns when an answer is provided."

	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: nil
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
]

{ #category : 'user interface' }
Object >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
]

{ #category : 'class membership' }
Object >> respondsTo: aSymbol  [
	"Answer whether the method dictionary of the receiver's class contains 
	aSymbol as a message selector."

	^self class canUnderstand: aSymbol
]

{ #category : 'finalization' }
Object >> retryWithGC: execBlock until: testBlock [
	"Retry execBlock as long as testBlock returns false. Do an incremental GC after the first try, a full GC after the second try."
	| blockValue |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.
]

{ #category : 'system primitives' }
Object >> run: aSelector with: arguments in: aReceiver [

	self halt: 'This object can not behave like a CompiledMethod!'.

]

{ #category : 'morphic' }
Object >> runningWorld [
	"Answer a morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!"

	^UISupervisor runningWorld
]

{ #category : 'events-accessing' }
Object >> setActionSequence: actionSequence [
forEvent: anEventSelector

	| action |
	"This is a good time to compact the action sequence of old, garbage collected stuff."
	action := actionSequence asMinimalRepresentation.
	action
		ifNil: [ self removeActionsForEvent: anEventSelector]
		ifNotNil: [
			self updateableActionMap
				at: anEventSelector asSymbol
				put: action]
]

{ #category : 'debugging-halt' }
Object >> setHaltOnce [
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: true
]

{ #category : 'private' }
Object >> setPinned: aBoolean [
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This primitive either pins or unpins an object, and answers if it was already pinned."
	"Note: In case of primitive error, assume pinning is not supported by this VM.
	Senders must check for nil return value and act accordingly.
	For instance, on October 2023, SqueakJS doesn't support pinning, even if it
	supports Spur images."

	<primitive: 184 error: ec>
	"^self primitiveFailed"
	^nil
]

{ #category : 'copying' }
Object >> shallowCopy [
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
	^ newObject
]

{ #category : 'printing' }
Object >> shortPrintString [
	"Answer a String whose characters are a description of the receiver.
	This is a short one, good for showing users (for example, in ObjectExplorers).
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 128.
]

{ #category : 'error handling' }
Object >> shouldBeImplemented [
	"Announce that this message should be implemented"

	self error: 'This message should be implemented'
]

{ #category : 'testing' }
Object >> shouldBePrintedAsLiteral [

	^self isLiteral
]

{ #category : 'testing' }
Object >> shouldBePrintedAsLiteralVisiting: aSet [
	"aSet is for detecting circular references.
	See implementors and senders."
	^self isLiteral
]

{ #category : 'error handling' }
Object >> shouldNotHappen [
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappen].
	See #shouldNotHappenBecause: also"
	
	self error: self shouldNotHappenErrorMessage
]

{ #category : 'error handling' }
Object >> shouldNotHappenBecause: aReason [
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappenBecause: 'The array has one element'].
	See #shouldNotHappen also"
	
	self error: self shouldNotHappenBecauseErrorMessage, aReason
]

{ #category : 'error handling' }
Object >> shouldNotHappenBecauseErrorMessage [

	^self shouldNotHappenErrorMessage, ' because: '
]

{ #category : 'error handling' }
Object >> shouldNotHappenErrorMessage [

	^'Should not happen'
]

{ #category : 'error handling' }
Object >> shouldNotImplement [
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'
]

{ #category : 'accessing' }
Object >> size [
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self class isVariable ifFalse: [self errorNotIndexable].
	^ 0
]

{ #category : 'system primitives' }
Object >> someObject [
	"Primitive. Answer the first object in the enumeration of all
	 objects."

	<primitive: 138>
	self primitiveFailed.
]

{ #category : 'private' }
Object >> species [
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class
]

{ #category : 'stepping' }
Object >> stepAt: millisecondSinceLast [
	"See comment at #wantsSteps"
]

{ #category : 'private' }
Object >> storeAt: offset inTempFrame: aContext [
	"This message had to get sent to an expression already on the stack
	as a Block argument being accessed by the debugger.
	Just re-route it to the temp frame."
	^ aContext tempAt: offset put: self
]

{ #category : 'object serialization' }
Object >> storeDataOn: aDataStream [
	"Store myself on a DataStream.  Answer self.  This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream.  NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects.  readDataFrom:size: reads back what we write here."
	| cntInstVars cntIndexedVars |

	cntInstVars := self class instSize.
	cntIndexedVars := self basicSize.
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].

	"Write fields of a variable length object.  When writing to a dummy 
		stream, don't bother to write the bytes"
	((aDataStream byteStream class == DummyStream) and: [self class isBits]) ifFalse: [
		self class isWeak
			ifTrue: [
				"For weak classes (for example ActiveModel's ActionMaps) write the referenced object only
				if referenced from elsewhere in the dumped object graph.
				This means, for instance that if we only dump a model, no observers are stored, 
				but if we store a view (i.e. a Morph), it is properly handled as an observer after the object graph is revived."
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPutWeak: (self basicAt: i)]]
			ifFalse: [
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPut: (self basicAt: i)]]]
]

{ #category : 'printing' }
Object >> storeOn: aStream  [
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
	aStream nextPutAll: ' yourself)'
]

{ #category : 'printing' }
Object >> storeString [
	"Answer a String representation of the receiver from which the receiver 
	can be reconstructed."

	^ String streamContents: [:s | self storeOn: s]
]

{ #category : 'error handling' }
Object >> subclassResponsibility [
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass should have overridden ', thisContext sender selector printString
]

{ #category : 'translation support' }
Object >> success: aBoolean [
	"For translation only; noop when running in Smalltalk."
]

{ #category : 'user interface support' }
Object >> textStylerClassFor: textGetter [
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil
]

{ #category : 'finalization' }
Object >> toFinalizeSend: aSelector to: aFinalizer [
	"When I am finalized (e.g., garbage collected) send aSelector to the appropriate finalizer (the guy who knows how to do any needed cleanup).
	WARNING: The finalizer is not allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector)
]

{ #category : 'finalization' }
Object >> toFinalizeSend: aSelector to: aFinalizer with: aResourceHandle [
	"When I am finalized (e.g., garbage collected) close the associated resource handle by sending aSelector to the appropriate finalizer (the guy who knows how to get rid of the resource).
	WARNING: Neither the finalizer nor the resource handle are allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	self == aResourceHandle ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector
			argument: aResourceHandle)
]

{ #category : 'debugging-halt' }
Object >> toggleHaltOnce [
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce]
		ifFalse: [self setHaltOnce]
]

{ #category : 'events-triggering' }
Object >> triggerEvent: anEventSelector [
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

	^(self actionForEvent: anEventSelector) value
]

{ #category : 'events-triggering' }
Object >> triggerEvent: anEventSelector with: anObject [

    ^self 
		triggerEvent: anEventSelector
		withArguments: {anObject}
]

{ #category : 'events-triggering' }
Object >> triggerEvent: anEventSelector withArguments: anArgumentList [

	^ (self actionForEvent: anEventSelector)
		valueWithArguments: anArgumentList
]

{ #category : 'pinning' }
Object >> unpin [
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is unpinned, and answers whether it was pinned."
	"Note: Some VMs might not support pinning (for instance, SqueakJS on October 2023).
	If the primitive fails, answer nil.
	Senders must check for nil return value and act accordingly."

	^self setPinned: false
]

{ #category : 'dependency mechanism' }
Object >> update: aspectSymbol [
	"Receive a change notice from an object we are observing. 
	The default behavior is to do nothing.
	Subclasses should act appropriately on the aspects they are interested in."

	^self
]

{ #category : 'events-accessing' }
Object >> updateableActionMap [

	^ActiveModel updateableActionMapFor: self
]

{ #category : 'evaluating' }
Object >> value [

	^self
]

{ #category : 'evaluating' }
Object >> valueWithArguments: aSequenceOfArguments [

	^self
]

{ #category : 'evaluating' }
Object >> valueWithPossibleArgument: anArg [

	^self
]

{ #category : 'translation support' }
Object >> var: varSymbol declareC: declString [
	"For translation only; noop when running in Smalltalk."
]

{ #category : 'copying' }
Object >> veryDeepCopy [
	"Do a full copy of an object graph.
	Some classes might chose not to include some ivars.
	For example, aMorph owner is only included 'weakely'.
	See #storeDataOn:, and in particular Morph>>storeDataOn:
	
	This method is included for comatibility with other Smalltalks and for reference.
	Each class should decide how to copy its instances.
	For example, Morph>>copy.
	Implementing #copy appropriately, and just calling it is better style than calling #veryDeepCopy (except, maybe, in low level, system code)"

	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)
]

{ #category : 'stepping' }
Object >> wantsSteps [
	"Overridden by morphic classes whose instances want to be stepped all the time,
	or by model classes who want their morphic views to be stepped all the time.
	
	Some classes might answer false to this message, and call
		#startStepping 
		#startSteppingStepTime:
		#stopStepping
	as appropriate"

	^ false
]

{ #category : 'events-registering' }
Object >> when: anEventSelector evaluate: anAction  [

	| actions |
	actions := self actionSequenceForEvent: anEventSelector.
	(actions includes: anAction)
		ifTrue: [^ self].
	self 
		setActionSequence: (actions copyWith: anAction)
		forEvent: anEventSelector
]

{ #category : 'events-registering' }
Object >> when: anEventNameSymbol [
send: aMessageSelector
to: anObserver
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
	self
		when: anEventNameSymbol
		send: aMessageSelector
		to: anObserver
		withArguments: #()
]

{ #category : 'events-registering' }
Object >> when: anEventNameSymbol [
send: aMessageSelector
to: anObserver
with: messageArgument
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."

	self
		when: anEventNameSymbol
		send: aMessageSelector
		to: anObserver
		withArguments: (Array with: messageArgument)
]

{ #category : 'events-registering' }
Object >> when: anEventNameSymbol [
send: aMessageSelector
to: anObserver
withArguments: messageArgumentsArray
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
	self
		when: anEventNameSymbol
		evaluate: (WeakMessageSend
			receiver: anObserver
			selector: aMessageSelector
			arguments: messageArgumentsArray)
]

{ #category : 'message handling' }
Object >> with: arg1 executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and arg1"

	<primitive: 189>
	^ self withArgs: {arg1} executeMethod: compiledMethod
]

{ #category : 'message handling' }
Object >> with: arg1 with: arg2 executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and arg1 & arg2"

	<primitive: 189>
	^ self withArgs: {arg1. arg2} executeMethod: compiledMethod
]

{ #category : 'message handling' }
Object >> with: arg1 with: arg2 with: arg3 executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and arg1, arg2, & arg3"

	<primitive: 189>
	^ self withArgs: {arg1. arg2. arg3} executeMethod: compiledMethod
]

{ #category : 'message handling' }
Object >> with: arg1 with: arg2 with: arg3 with: arg4 executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and arg1, arg2, arg3, & arg4"

	<primitive: 189>
	^ self withArgs: {arg1. arg2. arg3. arg4} executeMethod: compiledMethod
]

{ #category : 'message handling' }
Object >> withArgs: argArray executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and args in argArray"

	| selector |
	<primitive: 188>
	selector := Symbol new.
	self class addSelectorSilently: selector withMethod: compiledMethod.
	^ [self perform: selector withArguments: argArray]
		ensure: [self class basicRemoveSelector: selector]
]

{ #category : 'converting' }
Object >> withoutListWrapper [

	^self
]

{ #category : 'accessing' }
Object >> yourself [
	"Answer self."
]
