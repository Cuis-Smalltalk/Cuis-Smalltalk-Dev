'From Cuis 5.0 [latest update: #4702] on 27 July 2021 at 7:23:32 pm'!

!Object methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
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

!Object methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
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

!Object methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0	! !

!Object methodsFor: 'binding' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
bindingOf: aString
	^nil! !

!Object methodsFor: 'casing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
caseOf: aBlockAssociationCollection
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"! !

!Object methodsFor: 'casing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
caseOf: aBlockAssociationCollection otherwise: aBlock
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, answer the result
	 of evaluating aBlock."

	aBlockAssociationCollection associationsDo:
		[:assoc | (assoc key value = self) ifTrue: [^assoc value value]].
	^ aBlock value

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"! !

!Object methodsFor: 'class membership' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
class
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed! !

!Object methodsFor: 'class membership' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isKindOf: aClass 
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]! !

!Object methodsFor: 'class membership' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isMemberOf: aClass 
	"Answer whether the receiver is an instance of the class, aClass."

	^self class == aClass! !

!Object methodsFor: 'class membership' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
respondsTo: aSymbol 
	"Answer whether the method dictionary of the receiver's class contains 
	aSymbol as a message selector."

	^self class canUnderstand: aSymbol! !

!Object methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
caseError
	"Report an error from an in-line or explicit case statement."

	self error: 'Case not found, and no otherwise clause'! !

!Object methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
notify: aString at: location
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed. Subclasses can
	override this and insert an error message at location within aString."

	self notify: aString

	"nil notify: 'confirmation message' at: 12"! !

!Object methodsFor: 'system primitives' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
someObject
	"Primitive. Answer the first object in the enumeration of all
	 objects."

	<primitive: 138>
	self primitiveFailed.! !

!Object methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isInteger
	"Overridden to return true in Integer."

	^ false! !

!Object methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isNumber
	"Overridden to return true in Number, natch"
	^ false! !

!Object methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeAt: offset inTempFrame: aContext
	"This message had to get sent to an expression already on the stack
	as a Block argument being accessed by the debugger.
	Just re-route it to the temp frame."
	^ aContext tempAt: offset put: self! !


!Browser methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
editSelection
	^editSelection! !

!Browser methodsFor: 'metaclass' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
indicateClassMessages
	"Indicate that the message selection should come from the metaclass 
	messages."

	self metaClassIndicated: true! !

!Browser methodsFor: 'metaclass' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
indicateInstanceMessages
	"Indicate that the message selection should come from the class (instance) 
	messages."

	self metaClassIndicated: false! !

!Browser methodsFor: 'metaclass' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
metaClassIndicated
	"Answer the boolean flag that indicates which of the method dictionaries, 
	class or metaclass."

	^metaClassIndicated! !

!Browser methodsFor: 'system category list' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
systemCategoryList
	"Answer the class categories modelled by the receiver."

	^systemOrganizer categories! !


!MessageSet methodsFor: 'message list' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
messageList
	"Answer the current list of messages."

	^messageList! !

!MessageSet methodsFor: 'class list' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
metaClassIndicated
	"Answer the boolean flag that indicates whether
	this is a class method."

	^ self selectedClassOrMetaClass isMeta! !

!MessageSet methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
autoSelectString
	"Return the string to be highlighted when making new selections"
	^ autoSelectString! !

!MessageSet methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
autoSelectString: aString
	"Set the string to be highlighted when making new selections"
	autoSelectString _ aString! !


!MessageSet class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
messageList: anArray 
	"Answer an instance of me with message list anArray."

	^self new initializeMessageList: anArray! !


!ChangeList methodsFor: 'viewing access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
list
	^ list! !

!ChangeList methodsFor: 'viewing access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
listIndex
	^ listIndex! !

!ChangeList methodsFor: 'viewing access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
listSelectionAt: index
	^ listSelections at: index! !

!ChangeList methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
changeList
	^ changeList! !

!ChangeList methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
file
	^file! !


!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
contextVariablesInspector
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context."

	^contextVariablesInspector! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
interruptedContext
	"Answer the suspended context of the interrupted process."

	^contextStackTop! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
interruptedProcess
	"Answer the interrupted process."

	^interruptedProcess! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
proceedValue
	"Answer the value to return to the selected context when the interrupted 
	process proceeds."

	^proceedValue! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
proceedValue: anObject 
	"Set the value to be returned to the selected context when the interrupted 
	process proceeds."

	proceedValue _ anObject! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
receiver
	"Answer the receiver of the selected context, if any. Answer nil 
	otherwise."

	contextStackIndex = 0
		ifTrue: [^nil]
		ifFalse: [^self selectedContext receiver]! !

!Debugger methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
receiverInspector
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context's receiver."

	^receiverInspector! !

!Debugger methodsFor: 'code pane' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
doItContext
	"Answer the context in which a text selection can be evaluated."

	contextStackIndex = 0
		ifTrue: [^super doItContext]
		ifFalse: [^self selectedContext]! !

!Debugger methodsFor: 'code pane' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	^self receiver! !

!Debugger methodsFor: 'context stack (message list)' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
contextStackIndex
	"Answer the index of the selected context."

	^contextStackIndex! !

!Debugger methodsFor: 'context stack (message list)' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
contextStackList
	"Answer the array of contexts."

	^contextStackList! !

!Debugger methodsFor: 'context stack (message list)' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
messageListIndex
	"Answer the index of the currently selected context."

	^contextStackIndex! !

!Debugger methodsFor: 'context stack (message list)' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toggleContextStackIndex: anInteger 
	"If anInteger is the same as the index of the selected context, deselect it. 
	Otherwise, the context whose index is anInteger becomes the selected 
	context."

	self contextStackIndex: 
		(contextStackIndex = anInteger
			ifTrue: [0]
			ifFalse: [anInteger])
		oldContextWas:
		(contextStackIndex = 0
			ifTrue: [nil]
			ifFalse: [contextStack at: contextStackIndex])! !

!Debugger methodsFor: 'context stack menu' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
selectPC
	"Toggle the flag telling whether to automatically select the expression 
	currently being executed by the selected context."

	selectingPC _ selectingPC not! !

!Debugger methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
externalInterrupt: aBoolean

	externalInterrupt _ aBoolean ! !

!Debugger methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
newStack: stack
	| oldStack diff |
	oldStack _ contextStack.
	contextStack _ stack.
	(oldStack == nil or: [oldStack last ~~ stack last])
		ifTrue: [contextStackList _ contextStack collect: [:ctx | ctx printString].
				^ self].
	"May be able to re-use some of previous list"
	diff _ stack size - oldStack size.
	contextStackList _ diff <= 0
		ifTrue: [contextStackList copyFrom: 1-diff to: oldStack size]
		ifFalse: [diff > 1
				ifTrue: [contextStack collect: [:ctx | ctx printString]]
				ifFalse: [(Array with: stack first printString) , contextStackList]]! !


!FileList methodsFor: 'file list' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fileList
	"Answer the list of files in the current volume."

	^ list! !

!FileList methodsFor: 'file list' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fileListIndex
	"Answer the index of the currently selected file."

	^ listIndex! !

!FileList methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
folderString
	^ ' [...]'! !


!Inspector methodsFor: 'selecting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
selectionIndex
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. Answer the index into the list of the selected variable."

	^selectionIndex! !


!ContextVariablesInspector methodsFor: 'code' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
doItContext

	^object! !

!ContextVariablesInspector methodsFor: 'code' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
doItReceiver

	^object receiver! !


!Color class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
r: r g: g b: b alpha: alpha
	^ (self r: r g: g b: b) alpha: alpha! !


!UndefinedObject methodsFor: 'dependents access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
suspend
	"Kills off processes that didn't terminate properly"
	"Display reverse; reverse."  "<-- So we can catch the suspend bug"
	Processor terminateActive! !


!Behavior methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
parserClass
	"Answer a parser class to use for parsing method headers."

	^self compilerClass parserClass! !

!Behavior methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
compile: code 
	"Compile the argument, code, as source code in the context of the 
	receiver. Create an error notification if the code can not be compiled. 
	The argument is either a string or an object that converts to a string or a 
	PositionableStream on an object that converts to a string."

	^self compile: code notifying: nil! !

!Behavior methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
recompileChanges
	"Compile all the methods that are in the changes file.
	This validates sourceCode and variable references and forces
	methods to use the current bytecode set"

	self selectorsDo:
		[:sel | (self compiledMethodAt: sel) fileIndex > 1 ifTrue:
			[self recompile: sel from: self]]! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
superclass
	"Answer the receiver's superclass, a Class."

	^superclass! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sharedPools
	"Answer a Set of the names of the pools (Dictionaries) that the receiver 
	shares.
	9/12/96 tk  sharedPools have an order now"

	^ OrderedCollection new! !

!Behavior methodsFor: 'testing class hierarchy' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
inheritsFrom: aClass 
	"Answer whether the argument, aClass, is on the receiver's superclass 
	chain."

	| aSuperclass |
	aSuperclass _ superclass.
	[aSuperclass == nil]
		whileFalse: 
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass _ aSuperclass superclass].
	^false! !

!Behavior methodsFor: 'testing method dictionary' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
allUnsentMessages
	"Answer an array of all the messages defined by the receiver that are not sent anywhere in the system.  5/8/96 sw"

	^ Smalltalk allUnSentMessagesIn: self selectors! !

!Behavior methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
allSubclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDo: 
		[:cl | 
		aBlock value: cl.
		cl allSubclassesDo: aBlock]! !

!Behavior methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
selectSuperclasses: aBlock 
	"Evaluate the argument, aBlock, with the receiver's superclasses as the 
	argument. Collect into an OrderedCollection only those superclasses for 
	which aBlock evaluates to true. In addition, evaluate aBlock for the 
	superclasses of each of these successful superclasses and collect into the 
	OrderedCollection ones for which aBlock evaluates to true. Answer the 
	resulting OrderedCollection."

	| aSet |
	aSet _ Set new.
	self allSuperclasses do: 
		[:aSuperclass | 
		(aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
	^aSet! !

!Behavior methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
withAllSubclassesDo: aBlock 
	"Evaluate the argument, aBlock, for the receiver and each of its 
	subclasses."

	aBlock value: self.
	self allSubclassesDo: aBlock! !


!ClassDescription methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^self! !

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
removeCategory: aString 
	"Remove each of the messages categorized under aString in the method 
	dictionary of the receiver. Then remove the category aString."
	| categoryName |
	categoryName _ aString asSymbol.
	(self organization listAtCategoryNamed: categoryName) do:
		[:sel | self removeSelector: sel].
	self organization removeCategory: categoryName! !

!ClassDescription methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
acceptsLoggingOfCompilation
	"weird name is so that it will come lexically before #compile, so that a clean build can make it through.  7/7/96 sw"

	^ true! !

!ClassDescription methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
compile: code notifying: requestor 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: ClassOrganizer default
		 notifying: requestor! !

!ClassDescription methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
wantsChangeSetLogging
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.  7/12/96 sw"


	^ true! !

!ClassDescription methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copy: sel from: class 
	"Install the method associated with the first argument, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under -As yet not 
	classified-."

	self copy: sel
		from: class
		classified: nil! !

!ClassDescription methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyCategory: cat from: class 
	"Specify that one of the categories of messages for the receiver is cat, as 
	found in the class, class. Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fileOutChangedMessages: aSet on: aFileStream 
	"File a description of the messages of the receiver that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	self fileOutChangedMessages: aSet
		on: aFileStream
		moveSource: false
		toFile: 0! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
methodsFor: aString priorSource: sourcePosition inFile: fileIndex
	"Prior source pointer ignored when filing in."
	^ self methodsFor: aString! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	self selectorsDo: [:sel | self reformatMethodAt: sel]! !

!ClassDescription methodsFor: 'instance variables' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance 
	variables. Create an error notification if the argument is not found."

	self subclassResponsibility! !

!ClassDescription methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name! !


!Class methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
hasMethods
	"Answer a Boolean according to whether any methods are defined for the 
	receiver (includes whether there are methods defined in the receiver's 
	metaclass)."

	^super hasMethods or: [self class hasMethods]! !

!Class methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
compileAllFrom: oldClass
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self class compileAllFrom: oldClass class! !

!Class methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	super reformatAll.		"me..."
	self class reformatAll	"...and my metaclass"! !

!Class methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
shouldFileOutPool: aPoolName
	"respond with true if the user wants to file out aPoolName"
	^self confirm: ('FileOut the sharedPool ', aPoolName, '?')! !

!Class methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
shouldFileOutPools
	"respond with true if the user wants to file out the shared pools"
	^self confirm: 'FileOut selected sharedPools?'! !


!Metaclass methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^thisClass! !

!Metaclass methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
acceptsLoggingOfCompilation
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself.  6/18/96 sw"

	^ thisClass acceptsLoggingOfCompilation! !

!Metaclass methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
possibleVariablesFor: misspelled continuedFrom: oldResults

	^ thisClass possibleVariablesFor: misspelled continuedFrom: oldResults
! !

!Metaclass methodsFor: 'compiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
wantsChangeSetLogging
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself.  7/12/96 sw"

	^ thisClass wantsChangeSetLogging! !


!Magnitude methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
min: aMin max: aMax 

	^ (self min: aMin) max: aMax! !


!Number methodsFor: 'intervals' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
to: stop do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue _ self.
	[nextValue <= stop]
		whileTrue: 
			[aBlock value: nextValue.
			nextValue _ nextValue + 1]! !

!Number methodsFor: 'mathematical functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
arcCos 
	"The receiver is the cosine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcCos! !

!Number methodsFor: 'mathematical functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
arcSin
	"The receiver is the sine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcSin! !

!Number methodsFor: 'mathematical functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
cos
	"The receiver represents an angle measured in radians. Answer its cosine."

	^self asFloat cos! !

!Number methodsFor: 'mathematical functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sin
	"The receiver represents an angle measured in radians. Answer its sine."

	^self asFloat sin! !

!Number methodsFor: 'mathematical functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
tan
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self asFloat tan! !

!Number methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printStringBase: base
	^ String streamContents:
		[:strm | self printOn: strm base: base]! !

!Number methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeStringBase: base
	^ String streamContents: [:strm | self storeOn: strm base: base]! !

!Number methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isNumber
	^ true! !


!Integer methodsFor: 'arithmetic' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
alignedTo: anInteger
	"Answer the smallest number not less than receiver that is a multiple of anInteger."

	^(self+anInteger-1//anInteger)*anInteger

"5 alignedTo: 2"
"12 alignedTo: 3"! !

!Integer methodsFor: 'bit manipulation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
<< shiftAmount  "left shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount! !

!Integer methodsFor: 'bit manipulation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
bitInvert32
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF! !

!Integer methodsFor: 'system primitives' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	| j |  "Catches failure if LgInt replace primitive fails"
	j _ repStart.
	start to: stop do:
		[:i |
		self digitAt: i put: (replacement digitAt: j).
		j _ j+1]! !

!Integer methodsFor: 'truncation and round off' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
normalize 
	"SmallInts OK; LgInts override"
	^ self! !

!Integer methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyto: x
	| stop |
	stop _ self digitLength min: x digitLength.
	^ x replaceFrom: 1 to: stop with: self startingAt: 1! !


!Integer class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readFrom: aStream 
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStream base: 10! !


!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
negated 
	^ (self copyto: (LargeNegativeInteger new: self digitLength))
		normalize  "Need to normalize to catch SmallInteger minVal"! !


!SmallInteger methodsFor: 'arithmetic' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
- aNumber 
	"Primitive. Subtract the argument from the receiver and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation
	whatIsAPrimitive."

	<primitive: 2>
	^super - aNumber! !

!SmallInteger methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
asFloat
	"Primitive. Answer a Float that represents the value of the receiver.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 40>
	self primitiveFailed! !


!LookupKey methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
hash
	"Hash is reimplemented because = is implemented."

	^key hash! !


!Association methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream
	"Store in the format (key->value)"
	aStream nextPut: $(.
	key storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)! !


!MessageTally methodsFor: 'collecting leaves' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
bump: hitCount
	tally _ tally + hitCount! !

!MessageTally methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isPrimitives
	"Detect pseudo node used to carry tally of local hits"
	^ receivers == nil! !


!Date methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printFormat: formatArray 
	"Answer a String describing the receiver using the format denoted by the 
	argument, formatArray."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream format: formatArray.
	^aStream contents! !

!Date methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream

	self printOn: aStream format: #(1 2 3 $  3 1 )! !


!ContextPart methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
client
	"Answer the client, that is, the object that sent the message that created this context."

	^sender receiver! !

!ContextPart methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
method
	"Answer the method of this context."

	self subclassResponsibility! !

!ContextPart methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
stackPtr  "For use only by the SystemTracer"
	^ stackp! !


!Decompiler methodsFor: 'control' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
blockTo: end
	"Decompile a range of code as in statementsTo:, but return a block node."
	| exprs block oldBase |
	oldBase _ blockStackBase.
	blockStackBase _ stack size.
	exprs _ self statementsTo: end.
	block _ constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase _ oldBase.
	lastReturnPc _ -1.  "So as not to mislead outer calls"
	^block! !

!Decompiler methodsFor: 'public access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
tempAt: offset
	"Needed by BraceConstructor<PopIntoTemporaryVariable"

	^tempVars at: offset + 1! !


!Message methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
argument
	"Answer the first (presumably sole) argument"

	^args at: 1! !

!Message methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sends: aSelector
	"answer whether this message's selector is aSelector"

	^selector == aSelector! !


!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
keyboard
	"Answer the next character from the keyboard."

	^ self characterForKeycode: self primKbdNext! !

!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
keyboardPeek
	"Answer the next character in the keyboard buffer without removing it, or nil if it is empty."

	^ self characterForKeycode: self primKbdPeek! !

!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
keyboardPressed
	"Answer true if keystrokes are available."

	^self primKbdPeek notNil! !

!InputSensor methodsFor: 'user interrupts' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setInterruptKey: anInteger
	"Register the given keycode as the user interrupt key."

	self primSetInterruptKey: anInteger.
! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primCursorLocPut: aPoint
	"If the primitive fails, try again with a rounded point."

	<primitive: 91>
	^ self primCursorLocPutAgain: aPoint rounded! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primCursorLocPutAgain: aPoint
	"Do nothing if primitive is not implemented."

	<primitive: 91>
	^ self! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primKbdNext
	<primitive: 108>
	^ nil! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primKbdPeek
	<primitive: 109>
	^ nil! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primMouseButtons
	<primitive: 107>
	^ 0! !


!ProcessorScheduler methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
highestPriority: newHighestPriority
	"Change the number of priority levels currently available for use."

	| continue newProcessLists |
	(quiescentProcessLists size > newHighestPriority
		and: [self anyProcessesAbove: newHighestPriority])
			ifTrue: [self error: 'There are processes with priority higher than '
													,newHighestPriority printString].
	newProcessLists _ Array new: newHighestPriority.
	1 to: ((quiescentProcessLists size) min: (newProcessLists size)) do: 
		[:priority | newProcessLists at: priority put: (quiescentProcessLists at: priority)].
	quiescentProcessLists size to: newProcessLists size do: 
		[:priority | newProcessLists at: priority put: LinkedList new].
	quiescentProcessLists _ newProcessLists! !


!Collection methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
associationsDo: aBlock
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it."

	self do: aBlock! !

!Collection methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
collect: collectBlock thenSelect: selectBlock
	^ (self collect: collectBlock) select: selectBlock! !

!Collection methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
select: selectBlock thenCollect: collectBlock
	^ (self select: selectBlock) collect: collectBlock! !

!Collection methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
includesAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true! !

!Collection methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
includesAnyOf: aCollection 
	"Answer whether any element of aCollection is one of the receiver's elements."
	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first _ sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index _ 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index _ index+1]]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyReplaceAll: oldSubstring with: newSubstring 
	"Default is not to do token matching.
	See also String copyReplaceTokens:with:"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: false
	"'How now brown cow?' copyReplaceAll: 'ow' with: 'ello'"
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Pile'"! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
collect: aBlock 
	"Refer to the comment in Collection|collect:."
	| result |
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put: (aBlock value: (self at: index))].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reverseWith: aSequenceableCollection do: aBlock 
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  
	corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorNoMatch].
	self size
		to: 1
		by: -1
		do: [:index | aBlock value: (self at: index)
				value: (aSequenceableCollection at: index)]! !


!String methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1! !

!String methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"! !

!String methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
correctAgainst: wordList
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results _ self correctAgainst: wordList continuedFrom: nil.
	results _ self correctAgainst: nil continuedFrom: results.
	^ results! !

!String methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
truncateTo: smallSize
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]! !

!String methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x _ self at: i).
		x == $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !


!Symbol class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (aCollection as: String) asSymbol

"	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"! !


!CompiledMethod methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
numTemps
	"Answer the number of temporary variables used by the receiver."
	
	^ (self header bitShift: -18) bitAnd: 16r3F! !

!CompiledMethod methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
returnField
	"Answer the index of the instance variable returned by a quick return 
	method."
	| prim |
	prim _ self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]! !

!CompiledMethod methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isReturnSelf
	"Answer whether the receiver is a quick return of self."

	^ self primitive = 256! !

!CompiledMethod methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isReturnSpecial
	"Answer whether the receiver is a quick return of self or constant."

	^ self primitive between: 256 and: 263! !

!CompiledMethod methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeLiteralsOn: aStream forClass: aBehavior
	"Store the literals referenced by the receiver on aStream, each terminated by a space."

	| literal |
	2 to: self numLiterals + 1 do:
		[:index |
		 aBehavior storeLiteral: (self objectAt: index) on: aStream.
		 aStream space]! !

!CompiledMethod methodsFor: 'literals' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
literals
	"Answer an Array of the literals referenced by the receiver."
	| literals numberLiterals |
	literals _ Array new: (numberLiterals _ self numLiterals).
	1 to: numberLiterals do:
		[:index |
		literals at: index put: (self objectAt: index + 1)].
	^literals! !


!Bitmap methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
bitPatternForDepth: depth
	"The raw call on BitBlt needs a Bitmap to represent this color.  I already am Bitmap like.  I am already adjusted for a specific depth.  Interpret me as an array of (32/depth) Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary. 6/18/96 tk"

	^ self! !


!Semaphore methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
terminateProcess
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].! !


!OrderedCollection methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
at: anInteger 
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	(anInteger < 1 or: [anInteger + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^ array at: anInteger + firstIndex - 1]! !

!OrderedCollection methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
at: anInteger put: anObject 
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	index _ anInteger asInteger.
	(index < 1 or: [index + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^array at: index + firstIndex - 1 put: anObject]! !

!OrderedCollection methodsFor: 'adding' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
addFirst: newObject 
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex _ firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
addLast: newObject 
	"Add newObject to the end of the receiver. Answer newObject."

	lastIndex = array size ifTrue: [self makeRoomAtLast].
	lastIndex _ lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
growSize
	^ array size max: 2! !

!OrderedCollection methodsFor: 'removing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
removeFirst
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject _ array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex _ firstIndex + 1.
	^ firstObject! !

!OrderedCollection methodsFor: 'removing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
removeLast
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject _ array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex _ lastIndex - 1.
	^ lastObject! !


!OrderedCollection class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new: anInteger 
	"If a subclass adds fields, then it is necessary for that subclass to
	reimplement new:."

	^ super new setCollection: (Array new: anInteger)! !


!RunArray methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
	| size |
	size _ 0.
	1 to: runs size do: [:i | size _ size + (runs at: i)].
	^size! !

!RunArray methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
at: index setRunOffsetAndValue: aBlock 
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit _ runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue:  "cache not loaded, or beyond index - start over"
			[run _ 1.
			offset _ index-1]
		ifFalse:  "cache loaded and before index - start at cache"
			[run _ lastRun.
			offset _ lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset _ offset - (runs at: run).
			run _ run + 1].
	lastIndex _ index.  "Load cache for next access"
	lastRun _ run.
	lastOffset _ offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run _ run - 1.
			offset _ offset + (runs at: run)].
	^aBlock
		value: run	"an index into runs and values"
		value: offset	"zero-based offset from beginning of this run"
		value: (values at: run)	"value for this run"! !


!RunArray class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection _ self new.
	aCollection do: [:x | newCollection addLast: x].
	^newCollection

"	RunArray newFrom: {1. 2. 2. 3}
	{1. $a. $a. 3} as: RunArray
	({1. $a. $a. 3} as: RunArray) values
"! !


!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
array
	^ array! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
growSize
	^ array size max: 2! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
init: n
	"Initialize array to an array size of n"
	array _ Array new: n.
	tally _ 0! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noCheckAdd: anObject
	array at: (self findElementOrNil: anObject) put: anObject.
	tally _ tally + 1! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
rehash
	| newSelf |
	newSelf _ self species new: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array _ newSelf array! !

!Set methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
withArray: anArray
	"private -- for use only in copy"
	array _ anArray! !


!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new
	^ self new: 4! !

!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
new: nElements
	"Create a Set large enough to hold nElements without growing"
	^ super new init: (self sizeFor: nElements)! !

!Set class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sizeFor: nElements
	"Large enough size to hold nElements with some slop (see fullCheck)"
	nElements <= 0 ifTrue: [^ 1].
	^ nElements+1*4//3! !


!Dictionary methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc! !

!Dictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noCheckAdd: anObject
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally _ tally + 1! !

!Dictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
rehash
	"Smalltalk rehash."
	| newSelf |
	newSelf _ self species new: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array _ newSelf array! !

!Dictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary 
	"Support for coordinating class variable and global declarations
	with variables that have been put in Undeclared so as to
	redirect all references to the undeclared variable."

	(aDictionary includesKey: aKey)
		ifTrue: 
			[self atNewIndex: index 
				put: ((aDictionary associationAt: aKey) value: anObject).
			aDictionary removeKey: aKey]
		ifFalse: 
			[self atNewIndex: index put: (Association key: aKey value: anObject)]! !


!MethodDictionary methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
add: anAssociation
	^ self at: anAssociation key put: anAssociation value! !

!MethodDictionary methodsFor: 'removing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
removeKey: key ifAbsent: errorBlock 
	"The interpreter might be using this MethodDict while
	this method is running!!  Therefore we perform the removal
	in a copy, and then atomically become that copy"
	| copy |
	copy _ self copy.
	copy removeDangerouslyKey: key ifAbsent: [^ errorBlock value].
	self become: copy! !

!MethodDictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
keyAt: index

	^ self basicAt: index! !

!MethodDictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
methodArray
	^ array! !

!MethodDictionary methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
swap: oneIndex with: otherIndex
	| element |
	element _ self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	super swap: oneIndex with: otherIndex.
! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
verifyChanges		"Smalltalk verifyChanges"
	"Recompile all methods in the changes file."
	Smalltalk allBehaviorsDo: [:class | class recompileChanges].
! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
imageName: newName
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
bytesLeft
	"Answer the number of bytes of space available. Does a full garbage collection."

	^ self garbageCollect
! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
createStackOverflow
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; createStackOverflow"

	self createStackOverflow.  "infinite recursion"! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
garbageCollectMost
	"Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space."

	<primitive: 131>
	^ self primBytesLeft! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primBytesLeft
	"Primitive. Answer the number of bytes available for new object data.
	Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^ 0! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primLowSpaceSemaphore: aSemaphore
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
primSignalAtBytesLeft: numBytes
	"Tell the interpreter the low-space threshold in bytes. When the free
	space falls below this threshold, the interpreter will signal the low-space
	semaphore, if one has been registered.  Disable low-space interrupts if the
	argument is zero.  Fail if numBytes is not an Integer."

	<primitive: 125>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
signalLowSpace
	"Signal the low-space semaphore to alert the user that space is running low."

	LowSpaceSemaphore signal.! !

!SystemDictionary methodsFor: 'profiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
clearProfile
	"Clear the profile database."

	<primitive: 250>
! !

!SystemDictionary methodsFor: 'profiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
dumpProfile
	"Dump the profile database to a file."

	<primitive: 251>
! !

!SystemDictionary methodsFor: 'profiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
profile: aBlock
	"Make a virtual machine profile of the given block."
	"Note: Profiling support is provided so that VM implementors
	 can better understand and improve the efficiency of the virtual
	 machine. To use it, you must be running a version of the
	 virtual machine compiled with profiling enabled (which
	 makes it much slower than normal even when not profiling).
	 You will also need the CodeWarrior profile reader application."

	self stopProfiling.
	self clearProfile.
	self startProfiling.
	aBlock value.
	self stopProfiling.
	self dumpProfile.! !

!SystemDictionary methodsFor: 'profiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
startProfiling
	"Start profiling the virtual machine."

	<primitive: 252>
! !

!SystemDictionary methodsFor: 'profiling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
stopProfiling
	"Stop profiling the virtual machine."

	<primitive: 253>
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
lastQuitLogPosition
	^ LastQuitLogPosition! !

!SystemDictionary methodsFor: 'special objects' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
compactClassesArray  "Smalltalk compactClassesArray"
	"Return the array of 31 classes whose instances may be
	represented compactly"
	^ Smalltalk specialObjectsArray at: 29! !

!SystemDictionary methodsFor: 'special objects' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
specialObjectsArray  "Smalltalk specialObjectsArray at: 1"
	<primitive: 129>
	^ self primitiveFailed! !

!SystemDictionary methodsFor: 'browsing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
browseAllCallsOn: literal1 and: literal2 
	"Create and schedule a message browser on each method that calls on the 
	two Symbols, literal1 and literal2. For example, Smalltalk 
	browseAllCallsOn: #at: and: #at:put:."

	^self 
		browseMessageList: (self allCallsOn: literal1 and: literal2)
		name: literal1 printString , ' -and- ' , literal2 printString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
browseAllImplementorsOf: selector
	"Create and schedule a message browser on each method that implements 
	the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put:."

	^self browseMessageList: (self allImplementorsOf: selector) name: 'Implementors of ' , selector! !

!SystemDictionary methodsFor: 'browsing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
browseAllImplementorsOfList: selectorList
	"Create and schedule a message browser on each method that implements 
	the message whose selector is in the argument selectorList. For example, 
	Smalltalk browseAllImplementorsOf: #(at:put: size).
	1/16/96 sw: defer to the titled version"

	self browseAllImplementorsOfList: selectorList title: 'Implementors of all'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
browseAllSelect: aBlock
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example, 
	Smalltalk browseAllSelect: [:method | method numLiterals > 10]."

	^self browseMessageList: (self allSelect: aBlock) name: 'selected messages'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
browseMessageList: messageList name: label 
	"Create and schedule a MessageSet browser on messageList."
	^ self browseMessageList: messageList name: label autoSelect: nil! !


!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
next
	"Answer the next object accessible by the receiver."

	self subclassResponsibility! !

!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
next: anInteger 
	"Answer the next anInteger number of objects accessible by the receiver."

	| aCollection |
	aCollection _ OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection! !

!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
next: anInteger put: anObject 
	"Make anObject be the next anInteger number of objects accessible by the 
	receiver. Answer anObject."

	anInteger timesRepeat: [self nextPut: anObject].
	^anObject! !

!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextMatchAll: aColl
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save _ self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true! !

!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	self subclassResponsibility! !

!Stream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection! !

!Stream methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
atEnd
	"Answer whether the receiver can access any more objects."

	self subclassResponsibility! !

!Stream methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
do: aBlock 
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]! !


!PositionableStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection! !

!PositionableStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	position _ 0! !

!PositionableStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setToEnd
	"Set the position of the receiver to the end of the sequence of objects."

	position _ readLimit! !

!PositionableStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
skipTo: anObject 
	"Set the access position of the receiver to be past the next occurrence of 
	anObject. Answer whether anObject is found."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
unCommand
	"If this read stream is at a <, then skip up to just after the next >.  For removing html commands."
	| char |
	[self peek = $<] whileTrue: ["begin a block"
		[self atEnd == false and: [self next ~= $>]] whileTrue.
		"absorb characters"
		].
 ! !

!PositionableStream methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setFrom: newStart to: newStop

	position _ newStart - 1.
	readLimit _ newStop! !


!WriteStream methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextChunkPut: aString
	"Append the argument, aString, to the receiver, doubling embedded terminators."

	| i remainder terminator |
	terminator _ $!!.
	remainder _ aString.
	[(i _ remainder indexOf: terminator) = 0] whileFalse:
		[self nextPutAll: (remainder copyFrom: 1 to: i).
		self nextPut: terminator.  "double imbedded terminators"
		remainder _ remainder copyFrom: i+1 to: remainder size].
	self nextPutAll: remainder; nextPut: terminator! !


!WriteStream class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
on: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me on a copy of the argument, aCollection, 
	determined by the indices firstIndex and lastIndex. Position the instance 
	at the beginning of the collection."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex! !

!WriteStream class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
with: aCollection 
	"Answer an instance of me on the argument, aCollection, positioned to 
	store objects at the end of aCollection."

	^self basicNew with: aCollection! !

!WriteStream class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
with: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me on the subcollection of the argument, 
	aCollection, determined by the indices firstIndex and lastIndex. Position 
	the instance to store at the end of the subcollection."

	^self basicNew with: (aCollection copyFrom: firstIndex to: lastIndex)! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
name
	^ 'a stream'   "for fileIn compatibility"! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fileNameEndsWith: aString
	"See comment in FileStream fileNameEndsWith:"

	^false! !


!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPut: aByte
	"1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPutAll: aCollection
	"1/31/96 sw: made subclass responsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
	"Answer the size of the file in characters.
	 1/31/96 sw: made subclass responsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
atEnd
	"Answer true if the current position is >= the end of file position.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
position
	"Answer the current character position in the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
position: pos
	"Set the current character position in the file to pos.
	 1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reset
	"Set the current character position to the beginning of the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setToEnd
	"Set the current character position to the end of the File. The same as
	self position: self size.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file
	1/31/96 sw: made subclassResponsibility."
 
	self subclassResponsibility! !

!FileStream methodsFor: 'file accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
name
	"Answer the name of the file for the page the receiver is streaming over.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !


!StandardFileStream methodsFor: 'open/close' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
openReadOnly
	"Open the receiver as a read-only file.  1/31/96 sw"

	^ self open: name forWrite: false! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isBinary
	^ buffer1 class == ByteArray! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isDirectory
	"Answer whether the receiver represents a directory.  For the post-transition case, uncertain what to do.  2/14/96 sw"
	^ false! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
	"Answer the size of the file in characters.  2/12/96 sw"

	^ self primSize: fileID! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setToEnd
	"Set the position of the receiver to the end of file.  1/31/96 sw"

	self position: self size! !


!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
beginReference: anObject
    "We’re starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
getCurrentReference
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noteCurrentReference: typeID
    "PRIVATE -- If we support references for type typeID, remember
     the current byteStream position so we can add the next object to
     the ‘objects’ dictionary, and return true. Else return false.
     This method is here to be overridden by ReferenceStream"

    ^ false! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readBitmap
	"PRIVATE -- Read the contents of a Bitmap."

	^ Bitmap newFromStream: byteStream
	"Note that the reader knows that the size is in long words, but the data is in bytes."! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readFalse
    "PRIVATE -- Read the contents of a False."

    ^ false! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readNil
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readString asSymbol! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readTrue
    "PRIVATE -- Read the contents of a True."

    ^ true! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
tryToPutReference: anObject typeID: typeID
    "PRIVATE -- If we support references for type typeID, and if
       anObject already appears in my output stream, then put a
       reference to the place where anObject already appears. If we
       support references for typeID but didn’t already put anObject,
       then associate the current stream position with anObject in
       case one wants to nextPut: it again.
     Return true after putting a reference; false if the object still
       needs to be put.
     For DataStream this is trivial. ReferenceStream overrides this."

    ^ false! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeFalse: aFalse
    "PRIVATE -- Write the contents of a False."! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeInstance: anObject
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeNil: anUndefinedObject
    "PRIVATE -- Write the contents of an UndefinedObject."! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeTrue: aTrue
    "PRIVATE -- Write the contents of a True."! !

!DataStream methodsFor: 'write and read' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
writeUser: anObject
    "Write the contents of an arbitrary User instance (and its devoted class)."
    " 7/29/96 tk"

	"If anObject is an instance of a unique user class, will lie and say it has a generic class"
    ^ anObject storeDataOn: self! !

!DataStream methodsFor: 'other' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
atEnd
    "Answer true if the stream is at the end."

    ^ byteStream atEnd! !

!DataStream methodsFor: 'other' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
flush
    "Guarantee that any writes to me are actually recorded on disk. -- 11/17/92 jhm"

    ^ byteStream flush! !

!DataStream methodsFor: 'other' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
next: anInteger
    "Answer an Array of the next anInteger objects in the stream."
    | array |

    array _ Array new: anInteger.
    1 to: anInteger do: [:i |
        array at: i put: self next].
    ^ array! !

!DataStream methodsFor: 'other' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reset
    "Reset the stream."

    byteStream reset! !

!DataStream methodsFor: 'other' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
    "Answer the stream's size."

    ^ byteStream size! !


!DummyStream methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
subclassResponsibility
	"Do nothing.  Most messages to class Stream are defined as subclassResponsibility.  Just accept them.  8/17/96 tk"

	"No error.  Just go on."! !

!DummyStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
position
	"Return any random number.  Here is where the real lying begins.  We are a DummyStream afterall.  8/17/96 tk"

	^ 47 ! !


!Compiler methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
notify: aString 
	"Refer to the comment in Object|notify:."

	^self notify: aString at: sourceStream position + 1! !


!Compiler class methodsFor: 'evaluating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
evaluate: textOrString for: anObject logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag! !

!Compiler class methodsFor: 'evaluating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
evaluate: textOrString logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag! !


!Scanner methodsFor: 'expression types' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
advance

	| prevToken |
	prevToken _ token.
	self scanToken.
	^prevToken! !

!Scanner methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source _ inputStream.
	self step.
	self step.
	self scanToken! !


!Parser methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
notify: aString 
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset! !

!Parser methodsFor: 'scanning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
startOfNextToken
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark! !

!Parser methodsFor: 'primitives' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
allocateLiteral: lit
	encoder litIndex: lit! !

!Parser methodsFor: 'error correction' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
substituteSelector: selectorParts wordIntervals: spots
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset _ 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset _ self substituteWord: word wordInterval: interval offset: offset ]
! !


!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
assignmentCheck: encoder at: location
	"For messageNodes masquerading as variables for the debugger.
	For now we let this through - ie we allow stores ev
	into args.  Should check against numArgs, though."
	^ -1! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isComplex
	"Used for pretty printing to determine whether to start a new line"

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isConstantNumber  "Overridden in LiteralNode"
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"See comment in MessageNode."

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isSpecialConstant
	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toDoIncrement: ignored
	"Only meant for Messages or Assignments - else return nil"
	^ nil! !


!Encoder methodsFor: 'results' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
literals
	"Should only be used for decompiling primitives"
	^ literalStream contents! !

!Encoder methodsFor: 'encoding' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
encodeLiteral: object

	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet! !

!Encoder methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
classEncoding
	"This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view."
	^ class! !

!Encoder methodsFor: 'private' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reallyBind: name

	| node |
	node _ self newTemp: name.
	scopeTable at: name put: node.
	^node! !

!Encoder methodsFor: 'error handling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
notify: string at: location

	| req |
	requestor == nil
		ifFalse: 
			[req _ requestor.
			self release.
			req notify: string at: location].
	^false! !


!AssignmentNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
variable
	^variable! !

!AssignmentNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toDoIncrement: var
	var = variable ifFalse: [^ nil].
	(value isMemberOf: MessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]! !

!AssignmentNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
value
	^ value! !


!BraceNode methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
casesForwardDo: aBlock
	"For each case in forward order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	1 to: (numCases _ elements size) do:
		[:i |
		case _ elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]! !

!BraceNode methodsFor: 'enumerating' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
casesReverseDo: aBlock
	"For each case in reverse order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	(numCases _ elements size) to: 1 by: -1 do:
		[:i |
		case _ elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]! !

!BraceNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
elements: collection
	"Decompile."

	elements _ collection! !

!BraceNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
elements: collection sourceLocations: locations
	"Compile."

	elements _ collection.
	sourceLocations _ locations! !

!BraceNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
numElements

	^ elements size! !


!CascadeNode methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream indent: level
	self printOn: aStream indent: level precedence: 0! !

!CascadeNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
receiver: receivingObject messages: msgs
	" Transcript show: 'abc'; cr; show: 'def' "

	receiver _ receivingObject.
	messages _ msgs! !


!BlockNode methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
firstArgument
	^ arguments first! !

!BlockNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
statements
	^statements! !

!BlockNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
statements: val
	statements _ val! !

!BlockNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isJustCaseError

	^ statements size = 1 and:
		[statements first
			isMessage: #caseError
			receiver: [:r | r==NodeSelf]
			arguments: nil]! !

!BlockNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isQuick
	^ statements size = 1
		and: [statements first isVariableReference
				or: [statements first isSpecialConstant]]! !


!MethodNode methodsFor: 'code generation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
encoder
	^ encoder! !


!LiteralNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isConstantNumber
	^ key isNumber! !

!LiteralNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isSpecialConstant
	^ code between: LdTrue and: LdMinus1+3! !

!LiteralNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
literalValue

	^key! !


!SelectorNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isPvtSelector
	"Answer if this selector node is a private message selector."

	^key isPvtSelector! !


!VariableNode methodsFor: 'initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
asStorableNode: encoder
	^ self! !


!MessageNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
arguments
	^arguments! !

!MessageNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
receiver
	^receiver! !

!MessageNode methodsFor: 'equation translation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
selector
	^selector! !

!MessageNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred
	 evaluate to true with respect to receiver and the list of arguments.  If selSymbol or
	 either predicate is nil, it means 'don't care'.  Note that argsPred takes numArgs
	 arguments.  All block arguments are ParseNodes."

	^(selSymbol isNil or: [selSymbol==selector key]) and:
		[(rcvrPred isNil or: [rcvrPred value: receiver]) and:
			[(argsPred isNil or: [argsPred valueWithArguments: arguments])]]! !

!MessageNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toDoIncrement: variable
	(receiver = variable and: [selector key = #+]) 
		ifFalse: [^ nil].
	arguments first isConstantNumber
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]! !

!MessageNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toDoLimit: variable
	(receiver = variable and: [selector key = #<= or: [selector key = #>=]]) 
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]! !


!ReturnNode methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isSpecialConstant

	^expr isSpecialConstant! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
codeBrace: elements

	^BraceNode new elements: elements! !


!ParseStack methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream
	
	super printOn: aStream.
	aStream nextPutAll: ' at '; print: position; nextPutAll: ' of '; print: length! !


!DiskProxy class methodsFor: 'as yet unclassified' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
global: globalNameSymbol selector: selectorSymbol args: argArray
    "Create a new DiskProxy constructor with the given
     globalNameSymbol, selectorSymbol, and argument Array.
     It will internalize itself by looking up the global object name
     in the SystemDictionary (Smalltalk) and sending it this message
     with these arguments."

    ^ self new global: globalNameSymbol
             selector: selectorSymbol
                 args: argArray! !


!Form methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
extent
	^ width @ height! !

!Form methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
height
	^ height! !

!Form methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
width
	^ width! !

!Form methodsFor: 'analyzing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
innerPixelRectFor: pv orNot: not
	"Return a rectangle describing the smallest part of me that includes 
	all pixels of value pv.
	Note:  If orNot is true, then produce a copy that includes all pixels
	that are DIFFERENT from the supplied (background) value"

	| xTally yTally |
	xTally _ self xTallyPixelValue: pv orNot: not.
	yTally _ self yTallyPixelValue: pv orNot: not.
	^ ((xTally findFirst: [:t | t>0]) - 1) @ ((yTally findFirst: [:t | t>0]) - 1)
		corner:
			(xTally findLast: [:t | t>0])@(yTally findLast: [:t | t>0])! !

!Form methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream

	self storeOn: aStream base: 10! !

!Form methodsFor: 'filling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fillFromXColorBlock: colorBlock
	"Horizontal Gradient Fill.
	Supply relative x in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| xRel |
	0 to: width-1 do:
		[:x |  xRel _ x asFloat / (width-1) asFloat.
		self fill: (x@0 extent: 1@height) 
			fillColor: (colorBlock value: xRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromXColorBlock: [:x | Color r: x g: 0.0 b: 0.5]) display
"! !

!Form methodsFor: 'filling' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fillFromYColorBlock: colorBlock
	"Vertical Gradient Fill.
	Supply relative y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| yRel |
	0 to: height-1 do:
		[:y |  yRel _ y asFloat / (height-1) asFloat.
		self fill: (0@y extent: width@1) 
			fillColor: (colorBlock value: yRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromYColorBlock: [:y | Color r: y g: 0.0 b: 0.5]) display
"! !


!Form class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fromDisplay: aRectangle 
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen defined by aRectangle."

	^ (self extent: aRectangle extent depth: Display depth)
		fromDisplay: aRectangle! !

!Form class methodsFor: 'mode constants' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
blend
	"Answer the integer denoting BitBlt's alpha blend combination rule."
	^24! !


!Cursor class methodsFor: 'class initialization' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
startUp
	self currentCursor: self currentCursor! !


!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
clipRect
	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
combinationRule: anInteger 
	"Set the receiver's combination rule to be the argument, anInteger, a 
	number in the range 0-15."

	combinationRule _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
destForm
	^ destForm! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
destX: anInteger 
	"Set the top left x coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destX _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
destY: anInteger 
	"Set the top left y coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destY _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
height: anInteger 
	"Set the receiver's destination form height to be the argument, anInteger."

	height _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sourceForm: aForm 
	"Set the receiver's source form to be the argument, aForm."

	sourceForm _ aForm! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sourceOrigin: aPoint 
	"Set the receiver's source form coordinates to be those of the argument, 
	aPoint."

	sourceX _ aPoint x.
	sourceY _ aPoint y! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sourceRect: aRectangle 
	"Set the receiver's source form top left x and y, width and height to be 
	the top left coordinate and extent of the argument, aRectangle."

	sourceX _ aRectangle left.
	sourceY _ aRectangle top.
	width _ aRectangle width.
	height _ aRectangle height! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sourceX: anInteger 
	"Set the receiver's source form top left x to be the argument, anInteger."

	sourceX _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
sourceY: anInteger 
	"Set the receiver's source form top left y to be the argument, anInteger."

	sourceY _ anInteger! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
width: anInteger 
	"Set the receiver's destination form width to be the argument, anInteger."

	width _ anInteger! !

!BitBlt methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyForm: srcForm to: destPt rule: rule fillColor: color
	sourceForm _ srcForm.
	self fillColor: color.	"sets halftoneForm"
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	self copyBits! !

!BitBlt methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
fill: destRect fillColor: grayForm rule: rule
	"Fill with a Color, not a Form. 6/18/96 tk"
	sourceForm _ nil.
	self fillColor: grayForm.		"sets halftoneForm"
	combinationRule _ rule.
	destX _ destRect left.
	destY _ destRect top.
	sourceX _ 0.
	sourceY _ 0.
	width _ destRect width.
	height _ destRect height.
	self copyBits! !


!BitBlt class methodsFor: 'instance creation' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
toForm: aForm
	^ self new setDestForm: aForm! !


!Point methodsFor: 'arithmetic' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
abs
	"Answer a Point whose x and y are the absolute values of the receiver's x 
	and y."

	^ x abs @ y abs! !

!Point methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
max: aPoint 
	"Answer the lower right corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x max: aPoint x) @ (y max: aPoint y)! !

!Point methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
min: aPoint 
	"Answer the upper left corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x min: aPoint x) @ (y min: aPoint y)! !

!Point methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
min: aMin max: aMax 

	^ (self min: aMin) max: aMax! !

!Point methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
asIntegerPoint
	^ x asInteger @ y asInteger! !

!Point methodsFor: 'point functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
normal
	"Answer a Point representing the unit vector rotated 90 deg clockwise."

	| n |
	n _ y negated @ x.
	^n / (n x * n x + (n y * n y)) sqrt! !

!Point methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream 
	"x@y printed form is good for storing too"
	self printOn: aStream! !

!Point methodsFor: 'truncation and round off' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
truncateTo: grid
	"Answer a Point that is the receiver's x and y truncated to grid x and 
	grid y."
	| gridPoint |
	gridPoint _ grid asPoint.
	^(x truncateTo: gridPoint x) @ (y truncateTo: gridPoint y)! !


!Rectangle methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
boundingBox
	^ self! !

!Rectangle methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
corners
	"Return an array of corner points in the order of a quadrilateral spec for WarpBlt"
	^ Array with: self topLeft with: self bottomLeft with: self bottomRight with: self topRight! !

!Rectangle methodsFor: 'comparing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
hash
	"Hash is reimplemented because = is implemented."

	^origin hash bitXor: corner hash! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
amountToTranslateWithin: aRectangle
	"Answer a Point, delta, such that self + delta is forced within aRectangle."
	"Altered so as to prefer to keep self topLeft inside when all of self
	cannot be made to fit 7/27/96 di"
	| dx dy |
	dx _ 0.  dy _ 0.
	self right > aRectangle right ifTrue: [dx _ aRectangle right - self right].
	self bottom > aRectangle bottom ifTrue: [dy _ aRectangle bottom - self bottom].
	(self left + dx) < aRectangle left ifTrue: [dx _ aRectangle left - self left].
	(self top + dy) < aRectangle top ifTrue: [dy _ aRectangle top - self top].
	^ dx@dy! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
encompass: aPoint 
	"Answer a Rectangle that contains both the receiver and aPoint.  5/30/96 sw"

	^ Rectangle 
		origin: (origin min: aPoint)
		corner: (corner max:  aPoint)! !

!Rectangle methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
hasPositiveExtent
	^ (corner x > origin x) and: [corner y > origin y]! !

!Rectangle methodsFor: 'truncation and round off' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
truncateTo: grid
	"Answer a Rectangle whose origin and corner are truncated to grid x and grid y."

	^Rectangle origin: (origin truncateTo: grid)
				corner: (corner truncateTo: grid)! !


!StrikeFont methodsFor: 'emphasis' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
emphasis
	"Answer the integer code for synthetic bold, italic, underline, and 
	strike-out."

	^emphasis! !


!ChangeSet methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
name: anObject
	name _ anObject! !

!ChangeSet methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream
	"2/7/96 sw: provide the receiver's name in the printout"
	super printOn: aStream.
	aStream nextPutAll: ' named ', self name! !

!ChangeSet methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isMoribund
	"Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter.  2/7/96 sw"

	^ name == nil ! !


!PopUpMenu methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
labelString

	^ labelString! !

!PopUpMenu methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
lineArray

	^ lineArray! !

