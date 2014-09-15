'From Cuis 4.2 of 25 July 2013 [latest update: #1780] on 30 July 2013 at 1:13:44.634386 pm'!
'Description Please enter a description for this package '!
!provides: 'PluginMaker' 1 0!
!requires: 'SqueakCompatibility' 1 nil!
!requires: 'CompatibilityForPluginMakerForCuis' 1 nil!
!classDefinition: #Oop category: #'PluginMaker-SmartSyntaxPlugins'!
Behavior subclass: #Oop
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-SmartSyntaxPlugins'!
!classDefinition: 'Oop class' category: #'PluginMaker-SmartSyntaxPlugins'!
Oop class
	instanceVariableNames: ''!

!classDefinition: #Unsigned category: #'PluginMaker-SmartSyntaxPlugins'!
Behavior subclass: #Unsigned
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-SmartSyntaxPlugins'!
!classDefinition: 'Unsigned class' category: #'PluginMaker-SmartSyntaxPlugins'!
Unsigned class
	instanceVariableNames: ''!

!classDefinition: #CArray category: #'PluginMaker-InterpreterSimulation'!
Object subclass: #CArray
	instanceVariableNames: 'interpreter arrayBaseAddress ptrOffset unitSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'CArray class' category: #'PluginMaker-InterpreterSimulation'!
CArray class
	instanceVariableNames: ''!

!classDefinition: #BalloonArray category: #'PluginMaker-InterpreterSimulation'!
CArray subclass: #BalloonArray
	instanceVariableNames: 'simArray'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'BalloonArray class' category: #'PluginMaker-InterpreterSimulation'!
BalloonArray class
	instanceVariableNames: ''!

!classDefinition: #CCodeGenerator category: #'PluginMaker-Translation to C'!
Object subclass: #CCodeGenerator
	instanceVariableNames: 'translationDict inlineList constants variables variableDeclarations methods preparedMethodList variablesSetCache headerFiles globalVariableUsage useSymbolicConstants generateDeadCode doNotRemoveMethodList'
	classVariableNames: 'UseRightShiftForDivide'
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'CCodeGenerator class' category: #'PluginMaker-Translation to C'!
CCodeGenerator class
	instanceVariableNames: ''!

!classDefinition: #CCodeGeneratorGlobalStructure category: #'PluginMaker-Translation to C'!
CCodeGenerator subclass: #CCodeGeneratorGlobalStructure
	instanceVariableNames: 'localStructDef'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'CCodeGeneratorGlobalStructure class' category: #'PluginMaker-Translation to C'!
CCodeGeneratorGlobalStructure class
	instanceVariableNames: ''!

!classDefinition: #VMPluginCodeGenerator category: #'PluginMaker-Translation to C'!
CCodeGenerator subclass: #VMPluginCodeGenerator
	instanceVariableNames: 'pluginName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'VMPluginCodeGenerator class' category: #'PluginMaker-Translation to C'!
VMPluginCodeGenerator class
	instanceVariableNames: ''!

!classDefinition: #SmartSyntaxPluginCodeGenerator category: #'PluginMaker-SmartSyntaxPlugins'!
VMPluginCodeGenerator subclass: #SmartSyntaxPluginCodeGenerator
	instanceVariableNames: 'debugFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-SmartSyntaxPlugins'!
!classDefinition: 'SmartSyntaxPluginCodeGenerator class' category: #'PluginMaker-SmartSyntaxPlugins'!
SmartSyntaxPluginCodeGenerator class
	instanceVariableNames: ''!

!classDefinition: #CObjectAccessor category: #'PluginMaker-InterpreterSimulation'!
Object subclass: #CObjectAccessor
	instanceVariableNames: 'object offset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'CObjectAccessor class' category: #'PluginMaker-InterpreterSimulation'!
CObjectAccessor class
	instanceVariableNames: ''!

!classDefinition: #CArrayAccessor category: #'PluginMaker-InterpreterSimulation'!
CObjectAccessor subclass: #CArrayAccessor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'CArrayAccessor class' category: #'PluginMaker-InterpreterSimulation'!
CArrayAccessor class
	instanceVariableNames: ''!

!classDefinition: #CPluggableAccessor category: #'PluginMaker-InterpreterSimulation'!
CArrayAccessor subclass: #CPluggableAccessor
	instanceVariableNames: 'readBlock writeBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'CPluggableAccessor class' category: #'PluginMaker-InterpreterSimulation'!
CPluggableAccessor class
	instanceVariableNames: ''!

!classDefinition: #VMMakerException category: #'PluginMaker-Building'!
Error subclass: #VMMakerException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Building'!
!classDefinition: 'VMMakerException class' category: #'PluginMaker-Building'!
VMMakerException class
	instanceVariableNames: ''!

!classDefinition: #InterpreterPlugin category: #PluginMaker!
Object subclass: #InterpreterPlugin
	instanceVariableNames: 'interpreterProxy moduleName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker'!
!classDefinition: 'InterpreterPlugin class' category: #PluginMaker!
InterpreterPlugin class
	instanceVariableNames: 'timeStamp'!

!classDefinition: #SmartSyntaxInterpreterPlugin category: #'PluginMaker-SmartSyntaxPlugins'!
InterpreterPlugin subclass: #SmartSyntaxInterpreterPlugin
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-SmartSyntaxPlugins'!
!classDefinition: 'SmartSyntaxInterpreterPlugin class' category: #'PluginMaker-SmartSyntaxPlugins'!
SmartSyntaxInterpreterPlugin class
	instanceVariableNames: ''!

!classDefinition: #InterpreterProxy category: #'PluginMaker-InterpreterSimulation'!
Object subclass: #InterpreterProxy
	instanceVariableNames: 'successFlag remapBuffer stack method argumentCount bb thisSessionID'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-InterpreterSimulation'!
!classDefinition: 'InterpreterProxy class' category: #'PluginMaker-InterpreterSimulation'!
InterpreterProxy class
	instanceVariableNames: ''!

!classDefinition: #InterpreterSimulationObject category: #'PluginMaker-Plugins'!
Object subclass: #InterpreterSimulationObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Plugins'!
!classDefinition: 'InterpreterSimulationObject class' category: #'PluginMaker-Plugins'!
InterpreterSimulationObject class
	instanceVariableNames: ''!

!classDefinition: #ObjectMemory category: #'PluginMaker-Interpreter'!
Object subclass: #ObjectMemory
	instanceVariableNames: 'memory youngStart endOfMemory memoryLimit nilObj falseObj trueObj specialObjectsOop rootTable rootTableCount extraRoots extraRootCount weakRoots weakRootCount child field parentField freeBlock lastHash allocationCount lowSpaceThreshold signalLowSpace compStart compEnd fwdTableNext fwdTableLast remapBuffer remapBufferCount allocationsBetweenGCs tenuringThreshold gcSemaphoreIndex gcBiasToGrow gcBiasToGrowGCLimit gcBiasToGrowThreshold statFullGCs statFullGCMSecs statIncrGCs statIncrGCMSecs statTenures statRootTableOverflows freeContexts freeLargeContexts interruptCheckCounter totalObjectCount shrinkThreshold growHeadroom headerTypeBytes youngStartLocal statMarkCount statSweepCount statMkFwdCount statCompMoveCount statGrowMemory statShrinkMemory statRootTableCount statAllocationCount statSurvivorCount statGCTime statSpecialMarkCount statIGCDeltaTime statpendingFinalizationSignals forceTenureFlag'
	classVariableNames: 'AllButHashBits AllButMarkBit AllButMarkBitAndTypeMask AllButRootBit AllButTypeMask BaseHeaderSize BlockContextProto Byte0Mask Byte0Shift Byte1Mask Byte1Shift Byte1ShiftNegated Byte2Mask Byte2Shift Byte3Mask Byte3Shift Byte3ShiftNegated Byte4Mask Byte4Shift Byte4ShiftNegated Byte5Mask Byte5Shift Byte5ShiftNegated Byte6Mask Byte6Shift Byte7Mask Byte7Shift Byte7ShiftNegated Bytes3to0Mask Bytes7to4Mask BytesPerWord CharacterTable ClassArray ClassBitmap ClassBlockClosure ClassBlockContext ClassByteArray ClassCharacter ClassCompiledMethod ClassExternalAddress ClassExternalData ClassExternalFunction ClassExternalLibrary ClassExternalStructure ClassFloat ClassInteger ClassLargeNegativeInteger ClassLargePositiveInteger ClassMessage ClassMethodContext ClassPoint ClassProcess ClassPseudoContext ClassSemaphore ClassString ClassTranslatedMethod CompactClassMask CompactClasses ConstMinusOne ConstOne ConstTwo ConstZero ContextFixedSizePlusHeader CtxtTempFrameStart DoAssertionChecks DoBalanceChecks Done ExternalObjectsArray ExtraRootSize FalseObject GCTopMarker HashBits HashBitsOffset HeaderTypeClass HeaderTypeFree HeaderTypeGC HeaderTypeShort HeaderTypeSizeAndClass LargeContextBit LargeContextSize LongSizeMask MarkBit MethodContextProto NilContext NilObject ProcessSignalingLowSpace RemapBufferSize RootBit RootTableRedZone RootTableSize SchedulerAssociation SelectorAboutToReturn SelectorCannotInterpret SelectorCannotReturn SelectorDoesNotUnderstand SelectorMustBeBoolean SelectorRunWithIn ShiftForWord Size4Bit SizeMask SmallContextSize SpecialSelectors StackStart StartField StartObj TheDisplay TheFinalizationSemaphore TheInputSemaphore TheInterruptSemaphore TheLowSpaceSemaphore TheTimerSemaphore TrueObject TypeMask Upward WordMask'
	poolDictionaries: ''
	category: 'PluginMaker-Interpreter'!
!classDefinition: 'ObjectMemory class' category: #'PluginMaker-Interpreter'!
ObjectMemory class
	instanceVariableNames: 'timeStamp'!

!classDefinition: #PluginMaker category: #'PluginMaker-Building'!
Object subclass: #PluginMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Building'!
!classDefinition: 'PluginMaker class' category: #'PluginMaker-Building'!
PluginMaker class
	instanceVariableNames: ''!

!classDefinition: #TMethod category: #'PluginMaker-Translation to C'!
Object subclass: #TMethod
	instanceVariableNames: 'selector returnType args locals declarations primitive parseTree labels possibleSideEffectsCache complete export static sharedLabel sharedCase comment definingClass globalStructureBuildMethodHasFoo'
	classVariableNames: 'CaseStatements'
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TMethod class' category: #'PluginMaker-Translation to C'!
TMethod class
	instanceVariableNames: ''!

!classDefinition: #SmartSyntaxPluginTMethod category: #'PluginMaker-SmartSyntaxPlugins'!
TMethod subclass: #SmartSyntaxPluginTMethod
	instanceVariableNames: 'isPrimitive suppressingFailureGuards selectorOverride fullArgs parmSpecs rcvrSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-SmartSyntaxPlugins'!
!classDefinition: 'SmartSyntaxPluginTMethod class' category: #'PluginMaker-SmartSyntaxPlugins'!
SmartSyntaxPluginTMethod class
	instanceVariableNames: ''!

!classDefinition: #TParseNode category: #'PluginMaker-Translation to C'!
Object subclass: #TParseNode
	instanceVariableNames: 'comment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TParseNode class' category: #'PluginMaker-Translation to C'!
TParseNode class
	instanceVariableNames: ''!

!classDefinition: #TAssignmentNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TAssignmentNode
	instanceVariableNames: 'variable expression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TAssignmentNode class' category: #'PluginMaker-Translation to C'!
TAssignmentNode class
	instanceVariableNames: ''!

!classDefinition: #TCaseStmtNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TCaseStmtNode
	instanceVariableNames: 'expression firsts lasts cases'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TCaseStmtNode class' category: #'PluginMaker-Translation to C'!
TCaseStmtNode class
	instanceVariableNames: ''!

!classDefinition: #TConstantNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TConstantNode
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TConstantNode class' category: #'PluginMaker-Translation to C'!
TConstantNode class
	instanceVariableNames: ''!

!classDefinition: #TDefineNode category: #'PluginMaker-Translation to C'!
TConstantNode subclass: #TDefineNode
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TDefineNode class' category: #'PluginMaker-Translation to C'!
TDefineNode class
	instanceVariableNames: ''!

!classDefinition: #TGoToNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TGoToNode
	instanceVariableNames: 'label'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TGoToNode class' category: #'PluginMaker-Translation to C'!
TGoToNode class
	instanceVariableNames: ''!

!classDefinition: #TInlineNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TInlineNode
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TInlineNode class' category: #'PluginMaker-Translation to C'!
TInlineNode class
	instanceVariableNames: ''!

!classDefinition: #TLabeledCommentNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TLabeledCommentNode
	instanceVariableNames: 'label'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TLabeledCommentNode class' category: #'PluginMaker-Translation to C'!
TLabeledCommentNode class
	instanceVariableNames: ''!

!classDefinition: #TReturnNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TReturnNode
	instanceVariableNames: 'expression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TReturnNode class' category: #'PluginMaker-Translation to C'!
TReturnNode class
	instanceVariableNames: ''!

!classDefinition: #TSendNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TSendNode
	instanceVariableNames: 'selector receiver arguments isBuiltinOperator isExpression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TSendNode class' category: #'PluginMaker-Translation to C'!
TSendNode class
	instanceVariableNames: ''!

!classDefinition: #TStmtListNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TStmtListNode
	instanceVariableNames: 'arguments statements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TStmtListNode class' category: #'PluginMaker-Translation to C'!
TStmtListNode class
	instanceVariableNames: ''!

!classDefinition: #TVariableNode category: #'PluginMaker-Translation to C'!
TParseNode subclass: #TVariableNode
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PluginMaker-Translation to C'!
!classDefinition: 'TVariableNode class' category: #'PluginMaker-Translation to C'!
TVariableNode class
	instanceVariableNames: ''!


!Oop commentStamp: '<historical>' prior: 0!
Cooercion specification for Identity coercions:

	x asOop: Oop == x
	y asValue: Oop == y!

!Unsigned commentStamp: 'tpr 5/5/2003 12:21' prior: 0!
Coercion specification for 32-bit unsigned numbers within plugins.

Note: "x asOop: Unsigned" can cause garbage collection when x is outside SmallInteger range.!

!CArray commentStamp: '<historical>' prior: 0!
For Simulating the Interpreter only.  Coercing an Integer to a pointer (eg. cCoerce:to: 'char *') returns a CArray.  A CArray responds to at: and at:put: by reading/writing from the memory of the interpreter that sent the cCoerce:to: message in the first place.  A CArray responds to arithmetic by returning the new address.  Since longAt: sends // to its given address this is where the CArray converts back to a Integer for the memory fetch to work.!

!BalloonArray commentStamp: '<historical>' prior: 0!
BalloonArray keeps a shadow copy of its raw memory data in a Smalltalk array.  This allows support for C's inhomogeneous access, returning floats where Floats were stored, and negative ints where they were stored.  This ruse only works, of course where we have control over all the access.!

!CCodeGenerator commentStamp: 'tpr 5/2/2003 14:30' prior: 0!
This class oversees the translation of a subset of Smalltalk to C, allowing the comforts of Smalltalk during development and the efficiency and portability of C for the resulting interpreter.  
See VMMaker for more useful info!

!CCodeGeneratorGlobalStructure commentStamp: 'tpr 5/23/2003 11:17' prior: 0!
This subclass of CCodeGenerator adds support for sticking most global variables into a large global array. This in turn means that suitably minded C compilers can do a better job of accessing those variables; in particular the Mac OS use of PPC and Acorn use of ARM benfits by a substantial margin. 

Only simple globals are currently put in the array. Someday we might try adding pointers to the various arrays etc.!

!VMPluginCodeGenerator commentStamp: '<historical>' prior: 0!
I generate code that can be loaded dynamically from external libraries (e.g., DSOs on Unix or DLLs on Windows)!

!SmartSyntaxPluginCodeGenerator commentStamp: 'tpr 5/5/2003 16:03' prior: 0!
Subclass of CCodeGenerator, used in connection with TestInterpreterPlugin to generate named primitives with type coercion specifications. See the plugins implemeted as subclasses of TestInterpreterPlugin!

!CObjectAccessor commentStamp: '<historical>' prior: 0!
I am used to simulate the indexed access to any object during plugin simulation.!

!CArrayAccessor commentStamp: '<historical>' prior: 0!
I am used to simulate the indexed access to arrays during plugin simulation.!

!CPluggableAccessor commentStamp: 'tpr 5/5/2003 11:49' prior: 0!
This class adds generalized block parameter access to C objects for vm simulation!

!VMMakerException commentStamp: 'tpr 5/5/2003 12:28' prior: 0!
An exception class for the use of VMMaker!

!InterpreterPlugin commentStamp: '<historical>' prior: 0!
This class provides the basic framework for creating VM plugins. Most of the useful methods are on the class side; particularly take note of the messages like #shouldBeTranslated and #requiresPlatformFiles.!

!SmartSyntaxInterpreterPlugin commentStamp: '<historical>' prior: 0!
Subclass of InterpreterPlugin, used in connection with TestCodeGenerator for named primitives with type coercion specifications!

!InterpreterProxy commentStamp: '<historical>' prior: 0!
This class should provide the definition of what interpreter support plugins need and can have access to. Note that the proxy related files platforms - Cross - vm - sqVirtualMachine.[ch] are kept under the SVN tree at www.squeakvm.org .
The main use of the class is to support the vm simulator!

!ObjectMemory commentStamp: '<historical>' prior: 0!
This class describes a 32-bit direct-pointer object memory for Smalltalk.  The model is very simple in principle:  a pointer is either a SmallInteger or a 32-bit direct object pointer.

SmallIntegers are tagged with a low-order bit equal to 1, and an immediate 31-bit 2s-complement signed value in the rest of the word.

All object pointers point to a header, which may be followed by a number of data fields.  This object memory achieves considerable compactness by using a variable header size (the one complexity of the design).  The format of the 0th header word is as follows:

	3 bits	reserved for gc (mark, root, unused)
	12 bits	object hash (for HashSets)
	5 bits	compact class index
	4 bits	object format
	6 bits	object size in 32-bit words
	2 bits	header type (0: 3-word, 1: 2-word, 2: forbidden, 3: 1-word)

If a class is in the compact class table, then this is the only header information needed.  If it is not, then it will have another header word at offset -4 bytes with its class in the high 30 bits, and the header type repeated in its low 2 bits.  It the objects size is greater than 255 bytes, then it will have yet another header word at offset -8 bytes with its full word size in the high 30 bits and its header type repeated in the low two bits.

The object format field provides the remaining information as given in the formatOf: method (including isPointers, isVariable, isBytes, and the low 2 size bits of byte-sized objects).

This implementation includes incremental (2-generation) and full garbage collection, each with compaction and rectification of direct pointers.  It also supports a bulk-become (exchange object identity) feature that allows many objects to be becomed at once, as when all instances of a class must be grown or shrunk.

There is now a simple 64-bit version of the object memory.  It is the simplest possible change that could work.  It merely sign-extends all integer oops, and extends all object headers and oops by adding 32 zeroes in the high bits.  The format of the base header word is changed in one minor, not especially elegant, way.  Consider the old 32-bit header:
	ggghhhhhhhhhhhhcccccffffsssssstt
The 64-bit header is almost identical, except that the size field (now being in units of 8 bytes, has a zero in its low-order bit.  At the same time, the byte-size residue bits for byte objects, which are in the low order bits of formats 8-11 and 12-15, are now in need of another bit of residue.  So, the change is as follows:
	ggghhhhhhhhhhhhcccccffffsssssrtt
where bit r supplies the 4's bit of the byte size residue for byte objects.  Oh, yes, this is also needed now for 'variableWord' objects, since their size in 32-bit words requires a low-order bit.

See the comment in formatOf: for the change allowing for 64-bit wide bitmaps, now dubbed 'variableLong'.!

!TMethod commentStamp: 'dtl 9/15/2008 09:06' prior: 0!
A TMethod is a translation method, representing a MethodNode that is to be translated to C source. It has a parseTree of translation nodes that mirrors the parse tree of the corresponding Smalltalk method.!

!SmartSyntaxPluginTMethod commentStamp: '<historical>' prior: 0!
Variation of TMethod node of the Smalltalk C Code Generator, used in conjunction with TestCodeGenerator and TestInterpreterPlugin to generate named primitives with type coercion specifications.!

!TParseNode commentStamp: 'dtl 9/15/2008 09:05' prior: 0!
A TParseNode is node in the parse tree of a TMethod. Subclasses correspond to different types of nodes in a method parse tree. The tree of translation parse nodes mirrors the parse tree of a Smalltalk method, and is used for translating a Smalltalk method to C source.!

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 9/19/1999 20:39'!
asIf: aClass var: aString

	|index|
	index := aClass allInstVarNames 
		indexOf: aString
		ifAbsent: [self error: 'must use instVar name'].
	^self instVarAt: index
! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 10/5/1999 06:35'!
asIf: aClass var: aString asValue: someClass

	^(self asIf: aClass var: aString) asValue: someClass
! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 9/19/1999 20:40'!
asIf: aClass var: aString put: aValue

	|index|
	index := aClass allInstVarNames 
		indexOf: aString
		ifAbsent: [self error: 'must use instVar name'].
	^self instVarAt: index put: aValue
! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 9/20/1999 11:30'!
asOop: aClass

	(self class isVariable and: [self class instSize > 0])
		ifTrue: [self error: 'cannot auto-coerce indexable objects with named instance variables'].
	(aClass ccgCanConvertFrom: self)
		ifFalse: [self error: 'incompatible object for this coercion'].
	^self! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 9/19/1999 20:21'!
asSmallIntegerObj

	^self! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 10/5/1999 06:21'!
asValue: aClass

	^self! !

!Object methodsFor: '*PluginMaker-translation support'!
cCode: codeString
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'jm 2/15/1999 13:11'!
cCode: codeString inSmalltalk: aBlock
	"Support for Smalltalk-to-C translation. The given string is output literally when generating C code. If this code is being simulated in Smalltalk, answer the result of evaluating the given block."

	^ aBlock value
! !

!Object methodsFor: '*PluginMaker-translation support'!
cCoerce: value to: cType
	"Type coercion for translation only; just return the value when running in Smalltalk."

	^ value! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'sr 12/23/2001 21:38'!
debugCode: aBlock 
	"Sending this message tells the code generator that there is debug code in 
	aBlock. Debug code will be be generated only, if the correponding flag 
	has been set by TestCodeGenerator>>generateDebugCode:.
	In ST simulation just perform the debug code."
	aBlock value! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'ar 9/18/1998 23:27'!
export: aBoolean
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'di 7/14/2004 12:15'!
isCObjectAccessor

	^ false! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'dtl 11/9/2006 19:37'!
isDefined: directiveString inSmalltalk: conditionBlock comment: commentStringOrNil ifTrue: trueExpressionOrBlock
	"When translated, produces #ifdef #endif CPP directives.
	Example usage:

	self isDefined: 'HAVE_FOO'
		inSmalltalk: [self isFoo]
		comment: 'some platforms do not support foo properly'
		ifTrue: [self doThingsThatWorkIfFooIsPresent]

	Generated C code:
	
	# ifdef HAVE_FOO  // some platforms do not support foo properly
 		doThingsThatWorkIfFooIsPresent();
	# endif  // HAVE_FOO
	"

	^ self isDefined: directiveString
			inSmalltalk: conditionBlock
			comment: commentStringOrNil
			ifTrue: trueExpressionOrBlock
			ifFalse: []
! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'dtl 11/9/2006 19:59'!
isDefined: directiveString inSmalltalk: conditionBlock comment: commentStringOrNil ifTrue: trueExpressionOrBlock ifFalse: falseExpressionOrBlockOrNil
	"When translated, produces #ifdef #else #endif CPP directives.
	Example usage:

	Smalltalk Slang:
		self isDefined: 'HAVE_FOO'
			inSmalltalk: [self isFoo]
			comment: 'some platforms do not support foo properly'
			ifTrue: [self doThingsThatWorkIfFooIsPresent]
			ifFalse: [self doSomethingElseInstead]

	Generated C code:
		# ifdef HAVE_FOO  // some platforms do not support foo properly
			doThingsThatWorkIfFooIsPresent();
		# else
			doSomethingElseInstead();
		# endif  // HAVE_FOO

	Smalltalk Slang:
		hasFoo := self isDefined: 'HAVE_FOO'
					inSmalltalk: [self isFoo]
					comment: 'some platforms do not support foo properly'
					ifTrue: [true]
					ifFalse: [false].

	Generated C code:
		hasFoo =
	# ifdef HAVE_FOO  // some platforms do not support foo properly
			1
	# else
			0
	# endif  // HAVE_FOO
 		;

	"

	^ conditionBlock value
		ifTrue: [trueExpressionOrBlock value]
		ifFalse: [falseExpressionOrBlockOrNil value]
! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'dtl 8/14/2009 09:31'!
preprocessorExpression: directive
	"For translation only; noop when running in Smalltalk.
	When translated, emits directive as a C preprocessor directive.

	Example usage:

	self preprocessorExpression: 'ifdef HAS_FOO'.
	self doFooStuff.
	self preprocessorExpression: 'endif'.
	
	Generated C code:
	
	# ifdef HAS_FOO
		doFooStuff();
	# endif
	"! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'nk 4/5/2005 20:50'!
primitive: primName
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'nk 4/5/2005 20:50'!
primitive: primName parameters: parms
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'tpr 6/9/2003 16:40'!
primitive: primID parameters: parmSpecs receiver: rcvrSpec
"belongs in CCG package"
	| tMethod |
	tMethod := SmartSyntaxPluginTMethod new 
		fromContext: thisContext sender 
		primitive: primID 
		parameters: parmSpecs 
		receiver: rcvrSpec.
	^tMethod simulatePrologInContext: thisContext sender! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 1/1/2000 12:45'!
remapOop: oopOrList in: aBlock
	"For translation only; noop when running in Smalltalk."
	^aBlock value! !

!Object methodsFor: '*PluginMaker-translation support'!
returnTypeC: typeString
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support'!
sharedCodeNamed: label inCase: caseNumber
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 1/1/2000 12:46'!
stAt: index

	^self at: index! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 1/1/2000 12:46'!
stAt: index put: value

	^self at: index put: value! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 1/1/2000 22:42'!
stSize

	^self size! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'ar 2/21/2000 00:42'!
static: aBoolean
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'acg 12/18/1999 11:31'!
suppressFailureGuards: failureGuardFlag
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'nk 4/5/2005 20:45'!
touch: something
	"For translation only; eliminated by CCodeGenerator"! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'sma 3/3/2000 12:06'!
var: varSymbol type: typeString
	"For translation only; noop when running in Smalltalk."! !

!Object methodsFor: '*PluginMaker-translation support' stamp: 'sma 3/3/2000 12:06'!
var: varSymbol type: typeString array: array
	"For translation only; noop when running in Smalltalk."! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:05'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asNakedOopFrom: anInteger on: aStream! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 13:01'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg emitCExpression: aNode on: aStream! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:10'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg emitCExpression: aNode on: aStream! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 16:09'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asKindOf: self from: anInteger! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 11:12'!
ccgCanConvertFrom: anObject

	^anObject isKindOf: self! !

!Object class methodsFor: '*PluginMaker-plugin generation' stamp: 'ikp 3/31/2005 14:20'!
ccgDeclareCForVar: aSymbolOrString

	^'sqInt ', aSymbolOrString! !

!Boolean class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:06'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asBooleanValueFrom: anInteger on: aStream ! !

!Boolean class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:05'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg generateCoerceToBooleanObjectFrom: aNode on: aStream! !

!Boolean class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:10'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg generateCoerceToBooleanValueFrom: aNode on: aStream! !

!Boolean class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 17:08'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asBooleanValueFrom: anInteger! !

!ArrayedCollection methodsFor: '*PluginMaker-simulated image growing' stamp: 'nk 10/13/2003 22:26'!
copyGrownBy: n
	"Create a new collection containing all the elements from aCollection."

	^ (self class new: self size + n) replaceFrom: 1 to: self size with: self! !

!ArrayedCollection class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 10:03'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	self instSize > 0 ifTrue: 
		[self error: 'cannot auto-coerce arrays with named instance variables'].
	cg generateCoerceToObjectFromPtr: aNode on: aStream! !

!ArrayedCollection class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:18'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg 
		generateCoerceToPtr: (self ccgDeclareCForVar: '')
		fromObject: aNode on: aStream! !

!Array methodsFor: '*PluginMaker-interpreter simulator' stamp: 'di 5/9/2004 09:55'!
coerceTo: cTypeString sim: interpreterSimulator

	^ self! !

!Array class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:12'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asIntPtrFrom: anInteger on: aStream! !

!Array class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/19/1999 13:10'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg 
		ccgLoad: aBlock 
		expr: aString 
		asIntPtrFrom: anInteger
		andThen: (cg ccgValBlock: 'isIndexable')! !

!Array class methodsFor: '*PluginMaker-plugin generation' stamp: 'ikp 3/31/2005 14:20'!
ccgDeclareCForVar: aSymbolOrString

	^'sqInt *', aSymbolOrString! !

!Bitmap methodsFor: '*PluginMaker-interpreter simulator' stamp: 'ajh 8/20/2002 01:13'!
coerceTo: cTypeString sim: interpreter

	^ self! !

!ByteArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:13'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asCharPtrFrom: anInteger on: aStream! !

!ByteArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/19/1999 00:25'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg 
		ccgLoad: aBlock 
		expr: aString 
		asCharPtrFrom: anInteger
		andThen: (cg ccgValBlock: 'isBytes')! !

!ByteArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:13'!
ccgDeclareCForVar: aSymbolOrString

	^'char *', aSymbolOrString! !

!FloatArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:17'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asFloatPtrFrom: anInteger on: aStream! !

!FloatArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 17:07'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asWBFloatPtrFrom: anInteger! !

!FloatArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:14'!
ccgDeclareCForVar: aSymbolOrString

	^'float *', aSymbolOrString! !

!IntegerArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:17'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asIntPtrFrom: anInteger on: aStream! !

!IntegerArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/25/1999 15:00'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg 
		ccgLoad: aBlock 
		expr: aString 
		asIntPtrFrom: anInteger
		andThen: (cg ccgValBlock: 'isWords')! !

!IntegerArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:18'!
ccgDeclareCForVar: aSymbolOrString

	^'int *', aSymbolOrString! !

!String methodsFor: '*PluginMaker-Translation to C' stamp: 'dtl 10/25/2009 14:31'!
replaceLastOccurrence: oldSubstring with: newSubstring 
	"Answer a copy with the last occurrence of oldSubstring replaced by newSubstring."

	" 'int i' replaceLastOccurrence: 'i' with: 'i2' "
	" 'void *v' replaceLastOccurrence: 'v' with: 'v2' "
	" 'int intxintxintx' replaceLastOccurrence: 'i' with: 'I2' "
	" 'int intxintxintx' replaceLastOccurrence: 'FOO' with: 'BAR' "
	" 'int intxintxintx' replaceLastOccurrence: '' with: 'BAZ' "

	^ self class streamContents: [:ws | | pos rs | 
			rs := ReadStream on: self.
			pos := self findLastOccuranceOfString: oldSubstring startingAt: 1.
			pos > 0 ifTrue: [ws nextPutAll: (rs next: pos - 1); nextPutAll: newSubstring.
						rs next: oldSubstring size].
			ws nextPutAll: rs upToEnd]! !

!String class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/19/1999 00:21'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg 
		ccgLoad: aBlock 
		expr: aString 
		asCharPtrFrom: anInteger
		andThen: (cg ccgValBlock: 'isBytes')! !

!String class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 17:10'!
ccgDeclareCForVar: aSymbolOrString

	^'char *', aSymbolOrString! !

!WordArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:19'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asIntPtrFrom: anInteger on: aStream! !

!WordArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 13:16'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg 
		ccgLoad: aBlock 
		expr: aString 
		asUnsignedPtrFrom: anInteger
		andThen: (cg ccgValBlock: 'isWords')! !

!WordArray class methodsFor: '*PluginMaker-plugin generation' stamp: 'ikp 3/31/2005 14:19'!
ccgDeclareCForVar: aSymbolOrString

	^'usqInt *', aSymbolOrString! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'bf 3/16/2000 19:06'!
ccg: cg emitLoadFor: aString from: anInteger on: aStream

	cg emitLoad: aString asFloatValueFrom: anInteger on: aStream! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:05'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg generateCoerceToFloatObjectFrom: aNode on: aStream! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:10'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg generateCoerceToFloatValueFrom: aNode on: aStream! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 17:08'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asFloatValueFrom: anInteger! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 11:22'!
ccgCanConvertFrom: anObject

	^anObject class == self! !

!Float class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/17/1999 01:09'!
ccgDeclareCForVar: aSymbolOrString

	^'double ', aSymbolOrString! !

!Integer methodsFor: '*PluginMaker-interpreter simulator' stamp: 'nk 4/3/2004 12:46'!
coerceTo: cTypeString sim: interpreter

	| unitSize |

	cTypeString last = $* ifTrue: [  "C pointer"
		unitSize := cTypeString caseOf: {
		['char *'] -> [1].
		['int *'] -> [4].
		['float *'] -> [4].
		['unsigned *'] -> [4].
		['float *'] -> [4].
		}
		otherwise: [ (cTypeString beginsWith: 'char') ifTrue: [1] ifFalse: [4] ].
		^(CArray basicNew)
			interpreter: interpreter address: self unitSize: unitSize;
			yourself.
	].
	^ self  "C number (int, char, float, etc)"! !

!Integer methodsFor: '*PluginMaker-interpreter simulator' stamp: 'di 7/16/2004 15:06'!
signedIntFromLong
	"Self is an unsigned 32-bit integer"

	| sign |
	self < 0 ifTrue: [self error: 'only valid for unsigned ints'].
	sign := self bitAnd: 16r80000000.
	sign = 0 ifTrue: [^ self].
	^ self - sign - sign! !

!Integer methodsFor: '*PluginMaker-interpreter simulator' stamp: 'di 7/16/2004 15:06'!
signedIntFromShort
	"Self is an unsigned 16-bit integer in twos-comp form"

	| sign |
	self < 0 ifTrue: [self error: 'only valid for unsigned ints'].
	sign := self bitAnd: 16r8000.
	sign = 0 ifTrue: [^ self].
	^ self - sign - sign! !

!Integer methodsFor: '*PluginMaker-interpreter simulator' stamp: 'di 7/14/2004 12:27'!
signedIntToLong
	"Produces a 32-bit value in twos-comp form.  Sorry no error checking"

	self >= 0
		ifTrue: [^ self]
		ifFalse: [^ self + 16r80000000 + 16r80000000]
! !

!Integer methodsFor: '*PluginMaker-interpreter simulator' stamp: 'di 7/14/2004 12:26'!
signedIntToShort
	"Produces a 16-bit value (0-65k) in twos-comp form.  Sorry no error checking"

	self >= 0
		ifTrue: [^ self]
		ifFalse: [^ self + 16r8000 + 16r8000]
! !

!SmallInteger class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:04'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg generateCoerceToSmallIntegerObjectFrom: aNode on: aStream! !

!SmallInteger class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 10/5/1999 06:11'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg generateCoerceToSmallIntegerValueFrom: aNode on: aStream! !

!SmallInteger class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/18/1999 17:09'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asIntegerValueFrom: anInteger! !

!SmallInteger class methodsFor: '*PluginMaker-plugin generation' stamp: 'acg 9/20/1999 11:20'!
ccgCanConvertFrom: anObject

	^anObject class == self! !

!AssignmentNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:32'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"
	^TAssignmentNode new
		setVariable: variable asTranslatorNode
		expression: value asTranslatorNode;
		comment: comment! !

!BlockNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:34'!
asTranslatorNode
	"make a CCodeGenerator equivalent of me"
	| statementList newS |
	statementList := OrderedCollection new.
	statements
		do: [:s | 
			newS := s asTranslatorNode.
			newS isStmtList
				ifTrue: ["inline the statement list returned when a CascadeNode is 
					translated "
					statementList addAll: newS statements]
				ifFalse: [statementList add: newS]].
	^ TStmtListNode new
		setArguments: (arguments asArray
				collect: [:arg | arg key])
		statements: statementList;
		 comment: comment! !

!CascadeNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:34'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"
	^TStmtListNode new
		setArguments: #()
		statements: (messages collect:
			[ :msg | msg asTranslatorNode receiver: receiver asTranslatorNode ]);
		comment: comment! !

!LiteralNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:35'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"

	^TConstantNode new setValue: key! !

!VariableNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:36'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"
	name = 'true' ifTrue: [^ TConstantNode new setValue: true].
	name = 'false' ifTrue: [^ TConstantNode new setValue: false].
	^ TVariableNode new setName: name
! !

!MessageNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 6/3/2005 10:25'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"
	"selector is sometimes a Symbol, sometimes a SelectorNode!!
	On top of this, numArgs is needed due to the (truly grody) use of
	arguments as a place to store the extra expressions needed to generate
	code for in-line to:by:do:, etc.  see below, where it is used."
	| sel args |
	sel := (selector isSymbol) ifTrue: [selector] ifFalse: [selector key].
	args := (1 to: sel numArgs) collect:
			[:i | (arguments at: i) asTranslatorNode].
	(sel = #to:by:do: and: [arguments size = 7 and: [(arguments at: 7) notNil]])
		ifTrue: ["Restore limit expr that got moved by transformToDo:"
				args at: 1 put: (arguments at: 7) value asTranslatorNode].
	(sel = #or: and: [arguments size = 2 and: [(arguments at: 2) notNil]])
		ifTrue: ["Restore argument block that got moved by transformOr:"
				args at: 1 put: (arguments at: 2) asTranslatorNode].
	(sel = #ifFalse: and: [arguments size = 2 and: [(arguments at: 2) notNil]])
		ifTrue: ["Restore argument block that got moved by transformIfFalse:"
				args at: 1 put: (arguments at: 2) asTranslatorNode].
	^ TSendNode new
		setSelector: sel
		receiver: ((receiver == nil)
					ifTrue: [nil]
					ifFalse: [receiver asTranslatorNode])
		arguments: args! !

!MethodNode methodsFor: '*PluginMaker-C translation' stamp: 'hg 8/14/2000 15:56'!
asTranslationMethodOfClass: aClass
 
	^ aClass new
		setSelector: selectorOrFalse
		args: arguments
		locals: encoder tempsAndBlockArgs
		block: block
		primitive: primitive;
		comment: comment
! !

!ReturnNode methodsFor: '*PluginMaker-C translation' stamp: 'tpr 5/5/2003 12:36'!
asTranslatorNode
"make a CCodeGenerator equivalent of me"
	^TReturnNode new 
		setExpression: expr asTranslatorNode;
		comment: comment! !

!Oop class methodsFor: 'plugin generation' stamp: 'acg 9/20/1999 13:02'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg emitCExpression: aNode on: aStream! !

!Oop class methodsFor: 'plugin generation' stamp: 'acg 10/5/1999 06:10'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg emitCExpression: aNode on: aStream! !

!Oop class methodsFor: 'plugin generation' stamp: 'acg 9/18/1999 15:58'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asRawOopFrom: anInteger! !

!Oop class methodsFor: 'plugin generation' stamp: 'acg 9/20/1999 11:17'!
ccgCanConvertFrom: anObject

	^(anObject isKindOf: SmallInteger) not! !

!Oop class methodsFor: 'plugin generation' stamp: 'ikp 3/31/2005 14:20'!
ccgDeclareCForVar: aSymbolOrString

	^'sqInt ', aSymbolOrString! !

!Unsigned methodsFor: 'as yet unclassified' stamp: 'acg 9/20/1999 09:49'!
ccg: cg prolog: aBlock expr: aString index: anInteger

	^cg ccgLoad: aBlock expr: aString asUnsignedValueFrom: anInteger! !

!Unsigned methodsFor: 'as yet unclassified' stamp: 'ikp 3/31/2005 14:19'!
ccgDeclareCForVar: aSymbolOrString

	^'unsigned int ', aSymbolOrString! !

!Unsigned class methodsFor: 'as yet unclassified' stamp: 'acg 10/5/1999 06:04'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	cg generateCoerceToUnsignedObjectFrom: aNode on: aStream! !

!Unsigned class methodsFor: 'as yet unclassified' stamp: 'acg 10/5/1999 06:11'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg generateCoerceToUnsignedValueFrom: aNode on: aStream! !

!Unsigned class methodsFor: 'as yet unclassified' stamp: 'acg 9/20/1999 11:30'!
ccgCanConvertFrom: anObject

	anObject isInteger ifFalse: 
		[self error: 'Not an Integer object'. ^false].
	anObject >= 0 ifFalse: 
		[self error: 'Object is negative integer'. ^false].
	anObject < (2 raisedToInteger: 32) ifFalse: 
		[self error: 'Object is too large'. ^false].
	^true! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
* n

	^ self ptrAddress * n! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 01:43'!
+ n

	^ self ptrAddress + n! !

!CArray methodsFor: 'pointer arithmetic' stamp: 'ajh 8/20/2002 01:34'!
+= increment

	ptrOffset := ptrOffset + increment! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 01:43'!
- n

	^ self ptrAddress - n! !

!CArray methodsFor: 'pointer arithmetic' stamp: 'ajh 8/20/2002 01:35'!
-= decrement

	ptrOffset := ptrOffset - decrement! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
// n

	^ self ptrAddress // n! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
<< n

	^ self ptrAddress bitShift: n! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
>> n

	^ self ptrAddress bitShift: 0 - n! !

!CArray methodsFor: 'converting' stamp: 'ajh 8/20/2002 01:29'!
adaptToNumber: rcvr andSend: selector

	^ rcvr perform: selector with: self asInteger! !

!CArray methodsFor: 'converting' stamp: 'di 7/15/2004 16:55'!
asCArrayAccessor

	^ (CArrayAccessor on: self)
			+= -1   "Defeat the +1 offset in the accessor"! !

!CArray methodsFor: 'converting' stamp: 'ajh 8/20/2002 01:10'!
asInteger

	^ self ptrAddress! !

!CArray methodsFor: 'accessing' stamp: 'di 7/6/2004 09:32'!
at: offset

	ptrOffset = 0 ifFalse: [self error: 'only expect base address to receive at: message'].
	unitSize = 1 ifTrue: [^ interpreter byteAt: arrayBaseAddress + offset].
	unitSize = 4 ifTrue: [^ interpreter long32At: arrayBaseAddress + (offset * 4)].
	self halt: 'Can''t handle unitSize ', unitSize printString
! !

!CArray methodsFor: 'accessing' stamp: 'di 7/19/2004 12:01'!
at: offset put: val

	ptrOffset = 0 ifFalse: [self error: 'only expect base address to receive at:put: message'].
	unitSize = 1 ifTrue: [^ interpreter byteAt: arrayBaseAddress + offset put: val].
	unitSize = 4 ifTrue: [^ interpreter long32At: arrayBaseAddress + (offset * 4) put: val].
	self halt: 'Can''t handle unitSize ', unitSize printString
! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
bitAnd: n

	^ self ptrAddress bitAnd: n! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
bitOr: n

	^ self ptrAddress bitOr: n! !

!CArray methodsFor: 'int arithmetic' stamp: 'ajh 8/20/2002 00:35'!
bitShift: n

	^ self ptrAddress bitShift: n! !

!CArray methodsFor: 'converting' stamp: 'tpr 3/23/2005 12:36'!
coerceTo: cTypeString sim: interpreterSimulator

	cTypeString = 'int' ifTrue: [^ self ptrAddress].
	cTypeString = 'float *' ifTrue: [^ self asCArrayAccessor asFloatAccessor].
	cTypeString = 'int *' ifTrue: [^ self asCArrayAccessor asIntAccessor].
	cTypeString = 'unsigned' ifTrue: [^ self ptrAddress].
	^ self! !

!CArray methodsFor: 'converting' stamp: 'ajh 8/20/2002 01:23'!
doesNotUnderstand: message

	^ self asInteger perform: message selector withArguments: message arguments! !

!CArray methodsFor: 'accessing' stamp: 'di 7/16/2004 12:45'!
floatAt: index
	^ Float fromIEEE32Bit: (self at: index)! !

!CArray methodsFor: 'accessing' stamp: 'di 7/16/2004 12:45'!
floatAt: index put: value
	^ self at: index put: value asIEEE32BitWord! !

!CArray methodsFor: 'accessing' stamp: 'di 7/16/2004 12:45'!
intAt: index
	^ (self at: index) signedIntFromLong! !

!CArray methodsFor: 'accessing' stamp: 'di 7/16/2004 12:45'!
intAt: index put: signedInt
	^ self at: index put: signedInt signedIntToLong! !

!CArray methodsFor: 'private' stamp: 'ajh 8/20/2002 00:30'!
interpreter: interpreterSimulator address: arrayAddress unitSize: numBytes

	interpreter := interpreterSimulator.
	arrayBaseAddress := arrayAddress.
	unitSize := numBytes.
	ptrOffset := 0.
! !

!CArray methodsFor: 'private' stamp: 'ajh 8/20/2002 00:36'!
ptrAddress

	^ arrayBaseAddress + ptrOffset! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/16/2004 12:14'!
at: index
	| value |
	value := simArray at: index+1.
	"Debug only..."
	value ifNil:
		[self error: 'attempt to read an uninitialized field'.
		^ super at: index  "Maybe it was set in Squeak.  Return the raw value"].
	(self bitsOf: value) ~= (super at: index) ifTrue:
		[self error: 'inconsistent values'].
	^ value! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/16/2004 11:28'!
at: index put: value

	super at: index put: (self bitsOf: value).
	^ simArray at: index + 1 put: value.
	! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 13:34'!
bitsOf: value
	"Convert pos and neg ints and floats to 32-bit representations expected by C"

	value isInteger ifTrue:
		[value >= 0 ifTrue: [^ value].
		^ value + 16r80000000 + 16r80000000].
	value isFloat ifTrue:
		[^ value asIEEE32BitWord].
	self error: 'unexpected value for 32 bits'.
	^ 0! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 16:04'!
floatAt: index
	| value |
	value := self at: index.
	value isFloat ifFalse:
		[value = 0 ifTrue: [^ 0.0].
		self error: 'non-float was stored'.
		^ Float fromIEEE32Bit: value].
	^ value! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 13:00'!
floatAt: index put: value

	value isFloat
		ifFalse: [self error: 'inconsistent values'].
	^ self at: index put: value! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 13:02'!
intAt: index
	| value |
	value := self at: index.
	value isInteger
		ifFalse: [self error: 'inconsistent values'].
	^ value! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 13:01'!
intAt: index put: value

	value isInteger
		ifFalse: [self error: 'inconsistent values'].
	^ self at: index put: value! !

!BalloonArray methodsFor: 'memory access' stamp: 'di 7/15/2004 13:17'!
setSimArray: anArray

	simArray := anArray! !

!CCodeGenerator methodsFor: 'public' stamp: 'TPR 3/2/2000 11:22'!
addAllClassVarsFor: aClass
	"Add the class variables for the given class (and its superclasses) to the code base as constants."

	| allClasses |
	allClasses := aClass withAllSuperclasses.
	allClasses do: [:c | self addClassVarsFor: c].
! !

!CCodeGenerator methodsFor: 'public' stamp: 'tpr 2/27/2004 19:07'!
addClass: aClass 
	"Add the variables and methods of the given class to the code base."
	| source |
	self checkClassForNameConflicts: aClass.
	self addClassVarsFor: aClass.
	"ikp..."
	self addPoolVarsFor: aClass.
	variables addAll: aClass instVarNames.
	self retainMethods: aClass requiredMethodNames.
	
	'Adding Class ' , aClass name , '...'
		displayProgressAt: Sensor cursorPoint
		from: 0
		to: aClass selectors size
		during: [:bar | aClass selectors
				doWithIndex: 
					[:sel :i | 
					bar value: i.
					source := aClass sourceCodeAt: sel.
					self addMethod: ((Compiler new
							parse: source
							in: aClass
							notifying: nil)
							asTranslationMethodOfClass: self translationMethodClass)]].
	aClass declareCVarsIn: self! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 7/8/2003 11:16'!
addClassVarsFor: aClass
	"Add the class variables for the given class to the code base as constants."
	| val node |
	aClass classPool associationsDo: [:assoc | 
		val := assoc value.
		(useSymbolicConstants and:[self isCLiteral: val])
			ifTrue:[node := TDefineNode new setName: assoc key asString value: assoc value]
			ifFalse:[node := TConstantNode new setValue: assoc value].
		constants at: assoc key asString put: node].
! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 2/14/1999 01:08'!
addHeaderFile: aString
	"Add a header file. The argument must be a quoted string!!"
	headerFiles addLast: aString.! !

!CCodeGenerator methodsFor: 'utilities'!
addMethod: aTMethod
	"Add the given method to the code base."

	(methods includesKey:  aTMethod selector) ifTrue: [
		self error: 'Method name conflict: ', aTMethod selector.
	].
	methods at: aTMethod selector put: aTMethod.! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 2/3/2001 17:55'!
addMethodsForPrimitives: classAndSelectorList 
	| sel aClass source verbose meth |
	classAndSelectorList do: 
		[:classAndSelector | 
		aClass := Smalltalk at: (classAndSelector at: 1).
		self addAllClassVarsFor: aClass.
"TPR - should pool vars also be added here?"

		"find the method in either the class or the metaclass"
		sel := classAndSelector at: 2.
		(aClass includesSelector: sel)
			ifTrue: [source := aClass sourceCodeAt: sel]
			ifFalse: [source := aClass class sourceCodeAt: sel].

		"compile the method source and convert to a suitable translation 
		method "
		meth := (Compiler new
					parse: source
					in: aClass
					notifying: nil)
					asTranslationMethodOfClass: self translationMethodClass.

		(aClass includesSelector: sel)
			ifTrue: [meth definingClass: aClass]
			ifFalse: [meth definingClass: aClass class].
		meth primitive > 0 ifTrue:[meth preparePrimitiveName].
		"for old-style array accessing: 
		meth covertToZeroBasedArrayReferences."
		meth replaceSizeMessages.
		self addMethod: meth].

	"method preparation"
	verbose := false.
	self prepareMethods.
	verbose
		ifTrue: 
			[self printUnboundCallWarnings.
			self printUnboundVariableReferenceWarnings.
			Transcript cr].

	"code generation"
	self doInlining: true.

	methods do:[:m|
		"if this method is supposed to be a primitive (rather than a helper 
		routine), add assorted prolog and epilog items"
		m primitive > 0 ifTrue: [m preparePrimitivePrologue]].! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 7/8/2003 11:16'!
addPoolVarsFor: aClass 
	"Add the pool variables for the given class to the code base as constants."
	| val node |
	aClass sharedPools do: [:pool |
		pool bindingsDo: [:assoc |
			val := assoc value.
			(useSymbolicConstants and:[self isCLiteral: val])
				ifTrue:[node := TDefineNode new setName: assoc key asString value: assoc value]
				ifFalse:[node := TConstantNode new setValue: assoc value].
			constants at: assoc key asString put: node]].! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'dtl 7/2/2008 16:46'!
builtin: sel 
	"Answer true if the given selector is one of the builtin selectors."
	^ sel = #error:
		or: [(self memoryAccessSelectors includes: sel)
				or: [translationDict includesKey: sel]]! !

!CCodeGenerator methodsFor: 'utilities'!
cCodeForMethod: selector
	"Answer a string containing the C code for the given method."
	"Example:
		((CCodeGenerator new initialize addClass: TestCClass1; prepareMethods)
			cCodeForMethod: #ifTests)"

	| m s |
	m := self methodNamed: selector.
	m = nil ifTrue: [ self error: 'method not found in code base: ', selector ].

	s := (ReadWriteStream on: '').
	m emitCCodeOn: s generator: self.
	^ s contents! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'ar 3/16/2002 15:33'!
cFunctionNameFor: aSelector
	"Create a C function name from the given selector by omitting colons
	and prefixing with the plugin name if the method is exported."
	^aSelector copyWithout: $:! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'ikp 8/3/2004 20:16'!
cLiteralFor: anObject
	"Return a string representing the C literal value for the given object."
	(anObject isKindOf: Integer) ifTrue: [
		(anObject < 16r7FFFFFFF)
			ifTrue: [^ anObject printString]
			ifFalse: [^ anObject printString , ObjectMemory unsignedIntegerSuffix "ikp"]].
	(anObject isKindOf: String) ifTrue: [^ '"', anObject, '"' ].
	(anObject isKindOf: Float) ifTrue: [^ anObject printString ].
	anObject == nil ifTrue: [^ 'null' ].
	anObject == true ifTrue: [^ '1' ].			"ikp"
	anObject == false ifTrue: [^ '0' ].			"ikp"
	(anObject isKindOf: Character) ifTrue:[^anObject asString printString]. "ar"
	self error:								"ikp"
		'Warning: A Smalltalk literal could not be translated into a C constant: ', anObject printString.
	^'"XXX UNTRANSLATABLE CONSTANT XXX"'! !

!CCodeGenerator methodsFor: 'error notification' stamp: 'tpr 6/11/2003 16:36'!
checkClassForNameConflicts: aClass
	"Verify that the given class does not have constant, variable, or method names that conflict with those of previously added classes. Raise an error if a conflict is found, otherwise just return."

	"check for constant name collisions"
	aClass classPool associationsDo: [ :assoc |
		(constants includesKey: assoc key asString) ifTrue: [
			self error: 'Constant was defined in a previously added class: ', assoc key.
		].
	].
	"ikp..."
	aClass sharedPools do: [:pool |
		pool bindingsDo: [ :assoc |
			(constants includesKey: assoc key asString) ifTrue: [
				self error: 'Constant was defined in a previously added class: ', assoc key.
			].
		].
	].

	"check for instance variable name collisions"
	aClass instVarNames do: [ :varName |
		(variables includes: varName) ifTrue: [
			self error: 'Instance variable was defined in a previously added class: ', varName.
		].
	].

	"check for method name collisions"
	aClass selectors do: [ :sel |
		(methods includesKey: sel) ifTrue: [
			self error: 'Method was defined in a previously added class: ', sel.
		].
	].! !

!CCodeGenerator methodsFor: 'utilities'!
checkForGlobalUsage: vars in: aTMethod 
	| item |
	vars
		do: [:var | 
			"TPR - why the use of globalsAsSet here instead of globalVariables? 
			JMM - globalVariables is not initialized yet, variables is an OrderedCollection, 
				globalsAsSet returns variables as needed set"
			(self globalsAsSet includes: var)
				ifTrue: ["find the set of method names using this global var"
					item := globalVariableUsage
								at: var
								ifAbsent: [globalVariableUsage at: var put: Set new].
					"add this method name to that set"
					item add: aTMethod selector]].
	aTMethod referencesGlobalStructMakeZero! !

!CCodeGenerator methodsFor: 'public' stamp: 'ikp 9/26/97 14:48'!
codeString
	"Return a string containing all the C code for the code base. Used for testing."

	| stream |
	stream := ReadWriteStream on: (String new: 1000).
	self emitCCodeOn: stream doInlining: true doAssertions: true.
	^stream contents! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'dtl 6/7/2008 16:57'!
collectInlineList
	"Make a list of methods that should be inlined."
	"Details: The method must not include any inline C, since the translator cannot
	currently map variable names in inlined C code. The #inline: directive may be
	used to override this for cases in which the C code or declarations are harmless.
	Methods to be inlined must be small or called from only one place."

	| methodsNotToInline callsOf inlineIt hasCCode nodeCount senderCount
sel |
	methodsNotToInline := Set new: methods size.

	"build dictionary to record the number of calls to each method"
	callsOf := Dictionary new: methods size * 2.
	methods keys do: [ :s | callsOf at: s put: 0 ].

	"For each method, scan its parse tree once to:
		1. determine if the method contains C code or declarations
		2. determine how many nodes it has
		3. increment the sender counts of the methods it calls
		4. determine if it includes any C declarations or code"
	inlineList := Set new: methods size * 2.
	methods do: [ :m |
		inlineIt := #dontCare.
		(translationDict includesKey: m selector) ifTrue: [
			hasCCode := true.
		] ifFalse: [
			hasCCode := m declarations size > 0.
			nodeCount := 0.
			m parseTree nodesDo: [ :node |
				node isSend ifTrue: [
					sel := node selector.
					(sel = #cCode: or: [sel = #cCode:inSmalltalk:])
						ifTrue: [ hasCCode := true ].
					senderCount := callsOf at: sel ifAbsent: [ nil ].
					nil = senderCount ifFalse: [
						callsOf at: sel put: senderCount + 1.
					].
				].
				nodeCount := nodeCount + 1.
			].
			inlineIt := m extractInlineDirective.  "may be true, false, or
#dontCare"
		].
		(inlineIt ~= true and: [hasCCode or: [inlineIt = false]]) ifTrue: [
			"Don't inline if method has C code or if it contains a negative inline
			directive. If it contains a positive inline directive, permit inlining even
			if C code is present."
			methodsNotToInline add: m selector.
		] ifFalse: [
			((nodeCount < 40) or: [inlineIt = true]) ifTrue: [
				"inline if method has no C code and is either small or contains
inline directive"
				inlineList add: m selector.
			].
		].
	].

	callsOf associationsDo: [ :assoc |
		((assoc value = 1) and: [(methodsNotToInline includes: assoc key)
not]) ifTrue: [
			inlineList add: assoc key.
		].
	].! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 3/16/2002 18:00'!
declareModuleName: nameString
	"add the declaration of a module name, version and local/external tag"

	self var: #moduleName declareC:'const char *moduleName =
#ifdef SQUEAK_BUILTIN_PLUGIN
	"', nameString,' (i)"
#else
	"', nameString,' (e)"
#endif
'.! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'tpr 2/24/2004 20:27'!
doBasicInlining: inlineFlag
	"Inline the bodies of all methods that are suitable for inlining.
	This method does only the basic inlining suitable for both the core VM and plugins - no bytecode inlining etc"

	| pass progress |
	self collectInlineList.
	pass := 0.
	progress := true.
	[progress] whileTrue: [
		"repeatedly attempt to inline methods until no further progress is made"
		progress := false.
		('Inlining pass ', (pass := pass + 1) printString, '...')
			displayProgressAt: Sensor cursorPoint
			from: 0 to: methods size
			during: [:bar |
				methods doWithIndex: [:m :i |
					bar value: i.
					(m tryToInlineMethodsIn: self)
						ifTrue: [progress := true]]]].

! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'dtl 9/8/2008 23:48'!
doInlining: inlineFlag
	"Inline the bodies of all methods that are suitable for inlining."
	"Modified slightly for the core VM translator, since the first level of inlining for the interpret loop must be performed in order that the instruction implementations can easily discover their addresses. Remember to inline the bytecode routines as well"

	inlineFlag ifFalse: [
		self inlineDispatchesInMethodNamed: #interpret localizingVars: #().
		^ self].
	self doBasicInlining: inlineFlag.
	self inlineCaseStatementBranchesInMethodNamed: #interpret localizingVars: #().
	'Inlining bytecodes'
		displayProgressAt: Sensor cursorPoint
		from: 1 to: 2
		during: [:bar |
			self inlineDispatchesInMethodNamed: #interpret
				localizingVars: #(currentBytecode localIP localSP localHomeContext localReturnContext localReturnValue).
			bar value: 1.
			self removeMethodsReferingToGlobals: #(
					currentBytecode localIP localSP localHomeContext)
				except: #(interpret).
			bar value: 2].

	"make receiver on the next line false to generate code for all methods, even those that are inlined or unused"
	true ifTrue: [
		(methods includesKey: #interpret) ifTrue: [
			"only prune when generating the interpreter itself"
			self pruneUnreachableMethods]].
! !

!CCodeGenerator methodsFor: 'utilities'!
emitBuiltinConstructFor: msgNode on: aStream level: level
	"If the given selector is in the translation dictionary, translate it into a target code construct and return true. Otherwise, do nothing and return false."

	| action |
	action := translationDict at: msgNode selector ifAbsent: [ ^false ].
	self perform: action with: msgNode with: aStream with: level.
	^true! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 1/28/2007 13:52'!
emitCCodeOn: aStream doAssertions: assertionFlag
	"Emit C code for all methods in the code base onto the given stream. All inlined method calls should already have been expanded."

	self emitCHeaderOn: aStream.
	self emitCConstantsOn: aStream.
	self emitCFunctionPrototypes: preparedMethodList on: aStream.
	self emitCVariablesOn: aStream.
'Writing Translated Code...'
displayProgressAt: Sensor cursorPoint
from: 0 to: methods size
during: [:bar |
	preparedMethodList doWithIndex: [ :m :i | bar value: i.
		m emitCCodeOn: aStream generator: self.
]].
	self emitExportsOn: aStream.
! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 1/28/2007 14:10'!
emitCCodeOn: aStream doInlining: inlineFlag doAssertions: assertionFlag
	"Emit C code for all methods in the code base onto the given stream. All inlined method calls should already have been expanded."

	self prepareMethodsInlined: inlineFlag doAssertions: assertionFlag.
	^ self emitCCodeOn: aStream doAssertions: assertionFlag
! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'jmv 10/26/2009 08:54'!
emitCConstantsOn: aStream
	"Store the global variable declarations on the given stream."
	| unused constList node |
	unused := constants keys asSet.
	methods do: [ :meth |
		meth parseTree nodesDo: [ :n |
			n isConstant ifTrue: [ unused remove: n name ifAbsent: []]]].
	constList := constants keys reject: [ :any | unused includes: any].
	aStream nextPutAll: '/*** Constants ***/';
		 cr.
	constList asSortedCollection do: [ :varName |
		node := constants at: varName.
		node name isEmpty ifFalse: [
			aStream nextPutAll: '#define '.
			aStream nextPutAll: node name.
			aStream space.
			aStream nextPutAll: (self cLiteralFor: node value).
			aStream cr
		].
	].
	aStream cr.! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 11/9/2006 06:22'!
emitCExpression: aParseNode on: aStream
	"Emit C code for the expression described by the given parse node."

	aParseNode isLeaf ifTrue: [
		"omit parens"
		aParseNode asExpression emitCCodeOn: aStream level: 0 generator: self.
	] ifFalse: [
		aStream nextPut: $(.
		aParseNode asExpression emitCCodeOn: aStream level: 0 generator: self.
		aStream nextPut: $).
	].! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'tpr 3/7/2003 19:55'!
emitCFunctionPrototypes: methodList on: aStream 
	"Store prototype declarations for all non-inlined methods on the given stream."
	| exporting |
	aStream nextPutAll: '/*** Function Prototypes ***/'; cr.
	exporting := false.
	methodList do: [:m | 
		m export
			ifTrue: [exporting
					ifFalse: 
						[aStream nextPutAll: '#pragma export on'; cr.
						exporting := true]]
			ifFalse: [exporting
					ifTrue: 
						[aStream nextPutAll: '#pragma export off'; cr.
						exporting := false]].
		m emitCFunctionPrototype: aStream generator: self.
		aStream nextPutAll: ';'; cr].
	exporting ifTrue: [aStream nextPutAll: '#pragma export off'; cr]! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'jmv 7/30/2012 13:25'!
emitCHeaderForPrimitivesOn: aStream
	"Write a C file header for compiled primitives onto the given stream."

	aStream
		nextPutAll: '/* Automatically generated from Squeak (';
		nextPutAll: PluginMaker versionString;
		nextPutAll: ') on '.
	Time dateAndTimeNow do: [:e | aStream nextPutAll: e asString; nextPut: Character space].
	aStream
		nextPutAll: '*/';
		cr; cr;
		nextPutAll: '#include "sq.h"'; cr; cr.

	"Additional header files"
	headerFiles do:[:hdr|
		aStream nextPutAll:'#include '; nextPutAll: hdr; cr].

	aStream nextPutAll: '
#include "sqMemoryAccess.h"

/*** Imported Functions/Variables ***/
extern sqInt stackValue(sqInt);
extern sqInt stackIntegerValue(sqInt);
extern sqInt successFlag;

/* allows accessing Strings in both C and Smalltalk */
#define asciiValue(c) c
'.
	aStream cr.! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'jmv 7/30/2012 13:25'!
emitCHeaderOn: aStream
	"Write a C file header onto the given stream."

	aStream nextPutAll: '/* '.
	aStream nextPutAll: PluginMaker headerNotice.
	aStream nextPutAll: ' */'; cr; cr.
	self emitGlobalStructFlagOn: aStream.
	aStream nextPutAll: '#include "sq.h"'; cr.

	"Additional header files"
	headerFiles do:[:hdr|
		aStream nextPutAll:'#include '; nextPutAll: hdr; cr].

	aStream nextPutAll: '
#include "sqMemoryAccess.h"

sqInt printCallStack(void);
void defaultErrorProc(char *s) {
	/* Print an error message and exit. */
	static sqInt printingStack = false;

	printf("\n%s\n\n", s);
	if (!!printingStack) {
		/* flag prevents recursive error when trying to print a broken stack */
		printingStack = true;
		printCallStack();
	}
	exit(-1);
}
'.
	aStream cr.! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 11/9/2006 06:42'!
emitCTestBlock: aBlockNode on: aStream
	"Emit C code for the given block node to be used as a loop test."

	aBlockNode statements size > 1 ifTrue: [
		aBlockNode emitCCodeOn: aStream level: 0 generator: self.
	] ifFalse: [
		aBlockNode statements first asExpression emitCCodeOn: aStream level: 0 generator: self.
	].! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'tpr 4/12/2006 11:40'!
emitCVariablesOn: aStream 
	"Store the global variable declarations on the given stream."
	| varString |
	aStream nextPutAll: '/*** Variables ***/';
		 cr.
	variables asSortedCollection
		do: [:var | 
			varString := var asString.
			self isGeneratingPluginCode
				ifTrue: [varString = 'interpreterProxy'
						ifTrue: ["quite special..."
							aStream cr; nextPutAll: '#ifdef SQUEAK_BUILTIN_PLUGIN'.
							aStream cr; nextPutAll: 'extern'.
							aStream cr; nextPutAll: '#endif'; cr]
						ifFalse: [aStream nextPutAll: 'static ']].
			(variableDeclarations includesKey: varString)
				ifTrue: [aStream nextPutAll: (variableDeclarations at: varString) , ';'; cr]
				ifFalse: ["default variable declaration"
					aStream nextPutAll: 'sqInt ' , varString , ';'; cr]].
	aStream cr! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'tpr 1/10/2003 16:17'!
emitExportsOn: aStream
	"Store all the exported primitives in a form to be used by the internal named prim system"
	aStream nextPutAll:'

void* vm_exports[][3] = {'.
	self exportedPrimitiveNames do:[:primName|
		aStream cr;
			nextPutAll:'	{"", "'; 
			nextPutAll: primName; 
			nextPutAll:'", (void*)'; 
			nextPutAll: primName;
			nextPutAll:'},'.
	].
	aStream nextPutAll:'
	{NULL, NULL, NULL}
};
'.! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'ikp 9/10/2003 05:53'!
emitGlobalStructFlagOn: aStream
	"Default: do nothing.  Overridden in CCGenGlobalStruct."
! !

!CCodeGenerator methodsFor: 'public' stamp: 'dtl 2/5/2007 07:45'!
exportedPrimitiveNames
	"Return an array of all exported primitives"
	^methods select:[:m| m export] thenCollect:[:m| m selectorForCodeGeneration copyWithout: $:].
! !

!CCodeGenerator methodsFor: 'C translation'!
generateAnd: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' && '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ar 10/3/1998 13:45'!
generateAsFloat: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll:'((double) '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' )'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ikp 6/9/2004 16:13'!
generateAsInteger: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll:'((sqInt) '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' )'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:24'!
generateAt: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: '['.
	msgNode args first asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: ']'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:23'!
generateAtPut: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: '['.
	msgNode args first asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: '] = '.
	self emitCExpression: msgNode args last on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/11/2007 11:45'!
generateBaseHeaderSize: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: ' ', ObjectMemory baseHeaderSize asString
! !

!CCodeGenerator methodsFor: 'C translation'!
generateBitAnd: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' & '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateBitInvert32: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '~'.
	self emitCExpression: msgNode receiver on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateBitOr: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' | '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ikp 8/4/2004 16:29'!
generateBitShift: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| arg rcvr |
	arg := msgNode args first.
	rcvr := msgNode receiver.
	arg isConstant ifTrue: [
		"bit shift amount is a constant"
		aStream nextPutAll: '((usqInt) '.
		self emitCExpression: rcvr on: aStream.
		arg value < 0 ifTrue: [
			aStream nextPutAll: ' >> ', arg value negated printString.
		] ifFalse: [
			aStream nextPutAll: ' << ', arg value printString.
		].
		aStream nextPutAll: ')'.
	] ifFalse: [
		"bit shift amount is an expression"
		aStream nextPutAll: '(('.
		self emitCExpression: arg on: aStream.
		aStream nextPutAll: ' < 0) ? ((usqInt) '.
		self emitCExpression: rcvr on: aStream.
		aStream nextPutAll: ' >> -'.
		self emitCExpression: arg on: aStream.
		aStream nextPutAll: ') : ((usqInt) '.
		self emitCExpression: rcvr on: aStream.
		aStream nextPutAll: ' << '.
		self emitCExpression: arg on: aStream.
		aStream nextPutAll: '))'.
	].! !

!CCodeGenerator methodsFor: 'C translation'!
generateBitXor: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' ^ '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/10/2007 18:24'!
generateBytesPerWord: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: ' ', ObjectMemory bytesPerWord asString
! !

!CCodeGenerator methodsFor: 'C translation'!
generateCCoercion: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '(('.
	aStream nextPutAll: msgNode args last value.
	aStream nextPutAll: ') '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ')'.

! !

!CCodeGenerator methodsFor: 'accessing' stamp: 'ar 7/8/2003 11:26'!
generateDeadCode
	"Answer whether we should generate 'dead code' branches. This can be useful for hacking the VM when used in conjunction with #useSymbolicConstants, e.g., for code like:
		DoAssertionChecks ifTrue:[
			...
		].

	we will generate

		#define DoAssertionChecks 0
		...
		if(DoAssertionChecks) {
			...
		}.

	allowing us to change the #define (or redefine it as a variable) for later use."
	^generateDeadCode! !

!CCodeGenerator methodsFor: 'accessing' stamp: 'ar 7/8/2003 11:26'!
generateDeadCode: aBool
	"Indicate whether we should generate 'dead code' branches."
	generateDeadCode := aBool! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ikp 6/9/2004 16:14'!
generateDivide: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| rcvr arg divisor |
	rcvr := msgNode receiver.
	arg := msgNode args first.
	(arg isConstant and:
	 [UseRightShiftForDivide and:
	 [(divisor := arg value) isInteger and:
	 [divisor isPowerOfTwo and:
	 [divisor > 0 and:
	 [divisor <= (1 bitShift: 31)]]]]])
	ifTrue: [
		"use signed (arithmetic) right shift instead of divide"
		aStream nextPutAll: '((sqInt) '.
		self emitCExpression: rcvr on: aStream.
		aStream nextPutAll: ' >> ', (divisor log: 2) asInteger printString.
		aStream nextPutAll: ')'.
	] ifFalse: [
		self emitCExpression: rcvr on: aStream.
		aStream nextPutAll: ' / '.
		self emitCExpression: arg on: aStream].
! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:24'!
generateDoWhileFalse: msgNode on: aStream indent: level
	"Generate do {stmtList} while(!!(cond))"

	| stmts testStmt |
	stmts := msgNode receiver statements asOrderedCollection.
	testStmt := stmts removeLast.
	msgNode receiver setStatements: stmts.
	aStream nextPutAll: 'do {'; cr.
	msgNode receiver emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '} while(!!('.
	testStmt asExpression emitCCodeOn: aStream level: 0 generator: self.
	aStream nextPutAll: '))'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:24'!
generateDoWhileTrue: msgNode on: aStream indent: level
	"Generate do {stmtList} while(cond)"

	| stmts testStmt |
	stmts := msgNode receiver statements asOrderedCollection.
	testStmt := stmts removeLast.
	msgNode receiver setStatements: stmts.
	aStream nextPutAll: 'do {'; cr.
	msgNode receiver emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '} while('.
	testStmt asExpression emitCCodeOn: aStream level: 0 generator: self.
	aStream nextPutAll: ')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateEqual: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' == '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateGreaterThan: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' > '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateGreaterThanOrEqual: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' >= '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 10/29/2006 14:12'!
generateIfFalse: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."
	"Note: PP 2.3 compiler produces two arguments for ifFalse:, presumably
	 to help with inlining later. Taking the last agument should do the correct
	 thing even if your compiler is different."

	| const |
	const := self nilOrBooleanConstantReceiverOf: msgNode.
	const ifNotNil: [
		const ifFalse: [msgNode args first emitCCodeOn: aStream level: level generator: self].
		^ self].

	aStream nextPutAll: 'if (!!('.
	msgNode receiver asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: ')) {'; cr.
	msgNode args last emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 10/29/2006 14:12'!
generateIfFalseIfTrue: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."
	"Note: PP 2.3 compiler reverses the argument blocks for ifFalse:ifTrue:,
       presumably to help with inlining later. That is, the first argument
       is the block to be evaluated if the condition is true. Squeak's compiler
	  does not reverse the blocks, but you may need to fix this method if
	  you wish to cross-compile using VisualWorks."

	| const |
	const := self nilOrBooleanConstantReceiverOf: msgNode.
	const ifNotNil: [
		const
			ifTrue: [msgNode args last emitCCodeOn: aStream level: level generator: self]
			ifFalse: [msgNode args first emitCCodeOn: aStream level: level generator: self].
		^ self].

	aStream nextPutAll: 'if ('.
	msgNode receiver asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: ') {'; cr.
	msgNode args first emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '} else {'; cr.
	msgNode args last emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 10/29/2006 14:19'!
generateIfTrue: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| const |
	const := self nilOrBooleanConstantReceiverOf: msgNode.
	const ifNotNil: [
		const ifTrue: [msgNode args first emitCCodeOn: aStream level: level generator: self].
		^ self].

	aStream nextPutAll: 'if ('.
	msgNode receiver asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: ') {'; cr.
	msgNode args first emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 10/29/2006 14:13'!
generateIfTrueIfFalse: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| const |
	const := self nilOrBooleanConstantReceiverOf: msgNode.
	const ifNotNil: [
		const
			ifTrue: [msgNode args first emitCCodeOn: aStream level: level generator: self]
			ifFalse: [msgNode args last emitCCodeOn: aStream level: level generator: self].
		^ self].

	aStream nextPutAll: 'if ('.
	msgNode receiver asExpression emitCCodeOn: aStream level: level generator: self.
	aStream nextPutAll: ') {'; cr.
	msgNode args first emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '} else {'; cr.
	msgNode args last emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateInlineCCode: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: msgNode args first value.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 19:47'!
generateInlineCppDirective: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream cr; nextPutAll: '# ', msgNode args first value.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/12/2006 17:30'!
generateInlineCppIfDef: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| comment |
	aStream cr; nextPutAll: '# ifdef ', msgNode args first value.
	comment := msgNode args third value.
	(comment isKindOf: String)
		ifTrue: [aStream nextPutAll: '  // ', comment]
		ifFalse: ["nil argument, ignore it"].
	aStream cr.
	msgNode isExpression
		ifTrue:
			[aStream tab: level + 1; nextPut: $(.
			msgNode args fourth asExpression
				emitCCodeOn: aStream level: level + 1 generator: self.
			aStream nextPut: $); cr]
		ifFalse:
			[msgNode args fourth
				emitCCodeOn: aStream level: level generator: self].
	aStream nextPutAll: '# endif  // ', msgNode args first value; cr; tab: level! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/12/2006 17:30'!
generateInlineCppIfDefElse: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| comment alternateBlock alternateBlockIsNil |
	aStream cr; nextPutAll: '# ifdef ', msgNode args first value.
	comment := msgNode args third value.
	(comment isKindOf: String)
		ifTrue: [aStream nextPutAll: '  // ', comment]
		ifFalse: ["nil argument, ignore it"].
	aStream cr.
	msgNode isExpression
		ifTrue:
			[aStream tab: level + 1; nextPut: $(.
			msgNode args fourth asExpression
				emitCCodeOn: aStream level: level + 1 generator: self.
			aStream nextPut: $); cr]
		ifFalse:
			[msgNode args fourth
				emitCCodeOn: aStream level: level generator: self].
	alternateBlock := msgNode args fifth.
	alternateBlockIsNil := true. "check for nil #else clause"
	alternateBlock nodesDo: [:n |
		(n ~= alternateBlock and: [n name ~= 'nil'])
			ifTrue: [alternateBlockIsNil := false ]].
	(alternateBlockIsNil) ifFalse:
		[aStream nextPutAll: '# else'; cr.
		msgNode isExpression
			ifTrue:
				[aStream tab: level + 1; nextPut: $(.
				alternateBlock asExpression
					emitCCodeOn: aStream level: level + 1 generator: self.
				aStream nextPut: $); cr]
			ifFalse:
				[alternateBlock
					emitCCodeOn: aStream level: level generator: self]].
	aStream nextPutAll: '# endif  // ', msgNode args first value; cr; tab: level
! !

!CCodeGenerator methodsFor: 'C translation'!
generateInlineDirective: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '/* inline: '.
	aStream nextPutAll: msgNode args first name.
	aStream nextPutAll: ' */'.
! !

!CCodeGenerator methodsFor: 'C translation'!
generateIntegerObjectOf: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '(('.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ' << 1) | 1)'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateIntegerValueOf: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '('.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ' >> 1)'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateIsIntegerObject: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '('.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ' & 1)'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateIsNil: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' == '.
	aStream nextPutAll: (self cLiteralFor: nil).! !

!CCodeGenerator methodsFor: 'C translation'!
generateLessThan: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' < '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateLessThanOrEqual: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' <= '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateMax: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '(('.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' < '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ') ? '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ' : '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateMin: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '(('.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' < '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ') ? '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' : '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: ')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateMinus: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' - '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateModulo: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' % '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateNot: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '!!'.
	self emitCExpression: msgNode receiver on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateNotEqual: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' !!= '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateNotNil: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' !!= '.
	aStream nextPutAll: (self cLiteralFor: nil).! !

!CCodeGenerator methodsFor: 'C translation'!
generateOr: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' || '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ar 5/25/2000 16:36'!
generatePerform: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: '('.
	(msgNode args copyFrom: 2 to: msgNode args size) do:[:arg|
		self emitCExpression: arg on: aStream.
	] separatedBy:[aStream nextPutAll:', '].
	aStream nextPutAll:')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generatePlus: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' + '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ATG 4/8/2004 15:03'!
generatePreDecrement: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| varNode |
	varNode := msgNode receiver.
	varNode isVariable
		ifFalse: [ self error: 'preDecrement can only be applied to variables' ].
	aStream nextPutAll: '--'.
	aStream nextPutAll: (self returnPrefixFromVariable: varNode name).
! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ikp 9/11/2003 20:08'!
generatePreIncrement: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| varNode |
	varNode := msgNode receiver.
	varNode isVariable
		ifFalse: [ self error: 'preIncrement can only be applied to variables' ].
	aStream nextPutAll: '++'.
	aStream nextPutAll: (self returnPrefixFromVariable: varNode name).
! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ar 2/15/1999 21:43'!
generateRaisedTo: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll:'pow('.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll:')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateSequentialAnd: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' && ('.
	self emitCTestBlock: msgNode args first on: aStream.
	aStream nextPutAll: ')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateSequentialOr: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."
	"Note: PP 2.3 compiler produces two arguments for or:, presumably
	 to help with inlining later. Taking the last agument should do the correct
	 thing even if your compiler is different."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' || ('.
	self emitCTestBlock: msgNode args last on: aStream.
	aStream nextPutAll: ')'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateSharedCodeDirective: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '/* common code: '.
	aStream nextPutAll: msgNode args first value.
	aStream nextPutAll: ' */'.
! !

!CCodeGenerator methodsFor: 'C translation'!
generateShiftLeft: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' << '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'ikp 8/4/2004 18:25'!
generateShiftRight: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	aStream nextPutAll: '((usqInt) '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ')'.
	aStream nextPutAll: ' >> '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation'!
generateTimes: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: ' * '.
	self emitCExpression: msgNode args first on: aStream.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'jm 2/6/2001 20:03'!
generateToByDo: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| iterationVar step |
	(msgNode args last args size = 1) ifFalse: [
		self error: 'wrong number of block arguments'.
	].
	iterationVar := msgNode args last args first.
	aStream nextPutAll: 'for (', iterationVar, ' = '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: '; ', iterationVar,
		(((step := msgNode args at: 2) isConstant and: [step value < 0])
			ifTrue: [' >= '] ifFalse: [' <= ']).
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: '; ', iterationVar, ' += '.
	self emitCExpression: step on: aStream.
	aStream nextPutAll: ') {'; cr.
	msgNode args last emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation'!
generateToDo: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| iterationVar |
	(msgNode args last args size = 1) ifFalse: [
		self error: 'wrong number of block arguments'.
	].
	iterationVar := msgNode args last args first.
	aStream nextPutAll: 'for (', iterationVar, ' = '.
	self emitCExpression: msgNode receiver on: aStream.
	aStream nextPutAll: '; ', iterationVar, ' <= '.
	self emitCExpression: msgNode args first on: aStream.
	aStream nextPutAll: '; ', iterationVar, '++) {'; cr.
	msgNode args last emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'tpr 7/26/2003 10:23'!
generateTouch: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream - which is to say absolutely nothing"
! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'acg 12/22/1999 01:41'!
generateWhileFalse: msgNode on: aStream indent: level
	"Generate C code for a loop in one of the following formats, as appropriate:
		while(!!(cond)) { stmtList }
		do {stmtList} while(!!(cond))
		while(1) {stmtListA; if (cond) break; stmtListB}"

	msgNode receiver statements size <= 1
		ifTrue: [^self generateWhileFalseLoop: msgNode on: aStream indent: level].
	msgNode args first isNilStmtListNode
		ifTrue: [^self generateDoWhileFalse: msgNode on: aStream indent: level].
	^self generateWhileForeverBreakTrueLoop: msgNode on: aStream indent: level! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'acg 12/22/1999 01:41'!
generateWhileFalseLoop: msgNode on: aStream indent: level
	"Generate while(!!(cond)) {stmtList}."

	aStream nextPutAll: 'while (!!('.
	self emitCTestBlock: msgNode receiver on: aStream.
	aStream nextPutAll: ')) {'; cr.
	msgNode args first isNilStmtListNode ifFalse:
		[msgNode args first emitCCodeOn: aStream level: level + 1 generator: self].
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:28'!
generateWhileForeverBreakFalseLoop: msgNode on: aStream indent: level
	"Generate while(1) {stmtListA; if(!!(cond)) break; stmtListB}."

	| stmts testStmt |
	stmts := msgNode receiver statements asOrderedCollection.
	testStmt := stmts removeLast.
	msgNode receiver setStatements: stmts.
	level - 1 timesRepeat: [ aStream tab ].
	aStream nextPutAll: 'while (1) {'; cr.
	msgNode receiver emitCCodeOn: aStream level: level + 1 generator: self.
	(level + 1) timesRepeat: [ aStream tab ].
	aStream nextPutAll: 'if (!!('.
	testStmt asExpression emitCCodeOn: aStream level: 0 generator: self.
	aStream nextPutAll: ')) break;'; cr.
	msgNode args first emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 11/9/2006 06:28'!
generateWhileForeverBreakTrueLoop: msgNode on: aStream indent: level
	"Generate while(1) {stmtListA; if(cond) break; stmtListB}."

	| stmts testStmt |
	stmts := msgNode receiver statements asOrderedCollection.
	testStmt := stmts removeLast.
	msgNode receiver setStatements: stmts.
	level - 1 timesRepeat: [ aStream tab ].
	aStream nextPutAll: 'while (1) {'; cr.
	msgNode receiver emitCCodeOn: aStream level: level + 1 generator: self.
	(level + 1) timesRepeat: [ aStream tab ].
	aStream nextPutAll: 'if ('.
	testStmt asExpression emitCCodeOn: aStream level: 0 generator: self.
	aStream nextPutAll: ') break;'; cr.
	msgNode args first emitCCodeOn: aStream level: level + 1 generator: self.
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'acg 12/22/1999 01:41'!
generateWhileTrue: msgNode on: aStream indent: level
	"Generate C code for a loop in one of the following formats, as appropriate:
		while(cond) { stmtList }
		do {stmtList} while(cond)
		while(1) {stmtListA; if (!!(cond)) break; stmtListB}"

	msgNode receiver statements size <= 1
		ifTrue: [^self generateWhileTrueLoop: msgNode on: aStream indent: level].
	msgNode args first isNilStmtListNode
		ifTrue: [^self generateDoWhileTrue: msgNode on: aStream indent: level].
	^self generateWhileForeverBreakFalseLoop: msgNode on: aStream indent: level! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'acg 12/22/1999 01:38'!
generateWhileTrueLoop: msgNode on: aStream indent: level
	"Generate while(cond) {stmtList}."

	aStream nextPutAll: 'while ('.
	self emitCTestBlock: msgNode receiver on: aStream.
	aStream nextPutAll: ') {'; cr.
	msgNode args first isNilStmtListNode ifFalse:
		[msgNode args first emitCCodeOn: aStream level: level + 1 generator: self].
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!CCodeGenerator methodsFor: 'public'!
globalsAsSet
	"Used by the inliner to avoid name clashes with global variables."

	((variablesSetCache == nil) or:
	 [variablesSetCache size ~= variables size]) ifTrue: [
		variablesSetCache := variables asSet.
	].
	^ variablesSetCache! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 7/8/2003 11:16'!
initialize
	translationDict := Dictionary new.
	inlineList := Array new.
	constants := Dictionary new: 100.
	variables := OrderedCollection new: 100.
	variableDeclarations := Dictionary new: 100.
	methods := Dictionary new: 500.
	self initializeCTranslationDictionary.
	headerFiles := OrderedCollection new.
	globalVariableUsage := Dictionary new.
	useSymbolicConstants := true.
	generateDeadCode := true.! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'dtl 8/14/2009 09:41'!
initializeCTranslationDictionary 
	"Initialize the dictionary mapping message names to actions for C code generation."

	| pairs |
	translationDict := Dictionary new: 200.
	pairs := #(
	#&				#generateAnd:on:indent:
	#|				#generateOr:on:indent:
	#and:			#generateSequentialAnd:on:indent:
	#or:			#generateSequentialOr:on:indent:
	#not			#generateNot:on:indent:

	#+				#generatePlus:on:indent:
	#-				#generateMinus:on:indent:
	#*				#generateTimes:on:indent:
	#/				#generateDivide:on:indent:
	#//				#generateDivide:on:indent:
	#\\				#generateModulo:on:indent:
	#<<				#generateShiftLeft:on:indent:
	#>>				#generateShiftRight:on:indent:
	#min:			#generateMin:on:indent:
	#max:			#generateMax:on:indent:

	#bitAnd:		#generateBitAnd:on:indent:
	#bitOr:			#generateBitOr:on:indent:
	#bitXor:			#generateBitXor:on:indent:
	#bitShift:		#generateBitShift:on:indent:
	#bitInvert32	#generateBitInvert32:on:indent:

	#<				#generateLessThan:on:indent:
	#<=				#generateLessThanOrEqual:on:indent:
	#=				#generateEqual:on:indent:
	#>				#generateGreaterThan:on:indent:
	#>=				#generateGreaterThanOrEqual:on:indent:
	#~=				#generateNotEqual:on:indent:
	#==				#generateEqual:on:indent:
	#~~				#generateNotEqual:on:indent:
	#isNil			#generateIsNil:on:indent:
	#notNil			#generateNotNil:on:indent:

	#whileTrue: 	#generateWhileTrue:on:indent:
	#whileFalse:	#generateWhileFalse:on:indent:
	#whileTrue 		#generateDoWhileTrue:on:indent:
	#whileFalse		#generateDoWhileFalse:on:indent:
	#to:do:			#generateToDo:on:indent:
	#to:by:do:		#generateToByDo:on:indent:

	#ifTrue:		#generateIfTrue:on:indent:
	#ifFalse:		#generateIfFalse:on:indent:
	#ifTrue:ifFalse:	#generateIfTrueIfFalse:on:indent:
	#ifFalse:ifTrue:	#generateIfFalseIfTrue:on:indent:

	#at:				#generateAt:on:indent:
	#at:put:			#generateAtPut:on:indent:
	#basicAt:		#generateAt:on:indent:
	#basicAt:put:	#generateAtPut:on:indent:

	#integerValueOf:	#generateIntegerValueOf:on:indent:
	#integerObjectOf:	#generateIntegerObjectOf:on:indent:
	#isIntegerObject: 	#generateIsIntegerObject:on:indent:
	#cCode:				#generateInlineCCode:on:indent:
	#cCode:inSmalltalk:	#generateInlineCCode:on:indent:
	#cCoerce:to:			#generateCCoercion:on:indent:
	#preprocessorExpression:		#generateInlineCppDirective:on:indent:
	#isDefined:inSmalltalk:comment:ifTrue:	#generateInlineCppIfDef:on:indent:
	#isDefined:inSmalltalk:comment:ifTrue:ifFalse:	#generateInlineCppIfDefElse:on:indent:
	#preIncrement		#generatePreIncrement:on:indent:
	#preDecrement		#generatePreDecrement:on:indent:
	#inline:				#generateInlineDirective:on:indent:
	#sharedCodeNamed:inCase:	#generateSharedCodeDirective:on:indent:
	#asFloat				#generateAsFloat:on:indent:
	#asInteger			#generateAsInteger:on:indent:
	#anyMask:			#generateBitAnd:on:indent:
	#raisedTo:			#generateRaisedTo:on:indent:
	#touch:				#generateTouch:on:indent:
	#bytesPerWord		#generateBytesPerWord:on:indent:
	#baseHeaderSize		#generateBaseHeaderSize:on:indent:

	#perform:						#generatePerform:on:indent:
	#perform:with:					#generatePerform:on:indent:
	#perform:with:with:				#generatePerform:on:indent:
	#perform:with:with:with:		#generatePerform:on:indent:
	#perform:with:with:with:with:	#generatePerform:on:indent:

	).

	1 to: pairs size by: 2 do: [:i |
		translationDict at: (pairs at: i) put: (pairs at: i + 1)].
! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'dtl 6/21/2008 12:54'!
inlineCaseStatementBranchesInMethodNamed: selector localizingVars: varsList 
	"Inline case statement branches in the method with the given name."
	(self methodNamed: selector)
		ifNotNilDo: [:m | m inlineCaseStatementBranchesIn: self localizingVars: varsList]! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'ar 7/8/2003 11:27'!
inlineDispatchesInMethodNamed: selector localizingVars: varsList
	"Inline dispatches (case statements) in the method with the given name."

	| m varString |
	m := self methodNamed: selector.
	m = nil ifFalse: [
		m inlineCaseStatementBranchesIn: self localizingVars: varsList.
		m parseTree nodesDo: [ :n |
			n isCaseStmt ifTrue: [
				n customizeShortCasesForDispatchVar: 'currentBytecode' in: self method: m.
			].
		].
	].
	variables := variables asOrderedCollection.
	varsList do: [ :v |
		varString := v asString.
		variables remove: varString ifAbsent: [].
		(variableDeclarations includesKey: varString) ifTrue: [
			m declarations at: v asString put: (variableDeclarations at: varString).
			variableDeclarations removeKey: varString.
		].
	].
! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'ar 7/8/2003 11:04'!
isCLiteral: anObject
	(anObject isKindOf: Integer) ifTrue: [^true].
	(anObject isKindOf: String) ifTrue: [^true].
	(anObject isKindOf: Float) ifTrue: [^true].
	anObject == nil ifTrue: [^true].
	anObject == true ifTrue: [^true].			"ikp"
	anObject == false ifTrue: [^true].			"ikp"
	(anObject isKindOf: Character) ifTrue:[^true]. "ar"
	^false! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'ar 10/7/1998 17:53'!
isGeneratingPluginCode
	^false! !

!CCodeGenerator methodsFor: 'public' stamp: 'JMM 11/28/2002 11:52'!
isGlobalStructureBuild
	^false! !

!CCodeGenerator methodsFor: 'utilities'!
localizeGlobalVariables
	| candidates procedure |

	"find all globals used in only one method"
	candidates := globalVariableUsage select: [:e | e size = 1].
	variables removeAllFoundIn: candidates keys.

	"move any suitable global to be local to the single method using it"
	candidates keysAndValuesDo: [:key :targets | 
		targets do: [:name |
			procedure := methods at: name.
			procedure locals add: key.
			variableDeclarations at: key ifPresent: [:v | 
				procedure declarations at: key put: v.
				variableDeclarations removeKey: key]]].! !

!CCodeGenerator methodsFor: 'inlining'!
mayInline: sel
	"Answer true if the method with the given selector may be inlined."

	^ inlineList includes: sel! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'dtl 7/3/2008 12:16'!
memoryAccessSelectors
	"Answer the selectors used for low level memory access. These are traditionally
	implemented as C preprocessor macros (or static inlined functions) in the external
	support code, but may also be implemented as Smalltalk methods for translation to C."

	^ { #byteAt: . #byteAt:put: .
		#shortAt: . #shortAt:put: .
		#intAt: . #intAt:put: .
		#longAt: . #longAt:put: .
		#byteAtPointer: . #byteAtPointer:put: .
		#shortAtPointer: . #shortAtPointer:put: .
		#intAtPointer: . #intAtPointer:put: .
		#longAtPointer: . #longAtPointer:put: }! !

!CCodeGenerator methodsFor: 'utilities'!
methodNamed: selector
	"Answer the method in the code base with the given selector."

	^ methods at: selector ifAbsent: [ nil ]! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'ls 10/10/1999 13:55'!
methodStatsString
	"Return a string describing the size, # of locals, and # of senders of
each method. Note methods that have inline C code or C declarations."

	| methodsWithCCode sizesOf callsOf hasCCode nodeCount senderCount s
calls registers selr m |
	methodsWithCCode := Set new: methods size.
	sizesOf := Dictionary new: methods size * 2.  "selector -> nodeCount"
	callsOf := Dictionary new: methods size * 2.  "selector -> senderCount"

	"For each method, scan its parse tree once to:
		1. determine if the method contains C code or declarations
		2. determine how many nodes it has
		3. increment the sender counts of the methods it calls
		4. determine if it includes any C declarations or code"

	methods do: [ :m0 |  m := m0.
		(translationDict includesKey: m selector) ifTrue: [
			hasCCode := true.
		] ifFalse: [
			hasCCode := m declarations size > 0.
			nodeCount := 0.
			m parseTree nodesDo: [ :node |
				node isSend ifTrue: [
					selr := node selector.
					selr = #cCode: ifTrue: [ hasCCode := true ].
					senderCount := callsOf at: selr ifAbsent: [ 0 ].
					callsOf at: selr put: senderCount + 1.
				].
				nodeCount := nodeCount + 1.
			].
		].
		hasCCode ifTrue: [ methodsWithCCode add: m selector ].
		sizesOf at: m selector put: nodeCount.
	].

	s := WriteStream on: (String new: 5000).
	methods keys asSortedCollection do: [ :sel |
		m := methods at: sel.
		registers := m locals size + m args size.
		calls := callsOf at: sel ifAbsent: [0].
		registers > 11 ifTrue: [
			s nextPutAll: sel; tab.
			s nextPutAll: (sizesOf at: sel) printString; tab.
			s nextPutAll: calls printString; tab.
			s nextPutAll: registers printString; tab.
			(methodsWithCCode includes: sel) ifTrue: [ s nextPutAll: 'CCode' ].
		s cr.
		].
	].
	^ s contents! !

!CCodeGenerator methodsFor: 'utilities'!
methodsReferringToGlobal: v
	"Return a collection of methods that refer to the given global variable."

	| out |
	out := OrderedCollection new.
	methods associationsDo: [ :assoc |
		(assoc value freeVariableReferences includes: v) ifTrue: [
			out add: assoc key.
		].
	].
	^ out! !

!CCodeGenerator methodsFor: 'utilities'!
methodsThatCanInvoke: aSelectorList
	"Return a set of methods that can invoke one of the given selectors, either directly or via a sequence of intermediate methods."

	| out todo sel mSelector |
	out := Set new.
	todo := aSelectorList copy asOrderedCollection.
	[todo isEmpty] whileFalse: [
		sel := todo removeFirst.
		out add: sel.
		methods do: [ :m |
			(m allCalls includes: sel) ifTrue: [
				mSelector := m selector.
				((out includes: mSelector) or:
				 [todo includes: mSelector]) ifFalse: [
					todo add: mSelector.
				].
			].
		].
	].
	^ out
	! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'ar 7/8/2003 11:16'!
nilOrBooleanConstantReceiverOf: sendNode
	"Answer nil or the boolean constant that is the receiver of the given message send. Used to suppress conditional code when the condition is a translation-time constant."

	| rcvr val |
	generateDeadCode ifTrue:[^nil].
	rcvr := sendNode receiver.
	rcvr isConstant ifTrue: [
		val := rcvr value.
		((val == true) or: [val == false]) ifTrue: [^ val]].
	^ nil
! !

!CCodeGenerator methodsFor: 'utilities'!
prepareMethods
	"Prepare methods for browsing."

	| globals |
	globals := Set new: 200.
	globals addAll: variables.
	methods do: [ :m |
		(m locals, m args) do: [ :var |
			(globals includes: var) ifTrue: [
				self error: 'Local variable name may mask global when inlining: ', var.
			].
			(methods includesKey: var) ifTrue: [
				self error: 'Local variable name may mask method when inlining: ', var.
			].	
		].
		m bindClassVariablesIn: constants.
		m prepareMethodIn: self.
	].! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 1/28/2007 14:10'!
prepareMethodsInlined: inlineFlag doAssertions: assertionFlag
	"Prepare to emit C code for all methods in the code base. All inlined method calls should be expanded. Answer a list of methods to be emitted as C code."

	| verbose methodList |
	"method preparation"
	verbose := false.
	self prepareMethods.
	verbose ifTrue: [
		self printUnboundCallWarnings.
		self printUnboundVariableReferenceWarnings.
		Transcript cr.
	].
	assertionFlag ifFalse: [ self removeAssertions ].
	self doInlining: inlineFlag.

	"code generation"

	methodList := methods asSortedCollection: [ :m1 :m2 | m1 selector < m2 selector ].
	"clean out no longer valid variable names and then handle any global
		variable usage in each method"
	methodList do: [:m | self checkForGlobalUsage: m removeUnusedTemps in: m].
	self localizeGlobalVariables.
	^ preparedMethodList := methodList
! !

!CCodeGenerator methodsFor: 'private' stamp: 'sma 3/3/2000 12:08'!
printArray: array on: aStream
	| first |
	first := true.
	1 to: array size do:
		[:i |
		first 
			ifTrue: [first := false]
			ifFalse: [aStream nextPutAll: ', '].
		i \\ 16 = 1 ifTrue: [aStream cr].
		self printInt: (array at: i) on: aStream]! !

!CCodeGenerator methodsFor: 'private' stamp: 'sma 3/3/2000 12:13'!
printInt: int on: aStream
	aStream print: int.
	(int between: -2147483648 and: 2147483647)
		ifFalse: [(int between: 2147483648 and: 4294967295)
			ifTrue: [aStream nextPut: $U]
			ifFalse: [aStream nextPut: $L]]! !

!CCodeGenerator methodsFor: 'error notification'!
printUnboundCallWarnings
	"Print a warning message for every unbound method call in the code base."

	| knownSelectors undefinedCalls |
	undefinedCalls := Dictionary new.
	knownSelectors := translationDict keys asSet.
	knownSelectors add: #error:.
	methods do: [ :m | knownSelectors add: m selector ].
	methods do: [ :m |
		m allCalls do: [ :sel |
			(knownSelectors includes: sel) ifFalse: [
				(undefinedCalls includesKey: sel)
					ifTrue: [ (undefinedCalls at: sel) add: m selector ]
					ifFalse: [ undefinedCalls at: sel put: (OrderedCollection with: m selector) ].
			].
		].
	].

	Transcript cr.
	undefinedCalls keys asSortedCollection do: [ :undefined |
		Transcript show: undefined, ' -- undefined method sent by:'; cr.
		(undefinedCalls at: undefined) do: [ :caller |
			Transcript tab; show: caller; cr.
		].
	].! !

!CCodeGenerator methodsFor: 'error notification'!
printUnboundVariableReferenceWarnings
	"Print a warning message for every unbound variable reference in the code base."

	| undefinedRefs globalVars knownVars |
	undefinedRefs := Dictionary new.
	globalVars := Set new: 100.
	globalVars addAll: variables.
	methods do: [ :m |
		knownVars := globalVars copy.
		m args do: [ :var | knownVars add: var ].
		m locals do: [ :var | knownVars add: var ].
		m freeVariableReferences do: [ :varName |
			(knownVars includes: varName) ifFalse: [
				(undefinedRefs includesKey: varName)
					ifTrue: [ (undefinedRefs at: varName) add: m selector ]
					ifFalse: [ undefinedRefs at: varName put: (OrderedCollection with: m selector) ].
			].
		].
	].

	Transcript cr.
	undefinedRefs keys asSortedCollection do: [ :var |
		Transcript show: var, ' -- undefined variable used in:'; cr.
		(undefinedRefs at: var) do: [ :sel |
			Transcript tab; show: sel; cr.
		].
	].! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'ar 2/3/2001 17:08'!
pruneMethods: selectorList
	"Explicitly prune some methods"
	selectorList do:[:sel| methods removeKey: sel ifAbsent:[]].! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'tpr 3/2/2004 11:09'!
pruneUnreachableMethods
	"Remove any methods that are not reachable. Retain methods needed by the translated classes - see implementors of requiredMethodNames"
 	
	| newMethods |
	"add all the exported methods and all the called methods to the dNRML"
	methods do: [ :m |
		m export ifTrue:[doNotRemoveMethodList add: m selector].
		doNotRemoveMethodList addAll: m allCalls].

	"build a new dictionary of methods from the collection of all the ones to keep"			
	newMethods := Dictionary new: doNotRemoveMethodList size.
	doNotRemoveMethodList do:[:sel|
		methods at: sel ifPresent:[:meth| newMethods at: sel put: meth]].
	methods := newMethods! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	"Remove all assertions in method bodies.  This is for the benefit of inlining, which
	fails to recognise and disregard empty method bodies when checking the inlinability
	of sends."

	| newMethods |
	newMethods := Dictionary new.
	'Removing assertions...'
		displayProgressAt: Sensor cursorPoint
		from: 0 to: methods size
		during: [ :bar |
			methods doWithIndex: [ :m :i |
				bar value: i.
				m isAssertion ifFalse: [
					newMethods at: m selector put: m.
					m removeAssertions]]].
	methods := newMethods.! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'dtl 6/21/2008 09:21'!
removeMethodsReferingToGlobals: varList except: methodNames
	"Remove any methods (presumably inlined) that still contain references to the given obsolete global variables."

	| varListAsStrings mVars |
	varListAsStrings := varList collect: [ :sym | sym asString ].
		(methods keys copyWithoutAll: methodNames) do: [ :sel |
			mVars := (self methodNamed: sel) freeVariableReferences asSet.
			(mVars includesAnyOf: varListAsStrings)
				ifTrue: [methods removeKey: sel ifAbsent: []]]
! !

!CCodeGenerator methodsFor: 'utilities'!
reportRecursiveMethods
	"Report in transcript all methods that can call themselves directly or indirectly or via a chain of N intermediate methods."

	| visited calls newCalls sel called |
	methods do: [: m |
		visited := translationDict keys asSet.
		calls := m allCalls asOrderedCollection.
		5 timesRepeat: [
			newCalls := Set new: 50.
			[calls isEmpty] whileFalse: [
				sel := calls removeFirst.
				sel = m selector ifTrue: [
					Transcript show: m selector, ' is recursive'; cr.
				] ifFalse: [
					(visited includes: sel) ifFalse: [
						called := self methodNamed: sel.
						called = nil ifFalse: [ newCalls addAll: called allCalls ].
					].
					visited add: sel.
				].
			].
			calls := newCalls asOrderedCollection.
		].
	].! !

!CCodeGenerator methodsFor: 'inlining' stamp: 'tpr 2/27/2004 18:49'!
retainMethods: aListOfMethodsToKeep
"add aListOfMethodsToKeep to doNotRemoveMethodList so that they will not be pruned"
	doNotRemoveMethodList ifNil:[doNotRemoveMethodList := Set new:100].
	doNotRemoveMethodList addAll: aListOfMethodsToKeep.
	^aListOfMethodsToKeep! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'JMM 4/16/2002 22:39'!
returnPrefixFromVariable: aName
	^aName! !

!CCodeGenerator methodsFor: 'public' stamp: 'ikp 9/26/97 14:50'!
storeCodeOnFile: fileName doInlining: inlineFlag
	"Store C code for this code base on the given file."

	self storeCodeOnFile: fileName doInlining: inlineFlag doAssertions: true! !

!CCodeGenerator methodsFor: 'public' stamp: 'tpr 9/26/2001 07:28'!
storeCodeOnFile: fileName doInlining: inlineFlag doAssertions: assertionFlag
	"Store C code for this code base on the given file."

	| stream |
	stream := CrLfFileStream forceNewFileNamed: fileName.
	stream ifNil: [Error signal: 'Could not open C code file: ', fileName].
	self emitCCodeOn: stream doInlining: inlineFlag doAssertions: assertionFlag.
	stream close! !

!CCodeGenerator methodsFor: 'public' stamp: 'jmv 7/30/2012 13:25'!
storeHeaderOnFile: fileName bytesPerWord: bytesPerWord
	"Store C header code for this interpreter on the given file."

	| aStream |
	aStream := CrLfFileStream forceNewFileNamed: fileName.
	aStream ifNil: [Error signal: 'Could not open C header file: ', fileName].
	aStream
		nextPutAll: '/* ';
		nextPutAll: PluginMaker headerNotice;
		nextPutAll: ' */'; cr; cr.
	self writeVmmVersionOn: aStream.
	self writeDefineBytesPerWord: bytesPerWord on: aStream.
	self writeDefineMemoryAccessInImageOn: aStream.
	self writeDefaultMacrosOn: aStream.
	aStream close
! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'TPR 3/2/2000 11:45'!
translationMethodClass
	"return the class used to produce C translation methods from MethodNodes"
	^TMethod! !

!CCodeGenerator methodsFor: 'utilities' stamp: 'ar 7/17/1999 15:06'!
unreachableMethods
	"Return a collection of methods that are never invoked."

	| sent out |
	sent := Set new.
	methods do: [ :m |
		m export ifTrue:[sent add: m selector].
		sent addAll: m allCalls.
	].

	out := OrderedCollection new.
	methods keys do: [ :sel |
		(sent includes: sel) ifFalse: [ out add: sel ].
	].
	^ out! !

!CCodeGenerator methodsFor: 'accessing' stamp: 'ar 7/8/2003 11:23'!
useSymbolicConstants
	"Answer whether we should generate symbolic constants instead of their actual values"
	^useSymbolicConstants! !

!CCodeGenerator methodsFor: 'accessing' stamp: 'ar 7/8/2003 11:23'!
useSymbolicConstants: aBool
	"Indicate whether we should generate symbolic constants instead of their actual values"
	useSymbolicConstants := aBool! !

!CCodeGenerator methodsFor: 'public' stamp: 'ar 3/10/2000 17:58'!
var: varName declareC: declarationString
	"Record the given C declaration for a global variable."

	variableDeclarations at: varName asString put: declarationString.! !

!CCodeGenerator methodsFor: 'public' stamp: 'tpr 12/29/2005 15:59'!
var: varName type: type
"Use this in preference to #var:declareC: whenver possible since it avoids typing the varname twice and thus avoids the potential for a typo. See also #var:type:array:"
	self var: varName declareC: type , ' ' , varName! !

!CCodeGenerator methodsFor: 'public' stamp: 'tpr 12/29/2005 16:00'!
var: varName type: type array: array
"use this in preference to #var:declareC: when possible. This produces a C statment of the form
int * fooArray[]={1,2,3}
See also #var:type: for simple var decls" 
	self
		var: varName
		declareC: (String streamContents: [:s |
			s nextPutAll: type.
			s space.
			s nextPutAll: varName.
			s nextPutAll: '[] = {'.
			self printArray: array on: s.
			s nextPut: $}])! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 3/10/2010 22:13'!
writeDefaultMacrosOn: aStream
	"Write macros to provide default implementations of certain functions used by
	the interpreter. If not previously defined in config.h they will be defined here.
	The definitions will be available to any module that includes sqMemoryAccess.h.
	The default macros are chosen for backward compatibility with existing platform
	support code."

	aStream cr;
		nextPutAll: '#ifndef allocateMemoryMinimumImageFileHeaderSize'; cr;
		nextPutAll: ' /* Called by Interpreter>>allocateMemory:minimum:imageFile:headerSize: */'; cr;
		nextPutAll: ' /* Default definition if not previously set in config.h */'; cr;
		nextPutAll: ' #define allocateMemoryMinimumImageFileHeaderSize(',
						'heapSize, minimumMemory, fileStream, headerSize) \'; cr;
		nextPutAll: '    sqAllocateMemory(minimumMemory, heapSize)'; cr;
		nextPutAll: '#endif'; cr; cr;

		nextPutAll: '#ifndef sqImageFileReadEntireImage'; cr;
		nextPutAll: ' /* Called by Interpreter>>sqImage:read:size:length: */'; cr;
		nextPutAll: ' /* Default definition if not previously set in config.h */'; cr;
		nextPutAll: ' #define sqImageFileReadEntireImage(memoryAddress, ',
						'elementSize,  length, fileStream) \'; cr;
		nextPutAll: '    sqImageFileRead(memoryAddress, elementSize,  length, fileStream)'; cr;
		nextPutAll: '#endif'; cr; cr;

		nextPutAll: '#ifndef error'; cr;
		nextPutAll: ' /* error() function called from Interpreter */'; cr;
		nextPutAll: ' /* Default definition if not previously set in config.h */'; cr;
		nextPutAll: ' #define error(str) defaultErrorProc(str)'; cr;
		nextPutAll: '#endif'; cr; cr

! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 8/25/2008 22:36'!
writeDefineBytesPerWord: bytesPerWord on: aStream
	aStream
		nextPutAll: '#define SQ_VI_BYTES_PER_WORD ';
		print: bytesPerWord;
		cr! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'dtl 9/17/2008 10:27'!
writeDefineMemoryAccessInImageOn: aStream
	"If MemoryAccess is present in the image, then define MEMORY_ACCESS_IN_IMAGE as
	a C preprocessor macro. When MEMORY_ACCESS_IN_IMAGE is defined, the traditional
	C preprocessor macros for low level memory access are ignored and will be replaced
	by directly translated (and inlined) SLANG versions of the same. This enables visibility
	of the memory access functions for debuggers and profilers."

	(Smalltalk classNamed: #MemoryAccess)
		ifNotNilDo: [:ma | ma isEnabled
			ifTrue: [aStream nextPutAll: '#define MEMORY_ACCESS_IN_IMAGE 1'; cr]]! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'jmv 7/30/2012 13:25'!
writeVmmVersionOn: aStream
	"Write a version string macro suitable for identifying source code version
	of this interpreter. This is expected to be used in conjunction with a similar
	identifier for platform source code version, such the the VM can identify
	the source code version for its platform source and matching VMMaker source."

	aStream nextPutAll: '#define VMMAKER_VERSION "';
		nextPutAll: PluginMaker versionString;
		nextPut: $";
		cr; cr
! !

!CCodeGenerator class methodsFor: 'class initialization' stamp: 'jm 8/19/1998 10:03'!
initialize
	"CCodeGenerator initialize"

	UseRightShiftForDivide := true.
		"If UseRightShiftForDivide is true, the translator will generate a right-shift when it encounters a division by a constant that is a small power of two. For example, 'x / 8' will generate '((int) x >> 3)'. The coercion to int is done to make it clear that the C compiler should generate a signed shift."
		"Note: The Kernighan and Ritchie 2nd Edition C manual, p. 49, leaves the semantics of right-shifting a negative number open to the discretion of the compiler implementor. However, it strongly suggests that most compilers should generate an arithmetic right shift (i.e., shifting in the sign bit), which is the same as dividing by a power of two. If your compiler does not generate or simulate an arithmetic shift, then make this class variable false and re-translate."
! !

!CCodeGenerator class methodsFor: 'removing from system' stamp: 'jm 5/16/1998 10:26'!
removeCompilerMethods
	"Before removing the C code generator classes from the system, use this method to remove the compiler node methods that support it. This avoids leaving dangling references to C code generator classes in the compiler node classes."

	ParseNode withAllSubclasses do: [ :nodeClass |
		nodeClass removeCategory: 'C translation'.
	].
	Smalltalk at: #AbstractSound ifPresent: [:abstractSound |
		 abstractSound class removeCategory: 'primitive generation'].
! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator'!
buildSortedVariablesCollection
	"Build sorted vars, end result will be sorted collection based on static usage, 
	perhaps cache lines will like this!!"

	| globalNames sorted |

	globalNames := Bag new: globalVariableUsage size.
	globalVariableUsage keysAndValuesDo: [:k :v | 
		(variableDeclarations includesKey: k) ifFalse: 
			[globalNames add: k withOccurrences: v size]].	
	variableDeclarations keysDo: 
		[:e | globalNames add: e withOccurrences: 0].
	sorted := SortedCollection sortBlock: 
		[:a :b | (globalNames occurrencesOf: a) > (globalNames occurrencesOf: b)].
	sorted addAll: variables.
	^sorted! !

!CCodeGeneratorGlobalStructure methodsFor: 'utilities'!
checkForGlobalUsage: vars in: aTMethod 
	"override to handle global struct needs"
	super checkForGlobalUsage: vars in: aTMethod.
	"if localStructDef is false, we  don't ever need to include a reference to it in a function"
	localStructDef ifFalse:[^self].
	vars asSet do: [:var |
		"if any var is global and in the global var struct 
		tell the TMethod it will be refering to the  struct"
			  ((self globalsAsSet includes: var )
					and: [self placeInStructure: var ])
				ifTrue: [aTMethod referencesGlobalStructIncrementBy: (vars occurrencesOf: var)]]! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'tpr 10/29/2002 14:00'!
emitCCodeOn: aStream doInlining: inlineFlag doAssertions: assertionFlag
	super emitCCodeOn: aStream doInlining: inlineFlag doAssertions: assertionFlag.

	"if the machine needs the globals structure defined locally in the interp.c file, don't add the folowing function"
	localStructDef ifFalse:[self emitStructureInitFunctionOn: aStream]! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'ar 4/4/2006 20:44'!
emitCVariablesOn: aStream
	"Store the global variable declarations on the given stream.
	break logic into vars for structure and vars for non-structure"
	| varString structure nonstruct target |

	structure := WriteStream on: (String new: 32768).
	nonstruct := WriteStream on: (String new: 32768).
	aStream nextPutAll: '/*** Variables ***/'; cr.
	structure nextPutAll: 'struct foo {'; cr.
	self buildSortedVariablesCollection do: [ :var |
		varString := var asString.
		target := (self placeInStructure: var) 
			ifTrue: [structure]
			ifFalse: [nonstruct].
		(self isGeneratingPluginCode) ifTrue:[
			varString = 'interpreterProxy' ifTrue:[
				"quite special..."
				aStream cr; nextPutAll: '#ifdef SQUEAK_BUILTIN_PLUGIN'.
				aStream cr; nextPutAll: 'extern'.
				aStream cr; nextPutAll: '#endif'; cr.
			] ifFalse:[aStream nextPutAll:'static '].
		].
		(variableDeclarations includesKey: varString) ifTrue: [
			target nextPutAll: (variableDeclarations at: varString), ';'; cr.
		] ifFalse: [
			"default variable declaration"
			target nextPutAll: 'sqInt ', varString, ';'; cr.
		].
	].
	structure nextPutAll: ' } fum;';cr.

	"if the machine needs the fum structure defining locally, do it now"
	localStructDef ifTrue:[structure nextPutAll: 'struct foo * foo = &fum;';cr;cr].

	aStream nextPutAll: structure contents.
	aStream nextPutAll: nonstruct contents.
	aStream cr.! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'ikp 9/10/2003 05:55'!
emitGlobalStructFlagOn: aStream
	"Define SQ_USE_GLOBAL_STRUCT before including the header."

	aStream nextPutAll: '#define SQ_USE_GLOBAL_STRUCT 1'; cr; cr! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'tpr 10/9/2002 15:40'!
emitStructureInitFunctionOn: aStream 
	"For the VM using a global struct for most of the global vars (useful for ARM and PPC so far), append the initGlobalStructure() function"
	aStream 
		cr;
		nextPutAll: 'void initGlobalStructure(void) {foo = &fum;}';
		cr! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'tpr 10/29/2002 15:01'!
globalStructDefined: aBool
	localStructDef := aBool! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'tpr 10/29/2002 16:11'!
initialize
	super initialize.
	localStructDef := false! !

!CCodeGeneratorGlobalStructure methodsFor: 'public'!
isGlobalStructureBuild
	^true! !

!CCodeGeneratorGlobalStructure methodsFor: 'utilities'!
localizeGlobalVariables
"TPR - remove all the global vars destined for the structure that are only used once - not worth the space,
actually what will happen is the folding code will fold these variables into the method"

	super localizeGlobalVariables.
	globalVariableUsage := globalVariableUsage select: [:e | e size > 1].
! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator' stamp: 'tpr 12/22/2005 16:03'!
placeInStructure: var
	"See if we should put this array into a structure
	This has hard coded vars, should go somewhere else!!
	The variables listed are hardcoded as C in the interpreter thus they don't get resolved via TVariableNode logic
	Also let's ignore variables that have special definitions that require initialization, and the function def which has problems"

	| check |
	check := variableDeclarations at: var ifAbsent: [''].
	(check includes: $=) ifTrue: [^false].
	(check includes: $() ifTrue: [^false].

	(#( 'showSurfaceFn' 'memory' 'extraVMMemory' 'interpreterProxy') includes: var) ifTrue: [^false].
	^true.
	! !

!CCodeGeneratorGlobalStructure methodsFor: 'C code generator'!
returnPrefixFromVariable: aName
	^((self globalsAsSet includes: aName) and: [self placeInStructure: aName])
		ifTrue: ['foo->',aName]
		ifFalse: [aName]! !

!VMPluginCodeGenerator methodsFor: 'inlining' stamp: 'tpr 2/24/2004 20:30'!
doInlining: inlineFlag
"do inlining for a plugin"
	^self doBasicInlining: inlineFlag! !

!VMPluginCodeGenerator methodsFor: 'C code generator' stamp: 'tpr 4/10/2002 18:28'!
emitCHeaderForPrimitivesOn: aStream
	"Write a C file header for compiled primitives onto the given stream."

	self emitCHeaderOn: aStream.
	aStream nextPutAll: '
/*** Proxy Functions ***/
#define stackValue(i) (interpreterProxy->stackValue(i))
#define stackIntegerValue(i) (interpreterProxy->stackIntegerValue(i))
#define successFlag (!!interpreterProxy->failed())
#define success(bool) (interpreterProxy->success(bool))
#define arrayValueOf(oop) (interpreterProxy->arrayValueOf(oop))
#define checkedIntegerValueOf(oop) (interpreterProxy->checkedIntegerValueOf(oop))
#define fetchArrayofObject(idx,oop) (interpreterProxy->fetchArrayofObject(idx,oop))
#define fetchFloatofObject(idx,oop) (interpreterProxy->fetchFloatofObject(idx,oop))
#define fetchIntegerofObject(idx,oop) (interpreterProxy->fetchIntegerofObject(idx,oop))
#define floatValueOf(oop) (interpreterProxy->floatValueOf(oop))
#define pop(n) (interpreterProxy->pop(n))
#define pushInteger(n) (interpreterProxy->pushInteger(n))
#define sizeOfSTArrayFromCPrimitive(cPtr) (interpreterProxy->sizeOfSTArrayFromCPrimitive(cPtr))
#define storeIntegerofObjectwithValue(idx,oop,value) (interpreterProxy->storeIntegerofObjectwithValue(idx,oop,value))
#define primitiveFail() interpreterProxy->primitiveFail()
/* allows accessing Strings in both C and Smalltalk */
#define asciiValue(c) c

'.
	aStream cr.! !

!VMPluginCodeGenerator methodsFor: 'C code generator' stamp: 'jmv 7/30/2012 13:24'!
emitCHeaderOn: aStream
	"Write a C file header onto the given stream."

	aStream nextPutAll: '/* '.
	aStream nextPutAll: PluginMaker headerNotice.
	aStream nextPutAll: ' */';cr.

	aStream nextPutAll:'
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Default EXPORT macro that does nothing (see comment in sq.h): */
#define EXPORT(returnType) returnType

/* Do not include the entire sq.h file but just those parts needed. */
/*  The virtual machine proxy definition */
#include "sqVirtualMachine.h"
/* Configuration options */
#include "sqConfig.h"
/* Platform specific definitions */
#include "sqPlatformSpecific.h"

#define true 1
#define false 0
#define null 0  /* using ''null'' because nil is predefined in Think C */
#ifdef SQUEAK_BUILTIN_PLUGIN
#undef EXPORT
// was #undef EXPORT(returnType) but screws NorCroft cc
#define EXPORT(returnType) static returnType
#endif
'.

	"Additional header files"
	headerFiles do:[:hdr|
		aStream nextPutAll:'#include '; nextPutAll: hdr; cr].


	aStream nextPutAll: '
#include "sqMemoryAccess.h"

'.
	aStream cr.! !

!VMPluginCodeGenerator methodsFor: 'C code generator' stamp: 'tpr 1/10/2003 16:09'!
emitExportsOn: aStream
	"Store all the exported primitives in a form to be used by internal plugins"
	| prefix |
	aStream nextPutAll:'

#ifdef SQUEAK_BUILTIN_PLUGIN';cr.

	aStream nextPutAll:'

void* ', pluginName,'_exports[][3] = {'.
	prefix := '"', pluginName,'"'.
	self exportedPrimitiveNames do:[:primName|
		aStream cr;
			nextPutAll:'	{'; 
			nextPutAll: prefix; 
			nextPutAll:', "'; 
			nextPutAll: primName; 
			nextPutAll:'", (void*)'; 
			nextPutAll: primName;
			nextPutAll:'},'.
	].
	aStream nextPutAll:'
	{NULL, NULL, NULL}
};
'.
	aStream nextPutAll:'

#endif /* ifdef SQ_BUILTIN_PLUGIN */

'.! !

!VMPluginCodeGenerator methodsFor: 'public' stamp: 'ar 7/8/2003 10:53'!
generateCodeStringForPrimitives
"TPR - moved down from CCodeGenerator"
	| s methodList |
	s := ReadWriteStream on: (String new: 1000).
	methodList := methods asSortedCollection: [:m1 :m2 | m1 selector < m2 selector].
	self emitCHeaderForPrimitivesOn: s.
	self emitCConstantsOn: s.
	self emitCVariablesOn: s.
	self emitCFunctionPrototypes: methodList on: s.
	methodList do: [:m | m emitCCodeOn: s generator: self].
	self emitExportsOn: s.
	^ s contents
! !

!VMPluginCodeGenerator methodsFor: 'testing' stamp: 'ar 10/7/1998 17:54'!
isGeneratingPluginCode
	^true! !

!VMPluginCodeGenerator methodsFor: 'public' stamp: 'tpr 1/10/2003 16:20'!
localizeGlobalVariables
"TPR - we don't do this for plugins"! !

!VMPluginCodeGenerator methodsFor: 'public' stamp: 'tpr 1/10/2003 16:18'!
pluginName: aString
"TPR - moved from CCodeGenerator"
	"Set the plugin name when generating plugins."
	pluginName := aString.! !

!VMPluginCodeGenerator methodsFor: 'private' stamp: 'ar 4/4/2006 21:15'!
storeVirtualMachineProxyHeader: categoryList on: fileName
	"Store the interpreter definitions on the given file"
	| stream |
	stream := FileStream newFileNamed: fileName.
	stream nextPutAll:
'#ifndef _SqueakVM_H
#define _SqueakVM_H

/* Increment the following number if you change the order of
   functions listed or if you remove functions */
#define VM_PROXY_MAJOR 1

/* Increment the following number if you add functions at the end */
#define VM_PROXY_MINOR 0

typedef struct VirtualMachine {
	int (*minorVersion) (void);
	int (*majorVersion) (void);
'.

	categoryList do:[:assoc|
		stream cr; crtab; nextPutAll:'/* InterpreterProxy methodsFor: ''',assoc key, ''' */'; cr; crtab.
		assoc value asSortedCollection do:[:sel|
			(methods at: sel) emitProxyFunctionPrototype: stream generator: self.
			stream nextPutAll: ';'; crtab]].

	stream nextPutAll:'
} VirtualMachine;

#endif /* _SqueakVM_H */
'.
	stream close.! !

!VMPluginCodeGenerator methodsFor: 'private' stamp: 'tpr 4/12/2006 12:29'!
storeVirtualMachineProxyImplementation: categoryList on: fileName
	"Store the interpreter definitions on the given file"
	| stream |
	stream := FileStream newFileNamed: fileName.
	stream nextPutAll:'
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "sqVirtualMachine.h"'; cr;cr.
	stream nextPutAll:'/*** Function prototypes ***/'.

	categoryList do:[:assoc|
		stream cr; cr; nextPutAll:'/* InterpreterProxy methodsFor: ''',assoc key, ''' */'; cr.
		assoc value asSortedCollection do:[:sel|
			(methods at: sel) emitCFunctionPrototype: stream generator: self.
			stream nextPutAll: ';'; cr]].

	stream cr; nextPutAll:'struct VirtualMachine *VM = NULL;'; cr.
	stream cr; nextPutAll:
'static int majorVersion(void) {
	return VM_PROXY_MAJOR;
}

static int minorVersion(void) {
	return VM_PROXY_MINOR;
}

struct VirtualMachine* sqGetInterpreterProxy(void)
{
	if(VM) return VM;
	VM = (struct VirtualMachine *) calloc(1, sizeof(VirtualMachine));
	/* Initialize Function pointers */
	VM->majorVersion = majorVersion;
	VM->minorVersion = minorVersion;
'.
	categoryList do:[:assoc|
		stream cr; crtab; nextPutAll:'/* InterpreterProxy methodsFor: ''',assoc key, ''' */'; crtab.
		assoc value asSortedCollection do:[:sel|
		stream nextPutAll:'VM->';
			nextPutAll: (self cFunctionNameFor: sel);
			nextPutAll:' = ';
			nextPutAll: (self cFunctionNameFor: sel);
			nextPutAll:';';
			crtab]].

	stream cr; crtab; nextPutAll:'return VM;'; cr; nextPutAll:'}'; cr.
	stream close.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:32'!
ccgLoad: aBlock expr: aString asBooleanValueFrom: anInteger
	"Answer codestring for boolean coercion (with validating side-effect) of object, as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy booleanValueOf:';
		crtab: 2;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	')'])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asCharPtrFrom: anInteger
	"Answer codestring for character pointer to first indexable field of object (without validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''char *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asCharPtrFrom: anInteger andThen: valBlock
	"Answer codestring for character pointer to first indexable field of object (without validating side-effect unless specified in valBlock), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^(valBlock value: anInteger), '.',
	 (aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''char *''']))
	 ! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asFloatValueFrom: anInteger
	"Answer codestring for double precision coercion (with validating side-effect) of oop, as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy stackFloatValue: ';
		nextPutAll: anInteger asString])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asIntPtrFrom: anInteger
	"Answer codestring for integer pointer to first indexable field of object (without validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''int *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asIntPtrFrom: anInteger andThen: valBlock
	"Answer codestring for integer pointer to first indexable field of object (without validating side-effect unless specified in valBlock), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^(valBlock value: anInteger), '.',
	 (aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''int *''']))! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asIntegerValueFrom: anInteger
	"Answer codestring for integer coercion (with validating side-effect) of oop, as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy stackIntegerValue: ';
		nextPutAll: anInteger asString])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 9/19/1999 20:28'!
ccgLoad: aBlock expr: aString asKindOf: aClass from: anInteger

	^String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy success: (interpreterProxy';
		crtab: 2;
		nextPutAll: 'is: (interpreterProxy stackValue: ';
		nextPutAll: anInteger asString;
		nextPutAll: ')';
		crtab: 2;
		nextPutAll: 	'KindOf: ''';
		nextPutAll:	aClass asString;
		nextPutAll: ''').';
		crtab;
		nextPutAll: (self 
						ccgLoad: aBlock 
						expr: aString 
						asRawOopFrom: anInteger)]! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 9/19/1999 20:28'!
ccgLoad: aBlock expr: aString asMemberOf: aClass from: anInteger

	^String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy success: (interpreterProxy';
		crtab: 2;
		nextPutAll: 'is: (interpreterProxy stackValue: ';
		nextPutAll: anInteger asString;
		nextPutAll: ')';
		crtab: 2;
		nextPutAll: 	'MemberOf: ''';
		nextPutAll:	aClass asString;
		nextPutAll: ''').';
		crtab;
		nextPutAll: (self 
						ccgLoad: aBlock 
						expr: aString 
						asRawOopFrom: anInteger)]! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: exprString asNamedPtr: recordString from: anInteger
	"Answer codestring for integer pointer to first indexable field of object (without validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''';
		nextPutAll: recordString;
		nextPutAll: ' *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: exprString asNamedPtr: recordString from: anInteger andThen: valBlock
	"Answer codestring for integer pointer to first indexable field of object (without validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^(valBlock value: anInteger), '.',
	 (aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''';
		nextPutAll: recordString;
		nextPutAll: ' *''']))! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:29'!
ccgLoad: aBlock expr: aString asNonIntegerValueFrom: anInteger
	"Answer codestring for oop (with validating side effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy stackObjectValue: ';
		nextPutAll: anInteger asString])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:32'!
ccgLoad: aBlock expr: aString asRawOopFrom: anInteger
	"Answer a string for a Slang expression that will load an oop (without validation) from stack index anInteger.  Apply aBlock, a BlockContext instance that when passed an expression, will return a string assigning the expression to the desired identifier, to the string before answering.  aString is a Slang expression that refers to the stack value, once it has been loaded."

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy stackValue: ';
		nextPutAll: anInteger asString])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:31'!
ccgLoad: aBlock expr: aString asUnsignedPtrFrom: anInteger andThen: valBlock
	"Answer a codestring for integer pointer to first indexable field of object (without validating side-effect unless specified in valBlock), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^(valBlock value: anInteger), '.',
	 (aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy firstIndexableField:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''unsigned *''']))! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:31'!
ccgLoad: aBlock expr: aString asUnsignedValueFrom: anInteger
	"Answer a codestring for positive integer coercion (with validating side-effect) of oop, as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'interpreterProxy positive32BitValueOf:';
		crtab: 2;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	')'])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:31'!
ccgLoad: aBlock expr: aString asWBCharPtrFrom: anInteger
	"Answer codestring for char pointer to first indexable field of object (with validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy arrayValueOf:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''char *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:31'!
ccgLoad: aBlock expr: aString asWBFloatPtrFrom: anInteger
	"Answer codestring for single-precision float pointer to first indexable field of object (with validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy arrayValueOf:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''float *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 12/17/1999 07:31'!
ccgLoad: aBlock expr: aString asWBIntPtrFrom: anInteger
	"Answer codestring for integer pointer to first indexable field of object (with validating side-effect), as described in comment to ccgLoad:expr:asRawOopFrom:"

	^aBlock value: (String streamContents: [:aStream | aStream
		nextPutAll: 'self cCoerce: (interpreterProxy arrayValueOf:';
		crtab: 4;
		nextPutAll: '(interpreterProxy stackValue:';
		nextPutAll: anInteger asString;
		nextPutAll:	'))';
		crtab: 3;
		nextPutAll: 'to: ''int *'''])! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'tpr 4/12/2006 12:19'!
ccgSetBlock: aString

	^[:expr | aString, ' := ', expr]! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'acg 9/19/1999 13:05'!
ccgTVarBlock: anInteger

	^[:expr | '(thisContext tempAt: 1) tempAt: ', anInteger asString, ' put: (', expr, ')']! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'coercing' stamp: 'bf 9/24/1999 14:53'!
ccgValBlock: valString

	^[:index | String streamContents:
		[:aStream | aStream
			nextPutAll: 'interpreterProxy success: (interpreterProxy ';
			nextPutAll: valString;
			nextPutAll: ': (interpreterProxy stackValue: ';
			nextPutAll: index asString;
			nextPutAll: '))']] fixTemps! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asBooleanValueFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: ' = interpreterProxy->booleanValueOf(';
		crtab: 2;
		nextPutAll: 'interpreterProxy->stackValue(';
		nextPutAll: anInteger asString;
		nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asCharPtrFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = (char *) interpreterProxy->firstIndexableField(';
		crtab: 2;
		nextPutAll: 	'interpreterProxy->stackValueOf(';
		nextPutAll: anInteger asString;
		nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asFloatPtrFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = (float *) interpreterProxy->firstIndexableField(';
		crtab: 2;
		nextPutAll: 	'interpreterProxy->stackValueOf(';
		nextPutAll: anInteger asString;
		nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asFloatValueFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = interpreterProxy->stackFloatValue(';
		nextPutAll: anInteger asString;
		nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asIntPtrFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = (int *) interpreterProxy->firstIndexableField(';
		crtab: 2;
		nextPutAll: 	'interpreterProxy->stackValueOf(';
		nextPutAll: anInteger asString;
		nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:43'!
emitLoad: aString asIntegerValueFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = interpreterProxy stackIntegerValue(';
		nextPutAll: anInteger asString;
		nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:42'!
emitLoad: aString asKindOf: aClass from: anInteger on: aStream

	self emitLoad: aString asNakedOopFrom: anInteger on: aStream.
	aStream
		crtab;
		nextPutAll: 'interpreterProxy->success(interpreterProxy->isKindOf(';
		nextPutAll: aString;
		nextPutAll: 	', ''';
		nextPutAll:	aClass asString;
		nextPutAll: '''))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'bf 3/16/2000 19:20'!
emitLoad: aString asMemberOf: aClass from: anInteger on: aStream

	self emitLoad: aString asNakedOopFrom: anInteger on: aStream.
	aStream
		crtab;
		nextPutAll: 'interpreterProxy->success(interpreterProxy->isMemberOf(';
		nextPutAll: aString;
		nextPutAll: 	', ''';
		nextPutAll:	aClass asString;
		nextPutAll: '''))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/18/1999 14:23'!
emitLoad: aString asNakedOopFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: ' = interpreterProxy stackValue(';
		nextPutAll: anInteger asString;
		nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:44'!
emitLoad: aString asNonIntegerValueFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = interpreterProxy stackObjectValue(';
		nextPutAll: anInteger asString;
		nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'linking' stamp: 'acg 9/17/1999 01:44'!
emitLoad: aString asUnsignedValueFrom: anInteger on: aStream

	aStream
		nextPutAll: aString;
		nextPutAll: 	' = interpreterProxy->positive32BitValueOf(';
		crtab: 2;
		nextPutAll: 	'interpreterProxy->stackValue(';
		nextPutAll: anInteger asString;
		nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:08'!
generateAsBooleanObj: aNode on: aStream indent: anInteger

	aStream nextPutAll: '('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: 
		') ? interpreterProxy->trueObject(): interpreterProxy->falseObject()'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:09'!
generateAsCBoolean: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->booleanValueOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:09'!
generateAsCDouble: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->floatValueOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 21:53'!
generateAsCInt: aNode on: aStream indent: anInteger

	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ' >> 1'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:09'!
generateAsCUnsigned: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->positive32BitValueOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:10'!
generateAsCharPtr: aNode on: aStream indent: anInteger

	aStream nextPutAll: '(char *) interpreterProxy->firstIndexableField('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:10'!
generateAsFloatObj: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->floatObjectOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'TPR 2/25/2000 16:21'!
generateAsIfVar: aNode on: aStream indent: anInteger

	| cName fName class index |
	cName := String streamContents: 
		[:scStr | self emitCExpression: aNode args first on: scStr].
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'first arg must identify class'].
	fName := aNode args second value.
	index := class allInstVarNames
		indexOf: fName
		ifAbsent: [^self error: 'second arg must be instVar'].
	aStream 
		nextPutAll: 'interpreterProxy->fetchPointerofObject(';
		nextPutAll: (index - 1) asString;
		nextPutAll: ','.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'TPR 2/25/2000 16:41'!
generateAsIfVarAsValue: aNode on: aStream indent: anInteger

	| cName fName class index fetchNode |
	cName := String streamContents: 
		[:scStr | self emitCExpression: aNode args first on: scStr].
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'first arg must identify class'].
	fName := aNode args second value.
	index := class allInstVarNames
		indexOf: fName
		ifAbsent: [^self error: 'second arg must be instVar'].
	fetchNode := TSendNode new
		setSelector: #fetchPointer:ofObject:
		receiver: (TVariableNode new setName: 'interpreterProxy')
		arguments: (Array
			with: (TConstantNode new setValue: index - 1)
			with: aNode receiver).
	cName := aNode args third nameOrValue.
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'third arg must identify class'].
	class ccg: self generateCoerceToValueFrom: fetchNode on: aStream
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'TPR 2/25/2000 16:41'!
generateAsIfVarPut: aNode on: aStream indent: anInteger

	| cName fName class index |
	cName := String streamContents: 
		[:scStr | self emitCExpression: aNode args first on: scStr].
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'first arg must identify class'].
	fName := aNode args second value.
	index := class allInstVarNames
		indexOf: fName
		ifAbsent: [^self error: 'second arg must be instVar'].
	aStream 
		nextPutAll: 'interpreterProxy->storePointerofObjectwithValue(';
		nextPutAll: (index - 1) asString;
		nextPutAll: ','.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode args third on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:10'!
generateAsIntPtr: aNode on: aStream indent: anInteger

	aStream nextPutAll: '(int *) interpreterProxy->firstIndexableField('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 12/17/1999 07:23'!
generateAsOop: aNode on: aStream indent: anInteger

	| cName class |
	cName := aNode args first nameOrValue.
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'first arg must identify class'].
	class ccg: self generateCoerceToOopFrom: aNode receiver on: aStream! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:10'!
generateAsPositiveIntegerObj: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->positive32BitIntegerFor('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 20:47'!
generateAsSmallIntegerObj: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->integerObjectOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 12/17/1999 07:22'!
generateAsValue: aNode on: aStream indent: anInteger

	| cName class |
	cName := aNode args first nameOrValue.
	class := Smalltalk 
		at: (cName asSymbol) 
		ifAbsent: [nil].
	(class isNil not and: [class isBehavior]) ifFalse: 
		[^self error: 'first arg must identify class'].
	class ccg: self generateCoerceToValueFrom: aNode receiver on: aStream! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'dtl 6/1/2008 10:42'!
generateCPtrAsOop: aNode on: aStream indent: anInteger 

	aStream nextPutAll: '('.
	aStream nextPutAll: 'oopForPointer( '.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ' ) - ';
		 nextPutAll: ObjectMemory baseHeaderSize printString;
		 nextPut: $)! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:11'!
generateClass: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->fetchClassOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 12/25/1999 10:00'!
generateCoerceToBooleanObjectFrom: aNode on: aStream

	aStream nextPutAll: '('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: '? interpreterProxy->trueObject(): interpreterProxy->falseObject())'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 06:07'!
generateCoerceToBooleanValueFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->booleanValueOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 06:03'!
generateCoerceToFloatObjectFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->floatObjectOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 05:53'!
generateCoerceToFloatValueFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->floatValueOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'dtl 10/23/2007 06:33'!
generateCoerceToObjectFromPtr: aNode on: aStream
	"This code assumes no named instance variables"

	aStream nextPutAll: 'oopForPointer('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ') - ', ObjectMemory baseHeaderSize asString! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 05:57'!
generateCoerceToPtr: aString fromObject: aNode on: aStream
	"This code assumes no named instance variables"

	aStream 
		nextPutAll: '((';
		nextPutAll: aString;
		nextPutAll: ') interpreterProxy->firstIndexableField('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: '))'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 06:03'!
generateCoerceToSmallIntegerObjectFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->integerObjectOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 05:59'!
generateCoerceToSmallIntegerValueFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->integerValueOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 06:03'!
generateCoerceToUnsignedObjectFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->positive32BitIntegerFor('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'asOop:/asValue:' stamp: 'acg 10/5/1999 06:00'!
generateCoerceToUnsignedValueFrom: aNode on: aStream

	aStream nextPutAll: 'interpreterProxy->positive32BitValueOf('.
	self emitCExpression: aNode on: aStream.
	aStream nextPutAll: ')'! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'debug code' stamp: 'sr 4/8/2000 00:52'!
generateDebugCode
	^ debugFlag! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'debug code' stamp: 'sr 4/8/2000 00:52'!
generateDebugCode: aBool 
	debugFlag := aBool! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'sr 4/8/2000 02:38'!
generateDebugCode: aNode on: aStream indent: level 
	"Generate the C debug code for this message onto the given stream, if  
	compiled in debugMode."
	self generateDebugCode
		ifTrue: 
			[aStream nextPutAll: '/* DebugCode... */';
			 cr.
			aNode args first
				emitCCodeOn: aStream
				level: level
				generator: self.
			aStream tab: level.
			aStream nextPutAll: '/* ...DebugCode */']
		ifFalse: [aStream nextPutAll: '/* missing DebugCode */']! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:11'!
generateField: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->fetchPointerofObject('.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 23:40'!
generateFieldPut: aNode on: aStream indent: anInteger
		
	aStream nextPutAll: 'interpreterProxy->storePointerofObjectwithValue('.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode args second on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/16/1999 08:02'!
generateFromStack: aNode on: aStream indent: anInteger

	| idList |
	aNode args first isConstant ifFalse: [^self error: 'arg must be constant'].
	idList := aNode args first value.
	(1 to: idList size)
		do: [:i | 
			aStream 
				nextPutAll: (idList at: i);
				nextPutAll: ' = interpreterProxy->stackValue(';
				nextPutAll: (idList size - i) asString;
				nextPutAll: ')']
		separatedBy: [aStream nextPut: $;; crtab: anInteger].
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 20:50'!
generateIsBytes: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isBytes('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 20:50'!
generateIsFloat: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isFloatObject('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 20:49'!
generateIsIndexable: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isIndexable('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 20:49'!
generateIsInteger: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isIntegerValue('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:13'!
generateIsIntegerOop: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isIntegerObject('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:13'!
generateIsIntegerValue: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isIntegerValue('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:20'!
generateIsKindOf: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isKindOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','''.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ''')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:20'!
generateIsMemberOf: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isMemberOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','''.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ''')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:14'!
generateIsPointers: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isPointers('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:14'!
generateIsWords: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isWords('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:13'!
generateIsWordsOrBytes: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->isWordsOrBytes('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/19/1999 01:56'!
generateNext: msgNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| varNode |
	varNode := msgNode receiver.
	varNode isVariable
		ifFalse: [ self error: 'next can only be applied to variables' ].
	aStream nextPutAll: '*'.
	aStream nextPutAll: varNode name.
	aStream nextPutAll: '++'
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 12/31/1999 16:37'!
generateRemapOopIn: aNode on: aStream indent: level
	"Generate the C code for this message onto the given stream."

	| idList |
	idList := aNode args first nameOrValue.
	idList class == Array ifFalse: [idList := Array with: idList].
	idList do:
		[:each | 
		 aStream 
			nextPutAll: 'interpreterProxy->pushRemappableOop(';
			nextPutAll: each asString;
			nextPutAll: ');']
		separatedBy: [aStream crtab: level].
	aStream cr.
	aNode args second emitCCodeOn: aStream level: level generator: self.
	level timesRepeat: [aStream tab].
	idList reversed do:
		[:each |
		 aStream 
			nextPutAll: each asString;
			nextPutAll: ' = interpreterProxy->popRemappableOop()']
		separatedBy: [aStream nextPut: $;; crtab: level].! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:15'!
generateStAt: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->stObjectat('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ')'
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 9/15/1999 22:17'!
generateStAtPut: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->stObjectatput('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode args first on: aStream.
	aStream nextPutAll: ','.
	self emitCExpression: aNode args second on: aStream.
	aStream nextPutAll: ')'
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'acg 1/1/2000 22:41'!
generateStSize: aNode on: aStream indent: anInteger

	aStream nextPutAll: 'interpreterProxy->stSizeOf('.
	self emitCExpression: aNode receiver on: aStream.
	aStream nextPutAll: ')'.! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'initialize' stamp: 'sr 4/8/2000 00:53'!
initialize
	super initialize.
	debugFlag := false! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'translating builtins' stamp: 'tpr 12/21/2005 17:25'!
initializeCTranslationDictionary 
	"Initialize the dictionary mapping message names to actions for C code generation."

	| pairs |
	super initializeCTranslationDictionary.
	pairs := #(
		#asCInt						#generateAsCInt:on:indent:
		#asCUnsigned				#generateAsCUnsigned:on:indent:
		#asCBoolean					#generateAsCBoolean:on:indent:
		#asCDouble					#generateAsCDouble:on:indent:

		#asSmallIntegerObj			#generateAsSmallIntegerObj:on:indent:
		#asPositiveIntegerObj		#generateAsPositiveIntegerObj:on:indent:
		#asBooleanObj				#generateAsBooleanObj:on:indent:
		#asFloatObj					#generateAsFloatObj:on:indent:

		#asIf:var:					#generateAsIfVar:on:indent:
		#asIf:var:asValue:			#generateAsIfVarAsValue:on:indent:
		#asIf:var:put:				#generateAsIfVarPut:on:indent:
		#field:						#generateField:on:indent:
		#field:put:					#generateFieldPut:on:indent:
		
		#class						#generateClass:on:indent:

		#stSize						#generateStSize:on:indent:
		#stAt:						#generateStAt:on:indent:
		#stAt:put:					#generateStAtPut:on:indent:

		#asCharPtr					#generateAsCharPtr:on:indent:
		#asIntPtr					#generateAsIntPtr:on:indent:
		#cPtrAsOop					#generateCPtrAsOop:on:indent:
		#next						#generateNext:on:indent:

		#asOop:						#generateAsOop:on:indent:
		#asValue:					#generateAsValue:on:indent:

		#isFloat						#generateIsFloat:on:indent:
		#isIndexable					#generateIsIndexable:on:indent:
		#isIntegerOop				#generateIsIntegerOop:on:indent:
		#isIntegerValue				#generateIsIntegerValue:on:indent:
		#FloatOop					#generateIsFloatValue:on:indent:
		#isWords					#generateIsWords:on:indent:
		#isWordsOrBytes				#generateIsWordsOrBytes:on:indent:
		#isPointers					#generateIsPointers:on:indent:
		#isNil						#generateIsNil:on:indent:
		#isMemberOf:				#generateIsMemberOf:on:indent:
		#isKindOf:					#generateIsKindOf:on:indent:

		#fromStack:					#generateFromStack:on:indent:
		#clone						#generateClone:on:indent
		#new						#generateNew:on:indent
		#new:						#generateNewSize:on:indent
		#superclass					#generateSuperclass:on:indent:
		#remapOop:in:				#generateRemapOopIn:on:indent:
		#debugCode:					#generateDebugCode:on:indent:
	).

	1 to: pairs size by: 2 do: [:i |
		translationDict at: (pairs at: i) put: (pairs at: i + 1)].
! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'initialize' stamp: 'tpr 6/9/2003 16:40'!
translationMethodClass
	"return the class used to produce C translation methods from MethodNodes"
	^SmartSyntaxPluginTMethod! !

!SmartSyntaxPluginCodeGenerator methodsFor: 'transforming' stamp: 'ar 3/10/2000 17:59'!
var: varName as: aClass
	"Record the given C declaration for a global variable"

	variableDeclarations at: varName asString put: (aClass ccgDeclareCForVar: varName)! !

!CObjectAccessor methodsFor: 'pointer arithmetic' stamp: 'ar 10/9/1998 21:57'!
+ increment
	^self clone += increment! !

!CObjectAccessor methodsFor: 'pointer arithmetic' stamp: 'ar 10/9/1998 21:57'!
+= increment
	offset := offset + increment! !

!CObjectAccessor methodsFor: 'pointer arithmetic' stamp: 'ar 10/9/1998 21:58'!
- decrement
	^self clone -= decrement! !

!CObjectAccessor methodsFor: 'pointer arithmetic' stamp: 'ar 10/9/1998 21:58'!
-= decrement
	offset := offset - decrement! !

!CObjectAccessor methodsFor: 'converting' stamp: 'di 7/14/2004 17:36'!
asFloatAccessor

	^ self asPluggableAccessor
		atBlock: [:obj :index | obj floatAt: index]
		atPutBlock: [:obj :index :value | obj floatAt: index put: value]! !

!CObjectAccessor methodsFor: 'converting' stamp: 'di 7/14/2004 17:36'!
asIntAccessor

	^ self asPluggableAccessor
		atBlock: [:obj :index | obj intAt: index]
		atPutBlock: [:obj :index :value | obj intAt: index put: value]! !

!CObjectAccessor methodsFor: 'converting' stamp: 'acg 9/20/1999 11:08'!
asOop: aClass

	(aClass ccgCanConvertFrom: object)
		ifFalse: [^self error: 'incompatible object for autocoercion'].
	^object! !

!CObjectAccessor methodsFor: 'converting' stamp: 'di 7/14/2004 11:55'!
asPluggableAccessor
	^ (CPluggableAccessor on: object) += offset! !

!CObjectAccessor methodsFor: 'converting' stamp: 'ar 11/24/1998 20:51'!
asPluggableAccessor: accessorArray
	^((CPluggableAccessor on: object) += offset)
		readBlock: accessorArray first
		writeBlock: accessorArray last! !

!CObjectAccessor methodsFor: 'accessing' stamp: 'ar 10/9/1998 21:56'!
at: index
	^object instVarAt: index + offset + 1! !

!CObjectAccessor methodsFor: 'accessing' stamp: 'ar 10/9/1998 21:56'!
at: index put: value
	^object instVarAt: index + offset + 1 put: value! !

!CObjectAccessor methodsFor: 'converting' stamp: 'di 7/14/2004 17:38'!
coerceTo: cTypeString sim: interpreterSimulator

	cTypeString = 'float *' ifTrue: [^ self asFloatAccessor].
	cTypeString = 'int *' ifTrue: [^ self asIntAccessor].
	^ self! !

!CObjectAccessor methodsFor: 'private' stamp: 'ar 11/3/1998 22:37'!
getObject
	^object! !

!CObjectAccessor methodsFor: 'accessing' stamp: 'di 7/14/2004 12:13'!
isCObjectAccessor

	^ true! !

!CObjectAccessor methodsFor: 'accessing' stamp: 'yo 2/9/2001 11:23'!
object

	^ object! !

!CObjectAccessor methodsFor: 'accessing' stamp: 'yo 2/9/2001 11:23'!
offset

	^ offset
! !

!CObjectAccessor methodsFor: 'printing' stamp: 'ar 9/16/1998 21:38'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll:' on: ';
		print: object.! !

!CObjectAccessor methodsFor: 'printing' stamp: 'MPW 1/1/1901 22:01'!
printOnStream: aStream
	super printOnStream: aStream.
	aStream
		print:' on: ';
		write: object.! !

!CObjectAccessor methodsFor: 'private' stamp: 'ar 10/9/1998 21:56'!
setObject: anObject
	object := anObject.
	offset := 0.! !

!CObjectAccessor class methodsFor: 'instance creation' stamp: 'ar 9/16/1998 21:36'!
on: anObject
	^self new setObject: anObject! !

!CArrayAccessor methodsFor: 'comparing' stamp: 'yo 2/9/2001 11:23'!
< other

	^ (object == other object) and: [offset < other offset].! !

!CArrayAccessor methodsFor: 'comparing' stamp: 'yo 2/9/2001 11:24'!
<= other

	^ (object == other object) and: [offset <= other offset].! !

!CArrayAccessor methodsFor: 'comparing' stamp: 'yo 2/9/2001 11:24'!
> other

	^ (object == other object) and: [offset > other offset].! !

!CArrayAccessor methodsFor: 'comparing' stamp: 'yo 2/9/2001 11:24'!
>= other

	^ (object == other object) and: [offset >= other offset].! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'ar 10/9/1998 21:56'!
at: index
	^object at: index + offset + 1! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'ar 10/9/1998 21:56'!
at: index put: value
	^object at: index + offset + 1 put: value! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'ar 12/31/2001 01:36'!
byteAt: index
	^object byteAt: index + offset + 1! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'acg 9/19/1999 01:50'!
cPtrAsOop

	offset = 0 ifFalse: [self error: 'offset must be zero'].
	^object! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'jmv 10/23/2009 09:47'!
long32At: index
	| idx |
	idx := (offset + index) // 4 + 1.
	"Note: This is a special hack for BitBlt."
	(idx = (object basicSize + 1)) ifTrue:[^0].
	^object basicAt: idx! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'jmv 10/23/2009 09:47'!
long32At: index put: value
	^object basicAt: (offset + index) // 4 + 1 put: value! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'ar 10/10/1998 16:26'!
longAt: index
	| idx |
	idx := (offset + index) // 4 + 1.
	"Note: This is a special hack for BitBlt."
	(idx = (object basicSize + 1)) ifTrue:[^0].
	^object basicAt: idx! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'ar 10/10/1998 16:26'!
longAt: index put: value
	^object basicAt: (offset + index) // 4 + 1 put: value! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'acg 9/19/1999 01:48'!
next

	|val|
	val := self at: 0.
	offset := offset + 1.
	^val! !

!CArrayAccessor methodsFor: 'accessing' stamp: 'acg 9/19/1999 01:46'!
size

	^object size! !

!CPluggableAccessor methodsFor: 'accessing' stamp: 'ar 11/24/1998 20:45'!
at: index
	^readBlock value: object value: index + offset + 1! !

!CPluggableAccessor methodsFor: 'accessing' stamp: 'ar 11/24/1998 20:45'!
at: index put: value
	^writeBlock value: object value: index + offset + 1 value: value! !

!CPluggableAccessor methodsFor: 'initialize' stamp: 'di 7/14/2004 11:55'!
atBlock: rBlock atPutBlock: wBlock
	readBlock := rBlock.
	writeBlock := wBlock! !

!CPluggableAccessor methodsFor: 'initialize' stamp: 'ar 11/24/1998 20:51'!
readBlock: rBlock writeBlock: wBlock
	readBlock := rBlock.
	writeBlock := wBlock! !

!InterpreterPlugin methodsFor: 'initialize' stamp: 'ikp 8/3/2004 19:18'!
getInterpreter
	"Note: This is coded so that plugins can be run from Squeak."

	self returnTypeC: 'VirtualMachine *'.
	^interpreterProxy! !

!InterpreterPlugin methodsFor: 'initialize' stamp: 'ar 5/13/2000 02:00'!
getModuleName
	"Note: This is hardcoded so it can be run from Squeak.
	The module name is used for validating a module *after*
	it is loaded to check if it does really contain the module
	we're thinking it contains. This is important!!"
	self returnTypeC:'const char*'.
	self export: true.
	^moduleName! !

!InterpreterPlugin methodsFor: 'debugging' stamp: 'yo 1/1/2004 11:09'!
halt
	self cCode: '' inSmalltalk: [nil halt].! !

!InterpreterPlugin methodsFor: 'debugging' stamp: 'tpr 12/29/2005 16:34'!
msg: s 
	self var: #s type: 'char *'.
	self cCode: 'fprintf(stderr, "\n%s: %s", moduleName, s)' inSmalltalk: [Transcript cr; show: self class moduleName , ': ' , s; endEntry]! !

!InterpreterPlugin methodsFor: 'initialize' stamp: 'ar 4/4/2006 20:53'!
setInterpreter: anInterpreter 
	"Note: This is coded so that is can be run from Squeak."

	| ok |
	self export: true.
	self var: #anInterpreter type: #'struct VirtualMachine*'.
	interpreterProxy := anInterpreter.
	ok := self cCode: 'interpreterProxy->majorVersion() == VM_PROXY_MAJOR'.
	ok == false ifTrue: [^ false].
	ok := self cCode: 'interpreterProxy->minorVersion() >= VM_PROXY_MINOR'.
	^ ok! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'tpr 12/17/2003 16:52'!
allCallsOn
	"Answer a SortedCollection of all the methods that refer to me. Most classes simply defer to SystemDictionary>allCallsOn: but some have special requirements - plugins may have a module name that does not match the class name"

	self theNonMetaClass name ~= self moduleName asSymbol
		ifTrue:[^super allCallsOn, (self systemNavigation allCallsOn: self moduleName asSymbol)]
		ifFalse:[^super allCallsOn]! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'ar 5/12/2000 00:24'!
baseDirectoryName
	"Return the directory into which plugins should be generated by default."
	^FileDirectory default pathName! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'ikp 8/3/2004 18:55'!
buildCodeGeneratorUpTo: aPluginClass
	"Build a CCodeGenerator for the plugin"
	 | cg theClass |
	cg := self codeGeneratorClass new initialize.
	cg pluginName: self moduleName.
	"Add an extra declaration for module name"
	cg declareModuleName: self moduleNameAndVersion.

	theClass := aPluginClass.
	[theClass == Object | (theClass == InterpreterSimulationObject)] whileFalse:[
		cg addClass: theClass.
		theClass := theClass superclass].
	^cg! !

!InterpreterPlugin class methodsFor: 'private' stamp: 'tpr 6/9/2003 16:41'!
codeGeneratorClass
	"return the appropriate class of code generator for this kind ofplugin"

	^VMPluginCodeGenerator! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'ar 5/12/2000 00:34'!
declareCVarsIn: aCCodeGenerator
	"Note: This method must be implemented by all subclasses to declare variables."

	aCCodeGenerator 
		var: #interpreterProxy 
		type: #'struct VirtualMachine*'.
	self declareHeaderFilesIn: aCCodeGenerator.! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/23/2001 17:14'!
declareHeaderFilesIn: aCCodeGenerator
	self hasHeaderFile ifTrue:[
		aCCodeGenerator addHeaderFile: '"', self moduleName,'.h"'].! !

!InterpreterPlugin class methodsFor: 'instance creation' stamp: 'ar 12/31/2001 01:36'!
doPrimitive: primitiveName 
	| proxy plugin |
	proxy := InterpreterProxy new.
	proxy loadStackFrom: thisContext sender.
	plugin := self simulatorClass new.
	plugin setInterpreter: proxy.
	(plugin respondsTo: #initialiseModule) ifTrue:[plugin initialiseModule].
	plugin perform: primitiveName asSymbol.
	^ proxy stackValue: 0! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/23/2001 17:03'!
hasHeaderFile
	"If there is a single intrinsic header file to be associated with the plugin, here is where you want to flag"
	^false! !

!InterpreterPlugin class methodsFor: 'class initialization' stamp: 'ar 9/16/1998 20:26'!
initialize
	"Nothing to do ..."! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'RMF 3/27/2000 09:39'!
isCPP
	^ false! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/28/2008 19:03'!
isSuitablePluginForPlatform: platName
"Is this plugin one that should be built for platName? We do NOT test for the existence of any external files here"
	"generic answer is true"
	^true! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'sma 4/22/2000 12:32'!
moduleExtension
	^ self isCPP ifTrue: ['.cpp'] ifFalse: ['.c']! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'sma 3/3/2000 12:24'!
moduleName
	"Answer the receiver's module name that is used for the plugin's C code."

	^ self name asString! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'TPR 5/23/2000 15:33'!
moduleNameAndVersion
	"Answer the receiver's module name and version info that is used for the plugin's C code. The default is to append the code generation date, but any useful text is ok (keep it short)"

	^ self moduleName, Character space asString, Date today asString! !

!InterpreterPlugin class methodsFor: 'compiling' stamp: 'tpr 2/17/2005 13:19'!
noteCompilationOf: aSelector meta: isMeta
	"note the recompiliation by resetting the timeStamp "
	timeStamp := Time localSecondClock.
	^super noteCompilationOf: aSelector meta: isMeta! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 2/27/2004 19:05'!
requiredMethodNames
	"return the list of method names that should be retained for export or other support reasons"
	"just which methods?"
	^#()! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 7/2/2001 16:33'!
requiresCrossPlatformFiles
	"default is ok for most, any plugin needing cross platform files aside from a normal header must say so. See SoundCodecPlugin for example"
	^self hasHeaderFile! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 11/21/2000 11:53'!
requiresPlatformFiles
	"default is ok for most, any plugin needing platform specific files must say so"
	^false! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/14/2001 12:05'!
shouldBeTranslated
"is this class intended to be translated as a plugin? Most subclasses should answer true, but some such as:-
	TestInterpreterPlugin
	FlippArrayPlugin2
	InflatePlugin
	should answer false for various reasons."
	^true! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'ajh 8/21/2002 21:43'!
simulatorClass
	"For running from Smalltalk - answer a class that can be used to simulate the receiver, or nil if you want the primitives in this module to always fail, causing simulation to fall through to the Smalltalk code.  By default every non-TestInterpreterPlugin can simulate itself."

	^ self! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 9/26/2001 07:27'!
storeString: s onFileNamed: fileName
	"Store the given string in a file of the given name."

	| f |
	f := CrLfFileStream forceNewFileNamed: fileName.
	f nextPutAll: s.
	f close.! !

!InterpreterPlugin class methodsFor: 'accessing' stamp: 'tpr 3/26/2002 15:25'!
timeStamp
	^timeStamp ifNil:[0]! !

!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 4/9/2002 16:14'!
translateInDirectory: directory doInlining: inlineFlag
"This is the default method for writing out sources for a plugin. Several classes need special handling, so look at all implementors of this message"
	| cg fname fstat |
	 fname := self moduleName, '.c'.

	"don't translate if the file is newer than my timeStamp"
	fstat := directory entryAt: fname ifAbsent:[nil].
	fstat ifNotNil:[self timeStamp < fstat modificationTime ifTrue:[^nil]].

	self initialize.
	cg := self buildCodeGeneratorUpTo: self.
	cg storeCodeOnFile:  (directory fullNameFor: fname) doInlining: inlineFlag.
	^cg exportedPrimitiveNames asArray! !

!SmartSyntaxInterpreterPlugin methodsFor: 'debugging' stamp: 'sr 12/24/2001 00:29'!
sqAssert: aBool 
	self
		debugCode: [aBool
				ifFalse: [self error: 'Assertion failed!!'].
			^ aBool]! !

!SmartSyntaxInterpreterPlugin class methodsFor: 'private' stamp: 'tpr 6/9/2003 16:36'!
codeGeneratorClass
	"return the appropriate class of code generator for this kind ofplugin"

	^SmartSyntaxPluginCodeGenerator! !

!SmartSyntaxInterpreterPlugin class methodsFor: 'instance creation' stamp: 'tpr 6/28/2003 17:28'!
doPrimitive: primitiveName withArguments: argArray
	| proxy plugin |
	proxy := InterpreterProxy new.
	proxy loadStackFrom: thisContext sender.
	plugin := (self simulatorClass ifNil: [self]) new.
	plugin setInterpreter: proxy.
	^plugin perform: primitiveName asSymbol withArguments: argArray! !

!SmartSyntaxInterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 6/9/2003 16:44'!
shouldBeTranslated
"SmartSyntaxInterpreterPlugin should not be translated but its subclasses should"
	^self ~= SmartSyntaxInterpreterPlugin! !

!SmartSyntaxInterpreterPlugin class methodsFor: 'instance creation' stamp: 'tpr 6/28/2003 17:28'!
simulatorClass
	"For running from Smalltalk - answer a class that can be used to simulate the receiver, or nil if you want the primitives in this module to always fail, causing simulation to fall through to the Smalltalk code.
	By default SmartSyntaxInterpreterPlugin answers nil because methods in these plugins are intended to be embedded in code that pushes and pops from the stack and therefore cannot be run independently.  This wrapper code is generated when translated to C.  But, unfortunately, this code is missing during simulation.  There was an attempt to simulate this, but only the prologue code (getting arg from the stack) is simulated (see simulatePrologInContext:). The epologue code (popping args and pushing result) is not.  So I am making this nil until this can be fixed.
	Also, beware that primitive methods that take no args exactly match their primitive name (faking out InterpreterSimulator>>callExternalPrimitive:).  They should only be called from within wrapper code that simulates the prologue and epilogue.  Primitive method that take args don't have this accidental matching problem since their names contain colons while their primitive names do not. - ajh 8/21/2002"

	^ nil! !

!SmartSyntaxInterpreterPlugin class methodsFor: 'translation' stamp: 'sr 12/23/2001 22:24'!
translateDoInlining: inlineFlag locally: localFlag debug: debugFlag 
	^ self
		translate: self moduleName , '.c'
		doInlining: inlineFlag
		locally: localFlag
		debug: debugFlag! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:40'!
argumentCountOf: methodPointer
	^methodPointer numArgs! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/11/1998 03:01'!
arrayValueOf: oop
	self returnTypeC: 'void *'.
	self success: (self isWordsOrBytes: oop).
	^CArrayAccessor on: oop.! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
become: array1 with: array2
	array1 elementsExchangeIdentityWith: array2! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 21:27'!
booleanValueOf: obj
	obj == true ifTrue:[^true].
	obj == false ifTrue:[^false].
	self primitiveFail.
	^nil! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 9/18/1998 21:11'!
byteAt: accessor
	^accessor byteAt: 0! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 9/18/1998 21:12'!
byteAt: accessor put: value
	^accessor byteAt: 0 put: value! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/7/1998 18:23'!
byteSizeOf: oop
	"Return the size of the receiver in bytes"
	^oop class isBytes
		ifTrue:[(self slotSizeOf: oop)]
		ifFalse:[(self slotSizeOf: oop) * 4]! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
byteSwapped: w
	"Return the given integer with its bytes in the reverse order."

	^ ((w bitShift: -24) bitAnd: 16rFF) +
	  ((w bitShift: -8) bitAnd: 16rFF00) +
	  ((w bitShift: 8) bitAnd: 16rFF0000) +
	  ((w bitShift: 24) bitAnd: 16rFF000000)
! !

!InterpreterProxy methodsFor: 'special objects' stamp: 'ar 9/16/1998 21:43'!
characterTable
	^Character characterTable! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/4/1998 15:47'!
checkedIntegerValueOf: intOop
	(self isIntegerObject: intOop)
		ifTrue:[^self integerValueOf: intOop]
		ifFalse:[self primitiveFail. ^0].! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:42'!
classArray
	^Array! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classBitmap
	^Bitmap! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classByteArray
	^ByteArray! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classCharacter
	^Character! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/29/1999 22:03'!
classExternalAddress
	^Smalltalk at: #ExternalAddress ifAbsent:[nil]! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/29/1999 22:03'!
classExternalData
	^Smalltalk at: #ExternalData ifAbsent:[nil]! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/29/1999 22:03'!
classExternalFunction
	^Smalltalk at: #ExternalFunction ifAbsent:[nil]! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/29/1999 22:04'!
classExternalLibrary
	^Smalltalk at: #ExternalLibrary ifAbsent:[nil]! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/29/1999 22:04'!
classExternalStructure
	^Smalltalk at: #ExternalStructure ifAbsent:[nil]! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classFloat
	^Float! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 11/19/1999 14:29'!
classLargeNegativeInteger
	^LargeNegativeInteger! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classLargePositiveInteger
	^LargePositiveInteger! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classPoint
	^Point! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classSemaphore
	^Semaphore! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 21:43'!
classSmallInteger
	^SmallInteger! !

!InterpreterProxy methodsFor: 'special classes' stamp: 'ar 9/16/1998 20:43'!
classString
	^String! !

!InterpreterProxy methodsFor: 'instance creation' stamp: 'ar 9/18/1998 20:11'!
clone: oop
	^oop clone! !

!InterpreterProxy methodsFor: 'BitBlt support' stamp: 'ar 4/12/1999 23:29'!
copyBits
	bb copyBits.! !

!InterpreterProxy methodsFor: 'BitBlt support' stamp: 'ar 4/12/1999 23:29'!
copyBitsFrom: leftX to: rightX at: yValue
	bb copyBitsFrom: leftX to: rightX at: yValue.! !

!InterpreterProxy methodsFor: 'special objects' stamp: 'ar 9/16/1998 21:42'!
displayObject
	^Display! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
failed
	^successFlag not! !

!InterpreterProxy methodsFor: 'special objects' stamp: 'ar 9/16/1998 20:40'!
falseObject
	^false! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/10/1998 21:22'!
fetchArray: fieldIndex ofObject: objectPointer
	"Fetch the instance variable at the given index of the given object. Return the address of first indexable field of resulting array object, or fail if the instance variable does not contain an indexable bytes or words object."
	"Note: May be called by translated primitive code."

	| arrayOop |
	self returnTypeC: 'void *'.
	arrayOop := self fetchPointer: fieldIndex ofObject: objectPointer.
	^ self arrayValueOf: arrayOop
! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:07'!
fetchClassOf: oop
	^oop class! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/10/1998 21:21'!
fetchFloat: fieldIndex ofObject: objectPointer
	"Fetch the instance variable at the given index of the given object. Return the C double precision floating point value of that instance variable, or fail if it is not a Float."
	"Note: May be called by translated primitive code."

	| floatOop |
	self returnTypeC: 'double'.
	floatOop := self fetchPointer: fieldIndex ofObject: objectPointer.
	^ self floatValueOf: floatOop! !

!InterpreterProxy methodsFor: 'object access' stamp: 'tpr 3/15/2004 20:20'!
fetchInteger: fieldIndex ofObject: objectPointer
	"Note: May be called by translated primitive code."

	| intOop |
	self inline: false.
	intOop := self fetchPointer: fieldIndex ofObject: objectPointer.
	^self checkedIntegerValueOf: intOop! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 10/27/1999 14:13'!
fetchIntegerOrTruncFloat: fieldIndex ofObject: objectPointer
	"Support for BitBlt simulation only"
	| intOrFloat |
	intOrFloat := self fetchPointer: fieldIndex ofObject: objectPointer.
	(self isIntegerObject: intOrFloat) ifTrue: [^ self integerValueOf: intOrFloat].
	intOrFloat isFloat ifTrue:[^intOrFloat truncated].
	^self primitiveFail.! !

!InterpreterProxy methodsFor: 'object access' stamp: 'tpr 6/6/2005 19:11'!
fetchLong32: fieldIndex ofObject: oop
"fetchWord:ofObject: is rescinded as of VMMaker 3.8 64bit VM. This is the same code as used therein and may need revision for 64 bit cleanliness"
	^oop instVarAt: fieldIndex+1! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/10/1998 16:04'!
fetchPointer: index ofObject: oop
	^oop instVarAt: index+1! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/18/1998 20:26'!
firstFixedField: oop
	self returnTypeC:'void *'.
	^CObjectAccessor on: oop! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/10/1998 16:22'!
firstIndexableField: oop
	self returnTypeC:'void *'.
	^CArrayAccessor on: oop! !

!InterpreterProxy methodsFor: 'converting' stamp: 'tpr 12/29/2005 16:35'!
floatObjectOf: aFloat
	self var: #aFloat type: 'double '.
	aFloat class == Float ifFalse:[self error:'Not a float object'].
	^aFloat! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 16:09'!
floatValueOf: oop
	self returnTypeC:'double'.
	oop class == Float
		ifTrue:[^oop]
		ifFalse:[self primitiveFail. ^0.0].! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:52'!
fullDisplayUpdate
	Display display! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
fullGC
	Smalltalk garbageCollect.! !

!InterpreterProxy methodsFor: 'other' stamp: 'tpr 12/21/2005 18:49'!
getThisSessionID
	"Answer a session identifier which represents the current instance of Squeak.
	The identifier is expected to be unique among all instances of Squeak on a
	network at any point in time."

	[thisSessionID = 0]
		whileTrue:
			[thisSessionID := (Random new next * SmallInteger maxVal) asInteger].
	^ thisSessionID
! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 11/17/1999 22:04'!
includesBehavior: aClass ThatOf: aSuperclass
	^aClass includesBehavior: aSuperclass! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:52'!
incrementalGC
	Smalltalk garbageCollectMost.! !

!InterpreterProxy methodsFor: 'initialize' stamp: 'ar 9/16/1998 01:35'!
initialize
	successFlag := true.
	remapBuffer := OrderedCollection new.
	stack := OrderedCollection new.! !

!InterpreterProxy methodsFor: 'instance creation' stamp: 'ar 9/16/1998 01:11'!
instantiateClass: classPointer indexableSize: size
	^size = 0 
		ifTrue:[classPointer basicNew]
		ifFalse:[classPointer basicNew: size]! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 16:13'!
integerObjectOf: value
	value class == SmallInteger ifFalse:[self error:'Not a SmallInteger object'].
	^value! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 16:10'!
integerValueOf: oop
	oop class == SmallInteger ifFalse:[self error:'Not a SmallInteger'].
	^oop! !

!InterpreterProxy methodsFor: 'converting' stamp: 'tpr 12/29/2005 17:35'!
ioFilename: aCharBuffer fromString: aFilenameString ofLength: filenameLength resolveAliases: aBoolean
	"the vm has to convert aFilenameString via any canonicalization and char-mapping and put the result in aCharBuffer. This doesn't translate well in Smalltalk since we know how long strings are rather than considering them terminated by a 0 char. Do the best we can.
Note the resolveAliases flag - this is an awful artefact of OSX and Apples demented alias handling. When opening a file, the flag must be  true, when closing or renaming it must be false. Sigh."
	aCharBuffer replaceFrom:1 to: filenameLength with: aFilenameString! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 5/11/2000 20:05'!
ioLoadFunction: functionName From: moduleName
	"Dummy - provided by support code"
	^0! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/28/1999 18:33'!
ioLoadModule: moduleNameIndex OfLength: moduleLength
	"Dummy - provided by support code"
	^0! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/28/1999 18:34'!
ioLoadSymbol: functionNameIndex OfLength: functionLength FromModule: moduleHandle
	"Dummy - provided by support code"
	^0! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 5/13/2000 14:55'!
ioMicroMSecs
	^Time millisecondClockValue! !

!InterpreterProxy methodsFor: 'testing' stamp: 'tpr 12/29/2005 16:35'!
is: oop KindOf: aString
	"InterpreterProxy new is: 42 KindOf: 'Number'"
	| theClass |
	self var: #aString type:'char *'.
	theClass := Smalltalk at: aString asSymbol ifAbsent:[nil].
	^theClass isNil
		ifTrue:[false]
		ifFalse:[^oop isKindOf: theClass]! !

!InterpreterProxy methodsFor: 'testing' stamp: 'tpr 12/29/2005 16:35'!
is: oop MemberOf: aString
	"InterpreterProxy new is: 42 MemberOf:'SmallInteger'"
	| theClass |
	self var: #aString type:'char *'.
	theClass := Smalltalk at: aString asSymbol ifAbsent:[nil].
	^theClass isNil
		ifTrue:[false]
		ifFalse:[^oop isMemberOf: theClass]! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 12/5/2003 20:17'!
isArray: oop
	^(self isIntegerObject: oop) not and:[(oop class format bitAnd: 15) = 2]
! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:04'!
isBytes: oop
	^oop class isBytes! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 21:44'!
isFloatObject: oop
	^oop class == Float! !

!InterpreterProxy methodsFor: 'FFI support' stamp: 'ar 11/28/1999 19:04'!
isInMemory: address
	"Return true if the given address is in ST object memory"
	^true! !

!InterpreterProxy methodsFor: 'testing' stamp: 'acg 9/19/1999 13:11'!
isIndexable: oop
	^oop class isVariable! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:12'!
isIntegerObject: objectPointer
	^objectPointer class == SmallInteger! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:13'!
isIntegerValue: intValue
	^intValue class == SmallInteger! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 10/27/1999 14:21'!
isInterpreterProxy
	"Return true since I am not a real Interpreter simulation"
	^true! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:04'!
isPointers: oop
	^oop class isPointers! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:16'!
isWeak: oop
	^oop class isWeak! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 10/9/1998 22:19'!
isWords: oop
	^oop class isPointers not and:[oop class isBytes not]! !

!InterpreterProxy methodsFor: 'testing' stamp: 'ar 9/16/1998 01:05'!
isWordsOrBytes: oop
	^(self isBytes: oop) or:[self isWords: oop]! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:38'!
literal: offset ofMethod: methodPointer
	^methodPointer literals at: offset+1! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:40'!
literalCountOf: methodPointer
	^methodPointer numLiterals! !

!InterpreterProxy methodsFor: 'BitBlt support' stamp: 'ar 4/12/1999 23:29'!
loadBitBltFrom: bbOop
	bb := bbOop.! !

!InterpreterProxy methodsFor: 'initialize' stamp: 'ar 10/3/1998 18:50'!
loadStackFrom: aContext
	self push: aContext receiver.
	method := aContext method.
	argumentCount := method numArgs.
	1 to: argumentCount do:[:i| self push: (aContext at: i) ].! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 9/18/1998 21:11'!
longAt: accessor
	^accessor longAt: 0! !

!InterpreterProxy methodsFor: 'private' stamp: 'ar 9/18/1998 21:11'!
longAt: accessor put: value
	^accessor longAt: 0 put: value! !

!InterpreterProxy methodsFor: 'instance creation' stamp: 'ar 10/10/1998 16:14'!
makePointwithxValue: xValue yValue: yValue
	(xValue class == SmallInteger and:[yValue class == SmallInteger]) 
		ifFalse:[self error:'Not SmallInteger objects'].
	^xValue@yValue! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/7/1998 18:43'!
methodArgumentCount
	^argumentCount! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/7/1998 18:43'!
methodPrimitiveIndex
	^method primitive! !

!InterpreterProxy methodsFor: 'special objects' stamp: 'ar 9/16/1998 20:40'!
nilObject
	^nil! !

!InterpreterProxy methodsFor: 'object access' stamp: 'tpr 6/6/2005 19:28'!
obsoleteDontUseThisFetchWord: fieldIndex ofObject: oop
"fetchWord:ofObject: is rescinded as of VMMaker 3.8 64bit VM. This is a placeholder to sit in the sqVirtualMachine structure to support older plugins for a while"
	self halt: 'deprecated method'! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 01:41'!
pop: nItems
	1 to: nItems do:[:i| stack removeLast].! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 01:41'!
pop: nItems thenPush: oop
	self pop: nItems.
	self push: oop.! !

!InterpreterProxy methodsFor: 'instance creation' stamp: 'ar 9/16/1998 01:14'!
popRemappableOop
	^remapBuffer removeLast! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 16:11'!
positive32BitIntegerFor: integerValue
	integerValue isInteger ifFalse:[self error:'Not an Integer object'].
	^integerValue > 0
		ifTrue:[integerValue]
		ifFalse:[ (1 bitShift: 32) + integerValue]! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 10/10/1998 16:12'!
positive32BitValueOf: oop
	oop isInteger ifFalse:[self error:'Not an integer object'].
	oop < 0 
		ifTrue:[self primitiveFail. ^0]
		ifFalse:[^oop]! !

!InterpreterProxy methodsFor: 'converting' stamp: 'JMM 11/8/2001 15:26'!
positive64BitIntegerFor: integerValue
	integerValue isInteger ifFalse:[self error:'Not an Integer object'].
	^integerValue > 0
		ifTrue:[integerValue]
		ifFalse:[ (1 bitShift: 64) + integerValue]! !

!InterpreterProxy methodsFor: 'converting' stamp: 'JMM 11/8/2001 15:27'!
positive64BitValueOf: oop
	oop isInteger ifFalse:[self error:'Not an integer object'].
	oop < 0 
		ifTrue:[self primitiveFail. ^0]
		ifFalse:[^oop]! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
primitiveFail
	(self confirm:'A primitive is failing -- Stop simulation?') ifTrue:[self halt].
	successFlag := false.! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:39'!
primitiveIndexOf: methodPointer
	^methodPointer primitive! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 11/28/1999 17:43'!
primitiveMethod
	"Return the method an external primitive was defined in"
	^method! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 01:42'!
push: object
	stack addLast: object! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 10/10/1998 21:16'!
pushBool: trueOrFalse
	(trueOrFalse == true or:[trueOrFalse == false]) ifFalse:[self error:'Not a Boolean'].
	self push: trueOrFalse! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'tpr 12/29/2005 16:35'!
pushFloat: f
	self var: #f type: 'double '.
	f class == Float ifFalse:[^self error:'Not a Float'].
	self push: f.! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 10/10/1998 21:20'!
pushInteger: integerValue
	self push: (self integerObjectOf: integerValue).! !

!InterpreterProxy methodsFor: 'instance creation' stamp: 'ar 9/16/1998 01:14'!
pushRemappableOop: oop
	remapBuffer addLast: oop! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:52'!
showDisplayBits: aForm Left: l Top: t Right: r Bottom: b
	aForm == Display ifTrue:[
		Display forceToScreen: (Rectangle left: l right: r top: t bottom: b)].! !

!InterpreterProxy methodsFor: 'other' stamp: 'JMM 6/6/2000 21:00'!
signalSemaphoreWithIndex: semaIndex
	((Smalltalk externalObjects) at: semaIndex) signal! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 11/29/1999 22:01'!
signed32BitIntegerFor: integerValue
	integerValue isInteger ifFalse:[self error:'Not an Integer object'].
	^integerValue! !

!InterpreterProxy methodsFor: 'converting' stamp: 'ar 11/29/1999 22:00'!
signed32BitValueOf: oop
	oop isInteger ifFalse:[self error:'Not an integer object'].
	^oop! !

!InterpreterProxy methodsFor: 'converting' stamp: 'JMM 11/8/2001 15:27'!
signed64BitIntegerFor: integerValue
	integerValue isInteger ifFalse:[self error:'Not an Integer object'].
	^integerValue! !

!InterpreterProxy methodsFor: 'converting' stamp: 'JMM 11/8/2001 15:27'!
signed64BitValueOf: oop
	oop isInteger ifFalse:[self error:'Not an integer object'].
	^oop! !

!InterpreterProxy methodsFor: 'object access' stamp: 'tpr 12/29/2005 16:35'!
sizeOfSTArrayFromCPrimitive: cPtr
	"Note: Only called by translated primitive code."
	self var: #cPtr type: 'void *'.
	^self shouldNotImplement! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/7/1998 18:24'!
slotSizeOf: oop
	"Returns the number of slots in the receiver.
	If the receiver is a byte object, return the number of bytes.
	Otherwise return the number of words."
	^(oop basicSize) + (oop class instSize)! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:53'!
stObject: array at: index
	^array at: index! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 9/16/1998 01:53'!
stObject: array at: index put: value
	^array at: index put: value! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/7/1998 18:26'!
stSizeOf: oop
	"Return the number of indexable fields in the receiver"
	^oop basicSize! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/27/1998 15:22'!
stackFloatValue: offset
	| oop |
	self returnTypeC: 'double'.
	oop := self stackValue: offset.
	(self isFloatObject: oop) ifFalse: [self primitiveFail. ^0.0].
	^oop! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 22:07'!
stackIntegerValue: offset
	| oop |
	oop := self stackValue: offset.
	(self isIntegerObject: oop) ifFalse: [self primitiveFail. ^0].
	^oop! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 22:07'!
stackObjectValue: offset
	| oop |
	oop := self stackValue: offset.
	(self isIntegerObject: oop) ifTrue: [self primitiveFail. ^ nil].
	^oop! !

!InterpreterProxy methodsFor: 'stack access' stamp: 'ar 9/16/1998 11:47'!
stackValue: offset
	^stack at: stack size - offset.! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/25/1998 16:16'!
storeInteger: index ofObject: oop withValue: integer
	(self isIntegerValue: integer) 
		ifTrue:[^self storePointer: index ofObject: oop withValue: integer]
		ifFalse:[^self primitiveFail]! !

!InterpreterProxy methodsFor: 'object access' stamp: 'ar 10/10/1998 21:25'!
storePointer: index ofObject: oop withValue: valuePointer
	^oop instVarAt: index+1 put: valuePointer! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/19/1998 13:30'!
success: aBoolean
	successFlag not ifTrue:[^self].
	successFlag := successFlag and:[aBoolean].
	successFlag not ifTrue:[
		(self confirm:'A primitive is failing -- Stop simulation?') ifTrue:[self halt]].! !

!InterpreterProxy methodsFor: 'other' stamp: 'ar 9/16/1998 20:51'!
superclassOf: classPointer
	^classPointer superclass! !

!InterpreterProxy methodsFor: 'special objects' stamp: 'ar 9/16/1998 20:41'!
trueObject
	^true! !

!InterpreterProxy methodsFor: 'other' stamp: 'tpr 12/22/2005 17:48'!
vmEndianness
	"return 0 for little endian, 1 for big endian"
	^SmalltalkImage current endianness =#big ifTrue:[1] ifFalse:[0]! !

!InterpreterProxy class methodsFor: 'private' stamp: 'tpr 12/22/2005 17:13'!
validateProxyImplementation: anInterpreter 
	"InterpreterProxy validateProxyImplementation: Interpreter"

	| proxyClass catList |
	proxyClass := InterpreterProxy.
	catList := proxyClass organization categories copy asOrderedCollection.
	catList remove: 'initialize' ifAbsent:[].
	catList remove: 'private' ifAbsent:[].
	catList do:[:categ|
		(proxyClass organization listAtCategoryNamed: categ) do:[:selector|
			(anInterpreter canUnderstand: selector) 
				ifFalse:
					[self notifyWithLabel: selector, ' is not implemented in ', anInterpreter name]]]! !

!InterpreterSimulationObject methodsFor: 'simulation' stamp: 'di 8/5/2004 18:55'!
cCoerce: value to: cTypeString
	"Here the Simulator has a chance to create properly typed flavors of CArray access."

	value isCObjectAccessor ifTrue:
		[^ self getInterpreter cCoerce: value to: cTypeString].
	(value isMemberOf: CArray) ifTrue:
		[^ self getInterpreter cCoerce: value to: cTypeString].
	^ value! !

!InterpreterSimulationObject methodsFor: 'memory access' stamp: 'di 8/5/2004 20:56'!
long32At: byteAddress
	"Simulation support.  Answer the 32-bit word at byteAddress which must be 0 mod 4."

	^self getInterpreter long32At: byteAddress! !

!InterpreterSimulationObject methodsFor: 'memory access' stamp: 'di 8/5/2004 20:56'!
long32At: byteAddress put: a32BitValue
	"Simulation support.  Store the 32-bit value at byteAddress which must be 0 mod 4."

	^self getInterpreter long32At: byteAddress put: a32BitValue! !

!InterpreterSimulationObject methodsFor: 'memory access' stamp: 'ikp 8/3/2004 15:56'!
oopForPointer: aPointer
	"Simulation support.  Pointers and oops are the same when simulating; answer aPointer."

	^aPointer! !

!InterpreterSimulationObject methodsFor: 'memory access' stamp: 'ikp 8/3/2004 15:56'!
pointerForOop: anOop
	"Simulation support.  Pointers and oops are the same when simulating; answer anOop."

	^anOop! !

!ObjectMemory methodsFor: 'gc -- mark and sweep'!
aComment
	"The mark phase is based on a pointer reversing traversal. This is a little tricky because the class, which is needed by the traversal, may be in either the header (as a compact class index) or in the word above the header. See memo 'Revised object format'.
	Compact classes are marked and traced separately.
	How do you know that you are returning from having marked a class? Parent pointer has 10 in low bits.

Here are the states an object may be in, followed by what to do next in brackets []:

  Start Object: parentField is set, [obj := child]:
	obj is pointed at by a field in parent that is being traced now. obj is marked.
		[(parent goes up to the next field) field addr := obj. go to Upward]
	obj is pointed at by a field in parent that is being traced now. obj is unmarked. obj has no pointers.
		[put 10 into low bits of header. field addr := obj. go to Start Field (to process class word)]
	obj is pointed at by a field in parent that is being traced now. obj is unmarked. obj has pointers.
		[put 10 into low bits of header. point to last field. go to Start Field]

  Start Field: 
	Field ends in 10. It is the header. Short Class is not 0.
		[Set low bits to correct value. (have parent pointer) go to Upward]
	Field ends in 10. It is the header. Short Class is 0.
		[child := word above header. low bits of child := 01. class word := parentField. parentField := loc of class word. go to Start Obj]
	Field is Integer.
		[point one word up, go to Start Field]
	Field is oop.
		[child := field. field := parentField. parentField := loc of field. go to Start Obj]

  Upward [restore low bits of header (at field addr)]:
	parentField is 3. (bits 11, int 1).
		[done!!]
	parentField ends in 00.
		[child := field addr. field addr := parentField. parentField := field addr contents.
		field addr contents := child (addr of prev object. its oop). field addr - 4. go to Start Field]
	parentField ends in 01. Were tracing the class.
		[child := field addr. field addr := parentField (loc of class word). parentField := field addr contents.
		field addr contents := child (addr of prev object. its oop). field addr + 4 (header). go to Upward]
"! !

!ObjectMemory methodsFor: 'finalization' stamp: 'ar 1/18/2005 15:07'!
aFinalizationComment
	"This finalization scheme assumes to have weak classes in which the fields are not traced during the mark phase of a GC. This means, if an object is referenced only by any instances of weak classes it can be collected. In turn, we need to find out if an object referenced by a weak class is actually being collected because we have to invalidate the weak object pointer and to signal that the object has gone.
	How do we know that an object referenced by a weak class is being collected? Well,  this is based on two observations. First, objects will not change their relative locations in memory, meaning that if object A is created BEFORE object B it will always have a physical memory address which is LESS than B. Secondly, GC always works from a given starting address (youngStart during incremental GC; startOfMemory during fullGC) up to end of memory. If we can somehow garantuee that the weak reference is created after the object it points to we can easily implement the following simple scheme:
	1) Mark phase
		Do not trace the fields of any instances of weak classes.
	2) Sweep phase:
		a) Explicitly mark all free objects.
		b) 	If a weak reference is encountered check the the object it points to. 
			If the object is marked as free than we know that this weak reference's object is gone.
			Signal that it is gone.

	There is, however, one small problem with this approach. We cannot always garantuee that WeakReferences point backwards such as in the following piece of code:
		| o1 o2 w1 w2 |
		o1 := Object new.
		w1 := WeakReference on: o1.
		o2 := Object new.
		w2 := WeakReference on: o2.
		o1 become: o2.
The become: operation makes w1 point to o2 and because o2 has been created AFTER w1 the object reference in w1 points forward. Why might this be a problem? Well, if the GC would start after the weak reference AND free the object then the weak reference would simply point to an invalid memory location (since we've not been checking the weak reference during sweep phase).

	Fortunately, this can not happen in the current ObjectMemory implementation. Why? Well, the only GC not starting at the beginning of the memory is incremental GC. Incremental GC however is only executed in so-called youngSpace. If both, the weak reference AND the object it points to reside in youngSpace then we can still check the weak reference. If however, the weak reference is not in youngSpace but the object is, then the reference is itself a root for young space and will be processed by the GC.

	In the end, we just need a little adjustment in step 2b) of the above procedure which looks as follows:
		If the weak reference points 
			* backwards: check if the object header is marked free
			* forwards: check if the object has been marked in markPhase.

	Note that a number of finalizations will only be executed during a fullGC. So, if you must garantuee that some object has been finalized you definitely need to do a fullGC.

ar 3/20/98 17:20"

	self error:'Comment only'.! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'dtl 4/22/2007 20:46'!
accessibleObjectAfter: oop 
	"Return the accessible object following the given object or 
	free chunk in the heap. Return nil when heap is exhausted."
	| obj |
	self inline: false.
	obj := self objectAfter: oop.
	[self oop: obj isLessThan: endOfMemory]
		whileTrue: [(self isFreeObject: obj) ifFalse: [^ obj].
			obj := self objectAfter: obj].
	^ nil! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 6/8/2006 13:06'!
addGCRoot: varLoc
	"Add the given variable location to the extra roots table"
	self export: true.
	self var: #varLoc declareC: 'sqInt *varLoc'.
	extraRootCount >= ExtraRootSize ifTrue:[^false]. "out of space"
	extraRoots at: (extraRootCount := extraRootCount+1) put: varLoc.
	^true! !

!ObjectMemory methodsFor: 'initialization' stamp: 'dtl 4/22/2007 20:48'!
adjustAllOopsBy: bytesToShift 
	"Adjust all oop references by the given number of bytes. This 
	is done just after reading in an image when the new base 
	address of the object heap is different from the base address 
	in the image."
	"di 11/18/2000 - return number of objects found"

	| oop totalObjects |
	self inline: false.
	bytesToShift = 0 ifTrue: [^300000].
	"this is probably an improvement over the previous answer of 
	nil, but maybe we should do the obejct counting loop and 
	simply guard the adjustFieldsAndClass... with a bytesToShift 
	= 0 ifFalse: ?"
	totalObjects := 0.
	oop := self firstObject.
	[self oop: oop isLessThan: endOfMemory]
		whileTrue:
			[(self isFreeObject: oop)
				ifFalse:
					[totalObjects := totalObjects + 1.
					 self adjustFieldsAndClassOf: oop by: bytesToShift].
			 oop := self objectAfter: oop].
	^totalObjects! !

!ObjectMemory methodsFor: 'initialization' stamp: 'dtl 4/22/2007 17:25'!
adjustFieldsAndClassOf: oop by: offsetBytes 
	"Adjust all pointers in this object by the given offset."
	| fieldAddr fieldOop classHeader newClassOop |
	self inline: true.
	offsetBytes = 0 ifTrue: [^nil].
	fieldAddr := oop + (self lastPointerOf: oop).
	[self oop: fieldAddr isGreaterThan: oop]
		whileTrue: [fieldOop := self longAt: fieldAddr.
			(self isIntegerObject: fieldOop)
				ifFalse: [self longAt: fieldAddr put: fieldOop + offsetBytes].
			fieldAddr := fieldAddr - BytesPerWord].
	(self headerType: oop) ~= HeaderTypeShort
		ifTrue: ["adjust class header if not a compact class"
			classHeader := self longAt: oop - BytesPerWord.
			newClassOop := (classHeader bitAnd: AllButTypeMask) + offsetBytes.
			self longAt: oop - BytesPerWord put: (newClassOop bitOr: (classHeader bitAnd: TypeMask))]! !

!ObjectMemory methodsFor: 'become' stamp: 'dtl 4/22/2007 20:53'!
allYoung: array1 and: array2 
	"Return true if all the oops in both arrays, and the arrays 
	themselves, are in the young object space."
	| fieldOffset |
	(self oop: array1 isLessThan: youngStart)
		ifTrue: [^ false].
	(self oop: array2 isLessThan: youngStart)
		ifTrue: [^ false].
	fieldOffset := self lastPointerOf: array1.
	"same size as array2"
	[fieldOffset >= BaseHeaderSize] whileTrue:
		[(self oop: (self longAt: array1 + fieldOffset) isLessThan: youngStart)
			ifTrue: [^ false].
		(self oop: (self longAt: array2 + fieldOffset) isLessThan: youngStart)
			ifTrue: [^ false].
		fieldOffset := fieldOffset - BytesPerWord].
	^ true! !

!ObjectMemory methodsFor: 'allocation' stamp: 'ikp 3/27/2005 18:07'!
allocate: byteSize headerSize: hdrSize h1: baseHeader h2: classOop h3: extendedSize doFill: doFill with: fillWord 
	"Allocate a new object of the given size and number of header words. (Note: byteSize already includes space for the base header word.) Initialize the header fields of the new object and fill the remainder of the object with the given value.
	May cause a GC"

	| newObj remappedClassOop end i |
	self inline: true.
	self var: #i type: 'usqInt'.
	self var: #end type: 'usqInt'.
	"remap classOop in case GC happens during allocation"
	hdrSize > 1 ifTrue: [self pushRemappableOop: classOop].
	newObj := self allocateChunk: byteSize + (hdrSize - 1 * BytesPerWord).
	hdrSize > 1 ifTrue: [remappedClassOop := self popRemappableOop].

	hdrSize = 3
		ifTrue: [self longAt: newObj put: (extendedSize bitOr: HeaderTypeSizeAndClass).
			self longAt: newObj + BytesPerWord put: (remappedClassOop bitOr: HeaderTypeSizeAndClass).
			self longAt: newObj + (BytesPerWord*2) put: (baseHeader bitOr: HeaderTypeSizeAndClass).
			newObj := newObj + (BytesPerWord*2)].

	hdrSize = 2
		ifTrue: [self longAt: newObj put: (remappedClassOop bitOr: HeaderTypeClass).
			self longAt: newObj + BytesPerWord put: (baseHeader bitOr: HeaderTypeClass).
			newObj := newObj + BytesPerWord].

	hdrSize = 1
		ifTrue: [self longAt: newObj put: (baseHeader bitOr: HeaderTypeShort)].
	"clear new object"
	doFill ifTrue: [end := newObj + byteSize.
			i := newObj + BytesPerWord.
			[i < end] whileTrue: [self longAt: i put: fillWord.
					i := i + BytesPerWord]].
	DoAssertionChecks
		ifTrue: [self okayOop: newObj.
			self oopHasOkayClass: newObj.
			(self objectAfter: newObj) = freeBlock
				ifFalse: [self error: 'allocate bug: did not set header of new oop correctly'].
			(self objectAfter: freeBlock) = endOfMemory
				ifFalse: [self error: 'allocate bug: did not set header of freeBlock correctly']].

	^newObj! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 4/22/2007 21:04'!
allocateChunk: byteSize 
	"Allocate a chunk of the given size. Sender must be sure that  the requested size includes enough space for the header  word(s). " 
	"Details: To limit the time per incremental GC, do one every so many allocations. The number is settable via primitiveVMParameter to tune your memory system"
	| enoughSpace newFreeSize newChunk |
	self inline: true.

	allocationCount >= allocationsBetweenGCs
		ifTrue: ["do an incremental GC every so many allocations to  keep pauses short"
			self incrementalGC].

	enoughSpace := self sufficientSpaceToAllocate: byteSize.
	enoughSpace
		ifFalse: ["signal that space is running low, but proceed with allocation if possible"
			signalLowSpace := true.
			lowSpaceThreshold := 0. "disable additional interrupts until lowSpaceThreshold is reset by image"
			self saveProcessSignalingLowSpace.
			self forceInterruptCheck].
	(self oop: (self sizeOfFree: freeBlock) isLessThan: byteSize + BaseHeaderSize)
		ifTrue: [self error: 'out of memory'].

	"if we get here, there is enough space for allocation to  succeed "
	newFreeSize := (self sizeOfFree: freeBlock) - byteSize.
	newChunk := freeBlock.
	freeBlock := freeBlock + byteSize.

	"Assume: client will initialize object header of free chunk, so following is not needed:"
	"self setSizeOfFree: newChunk to: byteSize."
	self setSizeOfFree: freeBlock to: newFreeSize.
	allocationCount := allocationCount + 1.
	^newChunk! !

!ObjectMemory methodsFor: 'allocation' stamp: 'di 10/22/1999 11:52'!
allocateOrRecycleContext: needsLarge
	"Return a recycled context or a newly allocated one if none is available for recycling."
	| cntxt |
	needsLarge = 0
	ifTrue: [freeContexts ~= NilContext ifTrue:
				[cntxt := freeContexts.
				freeContexts := self fetchPointer: 0 ofObject: cntxt.
				^ cntxt]]
	ifFalse: [freeLargeContexts ~= NilContext ifTrue:
				[cntxt := freeLargeContexts.
				freeLargeContexts := self fetchPointer: 0 ofObject: cntxt.
				^ cntxt]].
	
	needsLarge = 0
		ifTrue: [cntxt := self instantiateContext: (self splObj: ClassMethodContext)
				sizeInBytes: SmallContextSize]
		ifFalse: [cntxt := self instantiateContext: (self splObj: ClassMethodContext)
				sizeInBytes: LargeContextSize].
	"Required init -- above does not fill w/nil.  All others get written."
	self storePointerUnchecked: 4 "InitialIPIndex" ofObject: cntxt
					withValue: nilObj.
	^ cntxt
! !

!ObjectMemory methodsFor: 'header access'!
baseHeader: oop

	^ self longAt: oop! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 4/22/2007 21:05'!
beRootIfOld: oop 
	"If this object is old, mark it as a root (because a new object 
	may be stored into it)"
	self inline: false.
	((self oop: oop isLessThan: youngStart)
			and: [(self isIntegerObject: oop) not])
		ifTrue: ["Yes, oop is an old object"
			self noteAsRoot: oop headerLoc: oop]! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 14:28'!
beRootWhileForwarding: oop
	"Record that the given oop in the old object area points to an object in the young area when oop may be forwarded."
	"Warning: No young objects should be recorded as roots. Callers are responsible for ensuring this constraint is not violated."

	| header fwdBlock |
	header := self longAt: oop.
	(header bitAnd: MarkBit) ~= 0
		ifTrue: ["This oop is forwarded"
				fwdBlock := (header bitAnd: AllButMarkBitAndTypeMask) << 1.
				DoAssertionChecks ifTrue: [ self fwdBlockValidate: fwdBlock ].
				self noteAsRoot: oop headerLoc: fwdBlock + BytesPerWord]
		ifFalse: ["Normal -- no forwarding"
				self noteAsRoot: oop headerLoc: oop]! !

!ObjectMemory methodsFor: 'become' stamp: 'brp 9/19/2003 16:09'!
become: array1 with: array2

	^ self become: array1 with: array2 twoWay: true copyHash: true
! !

!ObjectMemory methodsFor: 'become' stamp: 'tpr 3/23/2004 17:30'!
become: array1 with: array2 twoWay: twoWayFlag copyHash: copyHashFlag 
	"All references to each object in array1 are swapped with all references to the corresponding object in array2. That is, all pointers to one object are replaced with with pointers to the other. The arguments must be arrays of the same length. 
	Returns true if the primitive succeeds."
	"Implementation: Uses forwarding blocks to update references as done in compaction."
	(self isArray: array1) ifFalse: [^false].
	(self isArray: array2) ifFalse: [^false].
	(self lastPointerOf: array1) = (self lastPointerOf: array2) ifFalse: [^false].
	(self containOnlyOops: array1 and: array2) ifFalse: [^false].

	(self prepareForwardingTableForBecoming: array1 with: array2 twoWay: twoWayFlag) ifFalse: [^false]. "fail; not enough space for forwarding table"

	(self allYoung: array1 and: array2)
		ifTrue: ["sweep only the young objects plus the roots"
			self mapPointersInObjectsFrom: youngStart to: endOfMemory]
		ifFalse: ["sweep all objects"
			self mapPointersInObjectsFrom: self startOfMemory to: endOfMemory].
	twoWayFlag
		ifTrue: [self restoreHeadersAfterBecoming: array1 with: array2]
		ifFalse: [self restoreHeadersAfterForwardBecome: copyHashFlag].

	self initializeMemoryFirstFree: freeBlock. "re-initialize memory used for forwarding table"
	
	self forceInterruptCheck. "pretty much guaranteed to take a long time, so check for timers etc ASAP"

	^true "success"! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 10/17/2009 11:03'!
biasToGrow
	| growSize |
	self var: #growSize type: 'usqInt'.
	growSize :=  growHeadroom*3/2 - (self sizeOfFree: freeBlock).
	growSize > 0
		ifTrue: [self growObjectMemory: growSize]! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 4/22/2007 18:10'!
biasToGrowCheckGCLimit
	| growth |
	growth := (youngStart - memory) - gcBiasToGrowThreshold.
	growth < 0 ifTrue: [gcBiasToGrowThreshold := youngStart - memory].
	growth > gcBiasToGrowGCLimit
		 ifTrue: 
			[self fullGC.
			gcBiasToGrowThreshold := youngStart - memory].

					! !

!ObjectMemory methodsFor: 'allocation' stamp: 'ar 2/25/2001 17:45'!
bytesLeft: includingSwap
	^(self sizeOfFree: freeBlock) "already commited"
		+ (self sqMemoryExtraBytesLeft: includingSwap).! !

!ObjectMemory methodsFor: 'initialization' stamp: 'dtl 9/25/2009 17:43'!
bytesPerWord
	"Answer the size of an object memory word in bytes."

	^BytesPerWord! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:06'!
characterTable
	^self splObj: CharacterTable! !

!ObjectMemory methodsFor: 'memory access' stamp: 'dtl 4/22/2007 18:33'!
checkAddress: byteAddress 
	"Keep this method around for debugging the C code."
	(self oop: byteAddress isLessThan: self startOfMemory)
		ifTrue: [self error: 'bad address: negative'].
	(self oop: byteAddress isGreaterThanOrEqualTo: memoryLimit)
		ifTrue: [self error: 'bad address: past end of heap']! !

!ObjectMemory methodsFor: 'memory access'!
checkedByteAt: byteAddress
	"Assumes zero-based array indexing."

	self checkAddress: byteAddress.
	^ self byteAt: byteAddress! !

!ObjectMemory methodsFor: 'memory access'!
checkedByteAt: byteAddress put: byte
	"Assumes zero-based array indexing."

	self checkAddress: byteAddress.
	self byteAt: byteAddress put: byte.! !

!ObjectMemory methodsFor: 'memory access'!
checkedLongAt: byteAddress
	"Assumes zero-based array indexing. For testing in Smalltalk, this method should be overridden in a subclass."

	self checkAddress: byteAddress.
	self checkAddress: byteAddress + 3.
	^ self longAt: byteAddress! !

!ObjectMemory methodsFor: 'memory access'!
checkedLongAt: byteAddress put: a32BitInteger
	"Assumes zero-based array indexing. For testing in Smalltalk, this method should be overridden in a subclass."

	self checkAddress: byteAddress.
	self checkAddress: byteAddress + 3.
	self longAt: byteAddress put: a32BitInteger.! !

!ObjectMemory methodsFor: 'oop/chunk conversion' stamp: 'JMM 12/4/2002 19:55'!
chunkFromOop: oop
	"Compute the chunk of this oop by subtracting its extra header bytes."

	^ oop - (self extraHeaderBytes: oop)! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:06'!
classArray
	^self splObj: ClassArray! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:06'!
classBitmap
	^self splObj: ClassBitmap! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:06'!
classByteArray
	^self splObj: ClassByteArray! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:07'!
classCharacter
	^self splObj: ClassCharacter! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/21/1999 00:50'!
classExternalAddress
	^self splObj: ClassExternalAddress! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/17/1999 18:23'!
classExternalData
	^self splObj: ClassExternalData! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/17/1999 18:23'!
classExternalFunction
	^self splObj: ClassExternalFunction! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/17/1999 19:45'!
classExternalLibrary
	^self splObj: ClassExternalLibrary! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/21/1999 15:23'!
classExternalStructure
	^self splObj: ClassExternalStructure! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:07'!
classFloat
	^self splObj: ClassFloat! !

!ObjectMemory methodsFor: 'header access' stamp: 'di 6/13/2004 06:55'!
classHeader: oop

	^ self longAt: oop - BaseHeaderSize! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 11/21/1999 23:22'!
classLargeNegativeInteger
	^self splObj: ClassLargeNegativeInteger! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:07'!
classLargePositiveInteger
	^self splObj: ClassLargePositiveInteger! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:07'!
classPoint
	^self splObj: ClassPoint! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:11'!
classSemaphore
	^self splObj: ClassSemaphore! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:11'!
classSmallInteger
	^self splObj: ClassInteger! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:12'!
classString
	^self splObj: ClassString! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'tpr 3/24/2004 22:01'!
clearRootsTable
	"Clear the root bits of the current roots, then empty the roots 
	table. "
	"Caution: This should only be done when the young object 
	space is empty."
	"reset the roots table (after this, all objects are old so there 
	are no roots)"
	| oop |
	1 to: rootTableCount do: [:i | 
			"clear root bits of current root table entries"
			oop := rootTable at: i.
			self longAt: oop put: ((self longAt: oop) bitAnd: AllButRootBit).
			rootTable at: i put: 0].
	rootTableCount := 0! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 4/22/2007 21:10'!
clone: oop
	"Return a shallow copy of the given object. May cause GC"
	"Assume: Oop is a real object, not a small integer."

	| extraHdrBytes bytes newChunk remappedOop fromIndex toIndex lastFrom newOop header hash |
	self inline: false.
	self var: #lastFrom type: 'usqInt'.
	self var: #fromIndex type: 'usqInt'.
	extraHdrBytes := self extraHeaderBytes: oop.
	bytes := self sizeBitsOf: oop.
	bytes := bytes + extraHdrBytes.

	"allocate space for the copy, remapping oop in case of a GC"
	self pushRemappableOop: oop.
	"check it is safe to allocate this much memory. Return 0 if not"
	(self sufficientSpaceToAllocate: 2500 + bytes) ifFalse:[^0].
	newChunk := self allocateChunk: bytes.
	remappedOop := self popRemappableOop.

	"copy old to new including all header words"
	toIndex := newChunk - BytesPerWord.  "loop below uses pre-increment"
	fromIndex := (remappedOop - extraHdrBytes) - BytesPerWord.
	lastFrom := fromIndex + bytes.
	[fromIndex < lastFrom] whileTrue: [
		self longAt: (toIndex := toIndex + BytesPerWord) put: (self longAt: (fromIndex := fromIndex + BytesPerWord))].
	newOop := newChunk + extraHdrBytes.  "convert from chunk to oop"

	"fix base header: compute new hash and clear Mark and Root bits"
	hash := self newObjectHash.
	header := (self longAt: newOop) bitAnd: 16r1FFFF.
	"use old ccIndex, format, size, and header-type fields"
	header := header bitOr: ((hash << 17) bitAnd: 16r1FFE0000).
	self longAt: newOop put: header.
	^newOop
! !

!ObjectMemory methodsFor: 'become' stamp: 'di 8/3/2004 13:12'!
containOnlyOops: array1 and: array2 
	"Return true if neither array contains a small integer. You 
	can't become: integers!!"
	| fieldOffset |
	fieldOffset := self lastPointerOf: array1.
	"same size as array2"
	[fieldOffset >= BaseHeaderSize]
		whileTrue: [(self isIntegerObject: (self longAt: array1 + fieldOffset)) ifTrue: [^ false].
			(self isIntegerObject: (self longAt: array2 + fieldOffset)) ifTrue: [^ false].
			fieldOffset := fieldOffset - BytesPerWord].
	^ true! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:12'!
displayObject
	^self splObj: TheDisplay! !

!ObjectMemory methodsFor: 'oop/chunk conversion' stamp: 'JMM 7/7/2003 14:18'!
extraHeaderBytes: oopOrChunk
	"Return the number of extra bytes used by the given object's header."
	"Warning: This method should not be used during marking, when the header type bits of an object may be incorrect."

	"JMM should be an array lookup!!" 
	self inline: true.
	^ headerTypeBytes at: (self headerType: oopOrChunk).! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:12'!
falseObject
	^falseObj! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ikp 6/9/2004 23:16'!
fetchByte: byteIndex ofObject: oop

	^ self byteAt: oop + BaseHeaderSize + byteIndex! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 3/24/2004 12:35'!
fetchClassOf: oop 
	| ccIndex |
	self inline: true.
	(self isIntegerObject: oop) ifTrue: [^ self splObj: ClassInteger].

	ccIndex := (self baseHeader: oop) >> 12 bitAnd: 31.
	ccIndex = 0
		ifTrue: [^ (self classHeader: oop)
				bitAnd: AllButTypeMask]
		ifFalse: ["look up compact class"
			^ self fetchPointer: ccIndex - 1 ofObject: (self fetchPointer: CompactClasses ofObject: specialObjectsOop)]! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 3/24/2004 12:52'!
fetchClassOfNonInt: oop 
	| ccIndex |
	self inline: true.
	ccIndex := (self baseHeader: oop) >> 12 bitAnd: 31.
	ccIndex = 0
		ifTrue: [^ (self classHeader: oop)
				bitAnd: AllButTypeMask]
		ifFalse: ["look up compact class"
			^ self fetchPointer: ccIndex - 1 ofObject: (self fetchPointer: CompactClasses ofObject: specialObjectsOop)]! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 6/6/2005 19:29'!
fetchLong32: fieldIndex ofObject: oop
	" index by 32-bit units, and return a 32-bit value. Intended to replace fetchWord:ofObject:"

	^ self long32At: oop + BaseHeaderSize + (fieldIndex << 2)! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 7/4/2004 11:11'!
fetchLong32LengthOf: objectPointer
	"Gives size appropriate for, eg, fetchLong32"

	| sz |
	sz := self sizeBitsOf: objectPointer.
	^ (sz - BaseHeaderSize) >> 2! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 7/4/2004 08:33'!
fetchPointer: fieldIndex ofObject: oop
	"index by word size, and return a pointer as long as the word size"

	^ self longAt: oop + BaseHeaderSize + (fieldIndex << ShiftForWord)! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 7/4/2004 09:34'!
fetchWordLengthOf: objectPointer
	"NOTE: this gives size appropriate for fetchPointer: n, but not in general for, eg, fetchLong32, etc."

	| sz |
	sz := self sizeBitsOf: objectPointer.
	^ (sz - BaseHeaderSize) >> ShiftForWord! !

!ObjectMemory methodsFor: 'finalization' stamp: 'dtl 4/22/2007 21:18'!
finalizeReference: oop 
	"During sweep phase we have encountered a weak reference. 
	Check if  its object has gone away (or is about to) and if so, signal a 
	semaphore. "
	"Do *not* inline this in sweepPhase - it is quite an unlikely 
	case to run into a weak reference"
	| weakOop oopGone chunk firstField lastField |
	self inline: false.
	self var: #oop type: 'usqInt'.
	self var: #weakOop type: 'usqInt'.
	firstField := BaseHeaderSize + ((self nonWeakFieldsOf: oop) << ShiftForWord).
	lastField := self lastPointerOf: oop.
	firstField to: lastField by: BytesPerWord do: [:i | 
			weakOop := self longAt: oop + i.
			"ar 1/18/2005: Added oop < youngStart test to make sure we're not testing
			objects in non-GCable region. This could lead to a forward reference in
			old space with the oop pointed to not being marked and thus treated as free."
			(weakOop == nilObj or: [(self isIntegerObject: weakOop) or:[weakOop < youngStart]])

				ifFalse: ["Check if the object is being collected. 
					If the weak reference points  
					* backward: check if the weakOops chunk is free
					* forward: check if the weakOoop has been marked by GC"
					weakOop < oop
						ifTrue: [chunk := self chunkFromOop: weakOop.
							oopGone := ((self longAt: chunk) bitAnd: TypeMask) = HeaderTypeFree]
						ifFalse: [oopGone := ((self baseHeader: weakOop) bitAnd: MarkBit) = 0].
					oopGone ifTrue: ["Store nil in the pointer and signal the  interpreter "
							self longAt: oop + i put: nilObj.
							self signalFinalization: oop]]]! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'dtl 4/22/2007 21:19'!
firstAccessibleObject
	"Return the first accessible object in the heap."
	| obj |
	obj := self firstObject.
	[self oop: obj isLessThan: endOfMemory]
		whileTrue: [(self isFreeObject: obj) ifFalse: [^ obj].
			obj := self objectAfter: obj].
	self error: 'heap is empty'! !

!ObjectMemory methodsFor: 'object enumeration'!
firstObject
	"Return the first object or free chunk in the heap."

	^ self oopFromChunk: self startOfMemory! !

!ObjectMemory methodsFor: 'header access' stamp: 'di 10/6/2004 10:26'!
formatOf: oop
"       0      no fields
        1      fixed fields only (all containing pointers)
        2      indexable fields only (all containing pointers)
        3      both fixed and indexable fields (all containing pointers)
        4      both fixed and indexable weak fields (all containing pointers).

        5      unused
        6      indexable word fields only (no pointers)
        7      indexable long (64-bit) fields (only in 64-bit images)
 
    8-11      indexable byte fields only (no pointers) (low 2 bits are low 2 bits of size)
   12-15     compiled methods:
                   # of literal oops specified in method header,
                   followed by indexable bytes (same interpretation of low 2 bits as above)
"

	^ ((self baseHeader: oop) >> 8) bitAnd: 16rF! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'tpr 3/24/2004 22:02'!
fullCompaction
	"Move all accessible objects down to leave one big free chunk 
	at the end of memory."
	"Assume: Incremental GC has just been done to maximimize 
	forwarding table space."
	"need not move objects below the first free chunk"
	| sz |
	compStart := self lowestFreeAfter: self startOfMemory.
	compStart = freeBlock
		ifTrue: ["memory is already compact; only free chunk is at the end "
			^ self initializeMemoryFirstFree: freeBlock].
	(sz := self fwdTableSize: 8) < totalObjectCount
		ifTrue: ["Try to grow OM to make a single pass full GC"
			self growObjectMemory: totalObjectCount - sz + 10000 * 8].
	"work up through memory until all free space is at the end"
	[compStart < freeBlock]
		whileTrue: ["free chunk returned by incCompBody becomes start of next compaction"
			compStart := self incCompBody]! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'JMM 4/22/2005 10:03'!
fullGC
	"Do a mark/sweep garbage collection of the entire object memory. Free inaccessible objects but do not move them."

	| startTime |
	self inline: false.
	DoAssertionChecks ifTrue: [self reverseDisplayFrom: 0 to: 7].
	self preGCAction: true.
	startTime := self ioMicroMSecs.
	statSweepCount := statMarkCount := statMkFwdCount := statCompMoveCount := 0.
	self clearRootsTable.
	youngStart := self startOfMemory.  "process all of memory"
	self markPhase.
	"Sweep phase returns the number of survivors.
	Use the up-to-date version instead the one from startup."
	totalObjectCount := self sweepPhase.
	self fullCompaction.
	allocationCount := 0.
	statFullGCs := statFullGCs + 1.
	statGCTime := self ioMicroMSecs.
	statFullGCMSecs := statFullGCMSecs + (statGCTime - startTime).
	self capturePendingFinalizationSignals.

	youngStart := freeBlock.  "reset the young object boundary"
	self postGCAction.
	DoAssertionChecks ifTrue: [self reverseDisplayFrom: 0 to: 7].
! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 1/12/1999 14:09'!
fwdBlockGet: blkSize
	"Return the address of a two- or four-word forwarding block or nil if no more entries are available."

	fwdTableNext := fwdTableNext + blkSize.
	fwdTableNext <= fwdTableLast
		ifTrue: [ ^ fwdTableNext ]
		ifFalse: [ ^ nil ].  "no more forwarding blocks available"! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 20:36'!
fwdBlockValidate: addr 
	"Raise an error if the given address is not a valid forward table entry."
	((self oop: addr isGreaterThan: endOfMemory)
			and: [(self oop: addr isLessThanOrEqualTo: fwdTableNext)
					and: [(addr bitAnd: 3) = 0]])
		ifFalse: [self error: 'invalid fwd table entry']! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 15:29'!
fwdTableInit: blkSize
	"Set the limits for a table of two- or three-word forwarding blocks above the last used oop. The pointer fwdTableNext moves up to fwdTableLast. Used for compaction of memory and become-ing objects. Returns the number of forwarding blocks available."

	| |
	self inline: false.
	"set endOfMemory to just after a minimum-sized free block"
	self setSizeOfFree: freeBlock to: BaseHeaderSize.
	endOfMemory := freeBlock + BaseHeaderSize.

	"make a fake free chunk at endOfMemory for use as a sentinal in memory scans"
	self setSizeOfFree: endOfMemory to: BaseHeaderSize.

	"use all memory free between freeBlock and memoryLimit for forwarding table"
	"Note: Forward blocks must be quadword aligned."
	fwdTableNext := (endOfMemory + BaseHeaderSize + 7) bitAnd: WordMask-7.
	self flag: #Dan.  "Above line does not do what it says (quadword is 16 or 32 bytes)"

	fwdTableLast := memoryLimit - blkSize.  "last forwarding table entry"

	"return the number of forwarding blocks available"
	^ (fwdTableLast - fwdTableNext) // blkSize  "round down"! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 15:30'!
fwdTableSize: blkSize
	"Estimate the number of forwarding blocks available for compaction"
	| eom fwdFirst fwdLast |
	self inline: false.

	eom := freeBlock + BaseHeaderSize.
	"use all memory free between freeBlock and memoryLimit for forwarding table"

	"Note: Forward blocks must be quadword aligned."
	fwdFirst := (eom + BaseHeaderSize + 7) bitAnd: WordMask-7.
	self flag: #Dan.  "Above line does not do what it says (quadword is 16 or 32 bytes)"

	fwdLast := memoryLimit - blkSize.  "last forwarding table entry"

	"return the number of forwarding blocks available"
	^ (fwdLast - fwdFirst) // blkSize  "round down"! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 10/18/2009 12:50'!
growObjectMemory: delta 
	"Attempt to grow the object memory by the given delta 
	amount "
	| limit |
	self var: #delta type: 'usqInt'.
	self var: #limit type: 'usqInt'.
	(self isExcessiveAllocationRequest: delta shift: 0) ifFalse: [
		statGrowMemory := statGrowMemory + 1.
		limit := self sqGrowMemory: memoryLimit By: delta.
		limit = memoryLimit
			ifFalse: [memoryLimit := limit - 24.
				"remove a tad for safety"
				self initializeMemoryFirstFree: freeBlock]]! !

!ObjectMemory methodsFor: 'header access'!
hashBitsOf: oop

	^ ((self baseHeader: oop) >> 17) bitAnd: 16rFFF! !

!ObjectMemory methodsFor: 'header access'!
headerType: oop

	^ (self longAt: oop) bitAnd: TypeMask! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 14:55'!
incCompBody
	"Move objects to consolidate free space into one big chunk. Return the newly created free chunk."

	| bytesFreed |
	self inline: false.
	"reserve memory for forwarding table"
	self fwdTableInit: BytesPerWord*2.  "Two-word blocks"

	"assign new oop locations, reverse their headers, and initialize forwarding blocks"
	bytesFreed := self incCompMakeFwd.

	"update pointers to point at new oops"
	self mapPointersInObjectsFrom: youngStart to: endOfMemory.

	"move the objects and restore their original headers; return the new free chunk"
	^ self incCompMove: bytesFreed! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 21:21'!
incCompMakeFwd
	"Create and initialize forwarding blocks for all non-free objects  
	following compStart. If the supply of forwarding blocks is exhausted,  
	set compEnd to the first chunk above the area to be 
	compacted; otherwise, set it to endOfMemory. Return the number of 
	bytes to be freed."
	| bytesFreed oop fwdBlock newOop |
	self inline: false.
	bytesFreed := 0.
	oop := self oopFromChunk: compStart.
	[self oop: oop isLessThan: endOfMemory]
		whileTrue: [
				statMkFwdCount := statMkFwdCount + 1.
				(self isFreeObject: oop)
				ifTrue: [bytesFreed := bytesFreed + (self sizeOfFree: oop)]
				ifFalse: ["create a forwarding block for oop"
					fwdBlock := self fwdBlockGet: BytesPerWord*2.
					"Two-word block"
					fwdBlock = nil
						ifTrue: ["stop; we have used all available forwarding blocks"
							compEnd := self chunkFromOop: oop.
							^ bytesFreed].
					newOop := oop - bytesFreed.
					self initForwardBlock: fwdBlock mapping: oop to: newOop withBackPtr: false].
			oop := self objectAfterWhileForwarding: oop].
	compEnd := endOfMemory.
	^ bytesFreed! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'JMM 6/9/2007 12:06'!
incCompMove: bytesFreed 
	"Move all non-free objects between compStart and compEnd to their new  
	locations, restoring their headers in the process. Create a new free  
	block at the end of memory. Return the newly created free chunk. "
	"Note: The free block used by the allocator always must be the last free  
	block in memory. It may take several compaction passes to make all  
	free space bubble up to the end of memory."
	| oop next fwdBlock newOop header bytesToMove firstWord lastWord newFreeChunk sz target |
	self inline: false.
	self var: #firstWord type: 'usqInt'.
	self var: #lastWord type: 'usqInt'.
	self var: #w type: 'usqInt'.
	newOop := nil.
	oop := self oopFromChunk: compStart.
	[self oop: oop isLessThan: compEnd]
		whileTrue: [statCompMoveCount := statCompMoveCount + 1.
			next := self objectAfterWhileForwarding: oop.
			(self isFreeObject: oop)
				ifFalse: ["a moving object; unwind its forwarding block"
					fwdBlock := ((self longAt: oop) bitAnd: AllButMarkBitAndTypeMask) << 1.
					DoAssertionChecks
						ifTrue: [self fwdBlockValidate: fwdBlock].
					newOop := self longAt: fwdBlock.
					header := self longAt: fwdBlock + BytesPerWord.
					self longAt: oop put: header. "restore the original header"
					bytesToMove := oop - newOop. "move the oop (including any extra header words) "
					sz := self sizeBitsOf: oop.
					firstWord := oop - (self extraHeaderBytes: oop).
					lastWord := oop + sz - BaseHeaderSize.
					target := firstWord - bytesToMove.
					firstWord to: lastWord by: BytesPerWord
						do: [:w | 
							self longAt: target put: (self longAt: w).
							target := target + BytesPerWord]].
			oop := next].
	newOop = nil
		ifTrue: ["no objects moved"
			oop := self oopFromChunk: compStart.
			((self isFreeObject: oop) and: [(self objectAfter: oop) = (self oopFromChunk: compEnd)])
				ifTrue: [newFreeChunk := oop]
				ifFalse: [newFreeChunk := freeBlock]]
		ifFalse: ["initialize the newly freed memory chunk"
			"newOop is the last object moved; free chunk starts 
			right after it"
			newFreeChunk := newOop + (self sizeBitsOf: newOop).
			self setSizeOfFree: newFreeChunk to: bytesFreed].
	DoAssertionChecks
		ifTrue: [(self objectAfter: newFreeChunk) = (self oopFromChunk: compEnd)
				ifFalse: [self error: 'problem creating free chunk after compaction']].
	(self objectAfter: newFreeChunk) = endOfMemory
		ifTrue: [self initializeMemoryFirstFree: newFreeChunk]
		ifFalse: ["newFreeChunk is not at end of memory; re-install freeBlock "
			self initializeMemoryFirstFree: freeBlock].
	^ newFreeChunk! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'tpr 3/24/2004 22:03'!
incrementalCompaction
	"Move objects down to make one big free chunk. Compact the 
	last N objects (where N = number of forwarding table 
	entries) of the young object area."
	"Assume: compStart was set during the sweep phase"
	compStart = freeBlock
		ifTrue: ["Note: If compStart = freeBlock then either the young 
			space is already compact  or there are enough forwarding table entries to do a 
			one-pass incr. compaction."
			self initializeMemoryFirstFree: freeBlock]
		ifFalse: [self incCompBody]! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 10/17/2009 11:09'!
incrementalGC
	"Do a mark/sweep garbage collection of just the young object 
	area of object memory (i.e., objects above youngStart), using 
	the root table to identify objects containing pointers to 
	young objects from the old object area."
	| survivorCount startTime weDidGrow |
	self inline: false.
	rootTableCount >= RootTableSize
		ifTrue: ["root table overflow; cannot do an incremental GC (this should be very rare)"
			statRootTableOverflows := statRootTableOverflows + 1.
			^ self fullGC].
	DoAssertionChecks
		ifTrue: [self reverseDisplayFrom: 8 to: 15.
			self validateRoots; validate].

	self preGCAction: false.
	"incremental GC and compaction"

	startTime := self ioMicroMSecs.
	weakRootCount := 0.
	statSweepCount := statMarkCount := statMkFwdCount := statCompMoveCount := 0.
	self markPhase.
	1 to: weakRootCount do:[:i| self finalizeReference: (weakRoots at: i)].
	survivorCount := self sweepPhase.
	self incrementalCompaction.
	statAllocationCount := allocationCount.
	allocationCount := 0.
	statIncrGCs := statIncrGCs + 1.
	statGCTime := self ioMicroMSecs.
	statIGCDeltaTime := statGCTime - startTime.
	statIncrGCMSecs := statIncrGCMSecs + statIGCDeltaTime.
	self capturePendingFinalizationSignals.

	self forceInterruptCheck. "Force an an interrupt check ASAP.We could choose to be clever here and only do this under certain time conditions. Keep it simple for now"
	
	statRootTableCount  := rootTableCount.
	statSurvivorCount := survivorCount.
	weDidGrow := false.
	(((survivorCount > tenuringThreshold)
			or: [rootTableCount >= RootTableRedZone])
			or: [forceTenureFlag == true])
		ifTrue: ["move up the young space boundary if 
			* there are too many survivors: 
			this limits the number of objects that must be 
			processed on future incremental GC's 
			* we're about to overflow the roots table 
			this limits the number of full GCs that may be caused 
			by root table overflows in the near future"
			forceTenureFlag := false.
			statTenures := statTenures + 1.
			self clearRootsTable.
			(freeBlock < growHeadroom and: 
				[gcBiasToGrow > 0]) 
				ifTrue: [self biasToGrow.
						weDidGrow := true].
			youngStart := freeBlock].
	self postGCAction.
	DoAssertionChecks
		ifTrue: [self validateRoots; validate.
			self reverseDisplayFrom: 8 to: 15].
	weDidGrow ifTrue: [self biasToGrowCheckGCLimit]! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 15:00'!
initForwardBlock: fwdBlock mapping: oop to: newOop withBackPtr: backFlag 
	"Initialize the given forwarding block to map oop to newOop, 
	and replace oop's header with a pointer to the fowarding 
	block. "
	"Details: The mark bit is used to indicate that an oop is 
	forwarded. When an oop is forwarded, its header (minus the 
	mark bit) contains the address of its forwarding block. (The 
	forwarding block address is actually shifted right by one bit 
	so that its top-most bit does not conflict with the header's 
	mark bit; since fowarding blocks are stored on word 
	boundaries, the low two bits of the address are always zero.) 
	The first word of the forwarding block is the new oop; the 
	second word is the oop's orginal header. In the case of a 
	forward become, a four-word block is used, with the third 
	field being a backpointer to the old oop (for header fixup), 
	and the fourth word is unused. The type bits of the 
	forwarding header are the same as those of the original 
	header. "
	| originalHeader originalHeaderType |
	self inline: true.
	originalHeader := self longAt: oop.
	DoAssertionChecks
		ifTrue: [fwdBlock = nil ifTrue: [self error: 'ran out of forwarding blocks in become'].
			(originalHeader bitAnd: MarkBit) ~= 0
				ifTrue: [self error: 'object already has a forwarding table entry']].
	originalHeaderType := originalHeader bitAnd: TypeMask.
	self longAt: fwdBlock put: newOop.
	self longAt: fwdBlock + BytesPerWord put: originalHeader.
	backFlag ifTrue: [self longAt: fwdBlock + (BytesPerWord*2) put: oop].
	self longAt: oop put: (fwdBlock >> 1 bitOr: (MarkBit bitOr: originalHeaderType))! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'tpr 3/24/2004 21:54'!
initialInstanceOf: classPointer 
	"Support for instance enumeration. Return the first instance 
	of the given class, or nilObj if it has no instances."
	| thisObj thisClass |
	thisObj := self firstAccessibleObject.
	[thisObj = nil]
		whileFalse: [thisClass := self fetchClassOf: thisObj.
			thisClass = classPointer ifTrue: [^ thisObj].
			thisObj := self accessibleObjectAfter: thisObj].
	^ nilObj! !

!ObjectMemory methodsFor: 'initialization' stamp: 'dtl 10/11/2009 19:59'!
initializeMemoryFirstFree: firstFree 
	"Initialize endOfMemory to the top of oop storage space, reserving some space for forwarding blocks, and create the freeBlock from which space is allocated. Also create a fake free chunk at endOfMemory to act as a sentinal for memory scans. "
	"Note: The amount of space reserved for forwarding blocks should be chosen to ensure that incremental compactions can usually be done in a single pass. However, there should be enough forwarding blocks so a full compaction can be done in a reasonable number of passes, say ten. (A full compaction requires N object-moving passes, where N = number of non-garbage objects / number of forwarding blocks). 
	di 11/18/2000 Re totalObjectCount: Provide a margin of one byte per object to be used for forwarding pointers at GC time. Since fwd blocks are 8 bytes, this means an absolute worst case of 8 passes to compact memory. In most cases it will be adequate to do compaction in a single pass. "
	| fwdBlockBytes |
	self var: #firstFree type: 'usqInt'.
	self var: #fwdBlockBytes type: 'usqInt'.
	"reserve space for forwarding blocks"
	fwdBlockBytes := totalObjectCount bitAnd: WordMask - BytesPerWord + 1.
	(self oop: memoryLimit - fwdBlockBytes isGreaterThanOrEqualTo: firstFree + BaseHeaderSize)
		ifFalse: ["reserve enough space for a minimal free block of BaseHeaderSize bytes"
			fwdBlockBytes := memoryLimit - (firstFree + BaseHeaderSize)].

	"set endOfMemory and initialize freeBlock"
	endOfMemory := memoryLimit - fwdBlockBytes.
	freeBlock := firstFree.
	self setSizeOfFree: freeBlock to: endOfMemory - firstFree. "bytes available for oops"

	"make a fake free chunk at endOfMemory for use as a sentinel in memory scans"
	self setSizeOfFree: endOfMemory to: BaseHeaderSize.
	DoAssertionChecks
		ifTrue: [(freeBlock < endOfMemory and: [endOfMemory < memoryLimit])
				ifFalse: [self error: 'error in free space computation'].
			(self oopFromChunk: endOfMemory) = endOfMemory
				ifFalse: [self error: 'header format must have changed'].
			(self objectAfter: freeBlock) = endOfMemory
				ifFalse: [self error: 'free block not properly initialized']]! !

!ObjectMemory methodsFor: 'initialization' stamp: 'ar 6/8/2006 12:34'!
initializeObjectMemory: bytesToShift
	"Initialize object memory variables at startup time. Assume endOfMemory is initially set (by the image-reading code) to the end of the last object in the image. Initialization redefines endOfMemory to be the end of the object allocation area based on the total available memory, but reserving some space for forwarding blocks."
	"Assume: image reader initializes the following variables:
		memory
		endOfMemory
		memoryLimit
		specialObjectsOop
		lastHash
	"
	"di 11/18/2000 fix slow full GC"
	self inline: false.

	"set the start of the young object space"
	youngStart := endOfMemory.

	"image may be at a different address; adjust oops for new location"
	totalObjectCount := self adjustAllOopsBy: bytesToShift.

	self initializeMemoryFirstFree: endOfMemory. "initializes endOfMemory, freeBlock"

	specialObjectsOop := specialObjectsOop + bytesToShift.

	"heavily used special objects"
	nilObj	:= self splObj: NilObject.
	falseObj	:= self splObj: FalseObject.
	trueObj	:= self splObj: TrueObject.

	rootTableCount := 0.
	freeContexts := NilContext.
	freeLargeContexts := NilContext.
	allocationCount := 0.
	lowSpaceThreshold := 0.
	signalLowSpace := false.
	compStart := 0.
	compEnd := 0.
	fwdTableNext := 0.
	fwdTableLast := 0.
	remapBufferCount := 0.
	allocationsBetweenGCs := 4000.  "do incremental GC after this many allocations"
	tenuringThreshold := 2000.  "tenure all suriving objects if count is over this threshold"
	growHeadroom := 4*1024*1024. "four megabyte of headroom when growing"
	shrinkThreshold := 8*1024*1024. "eight megabyte of free space before shrinking"

	"garbage collection statistics"
	statFullGCs := 0.
	statFullGCMSecs := 0.
	statIncrGCs := 0.
	statIncrGCMSecs := 0.
	statTenures := 0.
	statRootTableOverflows := 0.
	statGrowMemory := 0.
	statShrinkMemory := 0.
	forceTenureFlag := 0.
	gcBiasToGrow := 0.
	gcBiasToGrowGCLimit := 0.
	extraRootCount := 0.
! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'tpr 3/24/2004 21:54'!
instanceAfter: objectPointer 
	"Support for instance enumeration. Return the next instance 
	of the class of the given object, or nilObj if the enumeration 
	is complete."
	| classPointer thisObj thisClass |
	classPointer := self fetchClassOf: objectPointer.
	thisObj := self accessibleObjectAfter: objectPointer.
	[thisObj = nil]
		whileFalse: [thisClass := self fetchClassOf: thisObj.
			thisClass = classPointer ifTrue: [^ thisObj].
			thisObj := self accessibleObjectAfter: thisObj].
	^ nilObj! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ikp 3/27/2005 18:07'!
instantiateClass: classPointer indexableSize: size 
	"NOTE: This method supports the backward-compatible split instSize field of the 
	class format word. The sizeHiBits will go away and other shifts change by 2 
	when the split fields get merged in an (incompatible) image change."

	| hash header1 header2 cClass byteSize format binc header3 hdrSize fillWord newObj sizeHiBits bm1 classFormat |
	self inline: false.
	DoAssertionChecks ifTrue: [size < 0
				ifTrue: [self error: 'cannot have a negative indexable field count']].
	hash := self newObjectHash.
	classFormat := self formatOfClass: classPointer.
	"Low 2 bits are 0"
	header1 := (classFormat bitAnd: 16r1FF00) bitOr: (hash << HashBitsOffset bitAnd: HashBits).
	header2 := classPointer.
	header3 := 0.
	sizeHiBits := (classFormat bitAnd: 16r60000) >> 9.
	cClass := header1 bitAnd: CompactClassMask. "compact class field from format word"
	byteSize := (classFormat bitAnd: SizeMask + Size4Bit) + sizeHiBits.
		"size in bytes -- low 2 bits are 0"
	"Note this byteSize comes from the format word of the class which is pre-shifted
		to 4 bytes per field.  Need another shift for 8 bytes per word..."
	byteSize := byteSize << (ShiftForWord-2).
	format := classFormat >> 8 bitAnd: 15.
	self flag: #sizeLowBits.
	format < 8
		ifTrue:
			[format = 6
				ifTrue: ["long32 bitmaps"
					bm1 := BytesPerWord-1.
					byteSize := byteSize + (size * 4) + bm1 bitAnd: LongSizeMask. "round up"
					binc := bm1 - ((size * 4) + bm1 bitAnd: bm1). "odd bytes"
					"extra low bit (4) for 64-bit VM goes in 4-bit (betw hdr bits and sizeBits)"
					header1 := header1 bitOr: (binc bitAnd: 4)]
				ifFalse: [byteSize := byteSize + (size * BytesPerWord) "Arrays and 64-bit bitmaps"]
			]
		ifFalse:
			["Strings and Methods"
			bm1 := BytesPerWord-1.
			byteSize := byteSize + size + bm1 bitAnd: LongSizeMask. "round up"
			binc := bm1 - (size + bm1 bitAnd: bm1). "odd bytes"
			"low bits of byte size go in format field"
			header1 := header1 bitOr: (binc bitAnd: 3) << 8.
			"extra low bit (4) for 64-bit VM goes in 4-bit (betw hdr bits and sizeBits)"
			header1 := header1 bitOr: (binc bitAnd: 4)].
	byteSize > 255
		ifTrue: ["requires size header word"
			header3 := byteSize.
			header1 := header1]
		ifFalse: [header1 := header1 bitOr: byteSize].
	header3 > 0
		ifTrue: ["requires full header"
			hdrSize := 3]
		ifFalse: [cClass = 0
				ifTrue: [hdrSize := 2]
				ifFalse: [hdrSize := 1]].
	format <= 4
		ifTrue: ["if pointers, fill with nil oop"
			fillWord := nilObj]
		ifFalse: [fillWord := 0].
	newObj := self allocate: byteSize headerSize: hdrSize h1: header1 h2: header2 h3: header3 doFill: true with: fillWord.
	^ newObj! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 7/7/2004 16:46'!
instantiateContext: classPointer sizeInBytes: sizeInBytes 
	"This version of instantiateClass assumes that the total object 
	size is under 256 bytes, the limit for objects with only one or 
	two header words. Note that the size is specified in bytes 
	and should include four bytes for the base header word."
	| hash header1 header2 hdrSize |
	hash := self newObjectHash.
	header1 := (hash << HashBitsOffset bitAnd: HashBits) bitOr: (self formatOfClass: classPointer).
	header2 := classPointer.
	(header1 bitAnd: CompactClassMask) > 0 "are contexts compact?"
		ifTrue: [hdrSize := 1]
		ifFalse: [hdrSize := 2].
	sizeInBytes <= SizeMask
		ifTrue: ["OR size into header1.  Must not do this if size > SizeMask"
				header1 := header1 + (sizeInBytes - (header1 bitAnd: SizeMask))]
		ifFalse: [hdrSize := 3.
				"Zero the size field of header1 if large"
				header1 := header1 - (header1 bitAnd: SizeMask)].
self flag: #Dan.  "Check details of context sizes"
	^ self allocate: sizeInBytes headerSize: hdrSize h1: header1 h2: header2 h3: LargeContextSize doFill: false with: 0! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 5/13/2005 10:31'!
instantiateSmallClass: classPointer sizeInBytes: sizeInBytes
	"This version of instantiateClass assumes that the total object 
	size is under 256 bytes, the limit for objects with only one or 
	two header words. Note that the size is specified in bytes 
	and should include 4 or 8 bytes for the base header word. 
	NOTE this code will only work for sizes that are an integral number of words
		(like not a 32-bit LargeInteger in a 64-bit system). 
	May cause a GC.
	Note that the created small object IS NOT FILLED and must be completed before returning it to Squeak. Since this call is used in routines that do jsut that we are safe. Break this rule and die."

	| hash header1 header2 hdrSize |
	(sizeInBytes bitAnd: (BytesPerWord-1)) = 0 ifFalse:
		[self error: 'size must be integral number of words'].
	hash := self newObjectHash.
	header1 := (hash << HashBitsOffset bitAnd: HashBits) bitOr: (self formatOfClass: classPointer).
	header2 := classPointer.
	(header1 bitAnd: CompactClassMask) > 0 "is this a compact class"
		ifTrue: [hdrSize := 1]
		ifFalse: [hdrSize := 2].
	header1 := header1 + (sizeInBytes - (header1 bitAnd: SizeMask+Size4Bit)).
	^ self allocate: sizeInBytes headerSize: hdrSize h1: header1 h2: header2 h3: 0 doFill: false with: 0! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ikp 8/4/2004 15:32'!
integerObjectOf: value

	^(value << 1) + 1! !

!ObjectMemory methodsFor: 'interpreter access'!
integerValueOf: objectPointer
	"Translator produces 'objectPointer >> 1'"

	((objectPointer bitAnd: 16r80000000) ~= 0)
		ifTrue: ["negative"
				^ ((objectPointer bitAnd: 16r7FFFFFFF) >> 1)
					- 16r3FFFFFFF - 1  "Faster than -16r40000000 (a LgInt)"]
		ifFalse: ["positive"
				^ objectPointer >> 1]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 12/5/2003 20:01'!
isArray: oop
	"Answer true if this is an indexable object with pointer elements, e.g., an array"
	^(self isNonIntegerObject: oop) and:[self isArrayNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 12/5/2003 20:02'!
isArrayNonInt: oop
	"Answer true if this is an indexable object with pointer elements, e.g., an array"
	^ (self formatOf: oop) = 2! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:09'!
isBytes: oop
	"Answer true if the argument contains indexable bytes. See comment in formatOf:"
	"Note: Includes CompiledMethods."
	^(self isNonIntegerObject: oop) and:[self isBytesNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:05'!
isBytesNonInt: oop
	"Answer true if the argument contains indexable bytes. See comment in formatOf:"
	"Note: Includes CompiledMethods."

	^ (self formatOf: oop)  >= 8! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'mga 2/20/2004 17:04'!
isCompiledMethod: oop
    "Answer whether the receiver is of compiled method format"
    ^(self formatOf: oop) >= 12
! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 10/20/2009 21:19'!
isExcessiveAllocationRequest: size shift: bits

	"Answer true if size is greater than (((2 raisedTo: 31) - 1) >> bits). Used
	to limit size of allocation requests to 31 bit integer maximum to prevent
	arithmetic overflow in subsequent calculations. Always answers false
	in interpreter simulation.

	Assumes that sizeof(int) is 4 for all platforms."

	| int32 shiftCount |
	self inline: true.
	self var: #int32 type: 'int'. "32 bit signed integer"
	int32 := size.
	shiftCount := 0.
	int32 < 0 ifTrue: [^ true].
	[shiftCount < bits]
		whileTrue: [int32 := int32 << 1.
					int32 < 0 ifTrue: [^ true].
					shiftCount := shiftCount +1].
	^ false
! !

!ObjectMemory methodsFor: 'header access'!
isFreeObject: oop

	^ (self headerType: oop) = HeaderTypeFree! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'dtl 4/22/2007 20:06'!
isInMemory: address 
	"Return true if the given address is in ST object memory"
	^ (self oop: address isGreaterThanOrEqualTo: self startOfMemory)
		and: [self oop: address isLessThan: endOfMemory]! !

!ObjectMemory methodsFor: 'interpreter access'!
isIntegerObject: objectPointer

	^ (objectPointer bitAnd: 1) > 0! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'dtl 11/13/2006 12:55'!
isIntegerValue: intValue
	"Return true if the given value can be represented as a Smalltalk integer value."
	"Use a shift and XOR to set the sign bit if and only if the top two bits of the given
	value are the same, then test the sign bit. Note that the top two bits are equal for
	exactly those integers in the range that can be represented in 31-bits or 63-bits.

	Operands are coerced to machine integer size so the test will work with 64 bit
	images on 32 bit hosts. When running on a 32 bit host, the cast to int has little
	or no performance impact for either 32 bit or 64 bit images.

	On a 64 bit host, the shift and XOR test is replaced by an explicit range check,
	which provides the best performance for both 32 bit and 64 bit images.

	If the range of small integers is enlarged for 64 bit images, this method must
	be updated accordingly."

	^ self isDefined: 'SQ_HOST32'
		inSmalltalk: [true]
		comment: 'cast to int for 64 bit image on 32 bit host'
		ifTrue: ((self cCoerce: intValue to: 'int')
					bitXor: ((self cCoerce: intValue to: 'int') << 1)) >= 0
		ifFalse: (intValue >= 16r-40000000 and: [intValue <= 16r3FFFFFFF])! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ar 11/16/2003 01:07'!
isNonIntegerObject: objectPointer

	^ (objectPointer bitAnd: 1) = 0! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'tpr 3/24/2004 10:46'!
isObjectForwarded: oop 
	"Return true if the given object has a forwarding table entry 
	during a compaction or become operation."
	^ (oop bitAnd: 1) = 0 and: ["(isIntegerObject: oop) not" ((self longAt: oop) bitAnd: MarkBit) ~= 0]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:08'!
isPointers: oop
	"Answer true if the argument has only fields that can hold oops. See comment in formatOf:"

	^(self isNonIntegerObject: oop) and:[self isPointersNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:05'!
isPointersNonInt: oop
	"Answer true if the argument has only fields that can hold oops. See comment in formatOf:"

	^ (self formatOf: oop) <= 4! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:09'!
isWeak: oop
	"Answer true if the argument has only weak fields that can hold oops. See comment in formatOf:"
	^(self isNonIntegerObject: oop) and:[self isWeakNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:05'!
isWeakNonInt: oop
	"Answer true if the argument has only weak fields that can hold oops. See comment in formatOf:"
	^ (self formatOf: oop) = 4! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:09'!
isWords: oop
	"Answer true if the argument contains only indexable words (no oops). See comment in formatOf:"

	^(self isNonIntegerObject: oop) and:[self isWordsNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:05'!
isWordsNonInt: oop
	"Answer true if the argument contains only indexable words (no oops). See comment in formatOf:"

	^ (self formatOf: oop) = 6! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:09'!
isWordsOrBytes: oop
	"Answer true if the contains only indexable words or bytes (no oops). See comment in formatOf:"
	"Note: Excludes CompiledMethods."
	^(self isNonIntegerObject: oop) and:[self isWordsOrBytesNonInt: oop]! !

!ObjectMemory methodsFor: 'header access' stamp: 'ar 11/16/2003 01:06'!
isWordsOrBytesNonInt: oop
	"Answer true if the contains only indexable words or bytes (no oops). See comment in formatOf:"
	"Note: Excludes CompiledMethods."

	| fmt |
	fmt := self formatOf: oop.
	^ fmt = 6 or: [(fmt >= 8) and: [fmt <= 11]]! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'di 6/11/2004 13:20'!
lastPointerOf: oop 
	"Return the byte offset of the last pointer field of the given object.  
	Works with CompiledMethods, as well as ordinary objects. 
	Can be used even when the type bits are not correct."
	| fmt sz methodHeader header contextSize |
	self inline: true.
	header := self baseHeader: oop.
	fmt := header >> 8 bitAnd: 15.
	fmt <= 4 ifTrue: [(fmt = 3 and: [self isContextHeader: header])
					ifTrue: ["contexts end at the stack pointer"
						contextSize := self fetchStackPointerOf: oop.
						^ CtxtTempFrameStart + contextSize * BytesPerWord].
				sz := self sizeBitsOfSafe: oop.
				^ sz - BaseHeaderSize  "all pointers"].
	fmt < 12 ifTrue: [^ 0]. "no pointers"

	"CompiledMethod: contains both pointers and bytes:"
	methodHeader := self longAt: oop + BaseHeaderSize.
	^ (methodHeader >> 10 bitAnd: 255) * BytesPerWord + BaseHeaderSize! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 15:04'!
lastPointerWhileForwarding: oop 
	"The given object may have its header word in a forwarding block. Find  
	the offset of the last pointer in the object in spite of this obstacle. "
	| header fwdBlock fmt size methodHeader contextSize |
	self inline: true.
	header := self longAt: oop.
	(header bitAnd: MarkBit) ~= 0
		ifTrue: ["oop is forwarded; get its real header from its forwarding table entry"
			fwdBlock := (header bitAnd: AllButMarkBitAndTypeMask) << 1.
			DoAssertionChecks
				ifTrue: [self fwdBlockValidate: fwdBlock].
			header := self longAt: fwdBlock + BytesPerWord].
	fmt := header >> 8 bitAnd: 15.
	fmt <= 4
		ifTrue: [(fmt = 3 and: [self isContextHeader: header])
				ifTrue: ["contexts end at the stack pointer"
					contextSize := self fetchStackPointerOf: oop.
					^ CtxtTempFrameStart + contextSize * BytesPerWord].
			"do sizeBitsOf: using the header we obtained"
			(header bitAnd: TypeMask) = HeaderTypeSizeAndClass
				ifTrue: [size := (self sizeHeader: oop) bitAnd: AllButTypeMask]
				ifFalse: [size := header bitAnd: SizeMask].
			^ size - BaseHeaderSize].
	fmt < 12 ifTrue: [^ 0]. "no pointers"
	methodHeader := self longAt: oop + BaseHeaderSize.
	^ (methodHeader >> 10 bitAnd: 255) * BytesPerWord + BaseHeaderSize! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 4/22/2007 21:24'!
lowestFreeAfter: chunk 
	"Return the first free block after the given chunk in memory."
	| oop oopHeader oopHeaderType oopSize |
	self inline: false.
	oop := self oopFromChunk: chunk.
	[self oop: oop isLessThan: endOfMemory]
		whileTrue: [oopHeader := self baseHeader: oop.
			oopHeaderType := oopHeader bitAnd: TypeMask.
			oopHeaderType = HeaderTypeFree
				ifTrue: [^ oop]
				ifFalse: [oopHeaderType = HeaderTypeSizeAndClass
						ifTrue: [oopSize := (self sizeHeader: oop) bitAnd: AllButTypeMask]
						ifFalse: [oopSize := oopHeader bitAnd: SizeMask]].
			oop := self oopFromChunk: oop + oopSize].
	self error: 'expected to find at least one free object'! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'ar 6/8/2006 11:55'!
mapPointersInObjectsFrom: memStart to: memEnd
	"Use the forwarding table to update the pointers of all non-free objects in the given range of memory. Also remap pointers in root objects which may contains pointers into the given memory range, and don't forget to flush the method cache based on the range"
	| oop |
	self inline: false.
	self compilerMapHookFrom: memStart to: memEnd.
	"update interpreter variables"
	self mapInterpreterOops.
	1 to: extraRootCount do:[:i |
		oop := (extraRoots at: i) at: 0.
		(self isIntegerObject: oop) ifFalse:[(extraRoots at: i) at: 0 put: (self remap: oop)]].
	self flushMethodCacheFrom: memStart to: memEnd.
	self updatePointersInRootObjectsFrom: memStart to: memEnd.
	self updatePointersInRangeFrom: memStart to: memEnd.
! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'dtl 4/22/2007 20:07'!
markAndTrace: oop
	"Mark all objects reachable from the given one.
	Trace from the given object even if it is old.
	Do not trace if it is already marked.
	Mark it only if it is a young object."
	"Tracer state variables:
		child		object being examined
		field		next field of child to examine
		parentField	field where child was stored in its referencing object"

	| header lastFieldOffset action statMarkCountLocal |
	header := self longAt: oop.
	(header bitAnd: MarkBit) = 0 ifFalse: [^ 0  "already marked"].

	"record tracing status in object's header"
	header := (header bitAnd: AllButTypeMask) bitOr: HeaderTypeGC.
	(self oop: oop isGreaterThanOrEqualTo: youngStart)
		ifTrue: [ header := header bitOr: MarkBit ].  "mark only if young"
	self longAt: oop put: header.

	"initialize the tracer state machine"
	parentField := GCTopMarker.
	child := oop.
	(self isWeakNonInt: oop) ifTrue: [
		"Set lastFieldOffset before the weak fields in the receiver"
		lastFieldOffset := (self nonWeakFieldsOf: oop) << ShiftForWord.
		"And remember as weak root"
		weakRootCount := weakRootCount + 1.
		weakRoots at: weakRootCount put: oop.
	] ifFalse: [
		"Do it the usual way"
		lastFieldOffset := self lastPointerOf: oop.
	].
	field := oop + lastFieldOffset.
	action := StartField.
	youngStartLocal := youngStart.
	statMarkCountLocal := statMarkCount.
	"run the tracer state machine until all objects reachable from oop are marked"
	[action = Done] whileFalse: [
		statMarkCountLocal := statMarkCountLocal + 1.
		action = StartField ifTrue: [ action := self startField ].
		action = StartObj ifTrue: [ action := self startObj ].
		action = Upward ifTrue: [ action := self upward ].
	].
	statMarkCount := statMarkCountLocal.! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'ar 6/9/2006 14:56'!
markPhase
	"Mark phase of the mark and sweep garbage collector. Set 
	the mark bits of all reachable objects. Free chunks are 
	untouched by this process."
	"Assume: All non-free objects are initially unmarked. Root 
	objects were unmarked when they were made roots. (Make 
	sure this stays true!!!!)."
	| oop |
	self inline: false.
	"clear the recycled context lists"
	freeContexts := NilContext.
	freeLargeContexts := NilContext.
	"trace the interpreter's objects, including the active stack 
	and special objects array"
	self markAndTraceInterpreterOops.
	statSpecialMarkCount := statMarkCount.
	"trace the roots"
	1 to: rootTableCount do: [:i | 
			oop := rootTable at: i.
			self markAndTrace: oop].
	1 to: extraRootCount do:[:i|
			oop := (extraRoots at: i) at: 0.
			(self isIntegerObject: oop) ifFalse:[self markAndTrace: oop]].
! !

!ObjectMemory methodsFor: 'header access'!
newObjectHash
	"Answer a new 16-bit pseudo-random number for use as an identity hash."

	lastHash := 13849 + (27181 * lastHash) bitAnd: 65535.
	^ lastHash
! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 3/24/2004 12:57'!
nilObject
	"For access from BitBlt module"
	^ nilObj! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'tpr 3/24/2004 22:06'!
noteAsRoot: oop headerLoc: headerLoc 
	"Record that the given oop in the old object area points to an 
	object in the young area. 
	HeaderLoc is usually = oop, but may be an addr in a 
	forwarding block."
	| header |
	self inline: true.
	header := self longAt: headerLoc.
	(header bitAnd: RootBit) = 0
		ifTrue: ["record oop as root only if not already recorded"
			rootTableCount < RootTableRedZone
				ifTrue: ["record root if there is enough room in the roots 
					table "
					rootTableCount := rootTableCount + 1.
					rootTable at: rootTableCount put: oop.
					self longAt: headerLoc put: (header bitOr: RootBit)]
				ifFalse: ["we're getting in the red zone"
					rootTableCount < RootTableSize
						ifTrue: ["but there's still space to record it"
							rootTableCount := rootTableCount + 1.
							rootTable at: rootTableCount put: oop.
							self longAt: headerLoc put: (header bitOr: RootBit).
							"but force an IGC on the next allocation"
							allocationCount := allocationsBetweenGCs + 1]]]! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'dtl 4/22/2007 20:09'!
objectAfter: oop 
	"Return the object or free chunk immediately following the 
	given object or free chunk in memory. Return endOfMemory 
	when enumeration is complete."
	| sz |
	self inline: true.
	DoAssertionChecks
		ifTrue: [(self oop: oop isGreaterThanOrEqualTo:endOfMemory)
					ifTrue: [self error: 'no objects after the end of memory']].
	(self isFreeObject: oop)
		ifTrue: [sz := self sizeOfFree: oop]
		ifFalse: [sz := self sizeBitsOf: oop].
	^ self oopFromChunk: oop + sz! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'di 7/1/2004 15:35'!
objectAfterWhileForwarding: oop
	"Return the oop of the object after the given oop when the actual header of the oop may be in the forwarding table."

	| header fwdBlock realHeader sz |
	self inline: true.
	header := self longAt: oop.
	(header bitAnd: MarkBit) = 0 ifTrue: [ ^ self objectAfter: oop ].  "oop not forwarded"

	"Assume: mark bit cannot be set on a free chunk, so if we get here,
	 oop is not free and it has a forwarding table entry"

	fwdBlock := (header bitAnd: AllButMarkBitAndTypeMask) << 1.
	DoAssertionChecks ifTrue: [ self fwdBlockValidate: fwdBlock ].
	realHeader := self longAt: fwdBlock + BytesPerWord.
	"following code is like sizeBitsOf:"
	(realHeader bitAnd: TypeMask) = HeaderTypeSizeAndClass
		ifTrue: [ sz := (self sizeHeader: oop) bitAnd: LongSizeMask ]
		ifFalse: [ sz := realHeader bitAnd: SizeMask ].

	^ self oopFromChunk: (oop + sz)! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'tpr 6/7/2005 10:13'!
obsoleteDontUseThisFetchWord: fieldIndex ofObject: oop
	"This message is deprecated but supported for a while via a tweak to sqVirtualMachine.[ch] Use fetchLong32, fetchLong64 or fetchPointer instead for new code"

	^self fetchLong32: fieldIndex ofObject: oop! !

!ObjectMemory methodsFor: 'oop comparison' stamp: 'dtl 4/22/2007 16:01'!
oop: anOop isGreaterThan: otherOop
	"Compare two oop values, treating them as object memory locations.
	Use #cCoerce:to: to ensure comparison of unsigned magnitudes. This
	method will be inlined during C translation."

	^ (self cCoerce: anOop to: #usqInt)
		> (self cCoerce: otherOop to: #usqInt)! !

!ObjectMemory methodsFor: 'oop comparison' stamp: 'dtl 4/22/2007 18:15'!
oop: anOop isGreaterThanOrEqualTo: otherOop
	"Compare two oop values, treating them as object memory locations.
	Use #cCoerce:to: to ensure comparison of unsigned magnitudes. This
	method will be inlined during C translation."

	^ (self cCoerce: anOop to: #usqInt)
		>= (self cCoerce: otherOop to: #usqInt)! !

!ObjectMemory methodsFor: 'oop comparison' stamp: 'dtl 4/22/2007 16:01'!
oop: anOop isLessThan: otherOop
	"Compare two oop values, treating them as object memory locations.
	Use #cCoerce:to: to ensure comparison of unsigned magnitudes. This
	method will be inlined during C translation."

	^ (self cCoerce: anOop to: #usqInt)
		< (self cCoerce: otherOop to: #usqInt)! !

!ObjectMemory methodsFor: 'oop comparison' stamp: 'dtl 4/22/2007 18:15'!
oop: anOop isLessThanOrEqualTo: otherOop
	"Compare two oop values, treating them as object memory locations.
	Use #cCoerce:to: to ensure comparison of unsigned magnitudes. This
	method will be inlined during C translation."

	^ (self cCoerce: anOop to: #usqInt)
		<= (self cCoerce: otherOop to: #usqInt)! !

!ObjectMemory methodsFor: 'oop/chunk conversion' stamp: 'JMM 12/4/2002 19:56'!
oopFromChunk: chunk
	"Compute the oop of this chunk by adding its extra header bytes."

	^ chunk + (self extraHeaderBytes: chunk)! !

!ObjectMemory methodsFor: 'interpreter access'!
popRemappableOop
	"Pop and return the possibly remapped object from the remap buffer."

	| oop |
	oop := remapBuffer at: remapBufferCount.
	remapBufferCount := remapBufferCount - 1.
	^ oop! !

!ObjectMemory methodsFor: 'garbage collection' stamp: 'dtl 4/22/2007 20:11'!
possibleRootStoreInto: oop value: valueObj 
	"oop is an old object. If valueObj is young, mark the object as a root."

	self inline: false.
	((self oop: valueObj isGreaterThanOrEqualTo: youngStart)
			and: [(self isIntegerObject: valueObj) not])
		ifTrue: ["Yes, valueObj is a young object"
				self noteAsRoot: oop headerLoc: oop]! !

!ObjectMemory methodsFor: 'become' stamp: 'tpr 3/23/2005 12:05'!
prepareForwardingTableForBecoming: array1 with: array2 twoWay: twoWayFlag 
	"Ensure that there are enough forwarding blocks to 
	accomodate this become, then prepare forwarding blocks for 
	the pointer swap. Return true if successful."
	"Details: Doing a GC might generate enough space for 
	forwarding blocks if we're short. However, this is an 
	uncommon enough case that it is better handled by primitive 
	fail code at the Smalltalk level."

	"Important note on multiple references to same object  - since the preparation of
	fwdBlocks is NOT idempotent we get VM crashes if the same object is referenced more
	than once in such a way as to require multiple fwdBlocks.
	oop1 forwardBecome: oop1 is ok since only a single fwdBlock is needed.
	oop1 become: oop1 would fail because the second fwdBlock woudl not have the actual object
	header but rather the mutated ref to the first fwdBlock.
	Further problems can arise with an array1 or array2 that refer multiply to the same 
	object. This would notbe expected input for programmer writen code but might arise from
	automatic usage such as in ImageSegment loading.
	To avoid the simple and rather common case of oop1 become*: oop1, we skip such pairs
	and simply avoid making fwdBlocks - it is redundant anyway"
	| entriesNeeded entriesAvailable fieldOffset oop1 oop2 fwdBlock fwdBlkSize |
	entriesNeeded := (self lastPointerOf: array1) // BytesPerWord. "need enough entries for all oops"
	"Note: Forward blocks must be quadword aligned - see fwdTableInit:."
	twoWayFlag
		ifTrue: ["Double the number of blocks for two-way become"
			entriesNeeded := entriesNeeded * 2.
			fwdBlkSize := BytesPerWord * 2]
		ifFalse: ["One-way become needs backPointers in fwd blocks."
			fwdBlkSize := BytesPerWord * 4].
	entriesAvailable := self fwdTableInit: fwdBlkSize.
	entriesAvailable < entriesNeeded
		ifTrue: [self initializeMemoryFirstFree: freeBlock.
			"re-initialize the free block"
			^ false].
	fieldOffset := self lastPointerOf: array1.
	[fieldOffset >= BaseHeaderSize]
		whileTrue: [oop1 := self longAt: array1 + fieldOffset.
			oop2 := self longAt: array2 + fieldOffset.
			"if oop1 == oop2, no need to do any work for this pair.
			May still be other entries in the arrays though so keep looking"
			oop1 = oop2
				ifFalse: [fwdBlock := self fwdBlockGet: fwdBlkSize.
					self
						initForwardBlock: fwdBlock
						mapping: oop1
						to: oop2
						withBackPtr: twoWayFlag not.
					twoWayFlag
						ifTrue: ["Second block maps oop2 back to oop1 for two-way become"
							fwdBlock := self fwdBlockGet: fwdBlkSize.
							self
								initForwardBlock: fwdBlock
								mapping: oop2
								to: oop1
								withBackPtr: twoWayFlag not]].
			fieldOffset := fieldOffset - BytesPerWord].
	^ true! !

!ObjectMemory methodsFor: 'interpreter access'!
pushRemappableOop: oop
	"Record the given object in a the remap buffer. Objects in this buffer are remapped when a compaction occurs. This facility is used by the interpreter to ensure that objects in temporary variables are properly remapped."

	remapBuffer at: (remapBufferCount := remapBufferCount + 1) put: oop.! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 4/22/2007 20:13'!
recycleContextIfPossible: cntxOop 
	"If possible, save the given context on a list of free contexts to 
	be recycled."
	"Note: The context is not marked free, so it can be reused 
	with minimal fuss. The recycled context lists are cleared at 
	every garbage collect."
	| header |
	self inline: true.
	"only recycle young contexts (which should be most of them)"
	(self oop: cntxOop isGreaterThanOrEqualTo: youngStart)
		ifTrue: [header := self baseHeader: cntxOop.
			(self isMethodContextHeader: header)
				ifTrue: ["It's a young context, alright."
					(header bitAnd: SizeMask) = SmallContextSize
						ifTrue: ["Recycle small contexts"
							self storePointerUnchecked: 0 ofObject: cntxOop withValue: freeContexts.
							freeContexts := cntxOop].
					(header bitAnd: SizeMask) = LargeContextSize
						ifTrue: ["Recycle large contexts"
							self storePointerUnchecked: 0 ofObject: cntxOop withValue: freeLargeContexts.
							freeLargeContexts := cntxOop]]]! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'tpr 3/24/2004 10:48'!
remap: oop 
	"Map the given oop to its new value during a compaction or 
	become: operation. If it has no forwarding table entry, 
	return the oop itself."
	| fwdBlock |
	self inline: false.
	(self isObjectForwarded: oop)
		ifTrue: ["get the new value for oop from its forwarding block"
			fwdBlock := ((self longAt: oop) bitAnd: AllButMarkBitAndTypeMask) << 1.
			DoAssertionChecks
				ifTrue: [self fwdBlockValidate: fwdBlock].
			^ self longAt: fwdBlock].
	^ oop! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 20:17'!
remapClassOf: oop 
	"Update the class of the given object, if necessary, using its forwarding table entry."
	"Note: Compact classes need not be remapped since the compact class field is just an index into the compact class 
	table. The header type bits show if this object has a compact class; we needn't look up the oop's real header."
	| classHeader classOop fwdBlock newClassOop newClassHeader |
	(self headerType: oop) = HeaderTypeShort ifTrue: [^ nil]. "compact classes needn't be mapped"

	classHeader := self longAt: oop - BytesPerWord.
	classOop := classHeader bitAnd: AllButTypeMask.
	(self isObjectForwarded: classOop)
		ifTrue: [fwdBlock := ((self longAt: classOop) bitAnd: AllButMarkBitAndTypeMask) << 1.
			DoAssertionChecks
				ifTrue: [self fwdBlockValidate: fwdBlock].
			newClassOop := self longAt: fwdBlock.
			newClassHeader := newClassOop bitOr: (classHeader bitAnd: TypeMask).
			self longAt: oop - BytesPerWord put: newClassHeader.
			"The following ensures that become: into an old object's class makes it a root. 
			It does nothing during either incremental or full compaction because 
			oop will never be < youngStart."
			((self oop: oop isLessThan: youngStart)
					and: [self oop: newClassOop isGreaterThanOrEqualTo: youngStart])
				ifTrue: [self beRootWhileForwarding: oop]]! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 20:18'!
remapFieldsAndClassOf: oop 
	"Replace all forwarded pointers in this object with their new oops, using the forwarding table. Remap its class as well, if 
	necessary. "
	"Note: The given oop may be forwarded itself, which means that its real header is in its forwarding table entry."
	| fieldOffset fieldOop fwdBlock newOop |
	self inline: true.
	fieldOffset := self lastPointerWhileForwarding: oop.
	[fieldOffset >= BaseHeaderSize]
		whileTrue: [fieldOop := self longAt: oop + fieldOffset.
			(self isObjectForwarded: fieldOop)
				ifTrue: ["update this oop from its forwarding block"
					fwdBlock := ((self longAt: fieldOop) bitAnd: AllButMarkBitAndTypeMask) << 1.
					DoAssertionChecks
						ifTrue: [self fwdBlockValidate: fwdBlock].
					newOop := self longAt: fwdBlock.
					self longAt: oop + fieldOffset put: newOop.
					"The following ensures that become: into old object makes it a root. 
					It does nothing during either incremental or full compaction because 
					oop will never be < youngStart."
					((self oop: oop isLessThan: youngStart)
							and: [self oop: newOop isGreaterThanOrEqualTo: youngStart])
						ifTrue: [self beRootWhileForwarding: oop]].
			fieldOffset := fieldOffset - BytesPerWord].
	self remapClassOf: oop! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 6/8/2006 13:06'!
removeGCRoot: varLoc
	"Remove the given variable location to the extra roots table"
	| root |
	self export: true.
	self var: #varLoc declareC: 'sqInt *varLoc'.
	self var: #root declareC:'sqInt *root'.
	1 to: extraRootCount do:[:i|
		root := extraRoots at: i.
		root == varLoc ifTrue:["swap varLoc with last entry"
			extraRoots at: i put: (extraRoots at: extraRootCount).
			extraRootCount := extraRootCount-1.
			^true]].
	^false "not found"! !

!ObjectMemory methodsFor: 'become' stamp: 'di 7/22/2004 17:50'!
restoreHeaderOf: oop 
	"Restore the original header of the given oop from its 
	forwarding block."
	| fwdHeader fwdBlock |
	fwdHeader := self longAt: oop.
	fwdBlock := (fwdHeader bitAnd: AllButMarkBitAndTypeMask) << 1.
	DoAssertionChecks
		ifTrue: [(fwdHeader bitAnd: MarkBit) = 0
				ifTrue: [self error: 'attempting to restore the header of an object that has no forwarding block'].
			self fwdBlockValidate: fwdBlock].
	self longAt: oop put: (self longAt: fwdBlock + BytesPerWord)! !

!ObjectMemory methodsFor: 'become' stamp: 'tpr 3/23/2005 12:07'!
restoreHeadersAfterBecoming: list1 with: list2 
	"Restore the headers of all oops in both lists. Exchange their hash bits so
	becoming objects in identity sets and dictionaries doesn't change their
	hash value."
	"See also prepareForwardingTableForBecoming:with:woWay: for notes
	regarding the case
	of oop1 = oop2"
	| fieldOffset oop1 oop2 hdr1 hdr2 |
	fieldOffset := self lastPointerOf: list1.
	[fieldOffset >= BaseHeaderSize]
		whileTrue: [oop1 := self longAt: list1 + fieldOffset.
			oop2 := self longAt: list2 + fieldOffset.
			oop1 = oop2
				ifFalse: [self restoreHeaderOf: oop1.
					self restoreHeaderOf: oop2.
					"Exchange hash bits of the two objects."
					hdr1 := self longAt: oop1.
					hdr2 := self longAt: oop2.
					self
						longAt: oop1
						put: ((hdr1 bitAnd: AllButHashBits) bitOr: (hdr2 bitAnd: HashBits)).
					self
						longAt: oop2
						put: ((hdr2 bitAnd: AllButHashBits) bitOr: (hdr1 bitAnd: HashBits))].
			fieldOffset := fieldOffset - BytesPerWord]! !

!ObjectMemory methodsFor: 'become' stamp: 'dtl 4/22/2007 21:34'!
restoreHeadersAfterForwardBecome: copyHashFlag 
	"Forward become leaves us with no original oops in the 
	mutated object list, 
	so we must enumerate the (four-word) forwarding blocks 
	where we have stored backpointers."
	"This loop start is copied from fwdTableInit:"
	| oop1 fwdBlock oop2 hdr1 hdr2 |
	fwdBlock := endOfMemory + BaseHeaderSize + 7 bitAnd: WordMask - 7.
	self flag: #Dan.  "See flag comment in fwdTableInit: (dtl)"
	fwdBlock := fwdBlock + (BytesPerWord*4).
	"fwdBlockGet: did a pre-increment"
	[self oop: fwdBlock isLessThanOrEqualTo: fwdTableNext
	"fwdTableNext points to the last active block"]
		whileTrue: [oop1 := self longAt: fwdBlock + (BytesPerWord*2).
			"Backpointer to mutated object."
			oop2 := self longAt: fwdBlock.
			self restoreHeaderOf: oop1.
			copyHashFlag
				ifTrue: ["Change the hash of the new oop (oop2) to be that of the old (oop1) 
					so mutated objects in hash structures will be 
					happy after the change."
					hdr1 := self longAt: oop1.
					hdr2 := self longAt: oop2.
					self longAt: oop2 put: ((hdr2 bitAnd: AllButHashBits) bitOr: (hdr1 bitAnd: HashBits))].
			fwdBlock := fwdBlock + (BytesPerWord*4)]! !

!ObjectMemory methodsFor: 'header access' stamp: 'tpr 3/24/2004 21:49'!
rightType: headerWord
	"Compute the correct header type for an object based on the size and compact class fields of the given base header word, rather than its type bits. This is used during marking, when the header type bits are used to record the state of tracing."

	(headerWord bitAnd: SizeMask) = 0  "zero size field in header word"
		ifTrue: [ ^HeaderTypeSizeAndClass ]
		ifFalse: [ (headerWord bitAnd: CompactClassMask) = 0
				ifTrue: [ ^HeaderTypeClass ]
				ifFalse: [ ^HeaderTypeShort ]].! !

!ObjectMemory methodsFor: 'header access' stamp: 'go 11/13/1998 17:04'!
setSizeOfFree: chunk to: byteSize
	"Set the header of the given chunk to make it be a free chunk of the given size."

	self longAt: chunk put: ((byteSize bitAnd: AllButTypeMask) bitOr: HeaderTypeFree).! !

!ObjectMemory methodsFor: 'allocation' stamp: 'JMM 10/22/2004 17:30'!
shrinkObjectMemory: delta 
	"Attempt to shrink the object memory by the given delta 
	amount "
	| limit |
	statShrinkMemory := statShrinkMemory + 1. 
	limit := self sqShrinkMemory: memoryLimit By: delta.
	limit = memoryLimit
		ifFalse: [memoryLimit := limit - 24.
			"remove a tad for safety"
			self initializeMemoryFirstFree: freeBlock]! !

!ObjectMemory methodsFor: 'header access' stamp: 'di 6/11/2004 16:34'!
sizeBitsOf: oop
	"Answer the number of bytes in the given object, including its base header, rounded up to an integral number of words."
	"Note: byte indexable objects need to have low bits subtracted from this size."

	| header |
	header := self baseHeader: oop.
	(header bitAnd: TypeMask) = HeaderTypeSizeAndClass
		ifTrue: [ ^ (self sizeHeader: oop) bitAnd: LongSizeMask ]
		ifFalse: [ ^ header bitAnd: SizeMask ].! !

!ObjectMemory methodsFor: 'header access' stamp: 'go 11/17/1998 15:57'!
sizeBitsOfSafe: oop
	"Compute the size of the given object from the cc and size fields in its header. This works even if its type bits are not correct."

	| header type |
	header := self baseHeader: oop.
	type := self rightType: header.
	type = HeaderTypeSizeAndClass
		ifTrue: [ ^ (self sizeHeader: oop) bitAnd: AllButTypeMask ]
		ifFalse: [ ^ header bitAnd: SizeMask ].! !

!ObjectMemory methodsFor: 'header access' stamp: 'di 6/11/2004 13:15'!
sizeHeader: oop

	^ self longAt: oop - (BytesPerWord*2)! !

!ObjectMemory methodsFor: 'header access' stamp: 'JMM 6/9/2007 15:31'!
sizeOfFree: oop
	"Return the size of the given chunk in bytes. Argument MUST be a free chunk."
	self returnTypeC: 'usqInt'.
	^ (self longAt: oop) bitAnd: AllButTypeMask! !

!ObjectMemory methodsFor: 'interpreter access'!
splObj: index
	"Return one of the objects in the SpecialObjectsArray"
	^ self fetchPointer: index ofObject: specialObjectsOop! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'di 7/1/2004 15:47'!
startField
	"Examine and possibly trace the next field of the object being 
	traced. See comment in markAndTrace for explanation of 
	tracer state variables."
	| typeBits childType |
	self inline: true.
	child := self longAt: field.
	typeBits := child bitAnd: TypeMask.
	(typeBits bitAnd: 1) = 1
		ifTrue: ["field contains a SmallInteger; skip it"
			field := field - BytesPerWord.
			^ StartField].
	typeBits = 0 ifTrue: ["normal oop, go down"
			self longAt: field put: parentField.
			parentField := field.
			^ StartObj].
	typeBits = 2 ifTrue: ["reached the header; do we need to process the class word? "
			(child bitAnd: CompactClassMask) ~= 0
				ifTrue: ["object's class is compact; we're done"
					"restore the header type bits"
					child := child bitAnd: AllButTypeMask.
					childType := self rightType: child.
					self longAt: field put: (child bitOr: childType).
					^ Upward]
				ifFalse: ["object has a full class word; process that class"
					child := self longAt: field - BytesPerWord. "class word"
					child := child bitAnd: AllButTypeMask. "clear type bits"
					self longAt: field - BytesPerWord put: parentField.
					parentField := field - BytesPerWord bitOr: 1.
					"point at class word; mark as working on the class. "
					^ StartObj]]! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'dtl 4/22/2007 21:26'!
startObj
	"Start tracing the object 'child' and answer the next action. 
	The object may be anywhere in the middle of being swept 
	itself. See comment in markAndTrace for explanation of 
	tracer state variables."
	| oop header lastFieldOffset |
	self inline: true.
	oop := child.
	(self oop: oop isLessThan: youngStartLocal)
		ifTrue: ["old object; skip it"
			field := oop.
			^ Upward].
	header := self longAt: oop.
	(header bitAnd: MarkBit) = 0
		ifTrue: ["unmarked; mark and trace"
			"Do not trace the object's indexed fields if it's a weak class "
			(self isWeakNonInt: oop)
				ifTrue: ["Set lastFieldOffset before the weak fields in the receiver "
					lastFieldOffset := (self nonWeakFieldsOf: oop) << ShiftForWord]
				ifFalse: ["Do it the usual way"
					lastFieldOffset := self lastPointerOf: oop].
			header := header bitAnd: AllButTypeMask.
			header := (header bitOr: MarkBit) bitOr: HeaderTypeGC.
			self longAt: oop put: header.
			field := oop + lastFieldOffset.
			^ StartField "trace its fields and class"]
		ifFalse: ["already marked; skip it"
			field := oop.
			^ Upward]! !

!ObjectMemory methodsFor: 'object enumeration' stamp: 'ikp 3/26/2005 14:04'!
startOfMemory
	"Return the start of object memory."

	^memory! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ikp 6/9/2004 23:16'!
storeByte: byteIndex ofObject: oop withValue: valueByte

	^ self byteAt: oop + BaseHeaderSize + byteIndex
		put: valueByte! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 7/4/2004 08:48'!
storeLong32: fieldIndex ofObject: oop withValue: valueWord

	^ self long32At: oop + BaseHeaderSize + (fieldIndex << 2)
		put: valueWord! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'dtl 4/22/2007 21:26'!
storePointer: fieldIndex ofObject: oop withValue: valuePointer
	"Note must check here for stores of young objects into old ones."

	(self oop: oop isLessThan: youngStart) ifTrue: [
		self possibleRootStoreInto: oop value: valuePointer.
	].

	^ self longAt: oop + BaseHeaderSize + (fieldIndex << ShiftForWord)
		put: valuePointer! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'di 6/23/2004 13:49'!
storePointerUnchecked: fieldIndex ofObject: oop withValue: valuePointer
	"Like storePointer:ofObject:withValue:, but the caller guarantees that the object being stored into is a young object or is already marked as a root."

	^ self longAt: oop + BaseHeaderSize + (fieldIndex << ShiftForWord)
			put: valuePointer
! !

!ObjectMemory methodsFor: 'interpreter access' stamp: 'ikp 3/26/2005 14:29'!
storeWord: fieldIndex ofObject: oop withValue: valueWord
	"This message is deprecated.  Use storeLong32, storeLong64 or storePointer"

	self abort! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 10/11/2009 13:57'!
sufficientSpaceAfterGC: minFree 
	"Return true if there is enough free space after doing a garbage collection. If not, signal that space is low."
	| growSize minFreePlus |
	self inline: false.
	self var: #minFree type: 'usqInt'.
	self var: #growSize type: 'usqInt'.
	self var: #minFreePlus type: 'usqInt'.

	self incrementalGC. "try to recover some space"

	(self oop: (self sizeOfFree: freeBlock) isLessThan: minFree)
		ifTrue: [signalLowSpace ifTrue: [^false]. "give up; problem is already noted"
			self fullGC. "try harder"
			"for stability, require more free space after doing an expensive full GC"
			minFreePlus := minFree + 15000.
			minFreePlus < minFree
				ifTrue: [^ false "arithmetic overflow"].
			(self oop: (self sizeOfFree: freeBlock) isGreaterThanOrEqualTo: minFreePlus)
				ifTrue: [^ true].

			"still not enough; attempt to grow object memory"
			growSize := minFree - (self sizeOfFree: freeBlock) + growHeadroom.
			self growObjectMemory: growSize.

			(self oop: (self sizeOfFree: freeBlock) isGreaterThanOrEqualTo: minFreePlus)
				ifTrue: [^true].

			"still not enough"
			^false].
	^true! !

!ObjectMemory methodsFor: 'allocation' stamp: 'dtl 10/19/2009 06:18'!
sufficientSpaceToAllocate: bytes
	"Return true if there is enough space to allocate the given number of bytes,
	perhaps after doing a garbage collection. Sender is responsible for ensuring
	that requested size does result in arithmetic overflow, see note below."

	| minFree |
	self inline: true.
	self var: #bytes type: 'usqInt'.
	self var: #minFree type: 'usqInt'.

	"Note: Arithmetic overflow may occur in calculation of minFree (indicated by
	minFree < bytes after calculation of minFree). The check is performed by sender
	to avoid redundant test here."
	minFree := lowSpaceThreshold + bytes + BaseHeaderSize.

	"check for low-space"
	(self oop: (self sizeOfFree: freeBlock) isGreaterThanOrEqualTo: minFree)
		ifTrue: [^true]
		ifFalse: [^self sufficientSpaceAfterGC: minFree].! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'dtl 4/22/2007 21:29'!
sweepPhase
	"Sweep memory from youngStart through the end of memory. Free all 
	inaccessible objects and coalesce adjacent free chunks. Clear the mark 
	bits of accessible objects. Compute the starting point for the first pass of 
	incremental compaction (compStart). Return the number of surviving 
	objects. "
	"Details: Each time a non-free object is encountered, decrement the 
	number of available forward table entries. If all entries are spoken for 
	(i.e., entriesAvailable reaches zero), set compStart to the last free 
	chunk before that object or, if there is no free chunk before the given 
	object, the first free chunk after it. Thus, at the end of the sweep 
	phase, compStart through compEnd spans the highest collection of 
	non-free objects that can be accomodated by the forwarding table. This 
	information is used by the first pass of incremental compaction to 
	ensure that space is initially freed at the end of memory. Note that 
	there should always be at least one free chunk--the one at the end of 
	the heap."
	| entriesAvailable survivors freeChunk firstFree oop oopHeader oopHeaderType hdrBytes oopSize freeChunkSize endOfMemoryLocal |
	self inline: false.
	self var: #oop type: 'usqInt'.
	self var: #endOfMemoryLocal type: 'usqInt'.
	entriesAvailable := self fwdTableInit: BytesPerWord*2.
	survivors := 0.
	freeChunk := nil.
	firstFree := nil.
	"will be updated later"
	endOfMemoryLocal := endOfMemory.
	oop := self oopFromChunk: youngStart.
	[oop < endOfMemoryLocal]
		whileTrue: ["get oop's header, header type, size, and header size"
			statSweepCount := statSweepCount + 1.
			oopHeader := self baseHeader: oop.
			oopHeaderType := oopHeader bitAnd: TypeMask.
			hdrBytes := headerTypeBytes at: oopHeaderType.
			(oopHeaderType bitAnd: 1) = 1
				ifTrue: [oopSize := oopHeader bitAnd: SizeMask]
				ifFalse: [oopHeaderType = HeaderTypeSizeAndClass
						ifTrue: [oopSize := (self sizeHeader: oop) bitAnd: LongSizeMask]
						ifFalse: ["free chunk" oopSize := oopHeader bitAnd: LongSizeMask]].
			(oopHeader bitAnd: MarkBit) = 0
				ifTrue: ["object is not marked; free it"
					"<-- Finalization support: We need to mark each oop chunk as free -->"
					self longAt: oop - hdrBytes put: HeaderTypeFree.
					freeChunk ~= nil
						ifTrue: ["enlarge current free chunk to include this oop"
							freeChunkSize := freeChunkSize + oopSize + hdrBytes]
						ifFalse: ["start a new free chunk"
							freeChunk := oop - hdrBytes.
							"chunk may start 4 or 8 bytes before oop"
							freeChunkSize := oopSize + (oop - freeChunk).
							"adjust size for possible extra header bytes"
							firstFree = nil ifTrue: [firstFree := freeChunk]]]
				ifFalse: ["object is marked; clear its mark bit and possibly adjust 
					the compaction start"
					self longAt: oop put: (oopHeader bitAnd: AllButMarkBit).
					"<-- Finalization support: Check if we're running about a weak class -->"
					(self isWeakNonInt: oop) ifTrue: [self finalizeReference: oop].
					entriesAvailable > 0
						ifTrue: [entriesAvailable := entriesAvailable - 1]
						ifFalse: ["start compaction at the last free chunk before this object"
							firstFree := freeChunk].
					freeChunk ~= nil
						ifTrue: ["record the size of the last free chunk"
							self longAt: freeChunk put: ((freeChunkSize bitAnd: LongSizeMask) bitOr: HeaderTypeFree).
							freeChunk := nil].
					survivors := survivors + 1].
			oop := self oopFromChunk: oop + oopSize].
	freeChunk ~= nil
		ifTrue: ["record size of final free chunk"
			self longAt: freeChunk put: ((freeChunkSize bitAnd: LongSizeMask) bitOr: HeaderTypeFree)].
	oop = endOfMemory
		ifFalse: [self error: 'sweep failed to find exact end of memory'].
	firstFree = nil
		ifTrue: [self error: 'expected to find at least one free object']
		ifFalse: [compStart := firstFree].

	^ survivors! !

!ObjectMemory methodsFor: 'plugin support' stamp: 'ar 10/7/1998 18:16'!
trueObject
	^trueObj! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 21:30'!
updatePointersInRangeFrom: memStart to: memEnd 
	"update pointers in the given memory range"
	| oop |
	self inline: false.
	oop := self oopFromChunk: memStart.
	[self oop: oop isLessThan: memEnd]
		whileTrue: [(self isFreeObject: oop)
				ifFalse: [self remapFieldsAndClassOf: oop].
			oop := self objectAfterWhileForwarding: oop]! !

!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'dtl 4/22/2007 20:31'!
updatePointersInRootObjectsFrom: memStart to: memEnd 
	"update pointers in root objects"
	| oop |
	self inline: false.
	1 to: rootTableCount do: [:i | 
			oop := rootTable at: i.
			((self oop: oop isLessThan: memStart)
					or: [self oop: oop isGreaterThanOrEqualTo: memEnd])
				ifTrue: ["Note: must not remap the fields of any object twice!!"
					"remap this oop only if not in the memory range 
					covered below"
					self remapFieldsAndClassOf: oop]]! !

!ObjectMemory methodsFor: 'gc -- mark and sweep' stamp: 'di 7/1/2004 15:58'!
upward
	"Return from marking an object below. Incoming: 
	field = oop we just worked on, needs to be put away 
	parentField = where to put it in our object 
	NOTE: Type field of object below has already been restored!!!!!! "
	| type header |
	self inline: true.
	(parentField bitAnd: 1) = 1
		ifTrue: [parentField = GCTopMarker
				ifTrue: ["top of the chain"
					header := (self longAt: field) bitAnd: AllButTypeMask.
					type := self rightType: header.
					self longAt: field put: (header bitOr: type). "install type on class oop"
					^ Done]
				ifFalse: ["was working on the extended class word"
					child := field. "oop of class"
					field := parentField - 1. "class word, ** clear the low bit **"
					parentField := self longAt: field.
					header := self longAt: field + BytesPerWord. "base header word"
					type := self rightType: header.
					self longAt: field put: (child bitOr: type). "install type on class oop"
					field := field + BytesPerWord. "point at header"
					"restore type bits"
					header := header bitAnd: AllButTypeMask.
					self longAt: field put: (header bitOr: type).
					^ Upward]]
		ifFalse: ["normal"
			child := field. "who we worked on below"
			field := parentField. "where to put it"
			parentField := self longAt: field.
			self longAt: field put: child.
			field := field - BytesPerWord. "point at header"
			^ StartField]! !

!ObjectMemory methodsFor: 'memory access' stamp: 'tpr 3/17/2005 18:40'!
validate
"null method just to stop compilation of interp.c from barfing"! !

!ObjectMemory methodsFor: 'memory access' stamp: 'dtl 4/22/2007 18:24'!
validateRoots 
	"Verify that every old object that points to a new object 
		has its root bit set, and
		appears in the rootTable.
	This method should not be called if the rootTable is full, because roots
	are no longer recorded, and incremental collections are not attempted.
	If DoAssertionChecks is true, this routine will halt on an unmarked root.
	Otherwise, this routine will merely return true in that case."
	| oop fieldAddr fieldOop header badRoot |
	self var: #oop type: 'usqInt'.
	self var: #fieldAddr type: 'usqInt'.
	self var: #fieldOop type: 'usqInt'.
	badRoot := false.
	oop := self firstObject.

	[oop < youngStart] whileTrue:
		[(self isFreeObject: oop) ifFalse:
			[fieldAddr := oop + (self lastPointerOf: oop).
			[fieldAddr > oop] whileTrue:
				[fieldOop := self longAt: fieldAddr.
				(fieldOop >= youngStart and: [(self isIntegerObject: fieldOop) not]) ifTrue:
					["fieldOop is a pointer to a young object"
					header := self longAt: oop.
					(header bitAnd: RootBit) = 0
					ifTrue:
						["Forbidden: points to young obj but root bit not set."
						DoAssertionChecks ifTrue: [self error: 'root bit not set'].
						badRoot := true]
					ifFalse:
						["Root bit is set"
						"Extreme test -- validate that oop was entered in rootTable too..."
						"Disabled for now...
						found := false.
						1 to: rootTableCount do:
							[:i | oop = (rootTable at: i) ifTrue: [found := true]].
						found ifFalse:
							[DoAssertionChecks ifTrue: [self error: 'root table not set'].
							badRoot := true].
						..."
						]].
				fieldAddr := fieldAddr - BytesPerWord]].
		oop := self objectAfter: oop].
	^ badRoot! !

!ObjectMemory class methodsFor: 'accessing' stamp: 'tpr 4/1/2005 14:25'!
baseHeaderSize
"To support SmartSyntaxPluginCodeGenerator"
	^BaseHeaderSize! !

!ObjectMemory class methodsFor: 'accessing' stamp: 'ikp 9/2/2004 14:08'!
bytesPerWord
	"Answer the width of an object pointer, in bytes."

	^BytesPerWord! !

!ObjectMemory class methodsFor: 'translation' stamp: 'dtl 4/14/2007 16:14'!
declareCAsOop: arrayOfVariableNames in: aCCodeGenerator
	"Declare the variables in arrayOfVariableNames with type representing position in object memory."

	arrayOfVariableNames
		do: [:varName | aCCodeGenerator var: varName type: 'usqInt']! !

!ObjectMemory class methodsFor: 'translation' stamp: 'dtl 10/17/2009 10:57'!
declareCVarsIn: aCCodeGenerator
	aCCodeGenerator var: #memory type:#'usqInt'.
	aCCodeGenerator
		var: #remapBuffer
		declareC: 'sqInt remapBuffer[', (RemapBufferSize + 1) printString, ']'.
	aCCodeGenerator
		var: #rootTable
		declareC: 'sqInt rootTable[', (RootTableSize + 1) printString, ']'.
	"Weak roots must be large enough for roots+remapBuffer+sizeof(allCallsOn: #markAndTrace:)"
	aCCodeGenerator
		var: #weakRoots
		declareC: 'sqInt weakRoots[', (RootTableSize + RemapBufferSize + 100) printString, ']'.
	aCCodeGenerator
		var: #extraRoots
		declareC: 'sqInt* extraRoots[', (ExtraRootSize + 1) printString, ']'.
	aCCodeGenerator
		var: #headerTypeBytes
		declareC: 'sqInt headerTypeBytes[4]'.
	
	self declareCAsOop: {
			#youngStart .
			#endOfMemory .
			#memoryLimit .
			#youngStartLocal .
			#freeBlock .
			#compStart .
			#compEnd .
			#fwdTableNext .
			#fwdTableLast .
			#gcBiasToGrowThreshold .
			#growHeadroom }
		in: aCCodeGenerator
! !

!ObjectMemory class methodsFor: 'initialization' stamp: 'ikp 9/22/2004 12:05'!
initBytesPerWord: nBytes

	BytesPerWord := nBytes.
	ShiftForWord := (BytesPerWord log: 2) rounded.
	"The following is necessary to avoid confusing the compiler with shifts that are larger than the width of the type on which they operate.  In gcc, such shifts cause incorrect code to be generated."
	BytesPerWord = 8
		ifTrue:					"64-bit VM"
			[Byte0Mask := 16r00000000000000FF.	Byte0Shift := 0.
			 Byte1Mask := 16r000000000000FF00.	Byte1Shift := 8.
			 Byte2Mask := 16r0000000000FF0000.	Byte2Shift := 16.
			 Byte3Mask := 16r00000000FF000000.	Byte3Shift := 24.
			 Byte4Mask := 16r000000FF00000000.	Byte4Shift := 32.
			 Byte5Mask := 16r0000FF0000000000.	Byte5Shift := 40.
			 Byte6Mask := 16r00FF000000000000.	Byte6Shift := 48.
			 Byte7Mask := 16rFF00000000000000.	Byte7Shift := 56.
			 Bytes3to0Mask := 16r00000000FFFFFFFF.
			 Bytes7to4Mask := 16rFFFFFFFF00000000]
		ifFalse:					"32-bit VM"
			[Byte0Mask := 16r00000000000000FF.	Byte0Shift := 0.
			 Byte1Mask := 16r000000000000FF00.	Byte1Shift := 8.
			 Byte2Mask := 16r0000000000FF0000.	Byte2Shift := 16.
			 Byte3Mask := 16r00000000FF000000.	Byte3Shift := 24.
			 Byte4Mask := 16r0000000000000000.	Byte4Shift := 0.		"unused"
			 Byte5Mask := 16r0000000000000000.	Byte5Shift := 0.		"unused"
			 Byte6Mask := 16r0000000000000000.	Byte6Shift := 0.		"unused"
			 Byte7Mask := 16r0000000000000000.	Byte7Shift := 0.		"unused"
			 Bytes3to0Mask := 16r0000000000000000.					"unused"
			 Bytes7to4Mask := 16r0000000000000000					"unused"].
	Byte1ShiftNegated := Byte1Shift negated.
	Byte3ShiftNegated := Byte3Shift negated.
	Byte4ShiftNegated := Byte4Shift negated.
	Byte5ShiftNegated := Byte5Shift negated.
	Byte7ShiftNegated := Byte7Shift negated.! !

!ObjectMemory class methodsFor: 'initialization' stamp: 'di 7/1/2004 14:44'!
initializeObjectHeaderConstants

	BytesPerWord ifNil: [BytesPerWord := 4].  "May get called on fileIn, so supply default"
	BaseHeaderSize := BytesPerWord.
	WordMask := (1 bitShift: BytesPerWord*8) - 1.
	
	"masks for type field"
	TypeMask := 3.
	AllButTypeMask := WordMask - TypeMask.

	"type field values"
	HeaderTypeSizeAndClass := 0.
	HeaderTypeClass := 1.
	HeaderTypeFree := 2.
	HeaderTypeShort := 3.

	"type field values used during the mark phase of GC"
	HeaderTypeGC := 2.
	GCTopMarker := 3.  "neither an oop, nor an oop+1, this value signals that we have crawled back up to the top of the marking phase."

	"Base header word bit fields"
	HashBits := 16r1FFE0000.
	AllButHashBits := WordMask - HashBits.
	HashBitsOffset := 17.
	SizeMask := 16rFC.
	Size4Bit := 0.
BytesPerWord = 8 ifTrue:
		[SizeMask := 16rF8.  "Lose the 4 bit in temp 64-bit chunk format"
		Size4Bit := 4].  "But need it for ST size"
	"Note SizeMask + Size4Bit gives the mask needed for size fits of format word in classes.
		This is used in instantiateClass:indexableSize: "
	LongSizeMask := WordMask - 16rFF + SizeMask.
	CompactClassMask := 16r1F000.

	"masks for root and mark bits"
	MarkBit := 1 bitShift: BytesPerWord*8 - 1.  "Top bit"
	RootBit := 1 bitShift: BytesPerWord*8 - 2.  "Next-to-Top bit"
	AllButMarkBit := WordMask - MarkBit.
	AllButRootBit := WordMask - RootBit.

	AllButMarkBitAndTypeMask := AllButTypeMask - MarkBit.! !

!ObjectMemory class methodsFor: 'initialization' stamp: 'eem 5/23/2008 16:47'!
initializeSpecialObjectIndices
	"Initialize indices into specialObjects array."

	NilObject := 0.
	FalseObject := 1.
	TrueObject := 2.
	SchedulerAssociation := 3.
	ClassBitmap := 4.
	ClassInteger := 5.
	ClassString := 6.
	ClassArray := 7.
	"SmalltalkDictionary := 8."  "Do not delete!!"
	ClassFloat := 9.
	ClassMethodContext := 10.
	ClassBlockContext := 11.
	ClassPoint := 12.
	ClassLargePositiveInteger := 13.
	TheDisplay := 14.
	ClassMessage := 15.
	ClassCompiledMethod := 16.
	TheLowSpaceSemaphore := 17.
	ClassSemaphore := 18.
	ClassCharacter := 19.
	SelectorDoesNotUnderstand := 20.
	SelectorCannotReturn := 21.
	ProcessSignalingLowSpace := 22.	"was TheInputSemaphore"
	SpecialSelectors := 23.
	CharacterTable := 24.
	SelectorMustBeBoolean := 25.
	ClassByteArray := 26.
	ClassProcess := 27.
	CompactClasses := 28.
	TheTimerSemaphore := 29.
	TheInterruptSemaphore := 30.
	SelectorCannotInterpret := 34.
	"Was MethodContextProto := 35."
	ClassBlockClosure := 36.
	"Was BlockContextProto := 37."
	ExternalObjectsArray := 38.
	ClassPseudoContext := 39.
	ClassTranslatedMethod := 40.
	TheFinalizationSemaphore := 41.
	ClassLargeNegativeInteger := 42.

	ClassExternalAddress := 43.
	ClassExternalStructure := 44.
	ClassExternalData := 45.
	ClassExternalFunction := 46.
	ClassExternalLibrary := 47.

	SelectorAboutToReturn := 48.
	SelectorRunWithIn := 49.
! !

!ObjectMemory class methodsFor: 'initialization' stamp: 'ar 6/8/2006 12:35'!
initializeWithBytesToWord:  numberOfBytesInAWord
	"ObjectMemory initializeWithBytesToWord: Smalltalk wordSize"

	self initBytesPerWord: numberOfBytesInAWord.

	"Translation flags (booleans that control code generation via conditional translation):"
	DoAssertionChecks := false.  "generate assertion checks"
	DoBalanceChecks := false. "generate stack balance checks"

	self initializeSpecialObjectIndices.
	self initializeObjectHeaderConstants.

	CtxtTempFrameStart := 6.  "Copy of TempFrameStart in Interp"
	ContextFixedSizePlusHeader := CtxtTempFrameStart + 1.
	SmallContextSize := ContextFixedSizePlusHeader + 16 * BytesPerWord.  "16 indexable fields"
	"Large contexts have 56 indexable fileds.  Max with single header word."
	"However note that in 64 bits, for now, large contexts have 3-word headers"
	LargeContextSize := ContextFixedSizePlusHeader + 56 * BytesPerWord.
	
	LargeContextBit := 16r40000.  "This bit set in method headers if large context is needed."
	NilContext := 1.  "the oop for the integer 0; used to mark the end of context lists"

	RemapBufferSize := 25.
	RootTableSize := 2500.  	"number of root table entries (4 bytes/entry)"
	RootTableRedZone := RootTableSize - 100.	"red zone of root table - when reached we force IGC"

	"tracer actions"
	StartField := 1.
	StartObj := 2.
	Upward := 3.
	Done := 4.

	ExtraRootSize := 2048. "max. # of external roots"! !

!ObjectMemory class methodsFor: 'translation' stamp: 'tpr 2/17/2005 13:19'!
noteCompilationOf: aSelector meta: isMeta
	"note the recompiliation by resetting the timeStamp "
	timeStamp := Time localSecondClock.
	^super noteCompilationOf: aSelector meta: isMeta! !

!ObjectMemory class methodsFor: 'translation' stamp: 'tpr 2/29/2004 19:51'!
requiredMethodNames
	"return the list of method names that should be retained for export or other support reasons"
	^ #(checkedLongAt: allocateChunk: firstAccessibleObject noteAsRoot:headerLoc: splObj:)! !

!ObjectMemory class methodsFor: 'translation' stamp: 'tpr 3/27/2002 12:53'!
timeStamp
	^timeStamp ifNil:[0]! !

!ObjectMemory class methodsFor: 'translation' stamp: 'ikp 8/3/2004 20:17'!
unsignedIntegerSuffix
	"Answer the suffix that should be appended to unsigned integer literals in generated code."

	^BytesPerWord = 4 ifTrue: ['U'] ifFalse: ['ULL']! !

!PluginMaker class methodsFor: 'version testing' stamp: 'jmv 7/30/2012 16:23'!
headerNotice
"return a string to be put at the front of generated code files"
	^String streamContents:[:strm|
		strm nextPutAll: 'Automatically generated on '.
		Time dateAndTimeNow do: [ :e | strm nextPutAll: e asString; nextPut: Character space].
		strm cr.
		strm nextPutAll: '   by PluginMaker '.
		strm nextPutAll: self  versionString.
		strm cr]! !

!PluginMaker class methodsFor: 'version testing' stamp: 'jmv 7/30/2012 13:24'!
versionString

	"VMMaker versionString"

	^'4.0.0'! !

!TMethod methodsFor: 'inlining support'!
addVarsDeclarationsAndLabelsOf: methodToBeInlined
	"Prepare to inline the body of the given method into the receiver by making the args and locals of the argument to the receiver be locals of the receiver. Record any type declarations for these variables. Record labels. Assumes that the variables have already be renamed to avoid name clashes."

	methodToBeInlined args, methodToBeInlined locals do: [ :v |
		(locals includes: v) ifFalse: [ locals addLast: v ].
	].
	methodToBeInlined declarations associationsDo: [ :assoc |
		declarations add: assoc.
	].
	methodToBeInlined labels do: [ :label |
		labels add: label.
	].! !

!TMethod methodsFor: 'utilities'!
allCalls
	"Answer a collection of selectors for the messages sent by this method."

	^parseTree allCalls! !

!TMethod methodsFor: 'inlining'!
argAssignmentsFor: meth args: argList in: aCodeGen
	"Return a collection of assignment nodes that assign the given argument expressions to the formal parameter variables of the given method."
	"Optimization: If the actual parameters are either constants or local variables in the target method (the receiver), substitute them directly into the body of meth. Note that global variables cannot be subsituted because the inlined method might depend on the exact ordering of side effects to the globals."

	| stmtList substitutionDict |
	stmtList := OrderedCollection new: 100.
	substitutionDict := Dictionary new: 100.
	meth args with: argList do: [ :argName :exprNode |
		(self isSubstitutableNode: exprNode intoMethod: meth in: aCodeGen) ifTrue: [
			substitutionDict at: argName put: exprNode.
			locals remove: argName.
		] ifFalse: [
			stmtList add: (TAssignmentNode new
				setVariable: (TVariableNode new setName: argName)
				expression: exprNode copyTree).
		].
	].
	meth parseTree: (meth parseTree bindVariablesIn: substitutionDict).
	^stmtList! !

!TMethod methodsFor: 'primitive compilation' stamp: 'tpr 4/12/2006 12:27'!
argConversionExprFor: varName stackIndex: stackIndex 
	"Return the parse tree for an expression that fetches and converts the 
	primitive argument at the given stack offset."
	| exprList decl stmtList |
	exprList := OrderedCollection new.
	(declarations includesKey: varName)
		ifTrue: 
			[decl := declarations at: varName.
			(decl includes: $*)
				ifTrue: 
					["array"
					exprList add: varName , ' := ', self vmNameString, ' arrayValueOf: (', self vmNameString, ' stackValue: (' , stackIndex printString , '))'.
					exprList add: varName , ' := ' , varName , ' - 1']
				ifFalse: 
					["must be a double"
					(decl findString: 'double' startingAt: 1)
						= 0 ifTrue: [self error: 'unsupported type declaration in a primitive method'].
					exprList add: varName , ' := ', self vmNameString, ' stackFloatValue: ' , stackIndex printString]]
		ifFalse: ["undeclared variables are taken to be integer"
			exprList add: varName , ' := ', self vmNameString, ' stackIntegerValue: ' , stackIndex printString].
	stmtList := OrderedCollection new.
	exprList do: [:e | stmtList addAll: (self statementsFor: e varName: varName)].
	^ stmtList! !

!TMethod methodsFor: 'accessing'!
args
	"The arguments of this method."

	^args! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/6/2003 20:09'!
asInlineNode
	^TInlineNode new method: self! !

!TMethod methodsFor: 'transformations'!
bindClassVariablesIn: constantDictionary
	"Class variables are used as constants. This method replaces all references to class variables in the body of this method with the corresponding constant looked up in the class pool dictionary of the source class. The source class class variables should be initialized before this method is called."

	parseTree := parseTree bindVariablesIn: constantDictionary.! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/6/2003 20:35'!
bindVariableUsesIn: aDictionary
	parseTree := parseTree bindVariableUsesIn: aDictionary.! !

!TMethod methodsFor: 'transformations' stamp: 'ar 11/19/1999 14:40'!
buildCaseStmt: aSendNode
	"Build a case statement node for the given send of dispatchOn:in:."
	"Note: the first argument is the variable to be dispatched on. The second argument is a constant node holding an array of unary selectors, which will be turned into sends to self."

	((aSendNode args size >= 2) and:
	 [aSendNode args second isConstant and:
	 [aSendNode args second value class = Array]]) ifFalse: [
		self error: 'wrong node structure for a case statement'.
	].

	^TCaseStmtNode new
		setExpression: aSendNode args first
		selectors: aSendNode args second value
		arguments: (aSendNode args copyFrom: 3 to: aSendNode args size)! !

!TMethod methodsFor: 'inlining'!
checkForCompleteness: stmtLists in: aCodeGen
	"Set the complete flag if none of the given statement list nodes contains further candidates for inlining."

	complete := true.
	stmtLists do: [ :stmtList |
		stmtList statements do: [ :node |
			(self inlineableSend: node in: aCodeGen) ifTrue: [
				complete := false.  "more inlining to do"
				^self
			].
		].
	].
	parseTree nodesDo: [ :n |
		(self inlineableFunctionCall: n in: aCodeGen) ifTrue: [
			complete := false.  "more inlining to do"
			^self
		].
	].! !

!TMethod methodsFor: 'primitive compilation'!
checkSuccessExpr
	"Return the parse tree for an expression that aborts the primitive if the successFlag is not true."

	| expr |
	expr := 'successFlag ifFalse: [^ nil ]'.
	^ self statementsFor: expr varName: ''
! !

!TMethod methodsFor: 'accessing' stamp: 'hg 8/14/2000 15:57'!
comment: aComment

	comment := aComment ! !

!TMethod methodsFor: 'inlining support'!
computePossibleSideEffectsIn: aCodeGen
	"Answer true if this method may have side effects. It has side effects if it assigns to a global variable. It may have side effects if it calls a non-built-in method."

	parseTree nodesDo: [ :node |
		node isSend ifTrue: [
			node isBuiltinOperator ifFalse: [ ^true ].
		].
	].
	^ false! !

!TMethod methodsFor: 'utilities' stamp: 'ar 7/6/2003 21:40'!
copy
	"Make a deep copy of this TMethod."

	^ (self class basicNew)
		setSelector: selector
		returnType: returnType
		args: args copy
		locals: locals copy
		declarations: declarations copy
		primitive: primitive
		parseTree: parseTree copyTree
		labels: labels copy
		complete: complete;
		sharedLabel: sharedLabel;
		sharedCase: sharedCase;
		yourself
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'jm 2/15/98 16:48'!
covertToZeroBasedArrayReferences
	"Replace the index expressions in at: and at:put: messages with (<expr> - 1), since C uses zero-based array indexing."
	"Note: Up through release 1.31, generated primitives used the convention that array variables pointed to the first element. That meant that Smalltalk one-based index expressions had to have one subtracted to yield a zero-based index. Later, we decided to adjust the base address by -1 once in the primitive prolog rather on every array access. This resulted in a five percent performance increase for the bitmap compress/decompress primitives. This method is retained as documentation and in case we choose to revert the the previous scheme."

	| oldIndexExpr newIndexExpr |
	parseTree nodesDo: [ :n |
		(n isSend and: [(n selector = #at:) or: [ n selector = #at:put: ]]) ifTrue: [
			oldIndexExpr := n args first.
			oldIndexExpr isConstant ifTrue: [
				"index expression is a constant: decrement the constant now"
				newIndexExpr := TConstantNode new setValue: (n args first value - 1).
			] ifFalse: [
				"index expression is complex: build an expression to decrement result at runtime"
				newIndexExpr := TSendNode new
					setSelector: #-
					receiver: oldIndexExpr
					arguments: (Array with: (TConstantNode new setValue: 1)).
			].
			n args at: 1 put: newIndexExpr.
		].
	].
! !

!TMethod methodsFor: 'accessing'!
declarations
	"The type declaration dictionary of this method."

	^declarations! !

!TMethod methodsFor: 'accessing' stamp: 'ar 2/3/2001 17:29'!
definingClass
	^definingClass! !

!TMethod methodsFor: 'accessing' stamp: 'ar 2/3/2001 17:29'!
definingClass: aClass
	definingClass := aClass.! !

!TMethod methodsFor: 'C code generation' stamp: 'hg 8/14/2000 15:41'!
emitCCodeOn: aStream generator: aCodeGen
	"Emit C code for this method onto the given stream. All calls to inlined methods should already have been expanded."

	self emitCCommentOn: aStream.	"place method comment before function"

	self emitCHeaderOn: aStream generator: aCodeGen.
	parseTree emitCCodeOn: aStream level: 1 generator: aCodeGen.
	aStream nextPutAll: '}'; cr.! !

!TMethod methodsFor: 'C code generation' stamp: 'hg 8/14/2000 16:09'!
emitCCommentOn: aStream
	"Emit the transferred Smalltalk comments as C comments."

	comment ifNotNil: [
		aStream cr;cr.
		1 to: comment size do: [:index | 
			aStream 
				nextPutAll: '/*'; tab;
				nextPutAll: (comment at: index);
				nextPutAll: ' */';
				cr]]! !

!TMethod methodsFor: 'C code generation' stamp: 'dtl 2/3/2007 11:02'!
emitCFunctionPrototype: aStream generator: aCodeGen
	"Emit a C function header for this method onto the given stream."

	| arg |
	export 
		ifTrue:[aStream nextPutAll:'EXPORT('; nextPutAll: returnType; nextPutAll:') ']
		ifFalse:[(aCodeGen isGeneratingPluginCode and:[self isStatic]) 
					ifTrue:[aStream nextPutAll:'static '].
				aStream nextPutAll: returnType; space].
	aStream nextPutAll: (aCodeGen cFunctionNameFor: self selectorForCodeGeneration), '('.
	args isEmpty ifTrue: [ aStream nextPutAll: 'void' ].
	1 to: args size do: [ :i |
		arg := args at: i.
		(declarations includesKey: arg) ifTrue: [
			aStream nextPutAll: (declarations at: arg).
		] ifFalse: [
			aStream nextPutAll: 'sqInt ', (args at: i).
		].
		i < args size ifTrue: [ aStream nextPutAll: ', ' ].
	].
	aStream nextPutAll: ')'.! !

!TMethod methodsFor: 'C code generation' stamp: 'ikp 6/9/2004 16:15'!
emitCHeaderOn: aStream generator: aCodeGen
	"Emit a C function header for this method onto the given stream."

	aStream cr. 
	self emitCFunctionPrototype: aStream generator: aCodeGen.
	aStream nextPutAll: ' {'; cr.
	self emitGlobalStructReferenceOn: aStream.
	locals do: [ :var |
		aStream nextPutAll: '    '.
		aStream nextPutAll: (declarations at: var ifAbsent: [ 'sqInt ', var]), ';'; cr.
	].
	locals isEmpty ifFalse: [ aStream cr ].! !

!TMethod methodsFor: 'C code generation'!
emitGlobalStructReferenceOn: aStream
	"Add a reference to the globals struct if needed"

	(self globalStructureBuildMethodHasFoo > 1)
		ifTrue: [aStream nextPutAll: 'register struct foo * foo = &fum;'; cr].
! !

!TMethod methodsFor: 'C code generation' stamp: 'ikp 6/9/2004 16:15'!
emitInlineOn: aStream level: level generator: aCodeGen
	"Emit C code for this method onto the given stream. All calls to inlined methods should already have been expanded."
	self removeUnusedTemps.
	sharedLabel ifNotNil:[
		aStream crtab: level-1; nextPutAll: sharedLabel; nextPutAll:':'.
		aStream crtab: level.
		aStream nextPutAll: '/* '; nextPutAll: selector; nextPutAll: ' */'.
		aStream crtab: level.
	].
	aStream nextPutAll:'{'; cr.
	locals do: [ :var |
		aStream tab: level+1.
		aStream nextPutAll: (declarations at: var ifAbsent: [ 'sqInt ', var]), ';'; cr.
	].
	parseTree emitCCodeOn: aStream level: level+1 generator: aCodeGen.
	aStream tab: level; nextPutAll: '}'; cr.! !

!TMethod methodsFor: 'C code generation' stamp: 'dtl 2/3/2007 11:03'!
emitProxyFunctionPrototype: aStream generator: aCodeGen
	"Emit an indirect C function header for this method onto the given stream."

	| arg |
	aStream nextPutAll: returnType; space.
	aStream nextPutAll: '(*', (aCodeGen cFunctionNameFor: self selectorForCodeGeneration), ')('.
	args isEmpty ifTrue: [ aStream nextPutAll: 'void' ].
	1 to: args size do: [ :i |
		arg := args at: i.
		(declarations includesKey: arg) ifTrue: [
			aStream nextPutAll: (declarations at: arg).
		] ifFalse: [
			aStream nextPutAll: 'sqInt ', (args at: i).
		].
		i < args size ifTrue: [ aStream nextPutAll: ', ' ].
	].
	aStream nextPutAll: ')'.! !

!TMethod methodsFor: 'inlining support'!
endsWithReturn
	"Answer true if the last statement of this method is a return."

	^ parseTree statements last isReturn! !

!TMethod methodsFor: 'inlining'!
exitVar: exitVar label: exitLabel
	"Replace each return statement in this method with an assignment to the exit variable followed by a goto to the given label. Return true if a goto was generated."
	"Optimization: If exitVar is nil, the return value of the inlined method is not being used, so don't add the assignment statement."

	| newStmts labelUsed |
	labelUsed := false.
	parseTree nodesDo: [ :node |
		node isStmtList ifTrue: [
			newStmts := OrderedCollection new: 100.
			node statements do: [ :stmt |
				(stmt isReturn) ifTrue: [
					exitVar = nil ifTrue: [
						stmt expression isLeaf ifFalse: [
							"evaluate return expression even though value isn't used"
							newStmts add: stmt expression.
						].
					] ifFalse: [
						"assign return expression to exit variable"
						newStmts add:
							(TAssignmentNode new
								setVariable: (TVariableNode new setName: exitVar)
								expression: stmt expression).
					].
					(stmt == parseTree statements last) ifFalse: [
						"generate a goto (this return is NOT the last statement in the method)"
						newStmts add: (TGoToNode new setLabel: exitLabel).
						labelUsed := true.
					].
				] ifFalse: [
					newStmts addLast: stmt.
				].
			].
			node setStatements: newStmts asArray.
		].
	].
	^labelUsed! !

!TMethod methodsFor: 'accessing' stamp: 'jm 11/24/1998 09:03'!
export

	^ export
! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/8/2003 13:13'!
extractExportDirective
	"Scan the top-level statements for an inlining directive of the form:

		self export: <boolean>

	 and remove the directive from the method body. Return the argument of the directive or false if there is no export directive."

	| result newStatements |
	result := false.
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		(stmt isSend and: [stmt selector = #export:]) ifTrue: [
			result := stmt args first value = true.
		] ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.
	^ result! !

!TMethod methodsFor: 'inlining support' stamp: 'ar 7/8/2003 11:44'!
extractInlineDirective
	"Scan the top-level statements for an inlining directive of the form:

		self inline: <boolean>

	 and remove the directive from the method body. Return the argument of the directive or #dontCare if there is no inlining directive."

	| result newStatements |
	sharedCase ifNotNil:[^false]. "don't auto-inline shared code; it gets handled specially"
	result := #dontCare.
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		(stmt isSend and: [stmt selector = #inline:]) ifTrue: [
			result := stmt args first value = true.
		] ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.
	^ result! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/7/2003 00:56'!
extractSharedCase
	"Scan the top-level statements for an shared case directive of the form:

		self sharedCodeNamed: <sharedLabel> inCase: <sharedCase>.

	and remove the directive from the method body."

	| newStatements |
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		(stmt isSend and: [stmt selector = #sharedCodeNamed:inCase:]) ifTrue: [
			sharedLabel := stmt args first value.
			sharedCase := stmt args last value
		] ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.
	sharedCase ifNotNil:[
		args isEmpty ifFalse:[self error: 'Cannot share code sections in methods with arguments'].
	].! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/8/2003 12:11'!
extractStaticDirective
	"Scan the top-level statements for an inlining directive of the form:

		self static: <boolean>

	 and remove the directive from the method body. Return the argument of the directive or true if there is no static directive."

	| result newStatements |
	result := true.
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		(stmt isSend and: [stmt selector = #static:]) ifTrue: [
			result := stmt args first value ~= false.
		] ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.
	^ result! !

!TMethod methodsFor: 'primitive compilation' stamp: 'tpr 4/12/2006 12:26'!
fetchRcvrExpr
	"Return the parse tree for an expression that fetches the receiver from the stack."

	| expr |
	expr := 'rcvr := ', self vmNameString, ' stackValue: (', args size printString, ')'.
	^ self statementsFor: expr varName: ''
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 16:19'!
fixUpReturns: argCount postlog: postlog
	"Replace each return statement in this method with (a) the given postlog, (b) code to pop the receiver and the given number of arguments, and (c) code to push the integer result and return."

	| newStmts |
	parseTree nodesDo: [:node |
		node isStmtList ifTrue: [
			newStmts := OrderedCollection new: 100.
			node statements do: [:stmt |
				stmt isReturn
					ifTrue: [
						(stmt expression isSend and:
						 ['primitiveFail' = stmt expression selector])
							ifTrue: [  "failure return"
								newStmts addLast: stmt expression.
								newStmts addLast: (TReturnNode new
									setExpression: (TVariableNode new setName: 'null'))]
							ifFalse: [  "normal return"
								newStmts addAll: postlog.
								newStmts addAll: (self popArgsExpr: argCount + 1).
								newStmts addLast: (TSendNode new
									setSelector: #pushInteger:
									receiver: (TVariableNode new setName: self vmNameString)
									arguments: (Array with: stmt expression)).
								newStmts addLast: (TReturnNode new
									setExpression: (TVariableNode new setName: 'null'))]]
					ifFalse: [
						newStmts addLast: stmt]].
			node setStatements: newStmts asArray]].
! !

!TMethod methodsFor: 'utilities'!
freeVariableReferences
	"Answer a collection of variables referenced this method, excluding locals, arguments, and pseudovariables."

	| refs |
	refs := Set new.
	parseTree nodesDo: [ :node |
		node isVariable ifTrue: [ refs add: node name asString ].
	].
	args do: [ :var | refs remove: var asString ifAbsent: [] ].
	locals do: [ :var | refs remove: var asString ifAbsent: [] ].
	#('self' 'nil' 'true' 'false') do: [ :var | refs remove: var ifAbsent: [] ].
	^ refs asSortedCollection! !

!TMethod methodsFor: 'accessing' stamp: 'jm 11/24/1998 09:03'!
globalStructureBuildMethodHasFoo
	^globalStructureBuildMethodHasFoo! !

!TMethod methodsFor: 'accessing'!
globalStructureBuildMethodHasFoo: number
	globalStructureBuildMethodHasFoo := number! !

!TMethod methodsFor: 'utilities'!
hasNoCCode
	"Answer true if the receiver does not use inlined C or C declarations, which are not currently renamed properly by the the inliner."

	declarations isEmpty ifFalse: [ ^ false ].

	parseTree nodesDo: [ :node |
		node isSend ifTrue: [
			node selector = #cCode: ifTrue: [ ^ false ].
		].
	].
	^ true! !

!TMethod methodsFor: 'inlining support'!
hasReturn
	"Answer true if this method contains a return statement."

	parseTree nodesDo: [ :n | n isReturn ifTrue: [ ^ true ]].
	^ false! !

!TMethod methodsFor: 'inlining' stamp: 'dtl 8/17/2008 18:46'!
inlineCaseStatementBranchesIn: aCodeGen localizingVars: varsList

	| stmt sel meth newStatements maxTemp usedVars exitLabel v |
	maxTemp := 0.
	parseTree nodesDo: [ :n |
		n isCaseStmt ifTrue: [
			n cases do: [ :stmtNode |
				stmt := stmtNode statements first.
				stmt isSend ifTrue: [
					sel := stmt selector.
					meth := aCodeGen methodNamed: sel.
					"Note, original version of this method tested for #hasNoCCode. Removed
					the test to permit inlining methods that may contain automatically
					generated C code for type conversions. -dtl"
					((meth ~= nil) and:
					 [meth args size = 0]) ifTrue: [
						meth := meth copy.
						meth hasReturn ifTrue: [
							exitLabel := self unusedLabelForInliningInto: self.
							meth exitVar: nil label: exitLabel.
							labels add: exitLabel.
						] ifFalse: [ exitLabel := nil ].

						meth renameLabelsForInliningInto: self.
						meth labels do: [ :label | labels add: label ].
						newStatements := stmtNode statements asOrderedCollection.
						newStatements removeFirst.

						exitLabel ~= nil ifTrue: [
							newStatements addFirst:
								(TLabeledCommentNode new
									setLabel: exitLabel comment: 'end case').
						].

						newStatements addFirst: meth asInlineNode.
						newStatements addFirst:
							(TLabeledCommentNode new setComment: meth selector).
						stmtNode setStatements: newStatements.
					].
				].
			].
		].
	].
	usedVars := (locals, args) asSet.
	1 to: maxTemp do: [ :i |
		v := ('t', i printString).
		(usedVars includes: v) ifTrue: [ self error: 'temp variable name conflicts with an existing local or arg' ].
		locals addLast: v.
	].

	"make local versions of the given globals"
	varsList do: [ :var |
		(usedVars includes: var) ifFalse: [ locals addFirst: var asString ].
	].
! !

!TMethod methodsFor: 'inlining'!
inlineCodeOrNilForStatement: aNode in: aCodeGen
	"If the given statement node can be inlined, answer the statements that replace it. Otherwise, answer nil."

	| stmts |
	aNode isReturn ifTrue: [
		(self inlineableSend: aNode expression in: aCodeGen) ifTrue: [
			stmts := self inlineSend: aNode expression
				directReturn: true exitVar: nil in: aCodeGen.
			^stmts
		].
	].
	aNode isAssignment ifTrue: [
		(self inlineableSend: aNode expression in: aCodeGen) ifTrue: [
			^self inlineSend: aNode expression
				directReturn: false exitVar: aNode variable name in: aCodeGen
		].
	].
	aNode isSend ifTrue: [
		(self inlineableSend: aNode in: aCodeGen) ifTrue: [
			^self inlineSend: aNode
				directReturn: false exitVar: nil in: aCodeGen
		].
	].
	^nil! !

!TMethod methodsFor: 'inlining'!
inlineFunctionCall: aSendNode in: aCodeGen
	"Answer the body of the called function, substituting the actual parameters for the formal argument variables in the method body."
	"Assume caller has established that:
		1. the method arguments are all substitutable nodes, and
		2. the method to be inlined contains no additional embedded returns."

	| sel meth substitutionDict |
	sel := aSendNode selector.
	meth := (aCodeGen methodNamed: sel) copy.
	meth renameVarsForInliningInto: self in: aCodeGen.
	meth renameLabelsForInliningInto: self.
	self addVarsDeclarationsAndLabelsOf: meth.
	substitutionDict := Dictionary new: 100.
	meth args with: aSendNode args do: [ :argName :exprNode |
		substitutionDict at: argName put: exprNode.
		locals remove: argName].
	meth parseTree bindVariablesIn: substitutionDict.
	^ meth statements first expression! !

!TMethod methodsFor: 'inlining' stamp: 'ikp 6/9/2004 16:15'!
inlineSend: aSendNode directReturn: directReturn exitVar: exitVar in: aCodeGen
	"Answer a collection of statments to replace the given send. directReturn indicates that the send is the expression of a return statement, so returns can be left in the body of the inlined method. If exitVar is nil, the value returned by the send is not used; thus, returns need not assign to the output variable."

	| sel meth exitLabel labelUsed inlineStmts |
	sel := aSendNode selector.
	meth := (aCodeGen methodNamed: sel) copy.
	meth renameVarsForInliningInto: self in: aCodeGen.
	meth renameLabelsForInliningInto: self.
	self addVarsDeclarationsAndLabelsOf: meth.
	meth hasReturn ifTrue: [
		directReturn ifTrue: [
			"propagate the return type, if necessary"
			returnType = meth returnType ifFalse: [ self halt ].  "caller's return type should be declared by user"
			returnType := meth returnType.
		] ifFalse: [
			exitLabel := self unusedLabelForInliningInto: self.
			labelUsed := meth exitVar: exitVar label: exitLabel.
			labelUsed
				ifTrue: [ labels add: exitLabel ]
				ifFalse: [ exitLabel := nil ].
		].
		"propagate type info if necessary"
		((exitVar ~= nil) and: [meth returnType ~= 'sqInt']) ifTrue: [
			declarations at: exitVar put: meth returnType, ' ', exitVar.
		].
	].
	inlineStmts := OrderedCollection new: 100.
	inlineStmts add: (TLabeledCommentNode new setComment: 'begin ', sel).
	inlineStmts addAll:
		(self argAssignmentsFor: meth args: aSendNode args in: aCodeGen).
	inlineStmts addAll: meth statements.  "method body"
	(directReturn and: [meth endsWithReturn not]) ifTrue: [
		inlineStmts add: (TReturnNode new setExpression: (TVariableNode new setName: 'nil')).
	].
	exitLabel ~= nil ifTrue: [
		inlineStmts add:
			(TLabeledCommentNode new
				setLabel: exitLabel comment: 'end ', meth selector).
	].
	^inlineStmts! !

!TMethod methodsFor: 'inlining'!
inlineableFunctionCall: aNode in: aCodeGen
	"Answer true if the given send node is a call to a 'functional' method--a method whose body is a single return statement of some expression and whose actual parameters can all be directly substituted."

	| m |
	aNode isSend ifFalse: [ ^false ].
	m := aCodeGen methodNamed: aNode selector.  "nil if builtin or external function"
	((m ~= nil) and: [m isFunctional and: [aCodeGen mayInline: m selector]]) ifTrue: [
		aNode args do: [ :a | (self isSubstitutableNode: a intoMethod: m in: aCodeGen) ifFalse: [ ^false ]].
		^true
	] ifFalse: [
		^false
	].! !

!TMethod methodsFor: 'inlining'!
inlineableSend: aNode in: aCodeGen
	"Answer true if the given send node is a call to a method that can be inlined."

	| m |
	aNode isSend ifFalse: [ ^false ].
	m := aCodeGen methodNamed: aNode selector.  "nil if builtin or external function"
	^(m ~= nil) and: [m isComplete and: [aCodeGen mayInline: m selector]]! !

!TMethod methodsFor: 'primitive compilation' stamp: 'ar 4/4/2006 21:15'!
instVarGetExprFor: varName offset: instIndex
	"Return the parse tree for an expression that fetches and converts the value of the instance variable at the given offset."

	| exprList decl stmtList |
	exprList := OrderedCollection new.
	(declarations includesKey: varName) ifTrue: [
		decl := declarations at: varName.
		(decl includes: $*) ifTrue: [  "array"
			exprList add:
				(varName, ' := ', self vmNameString, ' fetchArray: ', instIndex printString, ' ofObject: rcvr').
			exprList add: (varName, ' := ', varName, ' - 1').
		] ifFalse: [  "must be a double"
			((decl findString: 'double' startingAt: 1) = 0)
				ifTrue: [ self error: 'unsupported type declaration in a primitive method' ].
			exprList add:
				(varName, ' := ', self vmNameString, ' fetchFloat: ', instIndex printString, ' ofObject: rcvr').
		].
	] ifFalse: [  "undeclared variables are taken to be integer"
		exprList add:
			(varName, ' := ', self vmNameString, ' fetchInteger: ', instIndex printString, ' ofObject: rcvr').
	].
	stmtList := OrderedCollection new.
	exprList do: [:e | stmtList addAll: (self statementsFor: e varName: varName)].
	^ stmtList
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 16:13'!
instVarPutExprFor: varName offset: instIndex
	"Return the parse tree for an expression that saves the value of the integer instance variable at the given offset."

	| expr |
	(declarations includesKey: varName) ifTrue: [
		self error: 'a primitive method can only modify integer instance variables'.
	].
	expr := '', self vmNameString, ' storeInteger: ', instIndex printString, ' ofObject: rcvr withValue: ', varName.
	^ self statementsFor: expr varName: varName
! !

!TMethod methodsFor: 'inlining support' stamp: 'ikp 9/26/97 14:50'!
isAssertion
	^(selector beginsWith: 'assert') or: [selector beginsWith: 'verify']! !

!TMethod methodsFor: 'accessing'!
isComplete
	"A method is 'complete' if it does not contain any more inline-able calls."

	^complete! !

!TMethod methodsFor: 'inlining'!
isFunctional
	"Answer true if the receiver is a functional method. That is, if it consists of a single return statement of an expression that contains no other returns."

	(parseTree statements size = 1 and:
	 [parseTree statements last isReturn]) ifFalse: [ ^false ].
	parseTree statements last expression nodesDo: [ :n | n isReturn ifTrue: [ ^false ]].
	^true! !

!TMethod methodsFor: 'accessing' stamp: 'ar 5/9/2000 12:13'!
isStatic
	^static ifNil:[false].! !

!TMethod methodsFor: 'inlining' stamp: 'ar 5/9/2000 12:13'!
isSubstitutableNode: aNode
	"Answer true if the given parameter node is either a constant, a local variable, or a formal parameter of the receiver. Such parameter nodes may be substituted directly into the body of the method during inlining. Note that global variables cannot be subsituted because the inlined method might depend on the exact ordering of side effects to the globals."

	aNode isConstant ifTrue: [ ^true ].
	^aNode isVariable and:
		[(locals includes: aNode name) or:
		[args includes: aNode name]]! !

!TMethod methodsFor: 'inlining'!
isSubstitutableNode: aNode intoMethod: targetMeth in: aCodeGen
	"Answer true if the given parameter node is either a constant, a local variable, or a formal parameter of the receiver. Such parameter nodes may be substituted directly into the body of the method during inlining. Note that global variables cannot be subsituted into methods with possible side effects (i.e., methods that may assign to global variables) because the inlined method might depend on having the value of the global variable captured when it is passed in as an argument."

	| var |
	aNode isConstant ifTrue: [ ^ true ].

	aNode isVariable ifTrue: [
		var := aNode name.
		((locals includes: var) or: [args includes: var]) ifTrue: [ ^ true ].
		(#(self true false nil) includes: var) ifTrue: [ ^ true ].
		(targetMeth maySubstituteGlobal: var in: aCodeGen) ifTrue: [ ^ true ].
	].

	"scan expression tree; must contain only constants, builtin ops, and inlineable vars"
	aNode nodesDo: [ :node |
		node isSend ifTrue: [
			node isBuiltinOperator ifFalse: [ ^false ].
		].
		node isVariable ifTrue: [
			var := node name.
			((locals includes: var) or:
			 [(args includes: var) or:
			 [(#(self true false nil) includes: var) or:
			 [targetMeth maySubstituteGlobal: var in: aCodeGen]]]) ifFalse: [ ^ false ].
		].
		(node isConstant or: [node isVariable or: [node isSend]]) ifFalse: [ ^false ].
	].

	^ true! !

!TMethod methodsFor: 'accessing' stamp: 'ar 5/9/2000 12:13'!
labels

	^labels! !

!TMethod methodsFor: 'accessing' stamp: 'ar 5/9/2000 12:13'!
locals
	"The local variables of this method."

	^locals! !

!TMethod methodsFor: 'inlining support'!
maySubstituteGlobal: globalVar in: aCodeGen
	"Answer true if this method does or may have side effects on the given global variable."

	possibleSideEffectsCache = nil ifTrue: [
		"see if this calls any other method and record the result"
		possibleSideEffectsCache := self computePossibleSideEffectsIn: aCodeGen.
	].
	possibleSideEffectsCache ifTrue: [ ^ false ].

	parseTree nodesDo: [ :node |
		node isAssignment ifTrue: [
			node variable name = globalVar ifTrue: [ ^ false ].
		].
	].

	"if we get here, receiver calls no other method
	 and does not itself assign to the given global variable"
	^ true! !

!TMethod methodsFor: 'utilities'!
nodeCount
	"Answer the number of nodes in this method's parseTree (a rough measure of its size)."

	| cnt |
	cnt := 0.
	parseTree nodesDo: [ :n | cnt := cnt + 1 ].
	^cnt! !

!TMethod methodsFor: 'accessing'!
parseTree
	"The parse tree of this method."

	^parseTree! !

!TMethod methodsFor: 'accessing'!
parseTree: aNode
	"Set the parse tree of this method."

	parseTree := aNode.! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 16:13'!
popArgsExpr: argCount
	"Return the parse tree for an expression that pops the given number of arguments from the stack."

	| expr |
	expr := '', self vmNameString, ' pop: ', argCount printString.
	^ self statementsFor: expr varName: ''
! !

!TMethod methodsFor: 'transformations' stamp: 'ar 11/18/1999 20:09'!
prepareMethodIn: aCodeGen
	"Record sends of builtin operators and replace sends of the special selector dispatchOn:in: with case statement nodes."
	"Note: Only replaces top-level sends of dispatchOn:in:. Case statements must be top-level statements; they cannot appear in expressions."

	| stmts stmt |
	parseTree nodesDo: [ :node |
		node isSend ifTrue: [
			"record sends of builtin operators"
			(aCodeGen builtin: node selector) ifTrue: [ node isBuiltinOperator: true ].
		].
		node isStmtList ifTrue: [
			"replace dispatchOn:in: with case statement node"
			stmts := node statements.
			1 to: stmts size do: [ :i |
				stmt := stmts at: i.
				(stmt isSend and: [CaseStatements includes: stmt selector]) ifTrue: [
					stmts at: i put: (self buildCaseStmt: stmt).
				].
			].
		].
	].! !

!TMethod methodsFor: 'primitive compilation' stamp: 'ar 2/3/2001 17:33'!
preparePrimitiveName
	"Prepare the selector for this method in translation"
	| aClass |
	aClass := definingClass.
	primitive = 117 
		ifTrue:[selector := ((aClass includesSelector: selector)
					ifTrue: [aClass compiledMethodAt: selector]
					ifFalse: [aClass class compiledMethodAt: selector]) literals first at: 2.
				export := true]
		ifFalse:[selector := 'prim', aClass name, selector].

! !

!TMethod methodsFor: 'primitive compilation' stamp: 'ar 2/3/2001 17:36'!
preparePrimitivePrologue
	"Add a prolog and postlog to a primitive method. The prolog copies any instance variables referenced by this primitive method into local variables. The postlog copies values of assigned-to variables back into the instance. The names of the new locals are added to the local variables list.

The declarations dictionary defines the types of any non-integer variables (locals, arguments, or instance variables). In particular, it may specify the types:

	int *		-- an array of 32-bit values (e.g., a BitMap)
	short *		-- an array of 16-bit values (e.g., a SoundBuffer)
	char *		-- an array of unsigned bytes (e.g., a String)
	double		-- a double precision floating point number (e.g., 3.14159)

Undeclared variables are taken to be integers and will be converted from Smalltalk to C ints."

"Current restrictions:
	o method must not contain message sends
	o method must not allocate objects
	o method must not manipulate raw oops
	o method cannot access class variables
	o method can only return an integer"

	| prolog postlog instVarsUsed varsAssignedTo instVarList primArgCount varName endsWithReturn aClass |
selector == #setInterpreter: ifTrue:[self halt].
	aClass := definingClass.
	prolog := OrderedCollection new.
	postlog := OrderedCollection new.
	instVarsUsed := self freeVariableReferences asSet.
	varsAssignedTo := self variablesAssignedTo asSet.
	instVarList := aClass allInstVarNames.
	primArgCount := args size.

	"add receiver fetch and arg conversions to prolog"
	prolog addAll: self fetchRcvrExpr.
	1 to: args size do: [:argIndex |
		varName := args at: argIndex.
		prolog addAll:
			(self argConversionExprFor: varName stackIndex: args size - argIndex)].

	"add success check to postlog"
	postlog addAll: self checkSuccessExpr.

	"add instance variable fetches to prolog and instance variable stores to postlog"
	1 to: instVarList size do: [:varIndex |
		varName := instVarList at: varIndex.
		(instVarsUsed includes: varName) ifTrue: [
			locals add: varName.
			prolog addAll: (self instVarGetExprFor: varName offset: varIndex - 1).
			(varsAssignedTo includes: varName) ifTrue: [
				postlog addAll: (self instVarPutExprFor: varName offset: varIndex - 1)]]].
	prolog addAll: self checkSuccessExpr.

	locals addAllFirst: args.
	locals addFirst: 'rcvr'.
	args := args class new.
	locals asSet size = locals size
		ifFalse: [self error: 'local name conflicts with instance variable name'].
	endsWithReturn := self endsWithReturn.
	self fixUpReturns: primArgCount postlog: postlog.

	endsWithReturn
		ifTrue: [parseTree setStatements: prolog, parseTree statements]
		ifFalse: [
			postlog addAll: (self popArgsExpr: primArgCount).
			parseTree setStatements: prolog, parseTree statements, postlog].
! !

!TMethod methodsFor: 'accessing' stamp: 'jm 2/12/98 11:56'!
primitive
	"The primitive number of this method; zero if not a primitive."

	^ primitive
! !

!TMethod methodsFor: 'printing' stamp: 'TPR 3/2/2000 18:36'!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' (', selector, ')'.! !

!TMethod methodsFor: 'private' stamp: 'TPR 2/29/2000 18:45'!
printTempsAndVar: varName on: aStream 
	"add the required temps and the varname to the stream"
	aStream nextPutAll: '| rcvr stackPointer successFlag ' , varName , ' |';
	 cr! !

!TMethod methodsFor: 'transformations' stamp: 'ar 3/10/2000 21:18'!
recordDeclarations
	"Record C type declarations of the forms

		self returnTypeC: 'float'.
		self var: #foo declareC: 'float foo'
		self var: #foo type:'float'.

	 and remove the declarations from the method body."

	| newStatements isDeclaration varName varType |
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		isDeclaration := false.
		stmt isSend ifTrue: [
			stmt selector = #var:declareC: ifTrue: [
				isDeclaration := true.
				declarations at: stmt args first value asString put: stmt args last value.
			].
			stmt selector = #var:type: ifTrue: [
				isDeclaration := true.
				varName := stmt args first value asString.
				varType := stmt args last value.
				declarations at: varName put: (varType, ' ', varName).
			].
			stmt selector = #returnTypeC: ifTrue: [
				isDeclaration := true.
				returnType := stmt args last value.
			].
		].
		isDeclaration ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.! !

!TMethod methodsFor: 'accessing'!
referencesGlobalStructIncrementBy: value
	globalStructureBuildMethodHasFoo := globalStructureBuildMethodHasFoo + value.! !

!TMethod methodsFor: 'accessing'!
referencesGlobalStructMakeZero
	globalStructureBuildMethodHasFoo := 0! !

!TMethod methodsFor: 'transformations' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	parseTree removeAssertions! !

!TMethod methodsFor: 'transformations' stamp: 'jm 12/14/1998 08:35'!
removeFinalSelfReturn
	"The Smalltalk parser automatically adds the statement '^self' to the end of methods without explicit returns. This method removes such statements, since the generated code has no notion of 'self' anyway."

	| stmtList lastStmt |
	stmtList := parseTree statements asOrderedCollection.
	lastStmt := stmtList last.

	((lastStmt isReturn) and:
	 [(lastStmt expression isVariable) and:
	 ['self' = lastStmt expression name]]) ifTrue: [
		stmtList removeLast.
		parseTree setStatements: stmtList.
	].! !

!TMethod methodsFor: 'utilities' stamp: 'ar 7/8/2003 11:20'!
removeUnusedTemps
	"Remove all of the unused temps in this method. Answer a bag (why the hell a bag???) with the references."
	"After inlining some variable references are now obsolete, we could fix them there but the 
	code seems a bit complicated, the other choice to to rebuild the locals before extruding. This is done here"
	| refs |
	refs := Bag new.
	"find all the variable names referenced in this method"
	parseTree nodesDo: [ :node |
		node isVariable ifTrue: [ refs add: node name asString ].
		node isStmtList ifTrue: [refs addAll: node args]].
	"add all the non-arg declarations (might be variables usedonly in cCode sections)"
	refs addAll:((self declarations keys) reject: [:e | self args includes: e]).
	"reset the locals to be only those still referred to"
	locals := locals select: [:e | refs includes: e].
	^refs
! !

!TMethod methodsFor: 'inlining support' stamp: 'jm 12/13/1998 10:06'!
renameLabelsForInliningInto: destMethod
	"Rename any labels that would clash with those of the destination method."

	| destLabels usedLabels labelMap newLabelName |
	destLabels := destMethod labels asSet.
	usedLabels := destLabels copy.  "usedLabels keeps track of labels in use"
	usedLabels addAll: labels.
	labelMap := Dictionary new: 100.
	self labels do: [ :l |
		(destLabels includes: l) ifTrue: [
			newLabelName := self unusedNamePrefixedBy: 'l' avoiding: usedLabels.
			labelMap at: l put: newLabelName.
		].
	].
	self renameLabelsUsing: labelMap.! !

!TMethod methodsFor: 'inlining support'!
renameLabelsUsing: aDictionary
	"Rename all labels according to the old->new mappings of the given dictionary."

	labels := labels collect: [ :label |
		(aDictionary includesKey: label) ifTrue: [ aDictionary at: label ] ifFalse: [ label ].
	].

	parseTree nodesDo: [ :node |
		(node isGoTo and: [aDictionary includesKey: node label]) ifTrue: [
			node setLabel: (aDictionary at: node label).
		].
		(node isLabel and: [aDictionary includesKey: node label]) ifTrue: [
			node setLabel: (aDictionary at: node label).
		].
	].! !

!TMethod methodsFor: 'inlining support' stamp: 'dtl 8/3/2009 19:19'!
renameVariablesUsing: aDictionary
	"Rename all variables according to old->new mappings of the given dictionary."

	| newDecls newKey newValue |
	"map args and locals"
	args := args collect: [ :arg |
		(aDictionary includesKey: arg) ifTrue: [ aDictionary at: arg ] ifFalse: [ arg ].
	].
	locals := locals collect: [ :v |
		(aDictionary includesKey: v) ifTrue: [ aDictionary at: v ] ifFalse: [ v ].
	].

	"map declarations"
	newDecls := declarations species new.
	declarations associationsDo: [ :assoc |
		(aDictionary includesKey: assoc key)
			ifTrue: [ newKey := aDictionary at: assoc key.
					newValue := assoc value replaceLastOccurrence: assoc key with: newKey.
					newDecls at: newKey put: newValue]
			ifFalse: [ newDecls add: assoc ].
	].
	declarations := newDecls.

	"map variable names in parse tree"
	parseTree nodesDo: [ :node |
		(node isVariable and:
		 [aDictionary includesKey: node name]) ifTrue: [
			node setName: (aDictionary at: node name).
		].
		(node isStmtList and: [node args size > 0]) ifTrue: [
			node setArguments:
				(node args collect: [ :arg |
					(aDictionary includesKey: arg)
						ifTrue: [ aDictionary at: arg ]
						ifFalse: [ arg ].
				]).
		].
	].! !

!TMethod methodsFor: 'inlining support' stamp: 'jm 12/13/1998 10:07'!
renameVarsForCaseStmt
	"Rename the arguments and locals of this method with names like t1, t2, t3, etc. Return the number of variable names assigned. This is done to allow registers to be shared among the cases."

	| i varMap |
	i := 1.
	varMap := Dictionary new: 100.
	args, locals do: [ :v |
		varMap at: v put: ('t', i printString) asSymbol.
		i := i + 1.
	].
	self renameVariablesUsing: varMap.
	^ i - 1! !

!TMethod methodsFor: 'inlining support' stamp: 'jm 12/13/1998 10:07'!
renameVarsForInliningInto: destMethod in: aCodeGen
	"Rename any variables that would clash with those of the destination method."

	| destVars usedVars varMap newVarName |
	destVars := aCodeGen globalsAsSet copy.
	destVars addAll: destMethod locals.
	destVars addAll: destMethod args.
	usedVars := destVars copy.  "keeps track of names in use"
	usedVars addAll: args; addAll: locals.
	varMap := Dictionary new: 100.
	args, locals do: [ :v |
		(destVars includes: v) ifTrue: [
			newVarName := self unusedNamePrefixedBy: v avoiding: usedVars.
			varMap at: v put: newVarName.
		].
	].
	self renameVariablesUsing: varMap.! !

!TMethod methodsFor: 'transformations' stamp: 'ar 7/6/2003 21:31'!
replaceNodesIn: map
	parseTree := parseTree replaceNodesIn: map.! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 19:08'!
replaceSizeMessages
	"Replace sends of the message 'size' with calls to sizeOfSTArrayFromCPrimitive."

	| argExpr |
	parseTree nodesDo: [:n |
		(n isSend and: [n selector = #size]) ifTrue: [
			argExpr := TSendNode new
				setSelector: #+
				receiver: n receiver
				arguments: (Array with: (TConstantNode new setValue: 1)).
			n
				setSelector: #sizeOfSTArrayFromCPrimitive:
				receiver: (TVariableNode new setName: self vmNameString)
				arguments: (Array with: argExpr)]].
! !

!TMethod methodsFor: 'accessing'!
returnType
	"The type of the values returned by this method. This string will be used in the C declaration of this function."

	^returnType! !

!TMethod methodsFor: 'accessing'!
selector
	"The Smalltalk selector of this method."

	^selector! !

!TMethod methodsFor: 'accessing'!
selector: newSelector

	selector := newSelector.! !

!TMethod methodsFor: 'accessing' stamp: 'dtl 12/21/2008 14:55'!
selectorForCodeGeneration
	"Subclasses may allow a directive to override the selector specification."

	^ selector! !

!TMethod methodsFor: 'initialization' stamp: 'ikp 6/9/2004 16:16'!
setSelector: sel args: argList locals: localList block: aBlockNode primitive: aNumber
	"Initialize this method using the given information."

	selector := sel.
	returnType := 'sqInt'. 	 "assume return type is long for now"
	args := argList asOrderedCollection collect: [:arg | arg key].
	locals := localList asOrderedCollection collect: [:arg | arg key].
	declarations := Dictionary new.
	primitive := aNumber.
	parseTree := aBlockNode asTranslatorNode.
	labels := OrderedCollection new.
	complete := false.  "set to true when all possible inlining has been done"
	export := self extractExportDirective.
	static := self extractStaticDirective.
	self extractSharedCase.
	self removeFinalSelfReturn.
	self recordDeclarations.
	globalStructureBuildMethodHasFoo := 0.! !

!TMethod methodsFor: 'initialization' stamp: 'jm 2/12/98 11:55'!
setSelector: sel returnType: retType args: argList locals: localList declarations: decls primitive: primNumber parseTree: aNode labels: labelList complete: completeFlag
	"Initialize this method using the given information. Used for copying."

	selector := sel.
	returnType := retType.
	args := argList.
	locals := localList.
	declarations := decls.
	primitive := primNumber.
	parseTree := aNode.
	labels := labelList.
	complete := completeFlag.! !

!TMethod methodsFor: 'accessing' stamp: 'ar 7/6/2003 21:18'!
sharedCase
	^sharedCase! !

!TMethod methodsFor: 'accessing' stamp: 'ar 7/6/2003 21:41'!
sharedCase: aNumber
	sharedCase := aNumber.! !

!TMethod methodsFor: 'accessing' stamp: 'ar 7/6/2003 21:18'!
sharedLabel
	^sharedLabel! !

!TMethod methodsFor: 'accessing' stamp: 'ar 7/6/2003 21:40'!
sharedLabel: aString
	sharedLabel := aString! !

!TMethod methodsFor: 'accessing'!
statements

	parseTree isStmtList
		ifFalse: [ self error: 'expected method parse tree to be a TStmtListNode' ].
	((parseTree args = nil) or: [parseTree args isEmpty])
		ifFalse: [ self error: 'expected method parse tree to have no args' ].

	^parseTree statements! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 2/29/2000 18:47'!
statementsFor: sourceText varName: varName
	"Return the parse tree for the given expression. The result is the statements list of the method parsed from the given source text."
	"Details: Various variables are declared as locals to avoid Undeclared warnings from the parser."

	| s |
	s := WriteStream on: ''.
	s nextPutAll: 'temp'; cr; cr; tab.
	self printTempsAndVar: varName on: s.
	s nextPutAll: sourceText.
	^ ((Compiler new parse: s contents in: Object notifying: nil)
			asTranslationMethodOfClass: self class) statements
! !

!TMethod methodsFor: 'inlining' stamp: 'nk 4/5/2005 20:37'!
statementsListsForInlining
	"Answer a collection of statement list nodes that are candidates for inlining. Currently, we cannot inline into the argument blocks of and: and or: messages."

	| stmtLists |
	stmtLists := OrderedCollection new: 10.
	parseTree nodesDo: [ :node | 
		node isStmtList ifTrue: [ stmtLists add: node ].
	].
	parseTree nodesDo: [ :node | 
		node isSend ifTrue: [
			((node selector = #and:) or: [node selector = #or:]) ifTrue: [
				"Note: the PP 2.3 compiler produces two arg nodes for these selectors"
				stmtLists remove: node args first ifAbsent: [].
				stmtLists remove: node args last ifAbsent: [].
			].
			((node selector = #ifTrue:) or: [node selector = #ifFalse:]) ifTrue: [
				stmtLists remove: node receiver ifAbsent: [].
			].
			((node selector = #ifTrue:ifFalse:) or: [node selector = #ifFalse:ifTrue:]) ifTrue: [
				stmtLists remove: node receiver ifAbsent: [].
			].
			((node selector = #whileFalse:) or: [node selector = #whileTrue:]) ifTrue: [
				"Allow inlining if it is a [...] whileTrue/whileFalse.
				This is identified by having more than one statement in the 
				receiver block in which case the C code wouldn't work anyways"
				node receiver statements size = 1
					ifTrue:[stmtLists remove: node receiver ifAbsent: []].
			].
			(node selector = #to:do:) ifTrue: [
				stmtLists remove: node receiver ifAbsent: [].
				stmtLists remove: node args first ifAbsent: [].
			].
			(node selector = #to:by:do:) ifTrue: [
				stmtLists remove: node receiver ifAbsent: [].
				stmtLists remove: node args first ifAbsent: [].
				stmtLists remove: node args second ifAbsent: [].
			].
		].
		node isCaseStmt ifTrue: [
			"don't inline cases"
			node cases do: [: case | stmtLists remove: case ifAbsent: [] ].
		].
	].
	^stmtLists! !

!TMethod methodsFor: 'inlining' stamp: 'jm 12/13/1998 10:07'!
tryToInlineMethodsIn: aCodeGen
	"Expand any (complete) inline methods called by this method. Set the complete bit when all inlining has been done. Return true if something was inlined."

	| stmtLists didSomething newStatements inlinedStmts sendsToInline |
	didSomething := false.

	sendsToInline := Dictionary new: 100.
	parseTree nodesDo: [ :n |
		(self inlineableFunctionCall: n in: aCodeGen) ifTrue: [
			sendsToInline at: n put: (self inlineFunctionCall: n in: aCodeGen).
		].
	].
	sendsToInline isEmpty ifFalse: [
		didSomething := true.
		parseTree := parseTree replaceNodesIn: sendsToInline.
	].

	didSomething ifTrue: [
		possibleSideEffectsCache := nil.
		^didSomething
	].

	stmtLists := self statementsListsForInlining.
	stmtLists do: [ :stmtList | 
		newStatements := OrderedCollection new: 100.
		stmtList statements do: [ :stmt |
			inlinedStmts := self inlineCodeOrNilForStatement: stmt in: aCodeGen.
			(inlinedStmts = nil) ifTrue: [
				newStatements addLast: stmt.
			] ifFalse: [
				didSomething := true.
				newStatements addAllLast: inlinedStmts.
			].
		].
		stmtList setStatements: newStatements asArray.
	].

	didSomething ifTrue: [
		possibleSideEffectsCache := nil.
		^didSomething
	].

	complete ifFalse: [
		self checkForCompleteness: stmtLists in: aCodeGen.
		complete ifTrue: [ didSomething := true ].  "marking a method complete is progress"
	].
	^didSomething! !

!TMethod methodsFor: 'inlining'!
unusedLabelForInliningInto: targetMethod

	| usedLabels |
	usedLabels := labels asSet.
	usedLabels addAll: targetMethod labels.
	^self unusedNamePrefixedBy: 'l' avoiding: usedLabels! !

!TMethod methodsFor: 'inlining support'!
unusedNamePrefixedBy: aString avoiding: usedNames
	"Choose a unique variable or label name with the given string as a prefix, avoiding the names in the given collection. The selected name is added to usedNames."

	| n newVarName |
	n := 1.
	newVarName := aString, n printString.
	[usedNames includes: newVarName] whileTrue: [
		n := n + 1.
		newVarName := aString, n printString.
	].
	usedNames add: newVarName.
	^ newVarName! !

!TMethod methodsFor: 'utilities'!
variablesAssignedTo
	"Answer a collection of variables assigned to by this method."

	| refs |
	refs := Set new.
	parseTree nodesDo: [ :node |
		node isAssignment ifTrue: [ refs add: node variable name ].
	].
	^ refs! !

!TMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 16:08'!
vmNameString
	"return the string to use as the vm name in code generated for this method"
	^'self'! !

!TMethod class methodsFor: 'class initialization' stamp: 'ar 11/18/1999 20:06'!
initialize
	"TMethod initialize"	
	CaseStatements := IdentitySet new: 10.
	CaseStatements addAll: #(dispatchOn:in: dispatchOn:in:with: dispatchOn:in:with:with:).! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'acg 9/19/1999 11:19'!
args: anInteger

	^args := anInteger! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 12/15/1999 06:31'!
assign: variable expression: expression

	^TAssignmentNode new
		setVariable: variable
		expression: expression! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 9/19/1999 20:53'!
checkSuccessExpr
	"Return the parse tree for an expression that aborts the primitive if the successFlag is not true."

	| expr |
	expr := 'interpreterProxy failed ifTrue: [^nil]'.
	^ self statementsFor: expr varName: ''
! !

!SmartSyntaxPluginTMethod methodsFor: 'generating C code' stamp: 'ikp 3/31/2005 14:23'!
emitCHeaderOn: aStream generator: aCodeGen
	"Emit a C function header for this method onto the given stream."

	aStream cr.
	self emitCFunctionPrototype: aStream generator: aCodeGen.
	aStream nextPutAll: ' {'; cr.
	locals do: [ :var |
		aStream 
			tab; 
			nextPutAll: (declarations 
				at: var 
				ifAbsent: [ 'sqInt ', var]);
			nextPut: $;; 
			cr].
	locals isEmpty ifFalse: [ aStream cr ].! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'dtl 2/3/2007 11:40'!
extractPrimitiveDirectives
	"Set selectorOverride, and save args in fullArgs.  Scan top-level statements for a directive of the form:

		self	
			primitive: 	<string>
or
		self
			primitive:	<string>
			parameters: <list of class names>
or
		self
			primitive:	<string>
			parameters: <list of class names>
			receiver: <class name>

or an assignment of that expression to a local, and manipulate the state and parse tree accordingly."

	parseTree setStatements: (Array streamContents:
		[:sStream |
			parseTree statements do:
				[:stmt |
				 (self primitiveDirectiveWasHandled: stmt on: sStream)
					ifFalse: [sStream nextPut: stmt]]]).
	isPrimitive 
		ifTrue:
			[export := true.
			 parseTree 
				setStatements: self namedPrimitiveProlog, 
								parseTree statements.
			 self fixUpReturns.
			 self replaceSizeMessages.
			 ^true]
		ifFalse: [self removeFinalSelfReturn].
	^false! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'acg 12/18/1999 10:39'!
extractSuppressFailureGuardDirective
	"Scan the top-level statements for a pragma directive of the form:

		self suppressFailureGuards: <boolean>

	 and remove the directive from the method body. Answer the argument of the directive or false if there is no #supressFailureGuards: directive."

	| result newStatements |
	result := false.
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: [ :stmt |
		(stmt isSend and: [stmt selector = #suppressFailureGuards:]) ifTrue: [
			result := stmt args first name = 'true'.
		] ifFalse: [
			newStatements add: stmt.
		].
	].
	parseTree setStatements: newStatements asArray.
	^ result! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'TPR 3/1/2000 20:21'!
fixUpReturnOneStmt: stmt on: sStream

	stmt isReturn ifFalse: [^sStream nextPut: stmt].
	(stmt expression isSend and: ['primitiveFail' = stmt expression selector]) ifTrue: 
		["failure return"
		 sStream nextPut: stmt expression.
		 sStream nextPut: self nullReturnExpr.
		 ^nil].
	(stmt expression isVariable and: ['nil' = stmt expression name]) ifTrue: 
		["^ nil -- this is never right unless automatically generated"
		 sStream nextPut: stmt.
		 ^nil].
	(stmt expression isVariable and: ['self' = stmt expression name]) ifTrue: 
		["^ self"
		 self generateFailureGuardOn: sStream.
		 fullArgs isEmpty ifFalse:[ sStream nextPut: (self popExpr: fullArgs size)].
		 sStream nextPut: self nullReturnExpr.
		 ^nil].
	(stmt expression isVariable | stmt expression isConstant | suppressingFailureGuards) ifTrue:
		["^ variable or ^ constant or ^ expr without guardchecking"
		 self generateFailureGuardOn: sStream.
		 sStream nextPut: (self pop: fullArgs size + 1 thenReturnExpr: stmt expression).
		 sStream nextPut: self nullReturnExpr.
		 ^nil].
	"^ expr with guardchecking"
	sStream nextPut: (self assign: (self oopVariable: '_return_value') expression: stmt expression).
	self generateFailureGuardOn: sStream.
	sStream nextPut: (self pop: fullArgs size + 1 thenReturnExpr: (self oopVariable: '_return_value')).
	sStream nextPut: self nullReturnExpr
! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'acg 9/18/1999 01:49'!
fixUpReturns
	"Replace each return statement in this method with (a) the given postlog, (b) code to pop the receiver and the given number of arguments, and (c) code to push the integer result and return."

	parseTree nodesDo: [:node |
		node isStmtList ifTrue: [
			node setStatements: (Array streamContents:
				[:sStream |
				 node statements do: 
					[:stmt | self fixUpReturnOneStmt: stmt on: sStream]])]]! !

!SmartSyntaxPluginTMethod methodsFor: 'initializing' stamp: 'acg 9/19/1999 11:46'!
fromContext: aContext primitive: aString parameters: aClassList receiver: aClass

	fullArgs := args := aContext tempNames
				copyFrom: 1
				to: aContext method numArgs.
	self 
		primitive: aString
		parameters: aClassList
		receiver: aClass! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 12/18/1999 10:47'!
generateFailureGuardOn: sStream
	suppressingFailureGuards ifTrue: [^nil].
	sStream nextPutAll: self checkSuccessExpr
! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'ar 4/4/2006 21:10'!
handlePrimitiveDirective: aStmt on: sStream

	isPrimitive := true.
	fullArgs := args.
	locals addAll: args.
	args := OrderedCollection new.
	fullArgs with: parmSpecs do:
		[:argName :spec |
			declarations
				at: argName
				put: (spec ccgDeclareCForVar: argName)].
	aStmt isAssignment ifTrue:
		[declarations
			at: aStmt variable name
			put: (rcvrSpec ccgDeclareCForVar: aStmt variable name).
		 sStream nextPutAll: (self
			statementsFor:
				(rcvrSpec
					ccg:		SmartSyntaxPluginCodeGenerator new
					prolog:  [:expr | aStmt variable name, ' := ', expr]
					expr: 	aStmt variable name
					index: 	(fullArgs size))
			varName: '')].

	"only add the failure guard if there are args or it is an assignment"
	(fullArgs isEmpty not or:[aStmt isAssignment]) ifTrue:[self generateFailureGuardOn: sStream].
	^true.
! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'TPR 2/10/2000 17:43'!
isPrimitiveDirectiveSend: stmt
	
	stmt isSend ifTrue:
		[stmt selector = #primitive: ifTrue:
			[^self primitive: 	stmt args first value
				   parameters:	(Array new: args size withAll: #Oop)
				   receiver:		#Oop].
		 stmt selector = #primitive:parameters: ifTrue:
			[^self primitive: 	stmt args first value
				   parameters: 	stmt args second value
				   receiver:		#Oop].
		 stmt selector = #primitive:parameters:receiver: ifTrue:
			[^self primitive:		stmt args first value
				   parameters:	stmt args second value
				   receiver:		stmt args third value].
		^false].
	^false.
! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'ar 4/4/2006 21:10'!
namedPrimitiveProlog

	| cg |
	cg := SmartSyntaxPluginCodeGenerator new.
	^Array streamContents: [:sStream |
		1 to: fullArgs size do:
			[:i |
			 sStream nextPutAll: 
				(self 
					statementsFor: 
						((parmSpecs at: i) 
							ccg: 	cg
							prolog:  [:expr | (fullArgs at: i), ' := ', expr]
							expr: (fullArgs at: i)
							index: (fullArgs size - i))
					varName: '')]]! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 12/15/1999 05:50'!
nullReturnExpr

	^ TReturnNode new
		setExpression: (TVariableNode new setName: 'null')! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 9/20/1999 14:04'!
oldReplaceSizeMessages
	"Replace sends of the message 'size' with calls to sizeOfSTArrayFromCPrimitive."

	| argExpr |
	parseTree nodesDo: [:n |
		(n isSend and: [n selector = #size]) ifTrue: [
			argExpr := TSendNode new
				setSelector: #+
				receiver: n receiver
				arguments: (Array with: (TConstantNode new setValue: 1)).
			n
				setSelector: #sizeOfSTArrayFromCPrimitive:
				receiver: (TVariableNode new setName: 'interpreterProxy')
				arguments: (Array with: argExpr)]].
! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'ikp 3/31/2005 14:23'!
oopVariable: aString

	(locals includes: aString) ifFalse:
		[locals add: aString.
		 declarations
			at: aString 
			put: 'sqInt ', aString].
	^TVariableNode new setName: aString! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'acg 9/19/1999 11:18'!
parmSpecs

	^parmSpecs! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 12/15/1999 05:55'!
pop: anInteger thenReturnExpr: anExpression

	^TSendNode new
		setSelector: #pop:thenPush:
		receiver: (TVariableNode new setName: 'interpreterProxy')
		arguments: (Array 
			with: (TConstantNode new 
				setValue: anInteger)
			with: anExpression)! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'acg 12/15/1999 05:45'!
popExpr: anInteger

	^ TSendNode new
			 setSelector: #pop:
			 receiver: (TVariableNode new setName: 'interpreterProxy')
			 arguments: (Array 
				with: (TConstantNode new 
					setValue: anInteger))! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'dtl 2/3/2007 11:29'!
primitive: aString parameters: anArray receiver: aClassSymbol

	self selectorOverride: aString asSymbol.
	anArray size == args size ifFalse: 
		[^self error: self selectorOverride, ': incorrect number of parameter specifications'].
	parmSpecs := anArray collect:
		[:each | Smalltalk at: each ifAbsent:
			[^self error: self selectorOverride, ': parameter spec must be a Behavior']].
	parmSpecs do: [:each | each isBehavior ifFalse:
		[^self error: self selectorOverride, ': parameter spec must be a Behavior']].
	rcvrSpec := Smalltalk at: aClassSymbol asSymbol ifAbsent:
		[^self error: self selectorOverride, ': receiver spec must be a Behavior'].
	rcvrSpec isBehavior ifFalse:
		[^self error: self selectorOverride, ': receiver spec must be a Behavior'].
	^true! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'acg 9/17/1999 22:41'!
primitiveDirectiveWasHandled: stmt on: sStream

	(self isPrimitiveDirectiveSend: stmt) ifTrue:
		[^self handlePrimitiveDirective: stmt on: sStream].
	(stmt isAssignment and: 
		[self isPrimitiveDirectiveSend: stmt expression]) ifTrue:
			[^self handlePrimitiveDirective: stmt on: sStream].
	^false.
! !

!SmartSyntaxPluginTMethod methodsFor: 'private' stamp: 'TPR 3/2/2000 12:39'!
printTempsAndVar: varName on: aStream 
	"add the required temps and the varname to the stream"
	aStream nextPutAll: '| '.
	(#('rcvr' 'stackPointer' 'successFlag' 'interpreterProxy' ) reject: [:each | locals includes: each])
		do: [:each | aStream nextPutAll: each;
			 space].
	(locals reject: [:each | each first = $_])
		do: [:each | aStream nextPutAll: each;
			 space].
"don't add varName twice. Probably a deeper reason for this, but WTH. TPR"
	(locals includes: varName) ifFalse:[aStream nextPutAll: varName].
	aStream nextPutAll: '|';
	 cr! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'acg 9/19/1999 11:50'!
rcvrSpec

	^rcvrSpec! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'ar 3/10/2000 21:18'!
recordDeclarations
	"Record C type declarations of the forms

		self returnTypeC: 'float'.
		self var: #foo declareC: 'float foo'
		self var: #foo as: Class
		self var: #foo type: 'float'.

	 and remove the declarations from the method body."

	| newStatements isDeclaration theClass varName varType |
	newStatements := OrderedCollection new: parseTree statements size.
	parseTree statements do: 
		[:stmt |
		 isDeclaration := false.
		 stmt isSend ifTrue: 
			[stmt selector = #var:declareC: ifTrue:
				[isDeclaration := true.
				declarations at: stmt args first value asString put: stmt args last value].
			stmt selector = #var:type: ifTrue: [
				isDeclaration := true.
				varName := stmt args first value asString.
				varType := stmt args last value.
				declarations at: varName put: (varType, ' ', varName).
			].
			 stmt selector = #var:as: ifTrue:
				[isDeclaration := true.
				 theClass := Smalltalk 
					at: stmt args last name asSymbol
					ifAbsent: [^self error: 'declarator must be a Behavior'].
				 (theClass isKindOf: Behavior)
					ifFalse: [^self error: 'declarator must be a Behavior'].
				 declarations 
					at: stmt args first value asString 
					put: (theClass ccgDeclareCForVar: stmt args first value asString)].
			 stmt selector = #returnTypeC: ifTrue: 
				[isDeclaration := true.
				 returnType := stmt args last value]].
		 isDeclaration ifFalse: [newStatements add: stmt]].
	parseTree setStatements: newStatements asArray! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'acg 9/17/1999 18:18'!
removeFinalSelfReturn
	"The Smalltalk parser automatically adds the statement '^self' to the end of methods without explicit returns. This method removes such statements, since the generated code has no notion of 'self' anyway."

	| stmtList lastStmt |
	stmtList := parseTree statements asOrderedCollection.
	lastStmt := stmtList last.

	((lastStmt isReturn) and:
	 [(lastStmt expression isVariable) and:
	 ['self' = lastStmt expression name]]) ifTrue: [
		stmtList removeLast.
		parseTree setStatements: stmtList.
	].! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'TPR 3/2/2000 19:07'!
replaceArraySizeMessages
	"Replace sends of the message 'size' with calls to sizeOfSTArrayFromCPrimitive. Specialised version for generating primitives outside a plugin"

	super replaceSizeMessages
! !

!SmartSyntaxPluginTMethod methodsFor: 'transforming' stamp: 'acg 9/20/1999 14:04'!
replaceSizeMessages
	"Replace sends of the message 'size' with calls to sizeOfSTArrayFromCPrimitive."

	parseTree nodesDo: [:n |
		(n isSend and: [n selector = #size]) ifTrue: [
			n
				setSelector: #sizeOfSTArrayFromCPrimitive:
				receiver: (TVariableNode new setName: 'interpreterProxy')
				arguments: (Array with: n receiver)]].
! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'dtl 2/3/2007 11:23'!
selectorForCodeGeneration
	"A primitive directive my override the selector specification."

	^ self selectorOverride ifNil: [self selector]! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'dtl 2/3/2007 11:21'!
selectorOverride
	"A primitive directive allows the selector to be overridden in order to specify
	a preferred name for the generated primitive."

	^ selectorOverride! !

!SmartSyntaxPluginTMethod methodsFor: 'accessing' stamp: 'dtl 2/3/2007 11:21'!
selectorOverride: preferredSelectorName
	"A primitive directive allows the selector to be overridden in order to specify
	a preferred name for the generated primitive."

	selectorOverride := preferredSelectorName! !

!SmartSyntaxPluginTMethod methodsFor: 'initializing' stamp: 'ikp 3/31/2005 14:01'!
setSelector: sel args: argList locals: localList block: aBlockNode primitive: aNumber
	"Initialize this method using the given information."

	selector := sel.
	returnType := 'sqInt'. 	 "assume return type is sqInt for now"
	args := argList asOrderedCollection collect: [:arg | arg key].
	locals := localList asOrderedCollection collect: [:arg | arg key].
	declarations := Dictionary new.
	primitive := aNumber.
	parseTree := aBlockNode asTranslatorNode.
	labels := OrderedCollection new.
	complete := false.  "set to true when all possible inlining has been done"
	export := self extractExportDirective.
	static := self extractStaticDirective.
	self extractSharedCase.
	isPrimitive := false.  "set to true only if you find a primtive direction."
	suppressingFailureGuards := self extractSuppressFailureGuardDirective.
	self recordDeclarations.
	self extractPrimitiveDirectives.
! !

!SmartSyntaxPluginTMethod methodsFor: 'specifying primitives' stamp: 'tpr 6/9/2003 16:38'!
simulatePrologInContext: aContext

	|cg instructions |
	cg := SmartSyntaxPluginCodeGenerator new.
	parmSpecs keysAndValuesDo: 
		[:index :each |
		 instructions := ((parmSpecs at: index)
			ccg: cg 
			prolog: (cg ccgTVarBlock: index) 
			expr: '<foo>' 
			index: args size - index).
		 Compiler new 
			evaluate: instructions
			in: aContext 
			to: aContext receiver
			notifying: nil
			ifFail: nil].
	instructions := (rcvrSpec
		ccg: cg 
		prolog: [:expr | '^', expr]
		expr: '<foo>' 
		index: args size).
	 ^Compiler new 
		evaluate: instructions
		in: aContext 
		to: aContext receiver
		notifying: nil
		ifFail: nil! !

!SmartSyntaxPluginTMethod methodsFor: 'primitive compilation' stamp: 'TPR 3/2/2000 16:07'!
vmNameString
	"return the string to use as the vm name in code generated for this method"
	^'interpreterProxy'! !

!SmartSyntaxPluginTMethod class methodsFor: 'as yet unclassified' stamp: 'acg 9/19/1999 11:47'!
fromContext: aContext primitive: aString parameters: aClassList receiver: aClass

	^super new 
		fromContext: aContext 
		primitive: aString 
		parameters: aClassList 
		receiver: aClass
! !

!TParseNode methodsFor: 'as yet unclassified'!
allCalls
	"Answer a collection of selectors for the messages sent in this parse tree."

	| calls |
	calls := Set new: 100.
	self nodesDo: [ :node |
		node isSend ifTrue: [ calls add: node selector ].
	].
	^calls! !

!TParseNode methodsFor: 'converting' stamp: 'dtl 10/28/2006 18:50'!
asExpression
	"Sender expects to use this node as a simple expression rather than as as statement"

	^self! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'jm 12/10/1998 18:13'!
bindVariableUsesIn: aDictionary
	"Bind uses of all variables in the given dictionary, but do not change variables that appear on the left-hand-side of an assignment statement."

	^ self
! !

!TParseNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	^self! !

!TParseNode methodsFor: 'emit comments' stamp: 'hg 8/14/2000 15:32'!
comment: aComment

	comment := aComment ! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'nk 4/5/2005 21:01'!
copyTree
	self subclassResponsibility.! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'nk 4/5/2005 21:02'!
emitCCodeOn: aStream level: lev generator: gen
	self subclassResponsibility.! !

!TParseNode methodsFor: 'emit comments' stamp: 'hg 8/14/2000 16:13'!
emitCCommentOn: aStream level: level
	"Emit the transferred Smalltalk comments as C comments."

	comment ifNotNil: [
		comment isString ifTrue: [^self].	"safety catch"
		aStream cr.
		1 to: comment size do: [:index | 
			aStream 
				tab: level;
				nextPutAll: '/* ';
				nextPutAll: (comment at: index);
				nextPutAll: ' */';
				cr].
		aStream cr]! !

!TParseNode methodsFor: 'as yet unclassified'!
hasExplicitReturn

	self nodesDo: [ :node |
		node isReturn ifTrue: [ ^true ].
	].
	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	self! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
isAssertion
	^false! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
isAssignment

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isCaseStmt

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isComment

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isConstant

	^false! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'dtl 10/24/2006 21:56'!
isDirective
	"Preprocessor directive, e.g. a cpp macro"

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isGoTo

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isLabel

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isLeaf
	"Answer true if the receiver is a variable or a constant node."

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isReturn

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isSend

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isStmtList

	^false! !

!TParseNode methodsFor: 'as yet unclassified'!
isVariable

	^false! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'acg 12/17/1999 07:21'!
nameOrValue

	self error: 'object is neither variable nor constant'! !

!TParseNode methodsFor: 'as yet unclassified'!
nodeCount
	"Answer the number of nodes in this parseTree (a rough measure of its size)."

	| cnt |
	cnt := 0.
	self nodesDo: [ :n | cnt := cnt + 1 ].
	^cnt! !

!TParseNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	aBlock value: self.! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'JMM 11/23/2002 23:25'!
nodesVarCheckDo: aBlock

	aBlock value: self.! !

!TParseNode methodsFor: 'as yet unclassified'!
printOn: aStream 
	"Append a description of the receiver onto the given stream."

	self printOn: aStream level: 0.! !

!TParseNode methodsFor: 'as yet unclassified'!
printOn: aStream level: anInteger 
	"Typically overridden. If control actually gets here, avoid recursion loop by sending to super."

	super printOn: aStream.! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	"default: do nothing"! !

!TParseNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [self]! !

!TParseNode methodsFor: 'as yet unclassified' stamp: 'dtl 3/20/2005 14:23'!
requiresCLineTerminator

	^ self isComment not
! !

!TAssignmentNode methodsFor: 'inlining' stamp: 'mn 6/30/2000 13:19'!
bindVariableUsesIn: aDictionary
	"Do NOT bind the variable on the left-hand-side of an assignment statement."
	"was bindVariablesIn:"
	expression := expression bindVariableUsesIn: aDictionary.
! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	variable := variable bindVariablesIn: aDictionary.
	expression := expression bindVariablesIn: aDictionary.! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setVariable: variable copyTree
		expression: expression copyTree! !

!TAssignmentNode methodsFor: 'as yet unclassified' stamp: 'dtl 11/7/2006 15:40'!
emitCCodeOn: aStream level: level generator: aCodeGen

	| sel |
	self isVariableUpdatingAssignment ifTrue: [
		variable emitCCodeOn: aStream level: level generator: aCodeGen.
		sel := expression selector.
		sel = #+
			ifTrue: [aStream nextPutAll: ' += ']
			ifFalse: [aStream nextPutAll: ' -= '].
			expression args first asExpression
				emitCCodeOn: aStream level: level generator: aCodeGen.
	] ifFalse: [
		variable emitCCodeOn: aStream level: level generator: aCodeGen.
		aStream nextPutAll: ' = '.
		expression asExpression emitCCodeOn: aStream level: level generator: aCodeGen.
	].! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
expression

	^expression! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	variable inlineMethodsUsing: aDictionary.
	expression inlineMethodsUsing: aDictionary.! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
isAssignment

	^true! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
isVariableUpdatingAssignment
	"Return true if this assignment statement is of one of the forms:
		var = var + ...
		var = var - ...
	Such assignments statements can exploit the C updating assignment operators. For example, 'x += 4' can be generated instead of 'x = x + 4'. This produces better code under some C compilers, most notably the CodeWarrior 68K compiler."

	| sel |
	(expression isSend and: [expression receiver isVariable]) ifFalse: [^ false].
	sel := expression selector.
	^ (expression receiver name = variable name) and: [(sel = #+) or: [sel = #-]]! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	variable nodesDo: aBlock.
	expression nodesDo: aBlock.
	aBlock value: self.! !

!TAssignmentNode methodsFor: 'as yet unclassified' stamp: 'tpr 4/12/2006 12:24'!
printOn: aStream level: level

	variable printOn: aStream level: level.
	aStream nextPutAll: ' := '.
	expression printOn: aStream level: level + 2.! !

!TAssignmentNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions

	expression removeAssertions! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [
		variable := variable replaceNodesIn: aDictionary.
		expression := expression replaceNodesIn: aDictionary.
		self]! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
setVariable: varNode expression: expressionNode

	variable := varNode.
	expression := expressionNode.! !

!TAssignmentNode methodsFor: 'as yet unclassified'!
variable

	^variable! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'dtl 7/3/2008 09:37'!
assumesCurrentBytecodeMatchesCaseValue: statementList
	"Test for the special case of certain methods that rely on the value of
	the currentBytecode variable to be expanded to a constant. This is a
	performance optimization that permits the next bytecode to be fetched
	early while not affecting the prior value of currentBytecode, which
	will have been translated to a constant.
	
	See senders of #'requires currentBytecode to be expanded to a constant'
	for methods that rely on this behavior."

	| trickySelectors commentString firstLine |
	trickySelectors := {
			#pushLiteralConstantBytecode .
			#pushLiteralVariableBytecode .
			#pushReceiverVariableBytecode .
			#pushTemporaryVariableBytecode .
			#storeAndPopTemporaryVariableBytecode .
			#storeAndPopReceiverVariableBytecode
		}.
	firstLine := statementList statements first.
	firstLine isComment ifTrue: [
		commentString := firstLine asString.
		trickySelectors detect: [:e | commentString includesSubString: e]
			ifNone: [^ false].
		^ true].
	^ false! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'jm 12/10/1998 18:26'!
bindVariableUsesIn: aDictionary

	expression := expression bindVariableUsesIn: aDictionary.
	cases := cases collect: [ :c | c bindVariableUsesIn: aDictionary ].! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	expression := expression bindVariablesIn: aDictionary.
	cases := cases collect: [ :c | c bindVariablesIn: aDictionary ].! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
cases

	^cases! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setExpression: expression copyTree
		firsts: firsts copy
		lasts: lasts copy
		cases: (cases collect: [ :case | case copyTree ])! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'ar 7/6/2003 23:34'!
customizeCase: caseParseTree forVar: varName from: firstIndex to: lastIndex in: codeGen method: aTMethod
	"Return a collection of copies of the given parse tree, each of which has the value of the case index substituted for the given variable."

	| newCases dict newCase |
	newCases := OrderedCollection new.
	firstIndex to: lastIndex do: [ :caseIndex |
		dict := Dictionary new.
		dict at: varName put: (TConstantNode new setValue: caseIndex).
		newCase := caseParseTree copyTree bindVariableUsesIn: dict.
		self processSharedCodeBlocks: newCase forCase: caseIndex in: codeGen method: aTMethod.
		newCases addLast: newCase.
	].
	^ newCases! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'dtl 7/3/2008 09:57'!
customizeShortCasesForDispatchVar: varName in: codeGen method: aTMethod
	"Make customized versions of a short bytecode methods, substituting a constant having the case index value for the given variable. This produces better code for short bytecodes such as instance variable pushes that encode the index of the instance variable in the bytecode."

	| newFirsts newLasts newCases l f case expanded |
	newFirsts := OrderedCollection new.
	newLasts := OrderedCollection new.
	newCases := OrderedCollection new.
	1 to: cases size do: [ :i |
		l := lasts at: i.
		f := firsts at: i.
		case := cases at: i.
		expanded := false.
		(l - f) > 1 ifTrue: [  "case code covers multiple cases"
			(case nodeCount < 60
				or: [self assumesCurrentBytecodeMatchesCaseValue: case]) ifTrue: [
					newFirsts addAll: (f to: l) asArray.
					newLasts addAll: (f to: l) asArray.
					newCases addAll: (self customizeCase: case forVar: varName from: f to: l in: codeGen method: aTMethod).
					expanded := true
			]
		].
		expanded ifFalse: [
			self processSharedCodeBlocks: case forCase: f in: codeGen method: aTMethod.
			newFirsts addLast: f.
			newLasts addLast: l.
			newCases addLast: case.
		]
	].
	firsts := newFirsts asArray.
	lasts := newLasts asArray.
	cases := newCases asArray.
! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'dtl 11/9/2006 06:29'!
emitCCodeOn: aStream level: level generator: aCodeGen

	| indent |
	indent := (String new: level) collect: [ :ch | Character tab ].
	aStream nextPutAll: 'switch ('.
	expression asExpression emitCCodeOn: aStream level: level generator: aCodeGen.
	aStream nextPutAll: ') {'; cr.
	1 to: cases size do: [ :i |
		(firsts at: i) to: (lasts at: i) do: [ :caseIndex |
			aStream nextPutAll: indent, 'case ', caseIndex printString, ':'; cr.
		].
		(cases at: i) emitCCodeOn: aStream level: level + 1 generator: aCodeGen.
		aStream nextPutAll: indent; tab; nextPutAll: 'break;'.
		aStream cr.
	].
	level timesRepeat: [ aStream tab ].
	aStream nextPutAll: '}'.! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
expression

	^expression! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	expression inlineMethodsUsing: aDictionary.
	cases do: [ :c | c inlineMethodsUsing: aDictionary ].! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
isCaseStmt

	^true! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	expression nodesDo: aBlock.
	cases do: [ :c | c nodesDo: aBlock ].
	aBlock value: self.! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	aStream crtab: level.
	aStream nextPutAll: 'select '.
	expression printOn: aStream level: level.
	aStream nextPutAll: ' in'.
	1 to: cases size do: [ :i |
		(firsts at: i) to: (lasts at: i) do: [ :caseIndex |
			aStream crtab: level.
			aStream nextPutAll: 'case ', caseIndex printString, ':'.
		].
		aStream crtab: level + 1.
		(cases at: i) printOn: aStream level: level + 1.
	].
	aStream crtab: level.
	aStream nextPutAll: 'end select'.! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'ar 7/7/2003 00:18'!
processSharedCodeBlocks: caseTree forCase: caseIndex in: codeGen method: aTMethod
	"Process any shared code blocks in the case parse tree for the given case, either inlining them or making them a 'goto sharedLabel'."
	| map meth sharedNode exitLabel |
	exitLabel := nil.

	[sharedNode := nil.
	map := IdentityDictionary new.
	caseTree nodesDo:[:node|
		(node isSend 
			and:[(meth := codeGen methodNamed: node selector) notNil
			and:[meth sharedCase notNil]]) ifTrue:[
			meth sharedCase = caseIndex ifTrue:[
				sharedNode := meth.
				map at: node put: (TLabeledCommentNode new setComment: 'goto ', meth sharedLabel).
			] ifFalse:[
				map at: node put: (TGoToNode new setLabel: meth sharedLabel).
			].
		].
	].
	caseTree replaceNodesIn: map.
	"recursively expand"
	sharedNode == nil] whileFalse:[
		meth := sharedNode copy.
		(meth hasReturn) ifTrue: [
			exitLabel ifNil:[
				exitLabel := aTMethod unusedLabelForInliningInto: aTMethod.
				aTMethod labels add: exitLabel.
			].
			meth exitVar: nil label: exitLabel.
		].
		meth renameLabelsForInliningInto: aTMethod.
		aTMethod labels addAll: meth labels.
		caseTree setStatements: (caseTree statements copyWith: meth asInlineNode).
	].
	exitLabel ifNotNil:[
		caseTree setStatements: (caseTree statements copyWith:
			(TLabeledCommentNode new setLabel: exitLabel comment: 'end case')).

	].! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	expression removeAssertions.
	cases do: [ :case | case removeAssertions ].! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [
		expression := expression replaceNodesIn: aDictionary.
		cases := cases collect: [ :c | c replaceNodesIn: aDictionary ].
		self]! !

!TCaseStmtNode methodsFor: 'as yet unclassified'!
setExpression: aNode firsts: firstsList lasts: lastsList cases: caseList

	expression := aNode.
	firsts := firstsList.
	lasts := lastsList.
	cases := caseList.! !

!TCaseStmtNode methodsFor: 'as yet unclassified' stamp: 'ar 11/18/1999 20:08'!
setExpression: aNode selectors: selectorList arguments: anArray
	"Initialize the node from the given set of selectors."
	"Note: Each case is a statement list with containing one statement, a send to self of a selector from the given selector list. Having statement list nodes makes inlining easier later."

	| selfNode stmt lastSel firstInRun sel |
	expression := aNode.
	selfNode := TVariableNode new setName: 'self'.
	firsts := OrderedCollection new: 400.
	lasts := OrderedCollection new: 400.
	cases := OrderedCollection new: 400.
	lastSel := selectorList first.
	firstInRun := 0.
	1 to: selectorList size do: [ :i |
		sel := selectorList at: i.
		sel ~= lastSel ifTrue: [
			firsts add: firstInRun.
			lasts add: i - 2.
			stmt := TSendNode new setSelector: lastSel receiver: selfNode arguments: anArray.
			cases add: (TStmtListNode new setArguments: #() statements: (Array with: stmt)).
			lastSel := sel.
			firstInRun := i - 1.
		].
	].
	firsts add: firstInRun.
	lasts add: selectorList size - 1.
	stmt := TSendNode new setSelector: lastSel receiver: selfNode arguments: anArray.
	cases add: (TStmtListNode new setArguments: #() statements: (Array with: stmt)).! !

!TConstantNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setValue: value! !

!TConstantNode methodsFor: 'as yet unclassified'!
emitCCodeOn: aStream level: level generator: aCodeGen
	"Emit a C literal."

	aStream nextPutAll: (aCodeGen cLiteralFor: value).! !

!TConstantNode methodsFor: 'as yet unclassified'!
isConstant

	^true! !

!TConstantNode methodsFor: 'as yet unclassified'!
isLeaf

	^true! !

!TConstantNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:04'!
name
	^''! !

!TConstantNode methodsFor: 'as yet unclassified' stamp: 'acg 12/17/1999 07:19'!
nameOrValue

	^value! !

!TConstantNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	value storeOn: aStream.! !

!TConstantNode methodsFor: 'as yet unclassified'!
setValue: anObject

	value := anObject.! !

!TConstantNode methodsFor: 'as yet unclassified'!
value

	^value! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:11'!
copyTree

	^self class new
		setName: name value: value! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:08'!
emitCCodeOn: aStream level: level generator: aCodeGen
	"Emit a C literal."

	aStream nextPutAll: name.! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:04'!
name
	^name! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:05'!
name: aName
	name := aName! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:08'!
nameOrValue
	^name! !

!TDefineNode methodsFor: 'as yet unclassified' stamp: 'ar 7/8/2003 11:10'!
setName: aName value: aValue
	self name: aName.
	self setValue: aValue.! !

!TGoToNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new setLabel: label! !

!TGoToNode methodsFor: 'as yet unclassified'!
emitCCodeOn: aStream level: level generator: aCodeGen
	"Emit a C goto statement."

	aStream nextPutAll: 'goto '.
	aStream nextPutAll: label.! !

!TGoToNode methodsFor: 'as yet unclassified'!
isGoTo

	^true! !

!TGoToNode methodsFor: 'as yet unclassified'!
label

	^label! !

!TGoToNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	aStream nextPutAll: 'goto '.
	aStream nextPutAll: label.! !

!TGoToNode methodsFor: 'as yet unclassified'!
setLabel: aString

	label := aString.! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:34'!
bindVariableUsesIn: aDictionary
	method := method bindVariableUsesIn: aDictionary.
! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:15'!
copyTree

	^self class new
		method: method copy! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:19'!
emitCCodeOn: aStream level: level generator: aCodeGen
	method emitInlineOn: aStream level: level generator: aCodeGen.
! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:08'!
method
	^method! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:08'!
method: aTMethod
	method := aTMethod! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 20:36'!
nodesDo: aBlock
	method parseTree nodesDo: aBlock.
	aBlock value: self.! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 22:00'!
printOn: aStream level: anInteger
	method parseTree printOn: aStream level: anInteger! !

!TInlineNode methodsFor: 'accessing' stamp: 'ar 7/6/2003 21:30'!
replaceNodesIn: map
	^map at: self ifAbsent: [
		method replaceNodesIn: map.
		self]! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setLabel: label
		comment: comment! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
emitCCodeOn: aStream level: level generator: aCodeGen
	"Emit a C comment with optional label."

	self printOptionalLabelOn: aStream.
	aStream nextPutAll: '/* '.
	aStream nextPutAll: comment.
	aStream nextPutAll: ' */'.! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
isComment
	"Answer true if the receiver is just a comment (i.e., it has no label)."

	^label = nil! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
isLabel

	^true! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
isLeaf

	^true! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
label

	^label! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	self printOptionalLabelOn: aStream.
	aStream nextPut: $".
	aStream nextPutAll: comment.
	aStream nextPut: $".! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
printOptionalLabelOn: aStream

	label ~= nil ifTrue: [
		self unindentOneTab: aStream.
		aStream nextPutAll: label.
		aStream nextPut: $:.
		aStream tab.	
	].! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
setComment: commentString

	label := nil.
	comment := commentString.! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
setLabel: labelString

	label := labelString.! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
setLabel: labelString comment: commentString

	label := labelString.
	comment := commentString.! !

!TLabeledCommentNode methodsFor: 'as yet unclassified'!
unindentOneTab: aStream
	"Remove the last tab from the given stream if possible."

	(aStream isKindOf: ReadWriteStream) ifFalse: [ ^self ].
	aStream position > 0 ifTrue: [
		aStream position: aStream position - 1.
		"restore stream position if previous char was not a tab"
		aStream peek = Character tab ifFalse: [ aStream next ].
	].! !

!TReturnNode methodsFor: 'as yet unclassified' stamp: 'jm 12/10/1998 18:26'!
bindVariableUsesIn: aDictionary

	expression := expression bindVariableUsesIn: aDictionary.! !

!TReturnNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	expression := expression bindVariablesIn: aDictionary.! !

!TReturnNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setExpression: expression copyTree! !

!TReturnNode methodsFor: 'as yet unclassified' stamp: 'dtl 11/7/2006 11:49'!
emitCCodeOn: aStream level: level generator: aCodeGen

	aStream nextPutAll: 'return '.
	expression asExpression emitCCodeOn: aStream level: level generator: aCodeGen.! !

!TReturnNode methodsFor: 'as yet unclassified'!
expression

	^expression! !

!TReturnNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	expression := expression inlineMethodsUsing: aDictionary.! !

!TReturnNode methodsFor: 'as yet unclassified'!
isReturn

	^true! !

!TReturnNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	expression nodesDo: aBlock.
	aBlock value: self.! !

!TReturnNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	aStream nextPut: $^.
	expression printOn: aStream level: level.! !

!TReturnNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	expression removeAssertions! !

!TReturnNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [
		expression := expression replaceNodesIn: aDictionary.
		self]! !

!TReturnNode methodsFor: 'as yet unclassified'!
setExpression: aNode

	expression := aNode.! !

!TSendNode methodsFor: 'as yet unclassified'!
args

	^arguments! !

!TSendNode methodsFor: 'converting' stamp: 'dtl 11/7/2006 12:09'!
asExpression
	"Sender expects to use this node as a simple expression rather than as as statement"

	self isExpression: true.
	^self! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'jm 12/10/1998 18:26'!
bindVariableUsesIn: aDictionary

	receiver := receiver bindVariableUsesIn: aDictionary.
	arguments := arguments collect: [ :a | a bindVariableUsesIn: aDictionary ].! !

!TSendNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	receiver := receiver bindVariablesIn: aDictionary.
	arguments := arguments collect: [ :a | a bindVariablesIn: aDictionary ].! !

!TSendNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setSelector: selector
		receiver: receiver copyTree
		arguments: (arguments collect: [ :arg | arg copyTree ])
		isBuiltInOp: isBuiltinOperator! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'jm 12/14/1998 08:36'!
emitCCodeOn: aStream level: level generator: aCodeGen

	"If the selector is a built-in construct, translate it and return"
	(aCodeGen emitBuiltinConstructFor: self on: aStream level: level) ifTrue: [ ^self ].

	"Special case for pluggable modules. Replace messages to interpreterProxy
	by interpreterProxy->message(..) if the message is not builtin"
	(aCodeGen isGeneratingPluginCode and:[
		receiver isVariable and:[
			'interpreterProxy' = receiver name and:[
				self isBuiltinOperator not]]]) 
		ifTrue:[aStream nextPutAll:'interpreterProxy->'].
	"Translate this message send into a C function call."
	aStream nextPutAll: (aCodeGen cFunctionNameFor: selector), '('.
	(receiver isVariable and:
	 [('self' = receiver name) or: ['interpreterProxy' = receiver name]]) ifFalse: [
		"self is omitted from the arguments list of the generated call"
		"Note: special case for translated BitBltSimulator--also omit
		 the receiver if this is a send to the variable 'interpreterProxy'"
		receiver emitCCodeOn: aStream level: level generator: aCodeGen.
		arguments isEmpty ifFalse: [ aStream nextPutAll: ', ' ].
	].
	1 to: arguments size do: [ :i |
		(arguments at: i) emitCCodeOn: aStream level: level generator: aCodeGen.
		i < arguments size ifTrue: [ aStream nextPutAll: ', ' ].
	].
	aStream nextPutAll: ')'.! !

!TSendNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	arguments := arguments collect: [ :arg |
		arg inlineMethodsUsing: aDictionary.
	].
	"xxx inline this message if it is in the dictionary xxx"! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
isAssertion
	^(selector beginsWith: 'assert') or: [selector beginsWith: 'verify']! !

!TSendNode methodsFor: 'as yet unclassified'!
isBuiltinOperator

	^ isBuiltinOperator! !

!TSendNode methodsFor: 'as yet unclassified'!
isBuiltinOperator: builtinFlag

	isBuiltinOperator := builtinFlag.! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'dtl 10/24/2006 22:00'!
isDirective
	"Preprocessor directive, e.g. a cpp macro"

	^ {	#preprocessorExpression: .
		#isDefined:inSmalltalk:comment:ifTrue:ifFalse: .
		#isDefined:inSmalltalk:comment:ifTrue:
		} identityIncludes: selector! !

!TSendNode methodsFor: 'accessing' stamp: 'dtl 11/11/2009 20:59'!
isExpression
	"Answer the value of isExpression. True if this node is expected to be used
	as an expression. Normally false but may be set by #asExpression to indicate
	that this node should be translated as a simple expression rather than as
	a statement."

	^ isExpression ifNil: [isExpression := false]! !

!TSendNode methodsFor: 'accessing' stamp: 'dtl 11/7/2006 12:06'!
isExpression: anObject
	"Set the value of isExpression"

	isExpression := anObject! !

!TSendNode methodsFor: 'as yet unclassified'!
isSend

	^true! !

!TSendNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	receiver nodesDo: aBlock.
	arguments do: [ :arg | arg nodesDo: aBlock ].
	aBlock value: self.! !

!TSendNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	| keywords |
	receiver printOn: aStream level: level.
	arguments size = 0 ifTrue: [
		aStream space; nextPutAll: selector.
		^self
	].
	keywords := selector keywords.
	1 to: keywords size do: [ :i |
		aStream space.
		aStream nextPutAll: (keywords at: i); space.
		(arguments at: i) printOn: aStream level: level + 1.
	].! !

!TSendNode methodsFor: 'as yet unclassified'!
receiver

	^receiver! !

!TSendNode methodsFor: 'as yet unclassified'!
receiver: aNode

	receiver := aNode.! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	receiver removeAssertions.
	arguments do: [:arg | arg removeAssertions].! !

!TSendNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [
		receiver := receiver replaceNodesIn: aDictionary.
		arguments := arguments collect: [ :a | a replaceNodesIn: aDictionary ].
		self]! !

!TSendNode methodsFor: 'as yet unclassified' stamp: 'dtl 10/24/2006 22:05'!
requiresCLineTerminator

	^ (self isComment or: [self isDirective]) not
! !

!TSendNode methodsFor: 'as yet unclassified'!
selector

	^selector! !

!TSendNode methodsFor: 'as yet unclassified'!
setSelector: aSymbol receiver: rcvrNode arguments: argList

	selector := aSymbol.
	receiver := rcvrNode.
	arguments := argList asArray.
	isBuiltinOperator := false.! !

!TSendNode methodsFor: 'as yet unclassified'!
setSelector: aSymbol receiver: rcvrNode arguments: argList isBuiltInOp: builtinFlag

	selector := aSymbol.
	receiver := rcvrNode.
	arguments := argList asArray.
	isBuiltinOperator := builtinFlag.! !

!TStmtListNode methodsFor: 'as yet unclassified'!
args

	^arguments! !

!TStmtListNode methodsFor: 'converting' stamp: 'dtl 10/28/2006 18:51'!
asExpression

	^ (self statements size = 1)
		ifTrue: [statements first]
		ifFalse: [self error: 'multiple statements can not serve as a simple expression']! !

!TStmtListNode methodsFor: 'as yet unclassified' stamp: 'jm 12/10/1998 18:26'!
bindVariableUsesIn: aDictionary

	statements := statements collect: [ :s | s bindVariableUsesIn: aDictionary ].! !

!TStmtListNode methodsFor: 'as yet unclassified'!
bindVariablesIn: aDictionary

	statements := statements collect: [ :s | s bindVariablesIn: aDictionary ].! !

!TStmtListNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new
		setArguments: arguments copy
		statements: (statements collect: [ :s | s copyTree ])! !

!TStmtListNode methodsFor: 'as yet unclassified' stamp: 'dtl 10/28/2006 16:42'!
emitCCodeOn: aStream level: level generator: aCodeGen

	statements do: [:s |
		s emitCCommentOn: aStream level: level.
		aStream tab: level.
		s emitCCodeOn: aStream level: level generator: aCodeGen.
		(((self endsWithCloseBracket: aStream) not
			and: [(s isComment) not])
				and: [s requiresCLineTerminator])
			ifTrue: [aStream nextPut: $;].
		aStream cr].
! !

!TStmtListNode methodsFor: 'as yet unclassified' stamp: 'jm 11/25/1998 16:26'!
endsWithCloseBracket: aStream
	"Answer true if the given stream ends in a $} character."

	| ch pos |
	(pos := aStream position) > 0 ifTrue: [
		aStream position: pos - 1.
		ch := aStream next].
	^ ch = $}
! !

!TStmtListNode methodsFor: 'as yet unclassified'!
inlineMethodsUsing: aDictionary

	statements do: [ :s | s inlineMethodsUsing: aDictionary ].! !

!TStmtListNode methodsFor: 'as yet unclassified' stamp: 'acg 12/21/1999 02:25'!
isNilStmtListNode

	|stmt|
	statements size = 1 ifFalse: [^false].
	stmt := statements at: 1.
	^ stmt isVariable and: [stmt name = 'nil']! !

!TStmtListNode methodsFor: 'as yet unclassified'!
isStmtList

	^true! !

!TStmtListNode methodsFor: 'as yet unclassified'!
nodesDo: aBlock

	statements do: [ :s | s nodesDo: aBlock ].	
	aBlock value: self.! !

!TStmtListNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	aStream nextPut: $[.
	arguments size > 0 ifTrue: [
		arguments do: [ :arg | aStream nextPutAll: ' :', arg ].
		aStream nextPutAll: ' | '.
	].
	self printStatementsOn: aStream level: level.
	aStream nextPut: $].! !

!TStmtListNode methodsFor: 'as yet unclassified'!
printStatementsOn: aStream level: level

	statements size > 1 ifTrue: [ aStream crtab: level + 1 ].
	1 to: statements size do: [ :i |
		(statements at: i) printOn: aStream level: level.
		i = statements size ifTrue: [
			(statements size > 1) ifTrue: [
				aStream crtab: level.
			].
		] ifFalse: [
			aStream nextPut: $.; crtab: level + 1.
		].
	].! !

!TStmtListNode methodsFor: 'as yet unclassified' stamp: 'ikp 9/26/97 14:50'!
removeAssertions
	| newStatements |
	newStatements := OrderedCollection new: statements size.
	statements do: [ :stmt |
		stmt isAssertion ifFalse: [
			newStatements add: (stmt removeAssertions; yourself).
		]
	].
	self setStatements: newStatements asArray! !

!TStmtListNode methodsFor: 'as yet unclassified'!
replaceNodesIn: aDictionary

	^aDictionary at: self ifAbsent: [
		statements := statements collect: [ :s | s replaceNodesIn: aDictionary ].
		self]! !

!TStmtListNode methodsFor: 'as yet unclassified'!
setArguments: argList

	arguments := argList.! !

!TStmtListNode methodsFor: 'as yet unclassified'!
setArguments: argList statements: statementList
	"Initialize this method using the given information."

	arguments := argList.
	statements := statementList.! !

!TStmtListNode methodsFor: 'as yet unclassified'!
setStatements: stmtList

	statements := stmtList asOrderedCollection.! !

!TStmtListNode methodsFor: 'as yet unclassified'!
statements

	^statements! !

!TVariableNode methodsFor: 'as yet unclassified' stamp: 'di 6/5/2000 16:56'!
bindVariableUsesIn: aDictionary

	^ (aDictionary at: name ifAbsent: [^ self]) copyTree! !

!TVariableNode methodsFor: 'as yet unclassified' stamp: 'sma 5/24/2000 23:42'!
bindVariablesIn: aDictionary 
	^ (aDictionary at: name ifAbsent: [^ self]) copyTree! !

!TVariableNode methodsFor: 'as yet unclassified'!
copyTree

	^self class new setName: name! !

!TVariableNode methodsFor: 'as yet unclassified' stamp: 'JMM 4/5/2002 14:14'!
emitCCodeOn: aStream level: level generator: aCodeGen

	name = 'nil'
		ifTrue: [ aStream nextPutAll: (aCodeGen cLiteralFor: nil) ]
		ifFalse: [ aStream nextPutAll: (aCodeGen returnPrefixFromVariable: name) ].! !

!TVariableNode methodsFor: 'as yet unclassified'!
isLeaf

	^true! !

!TVariableNode methodsFor: 'as yet unclassified'!
isVariable

	^true! !

!TVariableNode methodsFor: 'as yet unclassified'!
name

	^name! !

!TVariableNode methodsFor: 'as yet unclassified' stamp: 'acg 12/17/1999 07:18'!
nameOrValue

	^name! !

!TVariableNode methodsFor: 'as yet unclassified'!
printOn: aStream level: level

	aStream nextPutAll: name.! !

!TVariableNode methodsFor: 'as yet unclassified'!
setName: aString

	name := aString.! !
CCodeGenerator initialize!
InterpreterPlugin initialize!
TMethod initialize!
