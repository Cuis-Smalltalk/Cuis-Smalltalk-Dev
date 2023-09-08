'From Cuis 6.0 [latest update: #5991] on 30 August 2023 at 12:17:41 pm'!

!CompiledMethod methodsFor: 'cleaning' stamp: 'jmv 8/30/2023 12:17:03'!
voidCogVMState
	"Tell the VM to remove all references to any machine code form of the method.
	 This primitive must be called whenever a method is in use and modified.  This is
	 more aggressive (and *much* more costly) than flushCache since it must search
	 through all context objects, making sure that none have a (hidden) machine code pc
	 in the receiver.  Since modifying a method will likely change the generated machine code,
	 modifying a method (rather than redefining it) requires this more aggressive flush.

	If this is needed for a bunch of methods, and not just one, consider calling
	Smalltalk voidCogVMState
	(primitive 214)"

	<primitive: 215>
	^self flushCache! !


!CompiledMethod methodsFor: 'cleaning' stamp: 'jmv 8/30/2023 12:04:05'!
flushCache
	"Tell the virtual machine to remove all references to this method from its method
	 lookup cache(s), if it has them. This is provided for backwards compatibility.
	 When a method is redefined (added to, removed from, or replaced in, a method
	 dictionary) then Symbol flushCache should be used. This is because adding or
	 removing a method may alter the method that a given selector should invoke
	 for methods other than the receiver.  For example, if a subclass inherits a
	 method and this is overridden, flushing the new method will not flush the existing
	 method.

	 If a method is modified in-place (for example, some bytecode is replaced by
	 bytecode that effects a breakpoint) then voidCogVMState should be used."

	<primitive: 116>! !


!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag defaultSelector encoderClass endPC flag frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isGetterOf:at: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: regularLiterals sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: notOptimizedMethodNode)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asMethodReference asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('source code ranges' messageSendsRangesOf:)
('user interface support' browse)
('copying' flattenTo:)
('cleaning' flushCache voidCogVMState)
!

