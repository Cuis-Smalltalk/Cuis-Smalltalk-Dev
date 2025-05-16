'From Cuis7.3 [latest update: #7158] on 14 May 2025 at 11:23:56 am'!

!ByteArray reorganize!
('[mk] access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At:bigEndian: int8At:put:bigEndian: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At:bigEndian: uint8At:put:bigEndian:)
('[mk] accessing' indexOf:startingAt:ifAbsent:)
('[mk] converting' asByteArray asString)
('[mk] testing' is:)
('[mk] system primitives' replaceFrom:to:with:startingAt:)
('accessing' atAllPut: bitSize indexOfAnyIn:startingAt:ifAbsent:)
('converting' asHex asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement)
('comparing' = hash)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' isLiteral)
('initialization' readHexFrom:)
!


!Semaphore reorganize!
('[mk] communication' signal wait)
('[mk] mutual exclusion' critical:)
('[mk] comparing' = hash)
('[mk] initialize-release' initSignals)
('[mk] private' species)
('communication' waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('testing' isSignaled)
('initialization' terminateProcess)
('printing' printOn:)
!


!Symbol reorganize!
('[mk] accessing' at:put: replaceFrom:to:with:startingAt:)
('[mk] comparing' =)
('[mk] testing' isSymbol)
('[mk] printing' storeOn:)
('[mk] private' species)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] private' errorNoModification flushCache string:)
('[mkObjectCopying] copying' shallowCopy)
('accessing' precedence)
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!Stream reorganize!
('[mk] writing' nextPutAll: print:)
('[mk] accessing' binary next)
('[mk] normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('[mk] nonhomogeneous accessing' nextString)
('[mk] testing' is:)
('accessing' basicNext contents flush localName next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when: openReadOnly printOn: readOnly)
('testing' atEnd closed isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print:fractionDigits: print:when:)
('FileStream compatibility' close)
('nonhomogeneous accessing' nextNumber nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!WriteStream reorganize!
('[mk] writing' next:into:startingAt: nextPut: nextPutAll: space)
('[mk] accessing' contents next:)
('[mk] positioning' position: reset)
('[mk] private' isCompatibleWithContents: on:)
('accessing' next next:putAll:startingAt: nextPutAllString:withAttributes: nextWordsInto: peek size upTo:delimiterIsTerminator:)
('positioning' resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!ContextPart reorganize!
('[mk] accessing' at: at:put: basicAt: basicAt:put: basicSize)
('[mk] debugger access' methodClass selector sender)
('[mk] printing' printOn:)
('accessing' client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself namedTempAt: namedTempAt:put: print:on: releaseTo: shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printStack:)
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

