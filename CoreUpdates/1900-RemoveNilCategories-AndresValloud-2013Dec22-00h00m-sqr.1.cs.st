'From Cuis 4.2 of 25 July 2013 [latest update: #1898] on 22 December 2013 at 12:04:45.383514 am'!

!ProjectX class reorganize!
('as yet unclassified' currentInterruptNameX: interruptNameX: newProcessIfUIX: spawnNewMorphicProcessFor: ui)
!


!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasInstVarRef hasMethodReturn home isExecutingBlock method methodReturnContext outerContext receiver removeSelf tempAt: tempAt:put:)
('closure support' asContext contextTag isClosureContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' pushArgs:from:)
('testing' atEnd isMethodContext)
('private' aboutToReturn:through: cannotReturn: instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-debugger' cachesStack hideFromDebugger)
('private-exceptions' isHandlerContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
!


!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('copying' copy shallowCopy)
('enumerating' collect: do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:by: species valuesInclude:)
('testing' includes: isInterval)
('arithmetic' + -)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:ifAbsent: at:incrementBy: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: integerAt: integerAt:put: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' asArray asByteArray asColorArray asFloatArray asIntegerArray asPointArray asStringWithNewline asWordArray concatenation isSequenceable readStream reverse reversed writeStream @)
('copying' , copyAfter: copyAfterLast: copyEmpty copyFrom:to: copyLast: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: copyWithFirst: copyWithoutFirst copyWithoutIndex: forceTo:paddingStartWith: forceTo:paddingWith: shallowCopy shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: collectWithIndex: combinations:atATimeDo: do: do:separatedBy: do:without: doWithIndex: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeCollect: groupsOf:atATimeDo: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:reverseDo: withIndexCollect: withIndexDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorFirstObject: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: includes: isSequenceableCollection)
('gui' do:displayingProgress:)
!

