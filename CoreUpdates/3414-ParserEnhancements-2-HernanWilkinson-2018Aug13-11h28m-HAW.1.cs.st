'From Cuis 5.0 of 7 November 2016 [latest update: #3411] on 13 August 2018 at 11:36:37 am'!

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/13/2018 11:35:34'!
rangesForLiteralNode: aName ifAbsent: aBlock

	| variableNode |
	
	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/13/2018 11:35:41'!
rangesForLiteralVariableNode: aName ifAbsent: aBlock

	| variableNode |
	
	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock! !


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockTemp: bindBlockTemp:within: bindTemp: bindTemp:in: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: rangesForInstanceVariable:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForTemporaryVariable:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap sourceRangeFor:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
('*Refactoring')
!

