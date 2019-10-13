'From Cuis 5.0 [latest update: #3912] on 10 October 2019 at 10:19:48 am'!
!classDefinition: #RecompilationFailure category: #'Exceptions Kernel'!
Error subclass: #RecompilationFailure
	instanceVariableNames: 'class selector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!RecompilationFailure methodsFor: 'private' stamp: 'HAW 10/10/2019 10:09:03'!
class: aClass messageText: aString
	class _ aClass.
	messageText _ aString! !


!RecompilationFailure class methodsFor: 'instance creation' stamp: 'HAW 10/10/2019 10:09:30'!
class: aClass messageText: aString
	^ self new class: aClass messageText: aString! !


!Encoder methodsFor: 'private' stamp: 'HAW 10/10/2019 10:15:32'!
warnAboutShadowed: name

	| msg |
	
	msg _ 'There already exists a variable named ', name, ' '.
	requestor addWarning: msg.
	Transcript newLine; show: msg.
	InMidstOfFileinNotification signal
		ifFalse: [
			requestor interactive
				ifTrue: [
					self notify: msg]
				ifFalse: [
					(RecompilationFailure
						class: class
						messageText: msg, String newLineString, 'Please use a different name') signal ]]! !

!methodRemoval: RecompilationFailure class #class:selector:messageText:!
RecompilationFailure class removeSelector: #class:selector:messageText:!
!methodRemoval: RecompilationFailure #class:selector:messageText:!
RecompilationFailure removeSelector: #class:selector:messageText:!
!methodRemoval: RecompilationFailure #messageText!
RecompilationFailure removeSelector: #messageText!
!classDefinition: #RecompilationFailure category: #'Exceptions Kernel'!
Error subclass: #RecompilationFailure
	instanceVariableNames: 'class'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!RecompilationFailure class reorganize!
('instance creation' class:messageText:)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
!

