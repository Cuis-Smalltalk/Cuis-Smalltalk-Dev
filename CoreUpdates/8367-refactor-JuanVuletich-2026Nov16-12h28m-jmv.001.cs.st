'From Cuis7.9 [latest update: #8366] on 16 November 2026 at 12:47:57 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Nov/2026, 3:45:23 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: doPad
	"Copy the source code for the method associated with selector onto the fileStream."

	| newPos methodCode methodCodeText codeStyler |

	methodCode := compiledMethod sourceOrDecompiled.
	
	"Convert method code to use ANSI assignments"
	(Preferences at: #fileOutANSIassignment) ifTrue: [
		codeStyler := SHTextStylerST80 new.
		codeStyler classOrMetaClass: compiledMethod methodClass.
		methodCodeText := methodCode asText.
		codeStyler instVarNamed: 'formattedText' put: methodCodeText.
		codeStyler getReady.
		codeStyler parseText.
		codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
		methodCode := methodCodeText string ].
		
	doPreamble ifTrue: [
		compiledMethod
			writeMethodPreambleTo: outStream class: self
			category: (self organization categoryOfElement: compiledMethod selector)
			withStamp: compiledMethod authorAndStampUTC ].
	
	doPad ifTrue: [
		outStream padSpacesTo: SourceFiles pointerScaleForWriting ].
	newPos := outStream position.
	outStream nextChunkPut: methodCode.

	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].

	outStream newLine.
	^newPos! !


!CompiledMethod methodsFor: 'method node' stamp: 'jmv 16/Nov/2026, 3:34:14 pm (UTC)'!
createMethodNodeOptimizeSpecialSends: doOptimizeSpecialSends
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self sourceOrDecompiled.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail.
	To avoid incopatibility with other parsers, we only send the message methodNodeFor:noPattern:optimizeSpecialSends: 
	when it is false that is the special case needed for the refactorings - Hernan"
	^ (doOptimizeSpecialSends
		ifTrue: [ 	(aClass methodNodeFor: source) ]
		ifFalse: [ (aClass methodNodeFor: source noPattern: false optimizeSpecialSends: false) ]) 
			ifNil: [ self decompile ].! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 3:35:03 pm (UTC)'!
sourceCode
	"Convenience"
	^self sourceOrDecompiled! !

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 16/Nov/2026, 3:34:34 pm (UTC)'!
hasLiteralExplicitly: aLiteral
	"Similar to #hasLiteral:
	But do not include methods that (for example) #hasLiteral: #==
	only because the Compiler implements #ifNil: that way.
	See #hasLiteral:.
	Useful for finding senders of selectors."

	| canBeSpecialLiteral |
	canBeSpecialLiteral := self encoderClass canBeSpecialLiteral: aLiteral.

	"If we don't hasLiteral: it, just answer false."
	(self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: canBeSpecialLiteral)
			ifFalse: [ ^false ].

	"Any literal but these appears for sure in source code, so it appears explicitly."
	canBeSpecialLiteral
		ifFalse: [ ^true ].

	"#at:put: is the only special selector that can't be found with #findString:
	But, it is also not generated implicitly by the Compiler.
	So we know it is actually sent in source code anyway."
	aLiteral = #at:put: ifTrue: [
		^true ].

	"Other special selectors, like #== must be found in source code to be considered."
	^(self sourceOrDecompiled findString: aLiteral) > 0! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 16/Nov/2026, 3:06:39 pm (UTC)'!
sourceCode

	^ self method sourceCode! !

!methodRemoval: CompiledMethod #getSourceFor:in: stamp: 'jmv 16/Nov/2026, 3:35:47 pm (UTC)'!
CompiledMethod removeSelector: #getSourceFor:in:!
!methodRemoval: CompiledMethod #getSource stamp: 'jmv 16/Nov/2026, 3:35:43 pm (UTC)'!
CompiledMethod removeSelector: #getSource!
