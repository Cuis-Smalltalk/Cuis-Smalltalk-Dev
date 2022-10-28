'From Cuis 6.0 [latest update: #5512] on 28 October 2022 at 3:01:08 pm'!

!Browser methodsFor: 'accessing' stamp: 'jmv 10/28/2022 14:16:39'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| codeString theClass |
	codeString _ input string.
	editSelection == #editSystemCategories ifTrue: [
		^ self changeSystemCategories: codeString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: codeString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: codeString stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: codeString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: codeString notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !


!MessageSet methodsFor: 'accessing' stamp: 'jmv 10/28/2022 14:17:34'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString _ input string.
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue: [
		oldSelector == #Comment ifTrue: [
			class comment: codeString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: codeString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: codeString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 10/28/2022 14:17:42'!
contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| codeString theClass |
	codeString _ input string.

	editSelection == #editComment 
		ifTrue: [
			theClass _ self selectedClass.
			theClass ifNil: [
				self inform: 'You must select a class
before giving it a comment.'.
				^ false ].
			theClass comment: codeString. 
			^ true ].
	editSelection == #editMessageCategories 
		ifTrue: [^ self changeMessageCategories: codeString].

	self inform:'You cannot change the current selection'.
	^false! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 10/28/2022 14:17:56'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString _ input string.
	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char = $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: codeString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: codeString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: codeString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Debugger methodsFor: 'accessing' stamp: 'jmv 10/28/2022 14:19:29'!
contents: input notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| codeString result selector classOfMethod category h newMethod |
	codeString _ input string.

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: codeString.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: codeString notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: codeString
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	self resetToSelectedContextWith: newMethod.
	
	^true! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 10/28/2022 14:24:24'!
compile: code notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode updatedCode |
	methodAndNode _ self
		basicCompile: code
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
	updatedCode _ CodeProvider normalizeCodeForCodeFiles: (requestor ifNil: [code] ifNotNil: [requestor text]).
	methodAndNode method putSource: updatedCode fromParseNode: methodAndNode node inFile: 2
			withPreamble: [ :f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: methodAndNode selector withMethod: methodAndNode method.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/28/2022 14:26:12'!
compile: code classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	
	| methodAndNode selector isExistingMethod updatedCode |	
	methodAndNode _ self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode _ CodeProvider normalizeCodeForCodeFiles: (requestor ifNil: [code] ifNotNil: [requestor text]).
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !


!Parser methodsFor: 'public access' stamp: 'jmv 5/22/2022 20:41:08'!
parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		startPosition _ myStream position.
		sourceCode _ myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		doIt ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ requestor text string readStream ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 10/28/2022 14:06:41'!
withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: model actualContents string noPattern: true ]
		on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 10/28/2022 13:05:09'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self selectedClassOrMetaClassOrUndefinedObject.
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents string ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/28/2022 14:13:21'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selection string
		do: [ :compiler :method :receiver :context |
			(method notNil and: [method isQuick not]) ifTrue: [
				self debug: method receiver: receiver in: context ]]
		ifFail: [].! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/29/2022 12:01:03'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^self stringClassToUse streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode ]! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/28/2022 14:13:28'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"	
	
	self lineSelectAndEmptyCheck: [^ ''].
	^self evaluate: self selection string andDo: aBlock ifFail: failBlock profiled: doProfile! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 10/28/2022 14:07:58'!
withSelectorAt: aPosition do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock
	| methodNode nodeAtRange nodeUnderCursor  |

	methodNode := self codeProvider
		methodNodeOf: model actualContents string
		ifErrorsParsing: [ :anError | ^ failBlock value ].

	aPosition < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: aPosition ifAbsent: [ ^ failBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ])
		ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].
	(nodeUnderCursor isLiteralVariableNode and: [ nodeUnderCursor isReferencingBehavior ])
		ifTrue: [ ^aBehaviorBlock value: nodeUnderCursor key value ].

	^ failBlock value.! !


!SmalltalkCompleter methodsFor: 'accessing' stamp: 'jmv 4/26/2021 19:28:15'!
allSource
	^model actualContents string! !

!methodRemoval: Character class #arrowDown stamp: 'jmv 10/28/2022 15:00:53'!
Character class removeSelector: #arrowDown!
!methodRemoval: Character class #arrowUp stamp: 'jmv 10/28/2022 15:00:53'!
Character class removeSelector: #arrowUp!
!methodRemoval: Character class #arrowRight stamp: 'jmv 10/28/2022 15:00:53'!
Character class removeSelector: #arrowRight!
!methodRemoval: Character class #arrowLeft stamp: 'jmv 10/28/2022 15:00:53'!
Character class removeSelector: #arrowLeft!
!methodRemoval: CodeProvider class #normalizeCode: stamp: 'jmv 10/28/2022 15:00:53'!
CodeProvider class removeSelector: #normalizeCode:!
