'From Cuis 5.0 of 7 November 2016 [latest update: #3698] on 17 April 2019 at 3:18:04 pm'!
!classDefinition: #Compiler category: #'Compiler-Kernel'!
Object subclass: #Compiler
	instanceVariableNames: 'sourceStream requestor class category context parser sourceStreamGetter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category sourceStreamGetter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Compiler methodsFor: 'private' stamp: 'jmv 4/17/2019 15:15:58'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self parser
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Parser methodsFor: 'public access' stamp: 'jmv 4/17/2019 15:17:07'!
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
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ ReadStream on: requestor text string ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/17/2019 15:15:33'!
compileSelectionFor: anObject in: evalContext ifFail: failBlock

	| methodNode method sourceCode compiler |

	sourceCode := self selectionDoItSourceCodeIn: evalContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler		
			compileDoIt: sourceCode
			in: anObject class
			context: evalContext
			notifying: self
			ifFail: [ ^ failBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method _ methodNode generate.
	method methodNode: methodNode.

	^{ #method -> method. #compiler -> compiler } asDictionary 
! !

!methodRemoval: Parser #sourceStreamGetter:!
Parser removeSelector: #sourceStreamGetter:!
!methodRemoval: Compiler #sourceStreamGetter:!
Compiler removeSelector: #sourceStreamGetter:!
!classDefinition: #Compiler category: #'Compiler-Kernel'!
Object subclass: #Compiler
	instanceVariableNames: 'sourceStream requestor class category context parser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
