'From Cuis 5.0 of 7 November 2016 [latest update: #3268] on 12 March 2018 at 12:16:15 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'jmv 3/11/2018 15:52:08'!
sourceStringPrettifiedAndDiffed
	"Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies"
	| class selector sourceString |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	(class isNil or: [ selector isNil ]) ifTrue: [ ^ 'missing' ].
	sourceString _ class
		ultimateSourceCodeAt: selector
		ifAbsent: [ ^ 'error' ].
	(self showingPrettyPrint or: [ self showingAnyKindOfPrettyDiffs ]) ifTrue: [
		sourceString _ class compilerClass new
			format: sourceString
			in: class
			notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		sourceString _ self diffFromPriorSourceFor: sourceString ].
	^ sourceString! !


!Browser methodsFor: 'message functions' stamp: 'jmv 3/12/2018 12:15:33'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	 self metaClassIndicated ifTrue: [
		selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: selector) not
			and: [Metaclass isScarySelector: selector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (selector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!Compiler methodsFor: 'public access' stamp: 'jmv 3/12/2018 09:04:09'!
compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock 
	"Answer a MethodNode for the argument, textOrStream. If the 
	MethodNode can not be created, notify the argument, aRequestor; if 
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root 
	of a parse tree. It can be told to generate a CompiledMethod to be 
	installed in the method dictionary of the argument, aClass."
	
	| methodNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	category _ aCategory.
	methodNode _ self translate: sourceStream noPattern: false ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^methodNode! !

!Compiler methodsFor: 'public access' stamp: 'jmv 3/12/2018 09:08:56'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class _ (aContext ifNil: [ receiver ] ifNotNil: [ aContext receiver ]) class.
	methodNode _ self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method _ methodNode generate.

	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: aContext].

	"Evaluate now."
	doProfile
		ifTrue: [
			AndreasSystemProfiler spyOn: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value! !

!methodRemoval: Compiler #from:class:classified:context:notifying:!
Compiler removeSelector: #from:class:classified:context:notifying:!
!methodRemoval: CodeProvider #validateMessageSource:forSelector:inClass:!
CodeProvider removeSelector: #validateMessageSource:forSelector:inClass:!
