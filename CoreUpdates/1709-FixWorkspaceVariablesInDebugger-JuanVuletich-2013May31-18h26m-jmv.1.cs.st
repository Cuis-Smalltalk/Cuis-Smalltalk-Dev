'From Cuis 4.1 of 12 December 2012 [latest update: #1708] on 31 May 2013 at 6:32:22.448758 pm'!

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 5/31/2013 18:28'!
xtraBindings
	"Used for Workspace variables"
	^Array streamContents: [ :strm |
		self literalsDo: [ :literal |
			(literal class == Association and: [ literal key isString and: [ literal key first isLowercase ]]) ifTrue: [
				strm nextPut: literal ]]]! !


!Compiler methodsFor: 'public access' stamp: 'jmv 5/29/2013 14:53'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class := (aContext == nil ifTrue: [receiver] ifFalse: [aContext receiver]) class.
	self from: textOrStream class: class context: aContext notifying: aRequestor.
	methodNode := self translate: sourceStream noPattern: true ifFail: [^failBlock value].

	method := self interactive
				ifTrue: [methodNode generateWithTempNames]
				ifFalse: [methodNode generate].

	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	logFlag ifTrue:
		[toLog := ((requestor respondsTo: #selection)  
			and:[(itsSelection := requestor selection) notNil
			and:[(itsSelectionString := itsSelection asString) isEmptyOrNil not]])
				ifTrue:[itsSelectionString]
				ifFalse:[sourceStream contents].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: aContext].

	"Evaluate now."
	value := receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method.

	^ value! !


!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'accessing' stamp: 'jmv 5/31/2013 18:30'!
tempNamesForContext: aContext
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^((self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents) collect: [ :pair |
			pair first]),
		(aContext method xtraBindings collect: [ :assoc | assoc key ])! !

!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'private' stamp: 'jmv 5/31/2013 18:29'!
privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	| nameRefPair namesAndRefs |
	namesAndRefs _ self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents.
	^index <= namesAndRefs size
		ifTrue: [
			nameRefPair _ namesAndRefs
				at: index
				ifAbsent: [ aContext errorSubscriptBounds: index ].
			self privateDereference: nameRefPair last in: aContext ]
		ifFalse: [
			(aContext method xtraBindings at: index - namesAndRefs size) value ]! !


!Parser methodsFor: 'error correction' stamp: 'jmv 5/30/2013 16:23'!
correctVariable: proposedVariable interval: spot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| binding userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(binding _ requestor bindingOf: proposedVariable)
		ifNotNil: [^encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value! !

