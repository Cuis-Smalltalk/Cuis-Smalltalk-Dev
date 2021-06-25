'From Haver 5.0 [latest update: #4641] on 19 June 2021 at 3:50:44 pm'!

!Compiler methodsFor: 'public access' stamp: 'KLG 6/19/2021 15:45:45'!
evaluateMethod: method to: receiver logged: doLog profiled: doProfile
	
	"See evaluate:in:to:notifying:ifFail:logged:profiled:
	It does the same but without compiling because it recevies the result of the compilation 
	as the parameter method. 
	self should have compile method"

	| value toLog itsSelection itsSelectionString |
	
	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	"Evaluate now."
	doProfile
		ifTrue: [
			TimeProfileBrowser onBlock: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value! !

