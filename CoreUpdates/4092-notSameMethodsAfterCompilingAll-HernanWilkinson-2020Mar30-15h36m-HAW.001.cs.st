'From Cuis 5.0 [latest update: #4085] on 30 March 2020 at 3:38:39 pm'!

!Compiler class methodsFor: 'utilities' stamp: 'HAW 3/30/2020 15:38:12'!
notSameMethodsAfterCompilingAll

	"
	self notSameMethodsAfterCompilingAll
	"
	
	| notEqual |
	
	notEqual := OrderedCollection new.

	ProtoObject withAllSubclassesDo: [ :class | 
		class selectorsDo: [ :selector | | currentCompiledMethod newCompiledMethod |
			currentCompiledMethod := class >> selector.
			newCompiledMethod := class
				basicCompile: currentCompiledMethod sourceCode
				notifying: nil
				trailer: class defaultMethodTrailer
				ifFail: [^self error: 'error compiling'].
			currentCompiledMethod = newCompiledMethod method ifFalse: [ notEqual add: currentCompiledMethod ]].	
		].

	^notEqual.! !

