'From Cuis6.3 [latest update: #6327] on 23 April 2024 at 12:00:24 pm'!

!Debugger methodsFor: 'shout styling' stamp: 'jmv 4/23/2024 11:34:09'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: 
			(self selectedContext selector = Scanner doItSelector ifFalse: [
				self selectedClassOrMetaClass]); "nil for DoIts"
		workspace: self.
	^true! !


!Scanner class methodsFor: 'doIt selectors' stamp: 'jmv 4/23/2024 11:37:52'!
doItSourceCodeHeaderWithContext: hasContext into: stream
"
	stream
		nextPutAll: (hasContext ifTrue: [ Scanner doItInSelector, ' ', Encoder doItInContextName ] ifFalse: [ Scanner doItSelector ]);
		newLine;
		newLine
"! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/23/2024 11:39:16'!
afterCompiling: aSourceCode do: aBlock for: aReceiver in: anEvaluationContext ifFail: aFailBlock
	
	| methodNode method sourceCode compiler |

	sourceCode := Scanner doItSourceCodeFor: aSourceCode in: anEvaluationContext.
	
	methodNode := [
		compiler := Compiler new.
		compiler		
			compileNoPattern: sourceCode
			in: aReceiver class
			context: anEvaluationContext
			notifying: self
			ifFail: [ ^ aFailBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method := methodNode generate.
	method methodNode: methodNode.

	^aBlock value: compiler value: method value: aReceiver value: anEvaluationContext 
! !

