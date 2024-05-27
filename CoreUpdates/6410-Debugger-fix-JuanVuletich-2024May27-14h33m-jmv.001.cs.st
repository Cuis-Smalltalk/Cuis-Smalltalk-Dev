'From Cuis7.1 [latest update: #6409] on 27 May 2024 at 2:33:31 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/27/2024 14:07:40'!
fullStack
	"Change from displaying the minimal stack to a full one."

	self contextStackList size > self class defaultDebugStackSize "Already expanded"
		ifTrue:
			[self changed: #flash]
		ifFalse:
			[self contextStackIndex = 0 ifFalse: [
				self toggleContextStackIndex: self contextStackIndex].
			self fullyExpandStack]! !

