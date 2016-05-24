'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 23 May 2016 at 3:06:17.220139 pm'!

!ProcessBrowser methodsFor: 'stack list' stamp: 'pb 5/23/2016 15:05'!
updateStackList: depth 
	| suspendedContext oldHighlight |
	selectedProcess
		ifNil: [ ^ self changeStackListTo: nil ].
	oldHighlight _ selectedContext.
	selectedProcess == Processor activeProcess
		ifTrue: [
			self changeStackListTo: (thisContext stackOfSize: depth)]
		ifFalse: [
			suspendedContext _ selectedProcess suspendedContext.
			self changeStackListTo: (suspendedContext ifNotNil: [suspendedContext stackOfSize: depth])].
	self context: ((stackList notNil and:  [(stackList includes: oldHighlight)]) ifTrue: [oldHighlight])! !

