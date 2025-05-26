'From Cuis7.3 [latest update: #7236] on 24 May 2025 at 7:50:44 pm'!

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 5/24/2025 19:49:53'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	self addSelector: aSymbol method: aMethod allImplemented: nil.! !

