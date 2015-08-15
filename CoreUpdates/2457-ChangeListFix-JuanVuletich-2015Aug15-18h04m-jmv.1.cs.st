'From Cuis 4.2 of 25 July 2013 [latest update: #2456] on 15 August 2015 at 6:04:59.66492 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/15/2015 18:04'!
scanCategory: category class: class meta: meta stamp: stamp
	| itemPosition method |
	[
		itemPosition _ file position.
		method _ file nextChunk.
		method size > 0 ]						"done when double terminators"
			whileTrue: [
				self
					addItem: (ChangeRecord new file: file position: itemPosition type: #method
							class: class category: category meta: meta stamp: stamp)
					text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
						, ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method)
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

