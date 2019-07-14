'From Cuis 5.0 [latest update: #3817] on 2 July 2019 at 11:35:21 am'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/2/2019 10:01:30'!
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
						, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

