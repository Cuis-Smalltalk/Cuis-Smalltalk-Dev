'From Cuis7.5 [latest update: #7443] on 27 August 2025 at 11:09:29 am'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/27/2025 10:54:18'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition libraryName: libNameOrNil
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| itemPosition method |
	[
		itemPosition := file position.
		method := file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp
					prior: priorVersionPosition
					libraryName: libNameOrNil)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (libNameOrNil ifNil: [''] ifNotNil: [' - library[', libNameOrNil, ']'])
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])].! !

