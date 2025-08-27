'From Cuis7.5 [latest update: #7437] on 25 August 2025 at 4:47:57 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/21/2025 12:48:00'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition libraryName: aStringOrNil
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
					libraryName: aStringOrNil)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 12:46:43'!
methods
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Answer a ClassCategoryReader for compiling messages that are not classified.
	Some chunk files may not include class category in the class declarations."

	^ ClassCategoryReader new setClass: self category: ClassOrganizer defaultMethodCategory.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 12:46:47'!
methodsFor: categoryName
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Answer a ClassCategoryReader for compiling the messages in the category, categoryName, of the receiver."

	^ ClassCategoryReader new setClass: self category: categoryName asSymbol

	"(False methodsFor: 'logical operations') inspect"! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 12:47:00'!
methodsFor: categoryName stamp: changeStamp
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*"

	^ self methodsFor: categoryName stamp: changeStamp prior: 0! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 12:47:02'!
methodsFor: categoryName stamp: changeStamp prior: indexAndOffset
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Prior source link ignored when filing in."
	
	^ ClassCategoryReader new setClass: self
				category: categoryName asSymbol
				changeStamp: changeStamp

"Most importantly, return the new ClassCategoryReader, so a fileIn will let it seize control.  So method will be placed in the proper category.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"! !

