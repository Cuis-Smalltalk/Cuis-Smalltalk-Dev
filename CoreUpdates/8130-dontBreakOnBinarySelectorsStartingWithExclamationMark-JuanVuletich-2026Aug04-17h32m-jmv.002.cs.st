'From Cuis7.9 [latest update: #8129] on 4 August 2026 at 5:44:07 pm'!

!PositionableStream methodsFor: 'reading' stamp: 'jmv 4/Aug/2026, 8:40:43 pm (UTC)'!
peekForString: aString
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextString currentPosition |
	self atEnd ifTrue: [^false].
	currentPosition := self position.
	nextString := self next: aString size.
	aString = nextString ifTrue: [
		^ true].
	self position: currentPosition.
	^ false! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 4/Aug/2026, 8:42:11 pm (UTC)'!
scanFrom: aStream
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."

	| methodSource methodProperties |
	[
		methodProperties := nil.
		aStream skipSeparators.
		(aStream peekForString: '!!methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: aStream nextChunk ].
		methodSource := aStream nextChunk withCuisLineEndings.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
		class
			compile: methodSource
			classified: category
			withStamp: changeStamp
			methodProperties: methodProperties
			notifying: nil
	] repeat.! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 4/Aug/2026, 8:43:17 pm (UTC)'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| methodProperties libNameOrNil itemPosition methodSource |
	[
		methodProperties := nil.
		libNameOrNil := nil.
		file skipSeparators.
		(file peekForString: '!!methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: file nextChunk.
				methodProperties do: [ :assoc |
					assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]]].
		file skipSeparators.
		itemPosition := file position.
		methodSource := file nextChunk.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
			self
				addItem: (MethodDefinitionChange new
					file: file position: itemPosition
					class: class category: category meta: meta stamp: stamp
					properties: methodProperties
					prior: priorVersionPosition)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: methodSource) ifNil: [''])
					, (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])
					", (' [methodProperties: ', methodProperties printString, '] ')"
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
		] repeat.
! !
