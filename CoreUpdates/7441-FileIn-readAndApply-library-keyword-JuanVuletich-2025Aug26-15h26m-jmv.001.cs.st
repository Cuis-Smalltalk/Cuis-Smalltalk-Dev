'From Cuis7.5 [latest update: #7440] on 26 August 2025 at 3:29:58 pm'!
!classDefinition: #ClassCategoryReader category: #'Kernel-Classes'!
Object subclass: #ClassCategoryReader
	instanceVariableNames: 'class category changeStamp libraryName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!ClassCategoryReader methodsFor: 'private' stamp: 'jmv 8/21/2025 15:10:58'!
setClass: aClass category: aCategory changeStamp: aString libraryName: aStringOrNil

	class := aClass.
	category := aCategory.
	changeStamp := aString.
	libraryName := aStringOrNil.! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 8/21/2025 15:05:51'!
compile: text classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor
	^ self compile: text classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor logSource: self acceptsLoggingOfCompilation! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 12:46:52'!
methodsFor: categoryName library: libraryName stamp: changeStamp
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*"

	^ self methodsFor: categoryName library: libraryName stamp: changeStamp prior: 0! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/25/2025 16:51:26'!
methodsFor: categoryName library: libraryName stamp: changeStamp prior: indexAndOffset
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Prior source link ignored when filing in."

	^ ClassCategoryReader new
			setClass: self
			category: categoryName asSymbol
			changeStamp: changeStamp
			libraryName: libraryName

"Most importantly, return the new ClassCategoryReader, so a fileIn will let it seize control.  So method will be placed in the proper category.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 15:12:14'!
scanFrom: aStream
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."
	| methodSource |
	[
		methodSource := aStream nextChunk withCuisLineEndings.
		methodSource size > 0 ] whileTrue: [
			class
				compile: methodSource
				classified: category
				withStamp: changeStamp
				libraryName: libraryName
				notifying: nil ]! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 8/21/2025 15:06:36'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				libraryName: libraryName
				notifying: nil ]].
	self isDoIt ifTrue: [
		((s := self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class)
			comment: self text
			stamp: stamp ]! !

!classDefinition: #ClassCategoryReader category: #'Kernel-Classes'!
Object subclass: #ClassCategoryReader
	instanceVariableNames: 'class category changeStamp libraryName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!
