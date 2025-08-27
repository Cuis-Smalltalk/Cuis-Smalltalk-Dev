'From Cuis7.5 [latest update: #7439] on 26 August 2025 at 3:16:24 pm'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 8/21/2025 15:33:19'!
compile: code classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor logSource: logSource
	
	| methodAndNode selector isExistingMethod updatedCode |	
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp libraryName: aStringOrNil
			notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category methodTimeStamp: changeStamp notifying: requestor.
	^ methodAndNode selector! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/26/2025 15:09:02'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp libraryName: aStringOrNil priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		aStringOrNil ifNotNil: [
			strm nextPutAll: ' library: '; print: aStringOrNil ].
		(changeStamp notNil and: [
			changeStamp notEmpty or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 8/25/2025 17:18:31'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self
		category: category
		withStamp: changeStamp
		libraryName: aStringOrNil
		inFile: 2
		priorMethod: priorMethodOrNil.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/25/2025 17:04:27'!
putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp libraryName: aStringOrNil inFile: fileIndex priorMethod: priorMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				libraryName: aStringOrNil
				priorMethod: priorMethod.
			file newLine ]! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 8/26/2025 15:12:30'!
compile: text classified: category withStamp: changeStamp notifying: requestor

	^ self compile: text classified: category withStamp: changeStamp libraryName: nil notifying: requestor logSource: self acceptsLoggingOfCompilation! !

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 8/26/2025 15:12:41'!
compileSilently: code classified: category notifying: requestor
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemChangeNotifier uniqueInstance 
		doSilently: [self compile: code classified: category withStamp: nil libraryName: nil notifying: requestor logSource: false].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/26/2025 15:10:35'!
reformatMethodAt: selector
	| newCodeString method | 
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStamp
		libraryName: nil
		inFile: 2
		priorMethod: method.! !

!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: stamp: 'jmv 8/26/2025 15:12:51'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:!
!methodRemoval: ClassDescription #compile:classified:withStamp:notifying:logSource: stamp: 'jmv 8/26/2025 15:12:45'!
ClassDescription removeSelector: #compile:classified:withStamp:notifying:logSource:!
!methodRemoval: ClassDescription #printCategoryChunk:on:withStamp:priorMethod: stamp: 'jmv 8/26/2025 15:12:54'!
ClassDescription removeSelector: #printCategoryChunk:on:withStamp:priorMethod:!
!methodRemoval: ClassDescription #logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: stamp: 'jmv 8/26/2025 15:12:48'!
ClassDescription removeSelector: #logMethodSource:forMethodWithNode:inCategory:withStamp:notifying:!
