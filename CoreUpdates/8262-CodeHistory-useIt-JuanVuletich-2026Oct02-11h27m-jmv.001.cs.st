'From Cuis7.9 [latest update: #8261] on 2 October 2026 at 11:28:47 am'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 1/Oct/2026, 2:49:11 pm (UTC)'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStampLocalTZ
	(Object compiledMethodAt: #printString) authorAndStampUTC
	"

	CodeHistory current
		methodSourceAndMetadataFor: self historyPointer
		into: [ :methodSource :stampString :methodPropertiesString :prevHistoryPointer :nextHistoryPointer |
			^stampString ].! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 2/Oct/2026, 2:14:01 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodProperties historyPointer |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
	[ historyPointer = 0 ] whileFalse: [
		CodeHistory current
			methodSourceAndMetadataFor: historyPointer
			into: [ :methodSource :stampString :methodPropertiesString :prevHistoryPointer :nextHistoryPointer |
				methodSource
					ifNil: [
						self addItem: (MethodDeletionChange new methodReference:
									(MethodReference class: classOfMethod selector: selectorOfMethod))
							text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
					ifNotNil: [
						methodProperties := Compiler evaluate: methodPropertiesString.
						self addItem:
								(MethodDefinitionChange new
									class: class name category: category meta: meta stamp: stampString
									properties: methodProperties
									selector: selectorOfMethod
									string: methodSource)
							text: stampString , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
								, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] ']) ].
				historyPointer := prevHistoryPointer ]].
! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 2/Oct/2026, 1:41:17 pm (UTC)'!
historyPointer
	sourceFileNumber ifNil: [^ 0].
	^CodeHistory historyPointerFromFileIndex: sourceFileNumber andPosition: filePositionHi! !

!methodRemoval: ChangeList class #methodSourceAndMetadataFor:into: stamp: 'jmv 2/Oct/2026, 2:27:56 pm (UTC)'!
ChangeList class removeSelector: #methodSourceAndMetadataFor:into:!
!methodRemoval: CuisSourceFileArray #filePositionFromHistoryPointer: stamp: 'jmv 2/Oct/2026, 2:27:56 pm (UTC)'!
CuisSourceFileArray removeSelector: #filePositionFromHistoryPointer:!
!methodRemoval: CuisSourceFileArray #historyPointerFromFileIndex:andPosition: stamp: 'jmv 2/Oct/2026, 2:27:56 pm (UTC)'!
CuisSourceFileArray removeSelector: #historyPointerFromFileIndex:andPosition:!
!methodRemoval: CuisSourceFileArray #fileIndexFromHistoryPointer: stamp: 'jmv 2/Oct/2026, 2:27:56 pm (UTC)'!
CuisSourceFileArray removeSelector: #fileIndexFromHistoryPointer:!

!CuisSourceFileArray reorganize!
('accessing' at: at:put: pointerScaleForWriting size)
('services' checkOKToAdd:at:in:)
('initialization' initialize initialize:)
!

