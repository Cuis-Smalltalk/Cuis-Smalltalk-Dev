'From Cuis7.9 [latest update: #8379] on 17 November 2026 at 5:01:34 pm'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 17/Nov/2026, 6:52:48 pm (UTC)'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStampLocalTZ
	(Object compiledMethodAt: #printString) authorAndStampUTC
	"

	CodeHistory current
		methodSourceAndMetadataFor: self historyPointer
		into: [ :methodSource :authorAndStampAsSaved :methodPropertiesString :prevHistoryPointer :nextHistoryPointer :isMethodRemoval |
			^authorAndStampAsSaved ].! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 17/Nov/2026, 6:56:15 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodProperties historyPointer authorAndStampLocalTZ |

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
			into: [ :methodSource :authorAndStampAsSaved :methodPropertiesString :prevHistoryPointer :nextHistoryPointer :isMethodRemoval |
				isMethodRemoval
					ifTrue: [
						self addItem: (MethodDeletionChange new methodReference:
									(MethodReference class: classOfMethod selector: selectorOfMethod))
							text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
					ifFalse: [
						"If there is no source and the following are not available, they will be '' and nil."
						authorAndStampLocalTZ := CompiledMethod authorAndStampLocalTZ: authorAndStampAsSaved.
						methodProperties := Compiler evaluate: methodPropertiesString.
						self addItem:
								(MethodDefinitionChange new
									class: class name category: category meta: meta stamp: authorAndStampLocalTZ
									properties: (methodProperties ifNil: [#()])
									selector: selectorOfMethod
									string: methodSource)
							text:
								"Maybe add an indication of library from where this version was loaded."
								"historyPointer printString, ' - ',
								prevHistoryPointer printString, ' - ',
								nextHistoryPointer printString, ' - ',"
								authorAndStampLocalTZ , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
								, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] ']) ].
				historyPointer := prevHistoryPointer ]].! !

