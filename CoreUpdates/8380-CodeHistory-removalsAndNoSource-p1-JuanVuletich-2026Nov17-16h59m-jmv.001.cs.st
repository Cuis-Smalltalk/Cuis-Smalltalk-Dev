'From Cuis7.9 [latest update: #8379] on 17 November 2026 at 5:00:05 pm'!

!CompiledMethod methodsFor: 'time stamp' stamp: ''!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStampLocalTZ
	(Object compiledMethodAt: #printString) authorAndStampUTC
	"
true ifTrue: [ ^'' ].
	CodeHistory current
		methodSourceAndMetadataFor: self historyPointer
		into: [ :methodSource :authorAndStampAsSaved :methodPropertiesString :prevHistoryPointer :nextHistoryPointer |
			^authorAndStampAsSaved ].! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: ''!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodProperties historyPointer authorAndStampLocalTZ |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
historyPointer := 0.
	[ historyPointer = 0 ] whileFalse: [
		CodeHistory current
			methodSourceAndMetadataFor: historyPointer
			into: [ :methodSource :authorAndStampAsSaved :methodPropertiesString :prevHistoryPointer :nextHistoryPointer |
				methodSource
					ifNil: [
						self addItem: (MethodDeletionChange new methodReference:
									(MethodReference class: classOfMethod selector: selectorOfMethod))
							text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
					ifNotNil: [
						authorAndStampLocalTZ := CompiledMethod authorAndStampLocalTZ: authorAndStampAsSaved.
						methodProperties := Compiler evaluate: methodPropertiesString.
						self addItem:
								(MethodDefinitionChange new
									class: class name category: category meta: meta stamp: authorAndStampLocalTZ
									properties: (methodProperties ifNil: [#()])
									selector: selectorOfMethod
									string: methodSource)
							text:
								"Acá podríamos poner una indicación de desde q librería se cargó, etc."
								historyPointer printString, ' - ',
								prevHistoryPointer printString, ' - ',
								nextHistoryPointer printString, ' - ',
								authorAndStampLocalTZ , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
								, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] ']) ].
				historyPointer := prevHistoryPointer ]].! !

