'From Cuis 5.0 [latest update: #5015] on 30 December 2021 at 10:41:06 am'!

!ClassDescription methodsFor: 'initialization' stamp: 'jmv 12/30/2021 10:40:34'!
canUpdateFrom: oldClass
	"If some compiled method that will become invalid is currently being executed,
	the shape conversion of instances is extremely insecure.
	Answer if migration of instances looks safe"
	| answer newMethod oldMethod selector newActualClass oldActualClass |
	answer _ true.
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				oldActualClass _ context receiver class.
				self withAllSubclassesDo: [ :c |
					c name = oldActualClass name ifTrue: [
						newActualClass _ c ]].
				selector _ context method selector.
				oldMethod _ oldActualClass lookupSelector: selector.
				newMethod _ newActualClass lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					('Class reshaping: ', context receiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. Class reshaping aborted.') print.
					answer _ false ].
				]]
		runningProcessSearchStart: nil.
	^answer! !

