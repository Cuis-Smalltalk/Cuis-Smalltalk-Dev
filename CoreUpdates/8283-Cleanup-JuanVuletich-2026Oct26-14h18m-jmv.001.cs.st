'From Cuis7.9 [latest update: #8281] on 26 October 2026 at 2:20:20 pm'!

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 4:44:13 pm (UTC)'!
symbolic
	"Answer a String that contains a list of all the byte codes in a method 
	with a short description of each."

	^ String streamContents: [ :strm |
		self longPrintOn: strm indent: 0 ].! !

!methodRemoval: Process #longPrintOn: stamp: 'jmv 26/Oct/2026, 5:19:48 pm (UTC)'!
Process removeSelector: #longPrintOn:!
!methodRemoval: UniFileStream #longPrintOn: stamp: 'jmv 26/Oct/2026, 5:19:47 pm (UTC)'!
UniFileStream removeSelector: #longPrintOn:!
!methodRemoval: CompiledMethod #longPrintOn: stamp: 'jmv 26/Oct/2026, 5:19:47 pm (UTC)'!
CompiledMethod removeSelector: #longPrintOn:!
!methodRemoval: Behavior #longPrintOn: stamp: 'jmv 26/Oct/2026, 5:19:47 pm (UTC)'!
Behavior removeSelector: #longPrintOn:!
!methodRemoval: Object #longPrintText stamp: 'jmv 26/Oct/2026, 5:19:40 pm (UTC)'!
Object removeSelector: #longPrintText!
!methodRemoval: Object #longPrintOn: stamp: 'jmv 26/Oct/2026, 5:19:46 pm (UTC)'!
Object removeSelector: #longPrintOn:!
!methodRemoval: Object #longPrintString stamp: 'jmv 26/Oct/2026, 5:19:39 pm (UTC)'!
Object removeSelector: #longPrintString!
