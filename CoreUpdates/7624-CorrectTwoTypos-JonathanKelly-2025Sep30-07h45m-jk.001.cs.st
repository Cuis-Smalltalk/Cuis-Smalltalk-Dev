'From Cuis7.5 [latest update: #7623] on 30 September 2025 at 7:48:46 am'!

!Object methodsFor: 'finalization' stamp: 'jk 9/30/2025 07:45:06'!
actAsExecutor
	"Prepare the receiver to act as executor for any resources associated with it"
	self breakDependents ! !

!Object methodsFor: 'testing' library: #MinimalKernel01 stamp: 'jk 9/30/2025 07:47:42'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	aSymbol is usually a class name (starting with uppercase) or a protocol conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false! !

