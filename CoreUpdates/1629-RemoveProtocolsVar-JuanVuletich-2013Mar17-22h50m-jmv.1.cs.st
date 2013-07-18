'From Cuis 4.1 of 12 December 2012 [latest update: #1628] on 17 March 2013 at 11:10:59 pm'!

!Object commentStamp: '<historical>' prior: 0!
Object is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass of Object) and its subclasses.

Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here.

Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes.

Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as isMorph.!

!classDefinition: 'Object class' category: #'Kernel-Objects'!
Object class
	instanceVariableNames: 'protocols '!

!Array methodsFor: 'as yet unclassified' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #Array or: [ super is: aSymbol ]! !


!Color methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #Color or: [ super is: aSymbol ]! !


!CompiledMethod methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #CompiledMethod or: [ super is: aSymbol ]! !


!FloatArray methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #FloatArray or: [ super is: aSymbol ]! !


!Form methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:36'!
is: aSymbol
	^ aSymbol == #Form or: [ super is: aSymbol ]! !


!ColorForm methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:36'!
is: aSymbol
	^ aSymbol == #ColorForm or: [ super is: aSymbol ]! !


!Cursor methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:36'!
is: aSymbol
	^ aSymbol == #Cursor or: [ super is: aSymbol ]! !


!Matrix methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #Matrix or: [ super is: aSymbol ]! !


!MessageSend methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:36'!
is: aSymbol
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]! !


!Morph methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	^ aSymbol == #Morph or: [ super is: aSymbol ]! !


!BorderedRectMorph methodsFor: 'testing' stamp: 'jmv 3/17/2013 22:54'!
is: aSymbol
	^ aSymbol == #BorderedRectMorph or: [ super is: aSymbol ]! !


!HaloMorph methodsFor: 'testing' stamp: 'jmv 3/17/2013 22:55'!
is: aSymbol
	^ aSymbol == #HaloMorph or: [ super is: aSymbol ]! !


!HandMorph methodsFor: 'classification' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	^ aSymbol == #HandMorph or: [ super is: aSymbol ]! !


!InnerTextMorph methodsFor: 'classification' stamp: 'jmv 11/14/2011 16:52'!
is: aSymbol
	^ aSymbol == #InnerTextMorph or: [ super is: aSymbol ]! !


!LayoutMorph methodsFor: 'testing' stamp: 'jmv 2/25/2011 10:48'!
is: aSymbol
	^ aSymbol == #LayoutMorph or: [ super is: aSymbol ]! !


!MorphicEvent methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	^ aSymbol == #MorphicEvent or: [ super is: aSymbol ]! !


!PluggableScrollPane methodsFor: 'testing' stamp: 'jmv 12/3/2011 00:10'!
is: aSymbol
	^ aSymbol == #ScrollPane or: [ super is: aSymbol ]! !


!Stream methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	"Return true if the receiver responds to the stream protocol (if that's what's asked)"
	^ aSymbol == #Stream or: [ super is: aSymbol ]! !


!SystemWindow methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	^ aSymbol == #SystemWindow or: [ super is: aSymbol ]! !


!Text methodsFor: 'comparing' stamp: 'jmv 12/2/2010 08:36'!
is: aSymbol
	^ aSymbol == #Text or: [ super is: aSymbol ]! !


!WeakMessageSend methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:37'!
is: aSymbol
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]! !


!Workspace methodsFor: 'testing' stamp: 'jmv 12/21/2010 23:20'!
is: aSymbol
	^ aSymbol == #ShoutEnabled or: [ super is: aSymbol ]! !


!Object methodsFor: 'testing' stamp: 'jmv 2/28/2011 15:43'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	Suggested by Igor Stasenko at
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2009-June/136793.html
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) pointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false! !


!CodeProvider methodsFor: 'testing' stamp: 'jmv 3/17/2013 22:53'!
is: aSymbol
	^(#(CodeProvider #ShoutEnabled) statePointsTo: aSymbol)
		or: [ super is: aSymbol ]! !


!Object class methodsFor: 'compiling' stamp: 'jmv 3/17/2013 23:01'!
noteCompilationOf: aSelector meta: isMeta
	"A hook allowing some classes to react to recompilation of certain selectors.
	This implementor catches class methods."! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 12/20/2010 15:12'!
is: aSymbol
	aSymbol == #ShoutEnabled ifTrue: [ ^textProvider is: aSymbol ].
	^ aSymbol == #hasTextProvider or: [ super is: aSymbol ]! !

!methodRemoval: Workspace class #gatherProtocols!
Workspace class removeSelector: #gatherProtocols!
!methodRemoval: WeakMessageSend class #gatherProtocols!
WeakMessageSend class removeSelector: #gatherProtocols!
!methodRemoval: Text class #gatherProtocols!
Text class removeSelector: #gatherProtocols!
!methodRemoval: SystemWindow class #gatherProtocols!
SystemWindow class removeSelector: #gatherProtocols!
!methodRemoval: SystemDictionary #allProtocols!
SystemDictionary removeSelector: #allProtocols!
!methodRemoval: Stream class #gatherProtocols!
Stream class removeSelector: #gatherProtocols!
!methodRemoval: PluggableTextModel class #gatherProtocols!
PluggableTextModel class removeSelector: #gatherProtocols!
!methodRemoval: PluggableScrollPane class #gatherProtocols!
PluggableScrollPane class removeSelector: #gatherProtocols!
!methodRemoval: MorphicEvent class #gatherProtocols!
MorphicEvent class removeSelector: #gatherProtocols!
!methodRemoval: LayoutMorph class #gatherProtocols!
LayoutMorph class removeSelector: #gatherProtocols!
!methodRemoval: InnerTextMorph class #gatherProtocols!
InnerTextMorph class removeSelector: #gatherProtocols!
!methodRemoval: HandMorph class #gatherProtocols!
HandMorph class removeSelector: #gatherProtocols!
!methodRemoval: HaloMorph class #gatherProtocols!
HaloMorph class removeSelector: #gatherProtocols!
!methodRemoval: BorderedRectMorph class #gatherProtocols!
BorderedRectMorph class removeSelector: #gatherProtocols!
!methodRemoval: Morph class #gatherProtocols!
Morph class removeSelector: #gatherProtocols!
!methodRemoval: MessageSend class #gatherProtocols!
MessageSend class removeSelector: #gatherProtocols!
!methodRemoval: Matrix class #gatherProtocols!
Matrix class removeSelector: #gatherProtocols!
!methodRemoval: Cursor class #gatherProtocols!
Cursor class removeSelector: #gatherProtocols!
!methodRemoval: ColorForm class #gatherProtocols!
ColorForm class removeSelector: #gatherProtocols!
!methodRemoval: Form class #gatherProtocols!
Form class removeSelector: #gatherProtocols!
!methodRemoval: FloatArray class #gatherProtocols!
FloatArray class removeSelector: #gatherProtocols!
!methodRemoval: CompiledMethod class #gatherProtocols!
CompiledMethod class removeSelector: #gatherProtocols!
!methodRemoval: Color class #gatherProtocols!
Color class removeSelector: #gatherProtocols!
!methodRemoval: CodeProvider class #gatherProtocols!
CodeProvider class removeSelector: #gatherProtocols!
!methodRemoval: Array class #gatherProtocols!
Array class removeSelector: #gatherProtocols!
!methodRemoval: Object class #gatherProtocols!
Object class removeSelector: #gatherProtocols!
!methodRemoval: Object class #initializeProtocols!
Object class removeSelector: #initializeProtocols!
!methodRemoval: Object class #protocols!
Object class removeSelector: #protocols!
!classDefinition: 'Object class' category: #'Kernel-Objects'!
Object class
	instanceVariableNames: ''!
!methodRemoval: Metaclass #protocols!
Metaclass removeSelector: #protocols!
