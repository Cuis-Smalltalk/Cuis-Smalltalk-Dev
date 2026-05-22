"
A SpecialSelectorNode is a subclass of SelectorNode that handles the special selectors, a high static and/or dynamic frequency set of selectors that are assigned their own bytecodes.  Special selectors both save space in the literal frame and allow an interpreter to implement these sends directly for certain classes of receiver and argument, for example the SmallIntegers, a technique known as static receiver prediction.
"
Class {
	#name : 'SpecialSelectorNode',
	#superclass : 'SelectorNode',
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'code generation' }
SpecialSelectorNode >> emitCode: stack args: nArgs encoder: encoder super: supered [
	"Generate a special selector send.
	 A super send of a special selector must be handled like a normal send."
	supered
		ifTrue:
			[super emitCode: stack args: nArgs encoder: encoder super: supered]
		ifFalse:
			[stack pop: nArgs.
			 encoder genSendSpecial: code numArgs: nArgs]
]

{ #category : 'code generation' }
SpecialSelectorNode >> sizeCode: encoder args: nArgs super: supered [
	"Size a special selector send.
	 A super send of a special selector must be handled like a normal send."
	^supered
		ifTrue: [super sizeCode: encoder args: nArgs super: supered]
		ifFalse: [encoder sizeSendSpecial: code numArgs: nArgs]
]
