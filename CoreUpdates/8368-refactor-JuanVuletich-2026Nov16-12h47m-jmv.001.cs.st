'From Cuis7.9 [latest update: #8366] on 16 November 2026 at 12:49:42 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'jmv 16/Nov/2026, 3:49:38 pm (UTC)'!
decompiledSource
	"Obtain a source string by decompiling the method's code.
	Also return the string."
	| class selector method |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	"Was method deleted while in another project?"
	method := class
		compiledMethodAt: selector
		ifAbsent: [ ^ '' ].
	currentCompiledMethod := method.
	^currentCompiledMethod decompileString! !

